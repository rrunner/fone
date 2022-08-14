library("shiny")
library("leaflet")
library("DT")
library("jsonlite")

# add this dependency to prevent shinyApps build error
# this seems to be related to jsonlite
library("curl")

# current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# store location data (mutable environment to enable caching)
loc <- new.env(parent = emptyenv())
loc <- vector(mode = "list", length = length(seq(current_year, 1950, by = -1)))
loc <- setNames(loc, paste0("y", seq(current_year, 1950, by = -1)))

# store result data (mutable environment to enable caching)
res <- new.env(parent = emptyenv())
res <- vector(mode = "list", length = length(seq(current_year, 1950, by = -1)))
res <- setNames(res, paste0("y", seq(current_year, 1950, by = -1)))

# download race locations for a given year
download_locations <- function(year) {
  if (!is.null(loc[[paste0("y", year)]])) {
    return(invisible(0))
  }
  url <- paste0("http://ergast.com/api/f1/", year, ".json")
  temp <- fromJSON(url, flatten = TRUE)

  # initialise result list for given year
  no_races <- as.numeric(temp$MRData$total)
  res[[paste0("y", year)]] <<- vector(mode = "list", length = no_races)

  keep <- c(
    "round",
    "Circuit.circuitName",
    "Circuit.Location.lat",
    "Circuit.Location.long",
    "Circuit.Location.locality",
    "url"
  )
  df <- temp$MRData$RaceTable$Races[, keep]
  names(df) <- c("round", "circuit", "lat", "long", "city", "url")
  loc[[paste0("y", year)]] <<- df
  invisible(0)
}

# download race results for a given year and race
download_results <- function(year, round) {
  if (!is.null((res[[paste0("y", year)]][[round]]))) {
    return(invisible(0))
  }
  url <- paste("http://ergast.com/api/f1", year, round, "results.json",
    sep = "/"
  )
  temp <- fromJSON(url, flatten = TRUE)
  df <- temp$MRData$RaceTable$Races$Results[[1]]

  # concatenate drivers full name
  df <- within(df, {
    driver <- paste(Driver.givenName, Driver.familyName)
    rm(Driver.givenName, Driver.familyName)
  })

  # reduce data frame and reorder columns
  keep <- c(
    "position", "number", "driver", "Constructor.name",
    "laps", "grid", "Time.time", "status", "points"
  )
  df <- df[, keep]

  names(df) <- c(
    "Position", "Number", "Driver", "Constructor",
    "Laps", "Grid", "Time", "Status", "Points"
  )
  res[[paste0("y", year)]][[round]] <<- df
  invisible(0)
}

# server logic
shinyServer(function(input, output, session) {

  # retrieve all races for selected year
  selected_year <- reactive({
    input_year <- input$year
    download_locations(input_year)
    loc[[paste0("y", input_year)]]
  })

  # retrieve selected round and circuit
  selected_circuit <- reactive({
    input_circuit <- input$circuit
    if (length(input_circuit) == 0L || input_circuit == "") {
      return()
    }
    round_circuit <- unlist(strsplit(input_circuit, " - "))
    round <- as.numeric(round_circuit[1])
    circuit <- round_circuit[2]
    sy <- selected_year()
    sy[sy$round == round & sy$circuit == circuit, ]
  })

  # focus world map when user selects another year
  observe({
    input$year
    updateTabsetPanel(session, inputId = "whichTab", selected = "map")
  })

  # retrieve year and round of last updated race in Ergast database
  yr <- reactive({
    # make a poll every 10 minutes
    invalidateLater(6e+05, session)
    temp <- fromJSON("http://ergast.com/api/f1/current/last.json",
      flatten = TRUE
    )
    list(
      last_year  = as.numeric(temp$MRData$RaceTable$season),
      last_round = as.numeric(temp$MRData$RaceTable$round)
    )
  })

  # generate years dynamically (render UI)
  output$year_list <- renderUI({
    selectInput(
      inputId = "year", label = "Select year:",
      choices = c(seq(yr()$last_year, 1950, by = -1))
    )
  })

  # generate circuits for past race events dynamically (render UI)
  output$circuit_list <- renderUI({
    # return empty circuit list when the app starts
    # - to make the 'year_list' and 'circuit_list' render simultanously in UI
    if (is.null(input$year)) {
      return(
        selectInput(
          inputId = "circuit",
          label = "Select circuit:",
          choices = ""
        )
      )
    }

    if (input$year == yr()$last_year) {
      circuits <- selected_year()[1:yr()$last_round, c("round", "circuit")]
      circuits <- paste(circuits$round, circuits$circuit, sep = " - ")
    } else {
      circuits <- selected_year()[, c("round", "circuit")]
      circuits <- paste(circuits$round, circuits$circuit, sep = " - ")
    }

    selectInput(
      inputId = "circuit",
      label = "Select circuit:",
      choices = c("", circuits)
    )
  })

  # retrieve result data
  result_data <- reactive({
    # return NULL if circuit is not selected (default behaviour)
    selected_circuit <- selected_circuit()
    if (is.null(selected_circuit)) {
      return()
    }
    round <- as.numeric(selected_circuit["round"])


    # Issue #1
    # Replicate issue:
    # - browse the result tab and select a circuit (*)
    # - select a new year
    # -  -> circuit_list gets updated
    # - would expect that input$circuit == "" and selected_year() becomes NULL
    #    -> instead selected_year() contains data for the new selected year
    #    -> input$circuit holds previous value (at *)
    # - an error occurs if selected_year() does not contain input$circuit
    # Resolve:
    # - if error, round is evaluated to NA
    # - fix by testing on is.na()
    if (is.na(round)) {
      return()
    }

    input_year <- input$year
    download_results(input_year, round)
    res[[paste0("y", input_year)]][[round]]
  })

  # pass text to output
  output$text <- renderText({
    "Select circuit above to display race results"
  })

  # pass table to output
  output$table <- renderDT(
    {
      result_data()
    },
    options = list(
      pageLength = 10,
      lengthMenu = c(3, 10, nrow(result_data()))
    )
  )

  # pass Wikipedia link to output (render UI)
  output$wikipedia <- renderUI({
    # return NULL if circuit is not selected (default behaviour)
    selected_circuit <- selected_circuit()
    if (is.null(selected_circuit)) {
      return()
    }
    url <- selected_circuit[, "url"]

    # this guardian is related to Issue #1
    # Resolve:
    # - if error, url is evaluated to character(0)
    # - fix by testing on zero length
    if (length(url) == 0L) {
      return()
    }

    tags$a(href = url, target = "_blank", "Race info on Wikipedia")
  })

  # pass text or table to output (render UI)
  output$text_or_table <- renderUI({
    if (is.null(result_data())) {
      return(textOutput("text"))
    }
    DTOutput("table")
  })

  # pass map to output (render UI)
  output$map <- renderLeaflet({

    # leaflet map
    lmap <- leaflet()
    lmap <- addTiles(lmap)

    # fetch circuit data
    selected_circuit <- selected_circuit()

    # draw map
    if (is.null(selected_circuit) || nrow(selected_circuit) == 0) {
      # world map
      positions <- selected_year()[, c("circuit", "lat", "long")]
      lmap <- setView(lmap, lng = 15, lat = 20, zoom = 2)
      lmap <- addMarkers(
        lmap,
        lng = as.numeric(positions$long),
        lat = as.numeric(positions$lat),
        popup = positions$circuit
      )
    } else {
      # circuit map
      circuit <- selected_circuit[, c("circuit", "lat", "long")]
      lmap <- setView(
        lmap,
        lng = as.numeric(circuit$long),
        lat = as.numeric(circuit$lat),
        zoom = 12
      )
      lmap <- addMarkers(
        lmap,
        lng = as.numeric(circuit$long),
        lat = as.numeric(circuit$lat),
        popup = circuit$circuit
      )
    }

    lmap
  })
})
