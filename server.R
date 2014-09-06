library(shiny)
library(rCharts)
library(RJSONIO)

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
  if (!is.null(loc[[paste0("y", year)]])) return(invisible(0))
  url <- paste0("http://ergast.com/api/f1/", year, ".json")
  temp <- fromJSON(url, encoding = "utf-8")

  # initialise result list for given year
  no_races <- as.numeric(temp$MRData$total)
  res[[paste0("y", year)]] <<- vector(mode = "list", length = no_races)

  loc[[paste0("y", year)]] <<- convert_locations(temp$MRData$RaceTable$Races)
  invisible(0)
}

# transform location data into a data.frame object
convert_locations <- function(d, no_races = length(d)) {
  local_data <-
    data.frame(
      round   = integer(no_races),
      circuit = character(no_races),
      lat     = numeric(no_races),
      long    = numeric(no_races),
      city    = character(no_races),
      url     = character(no_races),
      stringsAsFactors = FALSE
      )

  for (i in seq(no_races)) {
    local_data[i, ] <-
      list(
        as.integer(d[[i]]$round),
        d[[i]]$Circuit$circuitName,
        as.numeric(d[[i]]$Circuit$Location[1]),
        as.numeric(d[[i]]$Circuit$Location[2]),
        unname(d[[i]]$Circuit$Location[3]),
        d[[i]]$url
        )
  }

  local_data
}

# download race results for a given year and race
download_results <- function(year, round) {
  if (!is.null((res[[paste0("y", year)]][[round]]))) return(invisible(0))
  url <- paste("http://ergast.com/api/f1", year, round, "results.json",
               sep = "/")
  temp <- fromJSON(url, encoding = "utf-8")
  res[[paste0("y", year)]][[round]] <<-
    convert_results(temp$MRData$RaceTable$Races[[1]]$Results)
  invisible(0)
}

# transform results data into a data.frame object
convert_results <- function(d, no_drivers = length(d)) {
  result_list <-
    data.frame(
      Position    = integer(no_drivers),
      Number      = integer(no_drivers),
      Driver      = character(no_drivers),
      Constructor = character(no_drivers),
      Laps        = integer(no_drivers),
      Grid        = integer(no_drivers),
      Time        = character(no_drivers),
      Status      = character(no_drivers),
      Points      = numeric(no_drivers),
      stringsAsFactors = FALSE
      )

  for (i in seq(no_drivers)) {
    result_list[i, ] <-
      list(
        as.integer(d[[i]]$position),
        as.integer(d[[i]]$number),
        paste(d[[i]]$Driver["givenName"], d[[i]]$Driver["familyName"],
              sep = " "),
        d[[i]]$Constructor["name"],
        as.integer(d[[i]]$laps),
        as.integer(d[[i]]$grid),
        if (!is.null(d[[i]]$Time[2])) d[[i]]$Time[2] else "",
        d[[i]]$status,
        as.numeric(d[[i]]$points)
      )
  }

  result_list
}




# server logic
shinyServer(function(input, output, session) {

  # retrieve all races for selected year
  selected_year <- reactive({
    download_locations(input$year)
    loc[[paste0("y", input$year)]]
  })

  # retrieve selected circuit
  selected_circuit <- reactive({
    if (length(input$circuit) == 0L || input$circuit == "") return()
    selected_year()[selected_year()$circuit == input$circuit, ]
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
                     encoding = "utf-8")
    list(
      last_year  = as.numeric(temp$MRData$RaceTable$season),
      last_round = as.numeric(temp$MRData$RaceTable$round)
    )
  })

  # generate years dynamically (render UI)
  output$year_list <- renderUI({
    selectInput(inputId = "year", label = "Select year:",
                choices = c(seq(yr()$last_year, 1950, by = -1)))
  })

  # generate circuit list for past race events
  circuits <- reactive({
    # test for NULL when the app starts and UI list is not yet created
    if (is.null(input$year)) return()
    if (input$year == yr()$last_year) {
      selected_year()[1:yr()$last_round, "circuit"]
    } else {
      selected_year()[ ,"circuit"]
    }
  })

  # generate circuits dynamically (render UI)
  output$circuit_list <- renderUI({
    selectInput(inputId = "circuit", label = "Select circuit:",
                choices = c("", circuits()))
  })

  # retrieve result data
  result_data <- reactive({
    # return NULL if circuit is not selected (default behaviour)
    if (is.null(selected_circuit())) return()
    round <- as.numeric(selected_circuit()["round"])


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
    if (is.na(round)) return()

    download_results(input$year, round)
    res[[paste0("y", input$year)]][[round]]
  })

  # pass text to output
  output$text <- renderText({
    "Select circuit above to display race results"
  })

  # pass table to output
  output$table <- renderDataTable({
    result_data()
    },
    options = list(
      iDisplayLength = 10,
      aLengthMenu    = c(3, 10, nrow(result_data()))
      )
    )

  # pass Wikipedia link to output (render UI)
  output$wikipedia <- renderUI({
    # return NULL if circuit is not selected (default behaviour)
    if (is.null(selected_circuit())) return()
    url <- selected_circuit()[ ,"url"]

    # this guardian is related to Issue #1
    # Resolve:
    # - if error, url is evaluated to character(0)
    # - fix by testing on zero length
    if (length(url) == 0L) return()

    tags$a(href = url, target = "_blank", "Race info on Wikipedia")
    })

  # pass text or table to output (render UI)
  output$text_or_table <- renderUI({
    if (is.null(result_data())) return(textOutput("text"))
    dataTableOutput("table")
  })

  # pass map to output (render UI)
  output$map <- renderMap({

    # leaflet map
    lmap <- Leaflet$new()
    lmap$set(width = 850, height = 420)

    if (!is.null(selected_circuit())) {
      circuit <- selected_circuit()[ ,c("circuit", "lat", "long")]
      lmap$setView(c(circuit$lat, circuit$long), zoom = 12)
      lmap$marker(c(circuit$lat, circuit$long), bindPopup = circuit$circuit)
    } else {
      positions <- selected_year()[ ,c("circuit", "lat", "long")]
      lmap$setView(c(20, 15), zoom = 2)
      for (i in seq(nrow(positions))) {
        lmap$marker(c(positions$lat[i], positions$long[i]),
                    bindPopup = positions$circuit[i])
      }
    }

    lmap
  })
})
