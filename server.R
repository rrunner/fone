library(shiny)
library(rCharts)
library(RJSONIO)

# list of location data (to enable caching)
loc_lst <- new.env(parent=emptyenv())
loc_lst <- vector(mode="list", length=length(seq(last_year, 1950, by=-1)))
loc_lst <- setNames(loc_lst, paste0("y", seq(last_year, 1950, by=-1)))

# download file with race locations for a year
download_locations <- function(year) {
  if (!is.null(loc_lst[[paste0("y", year)]])) return(invisible(0))
  url <- paste0("http://ergast.com/api/f1/", year, ".json")
  temp <- fromJSON(url, encoding="utf-8")
  loc_lst[[paste0("y", year)]] <<- convert_locations(temp$MRData$RaceTable$Races)
  invisible(0)
}

# transform lists of location data into a data.frame object
convert_locations <- function(d, no_races=length(d)) {
  local_data <- data.frame(round=integer(no_races),
                           circuit=character(no_races),
                           lat=numeric(no_races),
                           long=numeric(no_races),
                           city=character(no_races),
                           event_occurred=logical(no_races),
                           stringsAsFactors=FALSE)

  for (i in seq(no_races)) {
    local_data[i, ] <- list(as.integer(d[[i]]$round),
                            d[[i]]$Circuit$circuitName,
                            as.numeric(d[[i]]$Circuit$Location[1]),
                            as.numeric(d[[i]]$Circuit$Location[2]),
                            unname(d[[i]]$Circuit$Location[3]),
                            as.Date(d[[i]]$date) < Sys.Date())
  }

  local_data
}




# list of result data (to enable caching), defaults to max 30 races per year
res_lst <- new.env(parent=emptyenv())
res_lst <- vector(mode="list", length=length(seq(last_year, 1950, by=-1)))
res_lst <- setNames(res_lst, paste0("y", seq(last_year, 1950, by=-1)))
res_lst <- lapply(res_lst, function(x) vector(mode="list", length=30))

# download file with race results for a given race and year
download_results <- function(year, round) {
  if (!is.null((res_lst[[paste0("y", year)]][[round]]))) return(invisible(0))
  url <- paste("http://ergast.com/api/f1", year, round, "results.json", sep="/")
  temp <- fromJSON(url, encoding="utf-8")
  res_lst[[paste0("y", year)]][[round]] <<-
    get_result_list(temp$MRData$RaceTable$Races[[1]]$Results)
  invisible(0)
}

# transform lists of results data into a data.frame object
get_result_list <- function(d, no_drivers=length(d)) {
  result_list <- data.frame(Position=integer(no_drivers),
                            Number=integer(no_drivers),
                            Driver=character(no_drivers),
                            Constructor=character(no_drivers),
                            Laps=integer(no_drivers),
                            Grid=integer(no_drivers),
                            Status=character(no_drivers),
                            Points=numeric(no_drivers),
                            stringsAsFactors=FALSE)

  for (i in seq(no_drivers)) {
    result_list[i, ] <- list(as.integer(d[[i]]$position),
                             as.integer(d[[i]]$number),
                             paste(d[[i]]$Driver["givenName"],
                                   d[[i]]$Driver["familyName"], sep=" "),
                             d[[i]]$Constructor["name"],
                             as.integer(d[[i]]$laps),
                             as.integer(d[[i]]$grid),
                             d[[i]]$status,
                             as.numeric(d[[i]]$points))
  }

  result_list
}




# get position for a given circuit (returns an numeric vector)
get_position <- function(d, circuit) {
  as.numeric(d[d$circuit == circuit, c("lat", "long")])
}




# server logic
shinyServer(function(input, output, session) {

  # retrieve location data
  local_data <- reactive({
    download_locations(input$year)
    loc_lst[[paste0("y", input$year)]]
  })

  # retrieve circuits data
  circuits <- reactive({
    local_data()[local_data()$event_occurred == TRUE, "circuit"]
  })

  # generate circuits to input list dynamically (no output)
  observe({
    updateSelectInput(session=session, inputId="circuit",
                      choices=circuits(),
                      selected="")
  })

  # retrieve result data
  result_data <- reactive({
    # return NULL if circuit is not selected (default behaviour)
    if (input$circuit == "") return()
    round <- local_data()[local_data()$circuit == input$circuit, "round"]
    download_results(input$year, round)
    res_lst[[paste0("y", input$year)]][[round]]
  })

  # pass text to output
  output$text <- renderText({
    "Select circuit above to display race results"
  })

  # pass table to output
  output$table <- renderDataTable({
    # fix to avoid the following in the log:
    # Error in fdata[1, 1] : incorrect number of dimensions
    # (this does not happen with renderTable)
    if (is.null(result_data())) return()
    result_data()
  }, options=list(iDisplayLength=10,
                  aLengthMenu=c(3, 10, nrow(result_data()))))

  # pass UI to output
  output$text_or_table <- renderUI({
    if (is.null(result_data())) return(textOutput("text"))
    dataTableOutput("table")
  })

  # pass map to output
  output$map <- renderMap({
    positions <- local_data()[ ,c("circuit", "lat", "long")]

    # Leaflet map
    lmap <- Leaflet$new()
    lmap$set(width=850, height=420)

    if (input$circuit != "") {
      pos <- get_position(positions, input$circuit)
      lmap$setView(c(pos), zoom=12)
      lmap$marker(c(pos), bindPopup=input$circuit)
    } else {
      lmap$setView(c(20, 15), zoom=2)
      for (i in seq(nrow(positions))) {
        lmap$marker(c(positions$lat[i], positions$long[i]),
                    bindPopup=positions$circuit[i])
      }
    }

    lmap
  })
})
