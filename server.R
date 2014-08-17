library(shiny)
library(rCharts)
library(RJSONIO)

# store location data (mutable environment to enable caching)
loc <- new.env(parent=emptyenv())
loc <- vector(mode="list", length=length(seq(last_year, 1950, by=-1)))
loc <- setNames(loc, paste0("y", seq(last_year, 1950, by=-1)))

# download race locations for a given year
download_locations <- function(year) {
  if (!is.null(loc[[paste0("y", year)]])) return(invisible(0))
  url <- paste0("http://ergast.com/api/f1/", year, ".json")
  temp <- fromJSON(url, encoding="utf-8")
  loc[[paste0("y", year)]] <<- convert_locations(temp$MRData$RaceTable$Races)
  invisible(0)
}

# transform location data into a data.frame object
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




# store result data (mutable environment to enable caching)
# defaults to max 30 races per year
res <- new.env(parent=emptyenv())
res <- vector(mode="list", length=length(seq(last_year, 1950, by=-1)))
res <- setNames(res, paste0("y", seq(last_year, 1950, by=-1)))
res <- lapply(res, function(x) vector(mode="list", length=30))

# download race results for a given year and race
download_results <- function(year, round) {
  if (!is.null((res[[paste0("y", year)]][[round]]))) return(invisible(0))
  url <- paste("http://ergast.com/api/f1", year, round, "results.json", sep="/")
  temp <- fromJSON(url, encoding="utf-8")
  res[[paste0("y", year)]][[round]] <<-
    convert_results(temp$MRData$RaceTable$Races[[1]]$Results)
  invisible(0)
}

# transform results data into a data.frame object
convert_results <- function(d, no_drivers=length(d)) {
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




# get position for any given circuit
get_position <- function(d, circuit) {
  d[d$circuit == circuit, c("lat", "long")]
}




# server logic
shinyServer(function(input, output, session) {

  # retrieve location data
  local_data <- reactive({
    download_locations(input$year)
    loc[[paste0("y", input$year)]]
  })

  # generate circuits to UI dynamically
  output$circuit_list <- renderUI({
    selectInput(inputId='circuit', label='Select circuit',
                choices=c("", local_data()[local_data()$event_occurred == TRUE,
                                           "circuit"]))
  })

  # retrieve result data
  result_data <- reactive({
    # return NULL if circuit is not selected (default behaviour)
    if (input$circuit == "") return()
    round <- local_data()[local_data()$circuit == input$circuit, "round"]
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

    # leaflet map
    lmap <- Leaflet$new()
    lmap$set(width=850, height=420)

    if (!is.null(input$circuit) && input$circuit != "") {
      pos <- get_position(positions, input$circuit)
      lmap$setView(c(pos$lat, pos$long), zoom=12)
      lmap$marker(c(pos$lat, pos$long), bindPopup=input$circuit)
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
