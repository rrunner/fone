library(shiny)
suppressPackageStartupMessages(library(ggplot2))
library(RJSONIO)
library(maps)
library(lubridate)

# world map
map_theme <- theme(axis.line = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title = element_blank(),
                   panel.grid = element_blank(),
                   panel.background = element_rect(fill = "lightsteelblue"))

map <- ggplot() + coord_cartesian(ylim=c(-60, 70), xlim=c(-150, 180)) +
       borders(database="world", colour="grey", fill="darkseagreen") +
       map_theme




# list of location data (to enable caching)
loc_lst <- new.env(parent=emptyenv())
loc_lst <- vector(mode="list", length=length(seq(year(today()), 1950, by=-1)))
loc_lst <- setNames(loc_lst, paste0("y", seq(year(today()), 1950, by=-1)))

# download file with race locations for a year
download_locations <- function(year) {
  if (!is.null(loc_lst[[paste0("y", year)]])) return(invisible(0))
  url <- paste0("http://ergast.com/api/f1/", year, ".json")
  temp <- fromJSON(url, encoding="utf-8")
  loc_lst[[paste0("y", year)]] <<- convert_locations(temp$MRData$RaceTable$Races)
  return(invisible(0))
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
res_lst <- vector(mode="list", length=length(seq(year(today()), 1950, by=-1)))
res_lst <- setNames(res_lst, paste0("y", seq(year(today()), 1950, by=-1)))
res_lst <- lapply(res_lst, function(x) vector(mode="list", length=30))

# download file with race results for a given race and year
download_results <- function(year, round) {
  if (!is.null((res_lst[[paste0("y", year)]][[round]]))) return(invisible(0))
  url <- paste("http://ergast.com/api/f1", year, round, "results.json", sep="/")
  temp <- fromJSON(url, encoding="utf-8")
  res_lst[[paste0("y", year)]][[round]] <<-
    get_result_list(temp$MRData$RaceTable$Races[[1]]$Results)
  return(invisible(0))
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




# get position for a given circuit
get_position <- function(d, circuit) {
  d[which(d$circuit == circuit), c("lat", "long")]
}




# server logic
shinyServer(function(input, output, session) {

  # retrieve location data
  local_data <- reactive({
    download_locations(input$year)
    loc_lst[[paste0("y", input$year)]]
  })

  # retrieve result data
  result_data <- reactive({
    if (input$circuit == "") return(data.frame())
    round <- local_data()[local_data()$circuit == input$circuit, "round"]
    download_results(input$year, round)
    res_lst[[paste0("y", input$year)]][[round]]
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

  # pass text to output
  output$text <- renderText({
    "No data in table. Select circuit above."
  })

  # pass table to output
  output$table <- renderDataTable({
    result_data()
  })

  # pass UI to output
  output$text_or_table <- renderUI({
    if (nrow(result_data()) == 0) textOutput("text")
    else dataTableOutput("table")
  })

  # pass plot to output
  output$plot <- renderPlot({
    positions <- local_data()[ ,c("circuit", "long", "lat")]
    map + geom_point(data=positions, aes(long, lat), color="black", size=1.5) +
          geom_point(data=get_position(positions, input$circuit),
                     aes(long, lat), color="red", size=1.5)
  })
})
