library(shiny)
library(ggplot2)
library(RJSONIO)
library(maps)

# world map
map_theme <- theme(axis.line = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title = element_blank(),
                   panel.grid = element_blank(),
                   panel.background = element_rect(fill = "lightsteelblue"))

map <- ggplot() + coord_cartesian(ylim=c(-60, 70), xlim=c(-150, 180)) + borders(database="world", colour="grey", fill="darkseagreen") + map_theme

# download file with race locations for any year
download_locations <- function(year) {
  url <- paste0("http://ergast.com/api/f1/",year,".json")
  fromJSON(url, encoding="utf-8")
}

# transform lists of location data into a data.frame object
convert_locations <- function(d, no_races = length(d)) {
  local_data <- data.frame()
  current_date <- Sys.Date()

  for (i in (1:no_races)) {
    temp_data <- data.frame(d[[i]]$round,
                            d[[i]]$Circuit$circuitName,
                            as.numeric(d[[i]]$Circuit$Location[1]),
                            as.numeric(d[[i]]$Circuit$Location[2]),
                            unname(d[[i]]$Circuit$Location[3]),
                            as.Date(d[[i]]$date) < current_date)
    local_data <- rbind(local_data, temp_data)
  }

  names(local_data) <- c("round", "circuit", "lat", "long", "city",
                         "event_occurred")
  local_data
}

# download file with race results for any given race and year
download_results <- function(year, round) {
  url <- paste("http://ergast.com/api/f1", year, round, "results.json", sep="/")
  fromJSON(url, encoding="utf-8")
}

# transform lists of results data into a data.frame object
get_result_list <- function(d, no_drivers = length(d)) {
  result_list <- data.frame()

  for (i in (1:no_drivers)) {
    temp_data <- data.frame(d[[i]]$position,
                            d[[i]]$number,
                            paste(d[[i]]$Driver["givenName"],
                                  d[[i]]$Driver["familyName"], sep=" "),
                            d[[i]]$Constructor["name"],
                            d[[i]]$laps,
                            d[[i]]$grid,
                            d[[i]]$status,
                            d[[i]]$points)
    result_list <- rbind(result_list, temp_data)
  }

  names(result_list) <- c("Position", "Number", "Driver", "Constructor", "Laps",
                          "Grid", "Status", "Points")
  row.names(result_list) <- NULL
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
    loc <- download_locations(input$year)
    convert_locations(loc$MRData$RaceTable$Races)
  })

  # retrieve circuits data
  circuits <- reactive({
    local_data()[local_data()$event_occurred == TRUE, "circuit"]
  })

  # generate circuits to input list dynamically
  observe({
    updateSelectInput(session=session, inputId="circuit",
                      choices=circuits(),
                      selected=circuits()[1])
  })

  # retrieve results
  output$result_data <- renderTable({
    loc <- local_data()
    round <- as.integer(loc[loc$circuit ==
                            input$circuit, "round"])
    res <- download_results(input$year, round)
    get_result_list(res$MRData$RaceTable$Races[[1]]$Results)
  }, include.rownames=FALSE)

  # world map
  output$map.plot <- renderPlot({
    positions <- local_data()
    print(map +
          geom_point(data=positions, aes(long, lat), color="black", size=1) +
          geom_point(data=get_position(positions, input$circuit),
                     aes(long, lat), color="red", size=1))
  })
})
