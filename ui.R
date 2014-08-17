library(shiny)
library(rCharts)

# user interface
shinyUI(fluidPage(

  # application title
  titlePanel("Formula 1: Race locations and results"),

  # user inputs
  fluidRow(
    column(2,
           selectInput('year', 'Select year:',
                       choices=seq(last_year, 1950, by=-1))
    ),
    column(8,
           uiOutput('circuit_list')
    )
  ),

  # display plot and result list in tabs
  mainPanel(
    tabsetPanel(
      tabPanel("Race locations", showOutput('map', 'leaflet'), value="location"),
      tabPanel("Race results", uiOutput("text_or_table"), value="result"),
      id="whichTab",
      type="pills"
    ), width=10
  )
))
