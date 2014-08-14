library(shiny)
library(lubridate)

# user interface
shinyUI(fluidPage(

  # application title
  titlePanel("Formula 1: Race results"),

  # user inputs
  fluidRow(
    column(3,
           selectInput('year', 'Select year',
                       choices=seq(year(today()), 1950, by=-1))
    ),
    column(8,
           selectInput('circuit', 'Select circuit', choices=NULL)
    )
  ),

  # display plot and result list in tabs
  mainPanel(
    tabsetPanel(
      tabPanel("Race locations", plotOutput("plot")),
      tabPanel("Race results", uiOutput("text_or_table"))
    )
  )
))
