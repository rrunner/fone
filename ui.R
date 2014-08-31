library(shiny)
library(rCharts)

# user interface
shinyUI(fluidPage(

  # application title
  titlePanel(title       = "Formula 1: Race locations and results",
             windowTitle = "fone"),

  # user inputs (rendered by server)
  fluidRow(
    column(2,
           uiOutput("year_list")
    ),
    column(8,
           uiOutput("circuit_list")
    )
  ),

  # display plot and result list in tabs
  mainPanel(
    tabsetPanel(
      tabPanel("Race locations", showOutput("map", "leaflet"),
               value = "location"),
      tabPanel("Race results",
               uiOutput("wikipedia"),
               tags$br(),
               uiOutput("text_or_table"), value = "result"),
      id   = "whichTab",
      type = "pills"
      ),
    width = 10)
  )
)
