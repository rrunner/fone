library("shiny")
library("rCharts")

# user interface
shinyUI(fluidPage(

  # import google fonts
  tags$head(
    tags$style(
      HTML("
        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
        @import url('//fonts.googleapis.com/css?family=Quicksand|Cabin:400,700');
      ")
    )
  ),

  # embed css
  titlePanel(
    title = list(
              h1("fone", style = "font-family: 'Lobster', cursive;
                        font-weight: 500; line-height: 1.1; color: #006dcc;"
                 ),
              h4("Formula 1 race presenter",
                  style = "font-family: 'Quicksand', cursive;
                  font-weight: 500; line-height: 0.3; color: #000000;")
              ),
    windowTitle = "fone"
    ),

  tags$br(),

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

      tabPanel(title = "Race locations",
               showOutput("map", "leaflet"),
               value = "map"
               ),

      tabPanel(title = "Race result",
               tags$br(),

               # uiOutput("wikipedia") returns
               # <div class="shiny-html-output"></div>
               # if wikipedia object is NULL (no output in UI)
               uiOutput("wikipedia"),
               tags$br(),
               tags$br(),
               uiOutput("text_or_table"),
               value = "result"
               ),
      id   = "whichTab",
      type = "pills"
      ),
    width = 10)
  )
)
