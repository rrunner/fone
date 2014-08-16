fone - a simple F1 result presenter
===================================

## Overview
*fone* is a [Shiny](http://shiny.rstudio.com/) application that displays locations and results of all past F1 races. 

*fone* fetches JSON objects from the [Ergast API](http://ergast.com/mrd/). The [leaflets](http://leafletjs.com/) maps are rendered by [rCharts](http://rcharts.io/). Shiny provides a reactive render function to encapsulate [DataTables](http://www.datatables.net/), which is utilised for the result list.

The application was from the beginning coursework for the [Coursera](https://www.coursera.org/) course *Developing Data Products* (July 2014). See Tag v0.1 for the final submitted project.
 
## Usage
The widgets allow the user to select any year and circuit from 1950 onwards.

## Access
Access [fone](https://rrunner.shinyapps.io/fone/)!
