library(lubridate)

# Create global variable "last_year" to use in server.R and ui.R,
# should the Ergast database not be loaded with location data before
# the start of a new year.
check_location_data_available <- function(yyyy=year(today())) {
  url <- paste0("http://ergast.com/api/f1/", yyyy, ".json")
  temp <- fromJSON(url, encoding="utf-8")
  if (length(temp$MRData$RaceTable$Races) > 0) return(yyyy)
  check_location_data_available(yyyy - 1)
}

last_year <- check_location_data_available()
rm(check_location_data_available)
