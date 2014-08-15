library(RJSONIO)

# Create global variable "last_year" to use in server.R and ui.R,
# should the Ergast database not be loaded with location data before
# the start of a new year.
check_location_data_available <- function(yyyy) {
  url <- paste0("http://ergast.com/api/f1/", yyyy, ".json")
  temp <- fromJSON(url, encoding="utf-8")
  if (length(temp$MRData$RaceTable$Races) > 0) return(yyyy)
  check_location_data_available(yyyy - 1)
}

current_year <- as.numeric(format(Sys.time(), "%Y"))
last_year <- check_location_data_available(current_year)
rm(check_location_data_available, current_year)
