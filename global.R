library(RJSONIO)

# Create global variable "last_year" and "last_round" to use in server.R and ui.R
#   - fetch the latest available race results in Ergast database
temp <- fromJSON("http://ergast.com/api/f1/current/last.json", encoding="utf-8")
last_year <- as.numeric(temp$MRData$RaceTable$season)
last_round <- as.numeric(temp$MRData$RaceTable$round)
rm(temp)
