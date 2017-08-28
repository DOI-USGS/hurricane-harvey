#fetch NWIS iv data, downsample to hourly

fetch.discharge <- function(viz){
  library(dataRetrieval)
  library(lubridate)
  library(dplyr)
  
  required <- c("depends", "location")
  checkRequired(viz, required)
  
  precipData <- readData(viz[['depends']][1]) 
  site_geo <- readData(viz[['depends']][2])
  site_ids <- site_geo@data
  site_ids <- site_ids$site_no
  precipData$full_dateTime <- as.POSIXct(precipData$DateTime, tz="America/New_York")
  
  time.steps <- unique(precipData$DateTime)
  time.steps <-  gsub(" ","T", time.steps)
  
  start.date <-  time.steps[1]
  end.date <- time.steps[length(time.steps)]

  discharge <- renameNWISColumns(readNWISdata(service="iv",
                                         parameterCd="00060",
                                         sites = site_ids,
                                         startDate = start.date,
                                         endDate = end.date,
                                         tz = "America/New_York"))

  # discharge <- filter(discharge, minute(dateTime)==0)
  discharge <- filter(discharge, dateTime %in% precipData$full_dateTime)
  
  location <- viz[['location']]
  saveRDS(discharge, file=location)
}




