library(readr)

fetch.nws_conversion <- function(viz){
  conversion.url <- "http://www.nws.noaa.gov/oh/hads/USGS/TX_USGS-HADS_SITES.txt"
  conversion.table <- read_delim(conversion.url,
                                 delim = "|",skip = 4,col_names = FALSE)

  names(conversion.table) <- c("NWS","USGS","GOES","NWS HSA","lat","lon","name")
  
  location <- viz[['location']]
  saveRDS(conversion.table, file=location)
}