process.classifyBins <- function(viz){
  library(dplyr)
  
  #need to get bins
  
  precip_breaks <- readData(viz[['depends']][1]) 
  precipData <- readData(viz[['depends']][2]) 

  precipData$precipVal <- precipData$precipVal/25.4 #convert mm to inches
  
  precipData <- precipData %>% mutate(cols = cut(precipVal, breaks = precip_breaks, labels = FALSE)) %>% 
    mutate(cols = ifelse(precipVal > tail(precip_breaks,1), length(precip_breaks), cols)) %>% 
    mutate(cols = ifelse(is.na(cols), 1, cols), cols = as.character(cols)) %>% select(fips, DateTime, cols)
    
  
  #want to cut down precipData to only relevant info?
  
  saveRDS(object = precipData, file = viz[['location']])
}

process.precip_breaks <- function(viz){
  colSteps <- readData(viz[['depends']][1]) #vector of actual color palette codes
  precip_breaks <- seq(0,by=0.1, length.out =length(colSteps))
  saveRDS(object = precip_breaks, file = viz[['location']])
}
