
fetch.stats <- function(viz){
  library(dataRetrieval)
  library(lubridate)
  library(dplyr)
  required <- c("depends", "location")
  checkRequired(viz, required)
  
  #stats service
  sites <- readData(viz[['depends']])
  
  reqBks <- seq(1,nrow(sites),by=10)
  
  stat.df <- data.frame()
  
  for(i in reqBks) {
    getSites <- sites$site_no[i:(i+9)]
    
    currentSites <- readNWISstat(siteNumbers = getSites,
                                 parameterCd = "00060", 
                                 statReportType="daily",
                                 statType=c("p10","p25","p50","p75","p90","mean"))
    
    currentSites[,grep("_va",names(currentSites))] <- sapply(currentSites[,grep("_va",names(currentSites))], function(x) as.numeric(x))
    
      
    
    stat.df <- bind_rows(stat.df,currentSites)
  }
  
  location <- viz[['location']]
  saveRDS(stat.df, file=location)
}
