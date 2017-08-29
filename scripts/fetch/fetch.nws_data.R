fetch.nws_data <- function(viz=as.viz("nws_data")){

  library(dplyr)
  library(xml2)
  
  depend.data <- readDepends(viz)
  sites <- depend.data[["sites-data"]]
  nws_conversion <- depend.data[["nws_conversion"]]

  sites <- left_join(sites, 
                     select(nws_conversion, NWS, USGS), 
                     by = c("site_no"="USGS"))
  
  sites$flood.stage <- NA
  sites$flood.stage.units <- NA
  
  for(i in sites$NWS){
    url.site <- paste0("https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=",i,"&output=xml")
    
    return_list <- GET(url.site)
    returnedDoc <- content(return_list,encoding = "UTF-8")
    nws.site <- xml_root(returnedDoc)
    sigstages <- xml_find_all(nws.site, "sigstages")
    
    if(length(sigstages) > 0){

      sites$flood.stage[which(sites$NWS %in% i)] <- as.numeric(xml_text(xml_find_all(sigstages, "flood")))
      sites$flood.stage.units[which(sites$NWS %in% i)] <- xml_attr(xml_find_all(sigstages, "flood"),"units")
    }
  }
  
  location <- viz[['location']]
  saveRDS(sites, file=location)
}