epsg_code <- '+init=epsg:3082' 


fetch.harvey_counties <- function(viz = as.viz('harvey-counties')){
  library(rgeos)
  library(dplyr)
  states <- viz[['states']]

  counties <- readData(viz[['depends']][1])
  counties <- counties[counties$STATE %in% states, ]
  FIPs <- as.character(counties$FIPS)
  countyName <- paste0(as.character(counties$COUNTY),', ', as.character(counties$STATE))
  counties <- rgeos::gSimplify(counties, 0.001)
  
  footy <- readData(viz[['depends']][2]) %>% spTransform(CRS(epsg_code)) %>% 
    gBuffer(width=200000, byid=TRUE )
  counties <- spTransform(counties, CRS(epsg_code))
  overlap <- gContains(footy, counties, byid = TRUE) %>% rowSums() %>% as.logical()
  
  
  
  counties <- SpatialPolygonsDataFrame(counties[overlap, ], 
                                       data = data.frame(FIPS=FIPs[overlap], countyName = countyName[overlap]), 
                                       match.ID = FALSE)
  saveRDS(counties, viz[['location']])
}




process.harvey_states <- function(viz){
  library(rgeos)
  library(sp)
  
  
  states <- readData(viz[['depends']])
  
  states <- rgeos::gSimplify(states, 0.01)
  states <- sp::spTransform(states, CRS(epsg_code))
  
  saveRDS(states, viz[['location']])
}

process.harvey_stateborders <- function(viz){
  library(rgeos)
  library(sp)
  include.states <- c("texas", "louisiana")
  states <- readData(viz[['depends']])
  states <- states[names(states) %in% include.states, ]
  states <- rgeos::gSimplify(states, 0.01)
  states <- spTransform(states, CRS(epsg_code))
  
  saveRDS(states, viz[['location']])
}

process.harvey_track <- function(viz){
  library(rgeos)
  library(sp)
  
  counties <- readData(viz[['depends']][2])
  track <- readData(viz[['depends']][1])
  track <- spTransform(track, CRS(proj4string(counties)))
  
  # here do "over" analysis for masking?
  
  saveRDS(track, viz[['location']])
}

# viz <- yaml.load_file("viz.yaml")
# viz <- viz$process
# viz <- viz[[which(unlist((lapply(viz, function(x) x$id == "harvey-sites"))))]]

fetch.harvey_sites <- function(viz=as.viz("harvey-sites")){
  library(rgeos)
  library(sp)
  library(dplyr)
  ignore.sites <- c('08041780', '08211503', '08028500', '08067070') # sites that hydropeak or are otherwise not representative
  counties <- readData(viz[['depends']][2])
  sites <- readData(viz[['depends']][1]) %>% 
    filter(!site_no %in% ignore.sites) %>% 
    arrange(desc(dec_lat_va))
  footy <- readData(viz[['depends']][3]) %>% spTransform(CRS(proj4string(counties))) %>% 
    gBuffer(width=20000, byid=TRUE )
  pts <- cbind(sites$dec_long_va, sites$dec_lat_va)
  sites <- SpatialPointsDataFrame(pts, proj4string=CRS("+proj=longlat +datum=WGS84"), 
                                     data = sites %>% select(site_no, station_nm) %>% data.frame)
  sites <- spTransform(sites, CRS(proj4string(counties)))
  overlap <- gContains(footy, sites, byid = TRUE) %>% rowSums() %>% as.logical()
  
  # here do "over" analysis for masking?
  
  saveRDS(sites[overlap, ], viz[['location']])
}

fetch.non_harvey_sites <- function(viz=as.viz("non-harvey-sites")){
  library(rgeos)
  library(sp)
  library(dplyr)
  sites <- readData(viz[['depends']][1])
  harvey.sites <- readData(viz[['depends']][2])
  sites <- filter(sites, !site_no %in% harvey.sites$site_no)
  pts <- cbind(sites$dec_long_va, sites$dec_lat_va)
  sites <- SpatialPointsDataFrame(pts, proj4string=CRS("+proj=longlat +datum=WGS84"), 
                                  data = sites %>% select(site_no, station_nm) %>% data.frame)
  sites <- spTransform(sites, CRS(proj4string(harvey.sites)))
  
  saveRDS(sites, viz[['location']])
}


process.timesteps <- function(viz){
  #"classifyBins",'storm-location'
  library(dplyr)
  library(jsonlite)
  times <- readData(viz[['depends']][1]) %>% select(DateTime) %>% 
    unique() %>% .$DateTime %>% as.POSIXct %>% format('%b %d %I:%M %p')
  cat(jsonlite::toJSON(list(times=times)), file = viz[['location']])
}

library(svglite)
library(xml2)
grab_spark <- function(vals){
  
  x = svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0,0,0,0))
    plot(vals, type='l', axes=F, ann=F)
  }, height=0.4, width=2)
  xml2::xml_attr(xml2::xml_find_first(x, '//*[local-name()="polyline"]'),'points')
}

process.discharge_sparks <- function(viz){
  library(dplyr)
  disch <- readData(viz[['depends']][1])
  times <- readData(viz[['depends']][2]) %>% .$times %>% 
    as.POSIXct(format = '%b %d %I:%M %p', tz= "America/New_York")
  interp_q <- function(x,y){
    approx(x, y, xout = times)$y %>% grab_spark
  }
  sparks <- group_by(disch, site_no) %>% filter(min(dateTime) <= times[2], max(dateTime) >= tail(times, 2L)[2]) %>% 
    summarize(points = interp_q(dateTime, Flow_Inst))
  saveRDS(sparks, viz[['location']])
}
process.storm_location <- function(viz){
  
  library(rgeos)
  library(sp)
  library(foreign)
  library(dplyr)
  
  shp.path <- file.path(tempdir(), 'tmp')
  if (!dir.exists(shp.path)){
    dir.create(shp.path)
  }
  unzip('cache/harvey.zip', exdir = shp.path)
  
  as.time <- function(YEAR, MONTH, DAY, HHMM){
    as.POSIXct(sprintf('%s-%s-%s %s', YEAR, MONTH, DAY, HHMM), format='%Y-%m-%d %H%M', tz="America/New_York")
  }
  
  warning('NWS file pattern is hard-coded here and is storm-specific')
  
  shp.data <- rgdal::readOGR(shp.path, layer = 'AL092017_pts') %>% data.frame %>% 
    filter(STORMNAME=="HARVEY") %>% 
    mutate(DateTime = as.time(YEAR, MONTH, DAY, HHMM)) %>% 
    select(LAT, LON, DateTime, INTENSITY)
  
  unlink(shp.path)
  
  startDate <- as.POSIXct(paste(viz[["start.date"]],"12:00:00"), tz="America/New_York")
  endDate <- as.POSIXct(paste(viz[["end.date"]],"22:00:00"), tz="America/New_York")
  
  t.out <- seq(startDate, by='hours', to = endDate)
  
  lat.out <- approx(shp.data$DateTime, shp.data$LAT, xout = t.out)$y
  lon.out <- approx(shp.data$DateTime, shp.data$LON, xout = t.out)$y
  pts <- cbind(lon.out[!is.na(lon.out)], lat.out[!is.na(lon.out)])
  location <- SpatialPoints(pts, proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  counties <- readData(viz[['depends']][2])
  
  location <- spTransform(location, CRS(proj4string(counties)))
  
  saveRDS(location, viz[['location']])
}
