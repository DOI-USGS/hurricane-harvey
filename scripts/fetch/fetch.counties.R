fetch.counties <- function(viz){

  library(httr)

  query <- 'http://cida.usgs.gov/gdp/geoserver/wfs?service=WFS&request=GetFeature&typeName=derivative:US_Counties&outputFormat=shape-zip&version=1.0.0'
  file <- GET(query, write_disk(viz[['location']], overwrite=T), progress())
  
}



#' take map arguments and return a projected sp object
#' 
#' @param \dots arguments passed to \code{\link[maps]{map}} excluding \code{fill} and \code{plot}
#' 
library(maps)
library(sp)
proj.string <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
to_sp <- function(...){
  library(maptools)
  library(maps)
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, CRS(proj.string))
  return(map.sp.t)
}


fetch.states <- function(viz){
  states.out <- to_sp('state')
  states.out <- rbind(states.out, to_sp('world','Mexico'))
  
  saveRDS(states.out, viz[['location']])
}

fetch.site_footy <- function(viz){
  library(httr)
  
  query <- 'http://cida.usgs.gov/gdp/geoserver/wfs?service=WFS&request=GetFeature&typeName=draw:Harvey_poly2&outputFormat=shape-zip&version=1.0.0'
  file <- GET(query, write_disk(viz[['location']], overwrite=T), progress())
}