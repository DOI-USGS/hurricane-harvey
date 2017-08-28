# viz <- yaml.load_file("viz.yaml")
# viz <- viz$process
# viz <- viz[[10]]

process.sites <- function(viz){
  library(rgeos)
  library(sp)
  required <- c("depends", "location")
  checkRequired(viz, required)
  
  sites <- readData(viz[['depends']][1])
  counties <- readData(viz[['depends']][2])

  coordinates(sites) <- ~ dec_long_va + dec_lat_va
  proj4string(sites) <- CRS("+proj=longlat +ellps=GRS80 +no_defs")
  sites <- spTransform(sites, CRS(proj4string(counties)))
  
  saveRDS(sites, viz[['location']])
}