
visualize.svg_base_map <- function(viz){
  geoms <- readDepends(viz)
  library(svglite)
  
  # 1) set up shell svg, w/ proper size and aspect
  # 2) add basic groups etc, including <defs><g id="template-geoms"/></defs> and <g id="styled-geoms"/>
  # 3) set the plot bounds, including aspect of map
  # 4) read in depends geoms
  # 5) loop through depends, trim, then add geoms to <use/> elements (in id="template-geoms"), with id="u-{id}"
  # 6) create geoms to mirror ids in <use/> elements, add attributes
}
get_sp_bbox <- function(sp){
  bb <- bbox(sp)
  Sr1 <- Polygon(cbind(c(bb[c(1, 3, 3, 1, 1)]), c(bb[c(2, 2, 4, 4, 2)])))
  Srs1 <- Polygons(list(Sr1), "s1")
  SpP <- SpatialPolygons(list(Srs1), proj4string = CRS(proj4string(sp)))
  return(SpP)
}

get_svg_geoms <- function(sp, ..., width = 10, height = 8, pointsize = 12, xlim, ylim){
  
  stopifnot(packageVersion('svglite') == '1.2.0.9002')
  
  if (missing(xlim)){
    xlim <- get_sp_lims(sp, ..., return = 'xlim')
  }
  if (missing(ylim)){
    ylim <- get_sp_lims(sp, ..., return = 'ylim')
  }
  
  geoms <- xml2::xml_new_document()
  
  
  rendered <- svglite::xmlSVG(width = width, height = height, pointsize = pointsize, standalone = F, {
    set_sp_plot()
    plot(sp, ..., xlim = xlim, ylim = ylim)
  })
  
  
  return(tail(xml_children(rendered), length(sp))) # removing the <rect/> element...
}

get_sp_lims <- function(sp, ..., width = 10, height = 8, pointsize = 12, return = c('xlim','ylim')){
  bb <- get_sp_bbox(sp)
  # now default plot
  # extract usr, return lims from usr
  .fun <- svglite::svgstring(width = width, height = height, pointsize = pointsize, standalone = F)
  suppressWarnings(plot(bb, ...)) # warning is for expandBB param, if used
  usr <- par('usr')
  dev.off()
  xlim = usr[c(1,2)]
  ylim = usr[c(3,4)]
  
  list.out <- list(xlim = xlim, ylim = ylim)
  
  if (length(return) > 1){
    return(list.out)
  } else {
    return(list.out[[return]])
  }
  
}

clip_sp <- function(sp, clip.box){
  message('not implemented yet')
}

set_sp_plot <- function(){
  par(mai=c(0,0,0,0), omi=c(0,0,0,0), xaxs = 'i', yaxs = 'i')
}