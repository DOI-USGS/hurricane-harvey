
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

#' calculate the bounding box of the sp object and return as `SpatialPolygons` object
#' @param sp a spatial object
#' 
#' @return a `SpatialPolygons` object that represents the bounding box of the input `sp`
get_sp_bbox <- function(sp){
  bb <- bbox(sp)
  Sr1 <- Polygon(cbind(c(bb[c(1, 3, 3, 1, 1)]), c(bb[c(2, 2, 4, 4, 2)])))
  Srs1 <- Polygons(list(Sr1), "s1")
  SpP <- SpatialPolygons(list(Srs1), proj4string = CRS(proj4string(sp)))
  return(SpP)
}


#' extract the svg elements from an sp object
#' 
#' @param sp a spatial object
#' @param ... additional arguments passed to the plotting methods of `sp` (e.g., `expandBB`)
#' @param width the width (in inches) of the svg view
#' @param height the height (in inches) of the svg view
#' @param pointsize number of pixels per inch
#' @param xlim x limits (in sp units) of the plot
#' @param ylim y limits (in sp units) of the plot
#' 
#' @return an `xml_document`
get_svg_geoms <- function(sp, ..., width = 10, height = 8, pointsize = 12, xlim, ylim){
  
  stopifnot(packageVersion('svglite') == '1.2.0.9002')
  
  if (missing(xlim)){
    xlim <- get_sp_lims(sp, ..., return = 'xlim')
  }
  if (missing(ylim)){
    ylim <- get_sp_lims(sp, ..., return = 'ylim')
  }
  
  # clip the spatial object so that it only contains features and data that are within the plotting range:
  clipped.sp <- clip_sp(sp, xlim, ylim)
  
  # establish the holder of the geoms as an xml doc
  geoms <- xml2::xml_new_document()
  
  rendered <- svglite::xmlSVG(width = width, height = height, pointsize = pointsize, standalone = F, {
    set_sp_plot()
    plot(clipped.sp, ..., xlim = xlim, ylim = ylim)
  })
  
  sp.geoms <- tail(xml2::xml_children(rendered), length(sp))
  
  # here either strip the important attributes out and re-add them with a xml_set_attrs call, or lapply the nodeset and add attrs one by one:
  
  return(sp.geoms) # removing the <rect/> element...
}

#' extract the plotting limits from a spatial object, given a sized svg view
#' 
#' @param sp a spatial object
#' @param ... additional arguments passed to the plotting methods of `sp` (e.g., `expandBB`)
#' @param width the width (in inches) of the svg view
#' @param height the height (in inches) of the svg view
#' @param pointsize number of pixels per inch
#' @param return what limits to return
#' 
#' @return a list or numeric vector, depending on what is specified for `return`

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

#' retain data and features for all polys/points within the plotting range,
#' while removing those outsite and truncating the boundaries of 
#' polygons to the bounds of the plotting range. 
#' 
#' @param sp a spatial object
#' @param xlim the x limits of the plot using the same coordinate system as `sp`
#' @param ylim the y limits of the plot using the same coordinate system as `sp`
#' 
#' @return a clipped sp object
clip_sp <- function(sp, xlim, ylim){
  message('clip_sp is not implemented yet')
  return(sp)
}

#' set up the basic plot par for a map
set_sp_plot <- function(){
  par(mai=c(0,0,0,0), omi=c(0,0,0,0), xaxs = 'i', yaxs = 'i')
}