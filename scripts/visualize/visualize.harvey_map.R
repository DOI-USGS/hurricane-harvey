# viz <- yaml.load_file("viz.yaml")
# viz <- viz$visualize
# viz <- viz[[which(unlist((lapply(viz, function(x) x$id == "harvey-map"))))]]

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
  Sr1 <- Polygon(cbind(c(bb[1, 3, 3, 1, 1]), c(bb[2, 2, 4, 4, 2])))
  Srs1 <- Polygons(list(Sr1), "s1")
  SpP <- SpatialPolygons(list(Srs1), proj4string = CRS(proj4string(sp)))
  return(SpP)
}

get_sp_lims <- function(sp, ...){
  bb <- get_sp_bbox(sp)
  # now default plot
  # extract usr, return lims from usr
  .fun <- svglite::svgstring(standalone = F, ...)
  usr <- par('usr')
  rm(.fun)
  
  return(list(xlim = usr[c(1,2)], ylim = usr[c(3,4)]))
}

visualize.harvey_map <- function(viz = as.viz("harvey-map")){
  
  counties <- readData(viz[['depends']][1])
  states <- readData(viz[['depends']][2])
  track <- readData(viz[['depends']][3])
  col.bins <- readData(viz[['depends']][4])
  storm <- readData(viz[['depends']][5])
  gages <- readData(viz[['depends']][6])
  legend.bins <- readData(viz[['depends']][7])
  legend.breaks <- readData(viz[['depends']][8])
  spark.sites <- readData(viz[['depends']][9])
  state.borders <- readData(viz[['depends']][10])
  non.harvey.gages <- readData(viz[['depends']][11])
  library(svglite)
  library(dplyr)
  
  set.plot <- function(){
    par(mai=c(0,0,0,0), omi=c(0,0,0,0), xaxs = 'i', yaxs = 'i')
    sp::plot(counties, expandBB = c(0.1,0.18, 0.05, 0.28))
  }
  num.harv.gages <- 0
  fip.cd <- as.character(counties$FIPS[counties@plotOrder])
  cty.name <- as.character(counties$countyName[counties@plotOrder])
  svg <- svglite::xmlSVG(width = 10, height = 8, {
    set.plot()
    sp::plot(state.borders, add=TRUE)
    sp::plot(states, add=TRUE)
    sp::plot(gages, pch=20, add=TRUE)
    sp::plot(storm, pch=20, add=TRUE)
  })
  library(rgeos)
  svg.addons <- svglite::xmlSVG(width = 10, height = 8, {
    set.plot()
    # doing this because not all of the gages are now in the plotting area. 
    usr <- par("usr")
    Sr1 <- Polygon(cbind(c(usr[1], usr[2], usr[2], usr[1], usr[1]),c(usr[3], usr[3], usr[4], usr[4], usr[3])))
    Srs1 <- Polygons(list(Sr1), "s1")
    SpP <- SpatialPolygons(list(Srs1), proj4string = CRS(proj4string(non.harvey.gages)))
    shown.inactive <- which(rgeos::gContains(SpP, non.harvey.gages, byid = TRUE))
    shown.inactive.gages <<- non.harvey.gages[shown.inactive, ]
    sp::plot(shown.inactive.gages, pch=20, add=TRUE) # doing this because we are coding based on counting numbers of circles...
    sp::plot(track, add=TRUE)
  })
  library(xml2)
  # let this thing scale:
  xml_attr(svg, "preserveAspectRatio") <- "xMidYMid meet" 
  xml_attr(svg, "id") <- "harvey-svg"
  
  vb <- strsplit(xml_attr(svg, 'viewBox'),'[ ]')[[1]]
  r <- xml_find_all(svg, '//*[local-name()="rect"]')
  
  xml_add_sibling(xml_children(svg)[[1]], 'rect', .where='before', width=vb[3], height=vb[4], class='background')
  xml_add_sibling(xml_children(svg)[[1]], 'desc', .where='before', viz[["alttext"]])
  xml_add_sibling(xml_children(svg)[[1]], 'title', .where='before', viz[["title"]])

  # clean up junk that svglite adds:
  .junk <- lapply(r, xml_remove)
  
  
  # find the paths for the counties, add IDs
  p <- xml_find_all(svg, '//*[local-name()="path"]')
  cr <- xml_find_all(svg, '//*[local-name()="circle"]')
  
  num.time <- group_by(col.bins, fips) %>% tally %>% .$n %>% unique()
  if (length(num.time) != 1){
    stop('all of the counties dont have the same number of timesteps!')
  }
  
  for (i in 1:length(counties)){
    steps <- paste('prcp', 1:num.time, sep='-')
    bins <- filter(col.bins, fips == fip.cd[i]) %>% .$cols
    time.classes <- paste(steps, bins, sep='-', collapse=' ') 
    xml_attr(p[[i]], 'id') <- paste0('FIP-', fip.cd[i])
    xml_attr(p[[i]], 'class') <- sprintf('county-polygon %s', time.classes)
    xml_attr(p[[i]], 'onmousemove') <- sprintf("hovertext('%s',evt);",cty.name[i])
    xml_attr(p[[i]], 'onmouseout') <- "hovertext(' ');"
    xml_attr(p[[i]], 'style') <- NULL
    xml_attr(p[[i]], 'clip-path') <- NULL

  }

  
  g.track <- xml_add_child(svg, 'g', id='track','class'='track-polyline')
  g.overlays <- xml_add_child(svg, 'g', id='map-overlays', .where = 3L)
  g.states <- xml_add_child(svg, 'g', id='states', .where = 3L)
  g.storm <- xml_add_child(svg, 'g', id='storm','class'='storm-dots')
  g.legend <- xml_add_child(svg, 'g', id='precip-legend','class'='legend', transform='translate(12,12)scale(0.8)')
  g.watermark <- xml_add_child(svg, 'g', id='usgs-watermark',transform=sprintf('translate(2,%s)scale(0.40)', as.character(as.numeric(vb[4])-62)))
  g.borders <- xml_add_child(svg, 'g', id='focus-borders') # on top
  
  rmv.i <- c()
  for (j in (i+1):(i+length(state.borders))){
    xml_add_child(g.track, 'path', d = xml_attr(p[j], 'd'), class='state-border', 'clip-path'= "url(#svg-bounds)")
    rmv.i <- c(rmv.i, j)
  }
  for (i in (j+1):length(p)){
    xml_add_child(g.states, 'path', d = xml_attr(p[i], 'd'), class='state-polygon', 'clip-path'= "url(#svg-bounds)")
    rmv.i <- c(rmv.i, i)
  }

  xml_remove(p[rmv.i])
  
  
  pl <- xml_find_all(svg.addons, '//*[local-name()="polyline"]')
  for (i in 1:length(pl)){
    xml_add_child(g.track, 'polyline', points = xml_attr(pl[i], 'points'))
  }

  
  
  cnt = 0; # count how many actually have data
  for (i in 1:length(gages)){
    svg.points <- filter(spark.sites, site_no == gages$site_no[i]) %>% .$points
    if (!is.null(svg.points) && !is.na(svg.points[1])){
      cnt = cnt+1
    }
  }
  d <- xml_find_all(svg, '//*[local-name()="defs"]')
  xml_remove(d)
  d <- xml_add_child(svg, 'defs') 
  m = xml_add_child(d, 'mask', id="spark-opacity", x="0", y="-1", width="1", height="3", maskContentUnits="objectBoundingBox")
  xml_add_child(m, 'rect', x="0", y="-1", width="1", height="3", style="fill-opacity: 0.18; fill: white;", id='spark-light-mask')
  xml_add_child(m, 'rect', x="0", y="-1", width="0", height="3", style="fill-opacity: 1; fill: white;", id='spark-full-mask')
  cp <- xml_add_child(d, 'clipPath', id="svg-bounds")
  xml_add_child(cp, 'rect', width=vb[3], height=vb[4])
  
  g.spark <- xml_add_child(svg, 'g', transform='translate(580,115)scale(0.97)')
  xml_add_child(g.spark, 'rect', x='-6', y = '-0.8em', width="147", height='2em', class='legend-box')
  xml_add_child(g.spark, 'text', x='67', 'Featured USGS gages', dy="0.1em", 'text-anchor'='middle', class='svg-text')
  xml_add_child(g.spark, 'text', x='67', '(normalized discharge)', dy='1.1em', 'text-anchor'='middle', class='svg-text smallprint-text')
  
  
  ys <- seq(20,as.numeric(vb[4])-190, length.out = cnt)
  cnt = 0;
  for (i in 1:length(gages)){ # FRAGILE - assumes all gages are on the map!!
    
    svg.points <- filter(spark.sites, site_no == gages$site_no[i]) %>% .$points
    if (!is.null(svg.points) && !is.na(svg.points[1])){
      cnt = cnt+1
      g.dot <- xml_add_child(g.storm, 'g', transform=sprintf('translate(%s,%s)', cx = xml_attr(cr[i], 'cx'), cy = xml_attr(cr[i], 'cy')))
      xml_add_child(g.dot, 'circle', id=sprintf('nwis-%s',gages$site_no[i]), r='3', class='nwis-dot',
                    onclick=sprintf("openNWIS('%s')", gages$site_no[i]),
                    onmouseover=sprintf("setBold('sparkline-%s');", gages$site_no[i]),
                    onmouseout=sprintf("setNormal('sparkline-%s');hovertext(' ');", gages$site_no[i]),
                    onmousemove=sprintf("hovertext('USGS %s',evt);",gages$site_no[i]))
      g.single <- xml_add_child(g.spark, 'g', transform=sprintf('translate(-5,%s)', ys[cnt])) 
      xml_add_child(g.single, 'polyline', points = svg.points[1], class='sparkline', id=paste0('sparkline-',gages$site_no[i]), 
                    onclick=sprintf("openNWIS('%s')", gages$site_no[i]),
                    onmouseover=sprintf("setBold('nwis-%s');", gages$site_no[i]),
                    onmouseout=sprintf("setNormal('nwis-%s');hovertext(' ');", gages$site_no[i]),
                    style="mask: url(#spark-opacity)",onmousemove=sprintf("hovertext('USGS %s',evt);",gages$site_no[i]))
    }
    
  }
  storm.i <- length(storm)
  for (i in length(cr):(length(gages)+1)){ # assumes that LAST of the storm is on the map!!
    xml_add_child(g.storm, 'circle', cx = xml_attr(cr[i], 'cx'), cy = xml_attr(cr[i], 'cy'), id=paste0('storm-',storm.i), r='8', class='storm-dot', opacity='0.0')
    storm.i <- storm.i - 1
  }
  
  non.cr <- xml_find_all(svg.addons, '//*[local-name()="circle"]')
  if (length(non.cr) != length(shown.inactive.gages)){
    stop('the count of non storm gages on the map doesnt match the count of named gages.')
  }
  #removing the js code from the inactive ones, so no need to match info
  for (i in 1:length(non.cr)){ 
    xml_add_child(g.storm, 'circle', cx = xml_attr(non.cr[i], 'cx'), cy = xml_attr(non.cr[i], 'cy'), 
                  class='nwis-inactive', r='1.5', onmouseout="hovertext(' ');", id = sprintf('inactive-%s', shown.inactive.gages$site_no[i]),
                  onmousemove=sprintf("hovertext('USGS %s',evt);", shown.inactive.gages$site_no[i]))
  }
  
  xml_add_child(g.legend, 'rect', x="-8", y="-8", width='185', height='265', class='legend-box')
  xml_add_child(g.legend, 'text', 'Legend', 'class'='legend-title svg-text', dy='0.75em')
  xml_add_child(g.legend, 'text', 'Inches per hour', 'class'='svg-text', dy='2em')
  xml_add_child(g.legend, 'text', 'dy'= "3.7em", class='smallprint-text svg-text', "(county average precip)")
  xml_add_child(g.overlays, 'text', 'Texas', dx = '250', dy = "50", 'text-anchor'='middle', 
                class='svg-text state-overlay-text')
  xml_add_child(g.overlays, 'text', 'Mexico', dx = '60', dy = "490", 'text-anchor'='middle', 
                class='svg-text state-overlay-text')
  xml_add_child(g.overlays, 'text', 'Louisiana', dx = '466', dy = "52", 'text-anchor'='middle', 
                class='svg-text state-overlay-text')
  xml_add_child(g.overlays, 'text', 'Mississippi', dx = '632', dy = "90", 'text-anchor'='middle', 
                class='svg-text state-overlay-text')
  xml_add_child(g.overlays, 'text', 'Gulf of Mexico', dx = '432', dy = "430", 'text-anchor'='middle', 
                class='svg-text gulf-overlay-text')
  
  ys <- as.character(seq(55, 180, length.out = length(legend.bins)))
  box.w <- '12'
  for (i in 1:length(legend.bins)){
    xml_add_child(g.legend, 'rect', 'height'=box.w, 'width'=box.w, y = ys[i], id=paste0('precip-bin-',i), fill=legend.bins[i], class='precip-legend-bin')
    leg.txt <- ifelse(i == length(legend.breaks), sprintf('> %s', legend.breaks[i]), sprintf('%s to %s', legend.breaks[i], legend.breaks[i+1]))
    xml_add_child(g.legend, 'text', x=box.w, 'dx'="0.5em", y=as.character(as.numeric(ys[i])+as.numeric(box.w)/2), 'dy'= "0.33em", class='precip-legend-text svg-text', leg.txt)
  }
  
  xml_add_child(g.legend, 'path', d=sprintf('M-4,%s h%s',as.character(as.numeric(ys[i])+30), 20), class='track-polyline')
  xml_add_child(g.legend, 'circle', cx = as.character(as.numeric(box.w)/2), r='8', class='storm-dot-legend', cy = as.character(as.numeric(ys[i])+30), class='storm-legend-dot')
  xml_add_child(g.legend, 'text', x=box.w, 'dx'="0.5em", y=as.character(as.numeric(ys[i])+30), 'dy'= "0.33em", class='storm-legend-text svg-text', "Harvey")
  
  xml_add_child(g.legend, 'circle', cx = as.character(as.numeric(box.w)/2 - 3), r='3', cy = as.character(as.numeric(ys[i])+50), class='nwis-legend-dot')
  xml_add_child(g.legend, 'text', x=box.w, 'dx'="-0.1em", y=as.character(as.numeric(ys[i])+50), 'dy'= "0.33em", class='nwis-legend-text svg-text', "USGS")
  xml_add_child(g.legend, 'text', x=box.w, 'dx'="41", y=as.character(as.numeric(ys[i])+50), 'dy'= "0.33em", class='smallprint-text svg-text', "stream gage (featured)")
  xml_add_child(g.legend, 'circle', cx = as.character(as.numeric(box.w)/2 - 3), r='3', cy = as.character(as.numeric(ys[i])+65), class='nwis-inactive')
  xml_add_child(g.legend, 'text', x=box.w, 'dx'="-0.1em", y=as.character(as.numeric(ys[i])+65), 'dy'= "0.33em", class='nwis-legend-text svg-text', "USGS")
  xml_add_child(g.legend, 'text', x=box.w, 'dx'="41", y=as.character(as.numeric(ys[i])+65), 'dy'= "0.33em", class='smallprint-text svg-text', "stream gage")
  
  xml_add_child(svg, 'text', ' ', id='timestamp-text', class='time-text svg-text', x="646.5", y="550", 'text-anchor'="middle")
  
  usgs.d="m234.95 15.44v85.037c0 17.938-10.132 36.871-40.691 36.871-27.569 0-40.859-14.281-40.859-36.871v-85.04h25.08v83.377c0 14.783 6.311 20.593 15.447 20.593 10.959 0 15.943-7.307 15.943-20.593v-83.377h25.08m40.79 121.91c-31.058 0-36.871-18.27-35.542-39.03h25.078c0 11.462 0.5 21.092 14.282 21.092 8.472 0 12.62-5.482 12.62-13.618 0-21.592-50.486-22.922-50.486-58.631 0-18.769 8.968-33.715 39.525-33.715 24.42 0 36.543 10.963 34.883 36.043h-24.419c0-8.974-1.492-18.106-11.627-18.106-8.136 0-12.953 4.486-12.953 12.787 0 22.757 50.493 20.763 50.493 58.465 0 31.06-22.75 34.72-41.85 34.72m168.6 0c-31.06 0-36.871-18.27-35.539-39.03h25.075c0 11.462 0.502 21.092 14.285 21.092 8.475 0 12.625-5.482 12.625-13.618 0-21.592-50.494-22.922-50.494-58.631 0-18.769 8.969-33.715 39.531-33.715 24.412 0 36.536 10.963 34.875 36.043h-24.412c0-8.974-1.494-18.106-11.625-18.106-8.144 0-12.955 4.486-12.955 12.787 0 22.757 50.486 20.763 50.486 58.465 0 31.06-22.75 34.72-41.85 34.72m-79.89-46.684h14.76v26.461l-1.229 0.454c-3.816 1.332-8.301 2.327-12.453 2.327-14.287 0-17.943-6.645-17.943-44.177 0-23.256 0-44.348 15.615-44.348 12.146 0 14.711 8.198 14.933 18.107h24.981c0.198-23.271-14.789-36.043-38.42-36.043-41.021 0-42.52 30.724-42.52 60.954 0 45.507 4.938 63.167 47.12 63.167 9.784 0 25.36-2.211 32.554-4.18 0.436-0.115 1.212-0.596 1.212-1.216v-59.598h-38.612v18.09"
  wave.d="m48.736 55.595l0.419 0.403c11.752 9.844 24.431 8.886 34.092 2.464 6.088-4.049 33.633-22.367 49.202-32.718v-10.344h-116.03v27.309c7.071-1.224 18.47-0.022 32.316 12.886m43.651 45.425l-13.705-13.142c-1.926-1.753-3.571-3.04-3.927-3.313-11.204-7.867-21.646-5.476-26.149-3.802-1.362 0.544-2.665 1.287-3.586 1.869l-28.602 19.13v34.666h116.03v-24.95c-2.55 1.62-18.27 10.12-40.063-10.46m-44.677-42.322c-0.619-0.578-1.304-1.194-1.915-1.698-13.702-10.6-26.646-5.409-29.376-4.116v11.931l6.714-4.523s10.346-7.674 26.446 0.195l-1.869-1.789m16.028 15.409c-0.603-0.534-1.214-1.083-1.823-1.664-12.157-10.285-23.908-7.67-28.781-5.864-1.382 0.554-2.7 1.303-3.629 1.887l-13.086 8.754v12.288l21.888-14.748s10.228-7.589 26.166 0.054l-0.735-0.707m68.722 12.865c-4.563 3.078-9.203 6.203-11.048 7.441-4.128 2.765-13.678 9.614-29.577 2.015l1.869 1.797c0.699 0.63 1.554 1.362 2.481 2.077 11.418 8.53 23.62 7.303 32.769 1.243 1.267-0.838 2.424-1.609 3.507-2.334v-12.234m0-24.61c-10.02 6.738-23.546 15.833-26.085 17.536-4.127 2.765-13.82 9.708-29.379 2.273l1.804 1.729c0.205 0.19 0.409 0.375 0.612 0.571l-0.01 0.01 0.01-0.01c12.079 10.22 25.379 8.657 34.501 2.563 5.146-3.436 12.461-8.38 18.548-12.507l-0.01-12.165m0-24.481c-14.452 9.682-38.162 25.568-41.031 27.493-4.162 2.789-13.974 9.836-29.335 2.5l1.864 1.796c1.111 1.004 2.605 2.259 4.192 3.295 10.632 6.792 21.759 5.591 30.817-0.455 6.512-4.351 22.528-14.998 33.493-22.285v-12.344"
  xml_add_child(g.watermark,'path', d=usgs.d, onclick="vizlab.clicklink('https://www2.usgs.gov/water/')", 'class'='watermark')
  xml_add_child(g.watermark,'path', d=wave.d, onclick="vizlab.clicklink('https://www2.usgs.gov/water/')", 'class'='watermark')
  
  g.tool <- xml_add_child(svg,'g',id='tooltip-group')
  xml_add_child(g.tool, 'rect', id="tooltip-box", height="24", class="tooltip-box")
  xml_add_child(g.tool, 'path', id="tooltip-point", d="M-6,-11 l6,10 l6,-11", class="tooltip-box")
  xml_add_child(g.tool, 'text', id="tooltip-text", dy="-1.1em", 'text-anchor'="middle", class="tooltip-text-label svg-text", " ")
  
  xml_remove(pl)
  xml_remove(cr)
  write_xml(svg, viz[['location']])
}
