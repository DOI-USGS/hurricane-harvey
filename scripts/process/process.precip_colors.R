process.precip_colors <- function(viz){
  library(RColorBrewer)
  library(jsonlite)
  cols <- RColorBrewer::brewer.pal(viz[['bins']], viz[['pallete']])
  
  cat(jsonlite::toJSON(cols), file = viz[['location']])
}