process.gage_colors <- function(viz){
  library(RColorBrewer)
  library(jsonlite)
  required <- c("bins", "pallete", "location")
  checkRequired(viz, required)
  
  cols <- RColorBrewer::brewer.pal(viz[['bins']], viz[['pallete']])
  names(cols) <- paste0("bin-",1:viz[['bins']])
  saveRDS(cols, viz[["location"]])
  # json <- lapply(1:viz[['bins']], function(x) {
  #   out <- list(cols[x])
  #   names(out) <- paste0('bin-', x)
  #   return(out)
  # })
  # cat(jsonlite::toJSON(json), file = viz[['location']])
}