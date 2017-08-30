library(yaml)
process.lastUpdated <- function(viz) {
  data <- viz[['depends']][['data']]
  data.file <- as.viz(data)[['location']]
  mtime <- file.mtime(data.file)
  cat(as.yaml(list(time = format(mtime))), file = viz[['location']])
}