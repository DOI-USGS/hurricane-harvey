publish.json <- function(viz) {
  required <- c("relpath", "mimetype")

  checkRequired(viz, required)
  
  output <- NULL
  if (!is.na(viz[['relpath']])) {
    output <- sprintf('<script src="%s?_c=%s" type="application/json"></script>',
                      viz[['relpath']],runif(1, 10000, 10000000))
  }
  return(output)
}