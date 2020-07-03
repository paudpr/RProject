#' Title
#'
#' @param datos 
#' @param config 
#'
#' @return
#' @import logging
#'
#' @examples
#' 

output <- function(output, config, path){
  
  nombre_archivo <- paste0(path, "output/prediccion")
  
  tryCatch(expr = {
    write.csv(output$prediccion, file = nombre_archivo, sep = config$input$sep, 
              row.names = FALSE)
  }, error = function(e){
    log_error("Ha fallado el guardado!", logger = "log")
    stop()
  })
  
  nombre_archivo <- paste0(path, "output/modelo.rds")
  
  tryCatch(expr = {
    saveRDS(output$modelo, file = nombre_archivo)
  }, error = function(e){
    logerror("Ha fallado el guardado del modelo!", logger = "log")
    stop()
  })
  
}