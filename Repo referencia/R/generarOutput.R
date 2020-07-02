#' Title
#'
#' @param output 
#' @param config 
#' @param path 
#'
#' @import logging
#' @return
#'
generarOutput <- function(output, config, path){

  marcaTmp <- Sys.time()
  
  nombreArchivo <- paste0(path, "output/deberesAlemania.csv")
  
  tryCatch(expr = {
    
    write.csv(output$prediccion, file = nombreArchivo, sep = config$input$sep,
              row.names = FALSE)
    
  }, error = function(e){
    
    logerror("Ha fallado el guardado!!", logger = 'log')
    stop()
  })
  
  
  nombreArchivo <- paste0(path, "output/modelo.rds")
  
  tryCatch(expr = {
    
    saveRDS(output$modelo, file = nombreArchivo)
    
  }, error = function(e){
    
    logerror("Ha fallado el guardado del modelo!!", logger = 'log')
    stop()
  })
  
  
}