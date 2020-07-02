#' @title leerDatos
#'
#' @param config 
#' @param path 
#'
#' @return
#' 
#' @import data.table
#' @import logging
#'
leerDatos <- function(config, path){
  
  pathDatos <- paste0(path, "data/", config$input$name)
  
  
  tryCatch(expr = {
    
    datos <- data.table::fread(pathDatos, sep = config$input$sep,
                               encoding = 'UTF-8', data.table = FALSE)
    
    
  }, error = function(e){
    
    logerror("Datos no encontrado en su ruta. Verifica el directorio de data y el config",
             logger = 'log')
    stop()
  })
  
  if(nrow(datos) == 0 | ncol(datos) == 0){
    
    logerror("Datos mal leido, verifica que tengan un buen formato. ",
             logger = 'log')
    stop()
    
  }
  
  return(datos)
  
}

