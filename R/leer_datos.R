#' @title leerDatos
#'
#' @param config 
#' @param path 
#'
#' @return
#' 
#' @import data.table
#' @import logging

leer_datos <- function(config, path){
  
  pathDatos <- paste0(path, "data/")
  
  
  tryCatch(expr = {
    #DATOS A TRADUCIR A R
    string<- config$columnas$predictoras
    lista_nombre <- strsplit(string,',')[[1]]
    #split ponerlo en el config
    lista_csv<-list()
    

    
    for (i in 1:length(lista_nombre)){
      datos <- data.table::fread(paste0(pathDatos,lista_nombre[i]), sep = ',',
                                 encoding = 'UTF-8', data.table = FALSE, header = TRUE)
      # if(nrow(datos) == 0 | ncol(datos) == 0){
      #   
      #   logerror("Datos mal leido, verifica que tengan un buen formato. ",
      #            logger = 'log')
      #   stop()
      #   
      # }
      
      lista_csv[[i]]<-datos
    }
      
  
    
    
  }, error = function(e){
    
    logerror("Datos no encontrado en su ruta. Verifica el directorio de data y el config",
             logger = 'log')
    stop()
  })
  

  
  return(lista_csv)
  
}

