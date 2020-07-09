#' Title
#'
#' @param path 
#'
#' @return
#' 
#' @import XML
#' @import logging
#'
#' @examples
leerConfig <- function(path){
  
  library(XML)
  
  
  configPath <- paste0(path, "config/config.xml")
  
  
  tryCatch(expr = {
    
    #Leer el xml y convertirlo a lista
    config <- XML::xmlToList(xmlParse(configPath))
    config$columnas$predictoras <- trimws(strsplit(config$columnas$predictoras, ",")[[1]])
    config$columnas$index$ano <-as.numeric(config$columnas$index$paisconfig$columnas$index$ano)
    
    
    
    
  }, error = function(e){
    
    logerror("Config no encontrado en su ruta. Verifica que se llame config.xml",
             logger = 'log')
    stop()
  })
  #loginfo("Config leido.", logger = 'log')
  
  validateConfigNodes(config)
  

  
  separadoresAceptados <- config$input$sep %in% c(",", ";")
  
  if(!separadoresAceptados){
    
    logerror("Sep solo puede valer ',' o ';' ", logger = 'log')
    stop()
    
  }
  return(config)
} 
#   config$columnas$predictorasNumericas <- trimws(strsplit(config$columnas$predictorasNumericas, ",")[[1]])



 
#   
#   return(config)
#   
# } 

#' @title validateConfigNodes
#'
#' @param config 
#'
#' @import logging
#' 
validateConfigNodes <- function(config){

  nodoPrincipal <- identical(names(config), c("columnas", "target"))
  nodoInde<-identical(names(config$columnas),c('index','predictoras'))
  nodoInput <- identical(names(config$columnas$index, c("pais",'ano')))
  nodos <-c(nodoPrincipal,nodoInde,nodoInput)

  # nodos <- c("nodoPrincipal" = nodoPrincipal, "nodoInput" = nodoInput,
  #            "nodoColumnas" = nodoColumnas, "nodoFechas" = nodoFechas,
  #            "nodoMails" = nodoMails)

  check <- all(nodos)

  if(!check){

    nodosMalos <- names(nodos)[!nodos]

    logerror(paste0("Los nodos: ", paste(nodosMalos, collapse = ", "),
                    " estan mal estructurados!"), logger = 'log')
    stop()

  }

}
