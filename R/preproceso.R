#' Title
#'
#' @param datos 
#' @param config 
#'
#' @return
#' @import logging
#'
#' @examples
library(reshape2)

preproceso <- function(lista_csv, config){
  #datos = el df

  for (i in 1:length(lista_csv)){
    #df$pais <-rownames(lista_csv[[i]])
    # col = colnames(i)
    col <-colnames(lista_csv[[i]])
    
    df2 <- melt(data = lista_csv[[i]], id.vars = 'pais',variable.name='anos', measure.vars = col[!(col %in% "country")])
    lista_csv[[i]]<-df2
  
  df <-lista_csv[[1]]
  for (i in 2:length(lista_csv)){
    df <- merge(df, lista_csv[[i]], by = c('pais','anos'))
  return(df)
  }
}
  
  
  
  
  #' Title
  #'
  #' @param datos 
  #' @param config 
  #'
  #' @return
  #' @import logging
  #'
  #' @examples
  #' @export
  
  
  
  preproceso_limpieza <- function(df, config){
    
    #check columnas a utilizar
    columnas_utilizadas <- c(config$columnas$index$pais, config$columnas$index$ano,
                             config$columnas$predictoras$cre_pobl, config$columnas$predictoras$cre_urb,
                             config$columnas$predictoras$den_pobl, config$columnas$predictoras$ener_pers, 
                             config$columnas$predictoras$esp_vida.csv)
    
    comprobacion_columnas <- columnas_utilizadas%in%colnames(df)
    comprobacion <- all(comprobacion_columnas)
    
    if(!comprobación){
      columnas_no <- columnas_utilizadas[!comprobacion_columnas]
      logerror(paste0("Las columnas:", paste(columnas_no, collapse = ","),
                      " no se encuentran en el dataframe."), logger = "log")
      stop()
      
    }
    
    
    df <- df[,columnas_utilizadas]
    
    
  limpieza_na <- function(df, config){
    
    df_select <- (df$country %in% config$columnas$index$pais) & 
      (df$anos %in% config$columnas$index$ano)
    #ahora miro en qué columna de esa fila hay na
    #ahora miro en qué columna de esa fila hay na
    n <-is.na(df[c(df_select),])
    df_selected <-df[,!c(n)]
    df_limpio <-na.omit(df_selected)
    return(df_limpio)

        
      
      }
  }
  
    
    
    
    
    
    #merge(x, y, by = c("k1","k2"))
    #tenemos un df con pais,ano y value
    #metemos value en una lista c() y esa lista de values se la introducimos al df
    #df = data.frame(c())
  }
    
  
  #columnasUtilizadas <- c(config$columnas$ID, config$columnas$predictorasNumericas,
                          # config$columnas$fuenteOriginal, config$columnas$dominio_mail,
                          # config$columnas$fechas$creacion, config$columnas$fechas$ultima_mod,
                          # config$columnas$fechas$apertura_ultimo, config$columnas$fechas$envio_ultimo,
                          # config$columnas$fechas$apertura_primero, config$columnas$fechas$envio_primero,
                          # config$columnas$fechas$visita_primero, config$columnas$fechas$visita_ultimo,
                          # config$columnas$mails$mailsDl, config$columnas$mails$mailsCl,
                          # config$columnas$mails$mailsOp, config$columnas$target, config$columnas$llamada)
  
  
  checkColumnas <- columnasUtilizadas %in% colnames(datos)
  
  check <- all(checkColumnas)
  
  if(!check){
    
    columnasMalos <- columnasUtilizadas[!checkColumnas]
    datos[is.na(datos[, config$columnas$mails$mailsOp]), config$columnas$mails$mailsOp] <- 0
    datos[is.na(datos[, config$columnas$mails$mailsDl]), config$columnas$mails$mailsDl] <- 0
    datos[is.na(datos[, config$columnas$mails$mailsCl]), config$columnas$mails$mailsCl] <- 0
    logerror(paste0("Les columnes: ", paste(columnasMalos, collapse = ", "),
                    " no se encuentran en el dataframe!"), logger = 'log')
    stop()
    
  }
  
  
  datos <- datos[, columnasUtilizadas]
  
  
  
  
  if(config$columnas$mails$ratios){
    
    datos <- createRatios(datos, config)
  }
  
  
  datos <- generarColumnas(datos, config)
  datosSplit <- splitearDatos(datos, config)
  
  return(datosSplit)
  
}

#' Title
#'
#' @param datos 
#' @param config 
#'
#' @return
#' @export
#'
#' @examples
createRatios <- function(datos, config){
  
  datos$mails_A_E <- datos[, config$columnas$mails$mailsOp] / datos[, config$columnas$mails$mailsDl]
  datos$mails_C_E <- datos[, config$columnas$mails$mailsCl] / datos[, config$columnas$mails$mailsDl]
  datos$mails_C_A <- datos[, config$columnas$mails$mailsCl] / datos[, config$columnas$mails$mailsOp]
  
  datos$mails_A_E[is.na(datos$mails_A_E)] <- 0
  datos$mails_C_E[is.na(datos$mails_C_E)] <- 0
  datos$mails_C_A[is.na(datos$mails_C_A)] <- 0
  
  return(datos)
  
}


#' @title generarColumnas
#'
#' @param datos 
#' @param config 
#'
#' @return
#' 
#' @import dummies
#'
#' @examples
generarColumnas <- function(datos, config){
  
  datos$diffCr_Ul <- abs(difftime(as.POSIXct(datos[, config$columnas$fechas$creacion]),
                                  as.POSIXct(datos[, config$columnas$fechas$ultima_mod]),
                                  units = 'hours'))
  
  datos$diffUltimo <- abs(difftime(as.POSIXct(datos[, config$columnas$fechas$apertura_ultimo]),
                                   as.POSIXct(datos[, config$columnas$fechas$envio_ultimo]),
                                   units = 'hours'))
  
  
  datos$diffPrimero <- abs(difftime(as.POSIXct(datos[, config$columnas$fechas$apertura_primero]),
                                    as.POSIXct(datos[, config$columnas$fechas$envio_primero]),
                                    units = 'hours'))
  
  datos$diffVisita <- abs(difftime(as.POSIXct(datos[, config$columnas$fechas$visita_primero]),
                                   as.POSIXct(datos[, config$columnas$fechas$visita_ultimo]),
                                   units = 'hours'))
  
  datos$diffCr_UlNA <- as.numeric(is.na(datos$diffCr_Ul))
  datos$diffPrimeroNA <- as.numeric(is.na(datos$diffPrimero))
  datos$diffUltimoNA <- as.numeric(is.na(datos$diffUltimo))
  datos$diffVisitaNA <- as.numeric(is.na(datos$diffVisita))
  
  datos$diffCr_Ul[is.na(datos$diffCr_Ul)] <- 0
  datos$diffVisita[is.na(datos$diffVisita)] <- 0
  datos$diffPrimero[is.na(datos$diffPrimero)] <- Inf
  datos$diffUltimo[is.na(datos$diffUltimo)] <- Inf
  
  if(!is.null(config$columnas$fechas$tiempos)){
    
    for(tiempo in config$columnas$fechas$tiempos){
      
      datos[, paste0("diffCr_Ul", tiempo)] <- as.numeric(datos$diffCr_Ul < tiempo)
      datos[, paste0("diffVisita", tiempo)] <- as.numeric(datos$diffVisita < tiempo)
      datos[, paste0("diffPrimero", tiempo)] <- as.numeric(datos$diffPrimero < tiempo)
      datos[, paste0("diffUltimo", tiempo)] <- as.numeric(datos$diffUltimo < tiempo)
      
    }    
  }
  
  datos$gmail <- datos[, config$columnas$dominio_mail] == "gmail.com"
  
  datos <- dummy.data.frame(datos, names = config$columnas$fuenteOriginal)
  
  return(datos)
}


#' Title
#'
#' @param datos 
#' @param config 
#'
#' @return
#' @export
#'
#' @examples
splitearDatos <- function(datos, config){
  
  positivos <- c("Cerrado Objetivo Alcanzado", "Entrevista programada",
                 "Interés Próxima Edición", "Test programado")
  negativos <- c("Cerrada", "Contacto No Válido", "Descartado Demasiados Intentos",
                 "Entrevista no superada", "Oportunidad Fallida")
  
  
  #Evaluado por estado
  contactosBuenos <- datos[datos[, config$columnas$target] %in% positivos, ]
  contactosMalos <- datos[datos[, config$columnas$target] %in% negativos, ]
  
  #Evaluar abiertos
  abiertos <- datos[datos[, config$columnas$target] == "Open", ]
  
  
  openNegativos <- c("Contacto No Válido", "Desacartada - Demasiados intentos")
  openNegativos2 <- grep("No interesado", unique(abiertos[, config$columnas$llamada]),
                         ignore.case = TRUE, value = TRUE)
  
  abiertosNegativos <- abiertos[abiertos[, config$columnas$llamada] %in% c(openNegativos, openNegativos2), ]
  
  contactosMalos <- rbind(contactosMalos, abiertosNegativos)
  
  
  
  openPositivos <- c("Interesado - Remito a info session (Interés Medio)", 
                     "Interesado - Remito a web (Interés Bajo)", 
                     "Interesado (Mucho interés)")
  
  resultadoDeLLamada <- abiertos[, config$columnas$llamada]
  interesados <- resultadoDeLLamada %in% openPositivos
  
  abiertosPositivos <- abiertos[interesados, ]
  
  contactosBuenos <- rbind(contactosBuenos, abiertosPositivos)
  
  predictOpen <- grep("Volver a llamar", unique(abiertos[, config$columnas$llamada]),
                      ignore.case = T, value = T)
  
  contactosPredict <- resultadoDeLLamada %in% predictOpen
  
  contactosPredict <- abiertos[is.na(resultadoDeLLamada) | contactosPredict, ]
  
  contactosBuenos$target <- 1
  contactosMalos$target <- 0
  
  contactosEntrenados <- rbind(contactosBuenos, contactosMalos)
  
  listaDatosSpliteados <- list(contactosPredict = contactosPredict,
                               contactosEntrenados = contactosEntrenados)
  
  return(listaDatosSpliteados)
  
}

