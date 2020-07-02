#' Title
#'
#' @param datos 
#' @param config 
#'
#' @import xgboost
#' @import logging
#'
#' @examples
generarModelo <- function(datosSplit, config){
  
  datosSplit <- seleccionColumnas(datosSplit, config)

  variablesToModel <- datosSplit$variablesToModel
  
  contactosEntrenados <- datosSplit$contactosEntrenados
  contactosPredict <- datosSplit$contactosPredict
  #Numero de contactos para entrenar
  numTrain <- round(nrow(contactosEntrenados) * 0.7)
  
  trainPos <- sample(1:nrow(contactosEntrenados), size = numTrain)
  
  
  train <- contactosEntrenados[trainPos, variablesToModel]
  test <- contactosEntrenados[-trainPos, variablesToModel]
  
  library(xgboost)
  #Creamos las matrices necesarias para el modelo
  dtrain = xgb.DMatrix(data=data.matrix(train), label= contactosEntrenados$target[trainPos])
  dtest = xgb.DMatrix(data=data.matrix(test), label= contactosEntrenados$target[-trainPos])
  
  
  
  #Definimos los parametros del modelo
  p = list(objective = "binary:logistic",
           eval_metric = "auc",
           max_depth = 12,
           eta = 2,
           subsample=1,
           colsample_bytree=0.6,
           num_boost_round=300,
           nrounds = 200)
  
  set.seed(123)
  #Entrenamos el modelo
  xgModel <- xgb.train(p, dtrain, p$nrounds,
                       list(val = dtest), print_every_n = 1, 
                       early_stopping_rounds = 50)
  
  
  #Predecimos el modelo para los datos que necesitamos predecir
  dPredict = xgb.DMatrix(data=data.matrix(contactosPredict[, variablesToModel]))
  scores <- predict(xgModel, dPredict)
  
  output <- data.frame(id = contactosPredict[, config$columnas$ID], 
                       prediccion = scores)
  
  return(list(prediccion = output, modelo = xgModel))
  
}




#' Title
#'
#' @param datosSplit 
#' @param config 
#'
#' @return
#' @export
#'
#' @examples
seleccionColumnas <- function(datosSplit, config) {
  #Seleccionamos las columnas que vamos a necesitar para la prediccion
  
  contactosPredict <- datosSplit$contactosPredict
  contactosEntrenados <- datosSplit$contactosEntrenados
  
  
  #Por un lado las columnas de las diferencias de fechas
  columnasDiff <- grep("diff", colnames(contactosEntrenados), ignore.case = T,
                       value = T)
  
  #Las columnas con los ratios de los mails
  columnasRatioMail <- grep("mails_", colnames(contactosEntrenados), ignore.case = T,
                            value = T)
  
  #Las columnas con las dummies de la fuente original
  columnasOriginalSource <- grep(config$columnas$fuenteOriginal, colnames(contactosEntrenados),
                                 ignore.case = T, value = T)
  
  #Agrupamos todas esas columnas que van a entrar al modelo
  variablesToModel <- c(config$columnas$predictorasNumericas, config$columnas$mails$mailsDl,
                        config$columnas$mails$mailsCl, config$columnas$mails$mailsOp,
                        columnasOriginalSource, columnasRatioMail, columnasDiff, "gmail")
  
  #Añadimos las columnas que son necesarias guardar
  colNecesarias <- c(variablesToModel, config$columnas$ID, config$columnas$llamada)
  
  #El dataframe entrenado tiene la columna target, pero el dataframe de predict no por lo que la añadimos
  colNecesariasEnt <- c(colNecesarias, "target")
  
  if(!all(colNecesariasEnt %in% colnames(contactosEntrenados))){
    
    logerror("Alguna variable predictora no se ha encontrado", logger = 'log')
    stop()
  }
  
  #Nos quedamos con las columnas que queremos de cada dataset
  
  contactosEntrenados <- contactosEntrenados[, colNecesariasEnt]
  contactosPredict <- contactosPredict[, colNecesarias]
  
  return(list(contactosEntrenados = contactosEntrenados, 
              contactosPredict = contactosPredict, 
              variablesToModel = variablesToModel))
}