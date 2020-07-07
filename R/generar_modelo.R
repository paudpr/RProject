#' Title
#'
#' @param datos 
#' @param config 
#'
#' @import xgboost
#' @import logging
#'
#' @examples
generar_modelo <- function(datos, config){

  datos <- selec_column(datos, config) #datos del DF que utilizo para entrenar
  
  num_train <- round(nrow(datos) * 0.7) #numero de datos para entrenar
  posicion_train <- sample(1:nrow(datos), size = num_train) #posiciones de los datos para entrenar en el DF
  
  train <- datos[posicion_train,] #conjunto de datos para train
  test <- datos[-posicion_train,] #conjunto de datos para test
  
  #modelo 1 --> Multiple LinearRegression
  linearMod <- lm(ener_pers ~ ., data = train)#data es el grupo d datos xra entrenar?
  print(linearMod)
  
  #modelo 2 --> SGDRegression 
  
  
  #modelo 3 --> SVM
  
  
  #modelo 4 --> RandomForest
}