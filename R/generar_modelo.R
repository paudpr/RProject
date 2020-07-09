#' Title
#'
#' @param datos 
#' @param config 
#'
#' @import sgd
#' @import e1071
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
  linear_mod <- lm(train$ener_pers ~ ., data = train) #data es el grupo d datos xra entrenar
  print(linear_mod)
  linear_mod_summary <- summary(linear_mod)
  
  test$ener_pers_predic <- predict(linear_mod, test)
  linear_mod_acc <- abs(sum(test$ener_pers - test$ener_pers_predic)/nrow(test))
  print(linear_mod_acc)
    
  #modelo 2 --> SGDRegression 
  sgd_mod <- sgd(train$ener_pers ~ ., data = train, model = "lm")
  print(sgd_mod)
  
  test$ener_pers_predic <- predict(sgd_mod, test)
  sgd_mod_acc <- abs(sum(test$ener_pers - test$ener_pers_predic)/nrow(test))
  print(sgd_mod_acc)
  
  #modelo 3 --> SVR Linear
  svr_mod <- svm(train$ener_pers ~ ., data = train)
  svr_mod
  
  test$ener_pers_predic <- predict(svr_mod, test)
  svr_mod_acc <- abs(sum(test$ener_pers - test$ener_pers_predic)/nrow(test))
  print(svr_mod_acc)
  
  #modelo 4 --> RandomForest
}