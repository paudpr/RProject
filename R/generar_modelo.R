#' Title
#'
#' @param datos 
#' @param config 
#'
#' @import sgd
#' @import e1071
#' @import logging
#' @import randomForest
#'
#' @examples
generar_modelo <- function(datos, config){

  datos <- selec_column(datos, config) #datos del DF que utilizo para entrenar
  
  num_train <- round(nrow(datos) * 0.7) #numero de datos para entrenar
  posicion_train <- sample(1:nrow(datos), size = num_train) #posiciones de los datos para entrenar en el DF
  
  train <- datos[posicion_train,] #conjunto de datos para train
  test <- datos[-posicion_train,] #conjunto de datos para test

  #modelo 1 --> Multiple LinearRegression
  linear_mod <- lm(train$ener_pers ~ ., data = train) #data es el grupo de datos xra entrenar
  test$ener_pers_predic <- predict(linear_mod, test)
  linear_mod_acc <- abs(sum(test$ener_pers - test$ener_pers_predic)/nrow(test))
  
  #modelo 2 --> SGDRegression 
  sgd_mod <- sgd(train$ener_pers ~ ., data = train, model = "lm")
  test$ener_pers_predic <- predict(sgd_mod, test)
  sgd_mod_acc <- abs(sum(test$ener_pers - test$ener_pers_predic)/nrow(test))
  
  
  #modelo 3 --> SVR Linear
  svr_mod <- svm(train$ener_pers ~ ., data = train)
  test$ener_pers_predic <- predict(svr_mod, test)
  svr_mod_acc <- abs(sum(test$ener_pers - test$ener_pers_predic)/nrow(test))
  
  
  #modelo 4 --> RandomForest
  rand_for_mod <- randomForest(train$ener_pers ~ ., data = train, importance = TRUE, ntrees = 50)
  test$ener_pers_predic <- predict(rand_for_mod, test)
  rand_for_mod_acc <- abs(sum(test$ener_pers - test$ener_pers_predic)/nrow(test))
  
  accuracies <- data.frame(models = c("Linear Regre", "SGD Regre", "SVM Regre", "RandomForest"), 
                           accuracy = c(linear_mod_acc, sgd_mod_acc, svr_mod_acc, rand_for_mod_acc))
  accuracies
}