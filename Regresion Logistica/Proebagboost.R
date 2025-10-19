# 1. Instalación de paquetes y descripción ----
install.packages(c("psych","e1071")) #psych Para estadistica descriptiva y e1071 para quick and easy implementation of SVMs.
install.packages("SmartEDA") #Para describir los datos (Análisis univariable)
install.packages("DataExplorer") #Para generar graphicos de los datos - punto 5.1 y 5.2
install.packages("fpc") #Para evalur k-means cluster con ASW (average sillouethe width)
install.packages("reshape2") #Para reestructurar y agregar data
install.packages("vcd")#Para visualizar data categorica - usado en el punto 8.3 grafico de mosaico
install.packages("corrplot")#Para realizar grafico de correlaciones
install.packages("ggplot2")
install.packages("cowplot")
install.packages("lmtest")
install.packages("MASS")
install.packages("leaps")
install.packages("caret") #Para Crear variables dummy y regularización
install.packages("glmnet")#Para Regularización
install.packages("gridExtra")#Para combinar ggplots
install.packages("ggthemes") #Para themes de ggplots :)
install.packages('lift')
install.packages('ROCR')
install.packages('ROSE')#para balancear over y undersamplibg
install.packages("bestglm") #best glm best subsets for logt

# 2. Instalación de librerias y descripción ----
library(readr)
library(readxl) #Para cargar bases de datos en Excel
library(psych) #Para estadistica descriptiva
library(e1071) #Offers quick and easy implementation of SVMs.
library(SmartEDA) #Para describir los datos (Análisis univariable) - punto 5.1
library(DataExplorer) #Para generar graphicos de los datos - punto 5.1 y 5.2
library(cluster) # Para calcular gap statistic
library(fpc)#Para evalur k-means cluster con ASW (average sillouethe width)
library(dplyr) #Para sacar muestras aleatorias de un dataset
library(ggplot2)#Para hacer graficos
library(caret)#para crear variables dummy
library(reshape2)#Para reestructurar y agregar data usado en el punto 8.3
library(vcd)#Para visualizar data categorica - usado en el punto 8.3 grafico de mosaico
library(corrplot)#Para realizar grafico de correlaciones
library(lmtest)
library(MASS)
library(leaps)
library(glmnet)
library(caret)
library(gridExtra)
library(ggthemes)
library(ROCR)
#library(lift)
library(ROSE)

library(tidyverse)
library(skimr)
library(ggpubr)
library(C50)
library(tidymodels)
library(doParallel)
library(xgboost)
library(gbm)
library(pROC)

# 3. Definir carpeta en la que voy a trabajar ----
setwd("/Users/juancuellar/Desktop/Maestría/Primer Semestre/Analítica 1/Talleres/Grupal/Taller Grupal 2")

# 4. Carga de datos -----
gc() 
rm(list = ls())
data_evaluacion = read_excel("testelco.xlsx")
data = read_excel("traintelco.xlsx")


## 6.1. Creación Variables dummy hombres, mujeres y empresas con base en tipo de cliente -----
data$hombres=ifelse(data$tipo_cliente==1,1,0)
data$mujeres=ifelse(data$tipo_cliente==2,1,0)
data$empresas=ifelse(data$tipo_cliente==3,1,0)

data_evaluacion$hombres=ifelse(data_evaluacion$tipo_cliente==1,1,0)
data_evaluacion$mujeres=ifelse(data_evaluacion$tipo_cliente==2,1,0)
data_evaluacion$empresas=ifelse(data_evaluacion$tipo_cliente==3,1,0)

data$id <- NULL
#data_evaluacion$id <- NULL
# data <- data[data$facturación >= 0, ]
# data_evaluacion <- data[data$facturación >= 0, ]

## 6.2. Determinar variables categoricas como factores -----
data$tipo_cliente=as.factor(data$tipo_cliente)
data_evaluacion$tipo_cliente=as.factor(data_evaluacion$tipo_cliente)

# data$resultado = as.factor(data$resultado)

## 6.3. Balanceo con Oversampling ------
### I. Dataset test -----
# Separación de resultado=0 y resultado=1
table(data$resultado)


data_cero <- subset(data,data$resultado==0)
data_uno <- subset(data,data$resultado==1)


# -----


# Creación de Semilla
semilla = 611101
set.seed(semilla) # Se deja la semilla para que el muestreo sea replicable


# Creación dataset test (20% de ceros)
sample_cero = sample.int(nrow(data_cero), round(0.2 * nrow(data_cero)))
data_cero_test = data_cero[sample_cero, ]
data_cero_rest = data_cero[-sample_cero, ]

# Creación dataset test (20% de unos)
sample_uno = sample.int(nrow(data_uno), round(0.2 * nrow(data_uno)))
data_uno_test = data_uno[sample_uno, ]
data_uno_rest = data_uno[-sample_uno, ]

# Fundo las dos
data_test = rbind(data_cero_test, data_uno_test)

# Realizar oversampling de la clase minoritaria (unos)
oversample_factor = nrow(data_cero_rest) / nrow(data_uno_rest)
sample_uno_oversampled = data_uno_rest[rep(1:nrow(data_uno_rest), times = ceiling(oversample_factor)), ]

# Creación dataset train (50% de unos)
sample_uno_train = sample.int(nrow(sample_uno_oversampled), round(0.5 * nrow(sample_uno_oversampled)))
data_uno_train = sample_uno_oversampled[sample_uno_train, ]
data_uno_valid = sample_uno_oversampled[-sample_uno_train, ]

# Pongo la misma cantidad de ceros y unos en entrenamiento
sample_cero_train = sample.int(nrow(data_cero_rest), nrow(data_uno_train))
data_cero_train = data_cero_rest[sample_cero_train, ]
data_cero_valid = data_cero_rest[-sample_cero_train, ]

# Creo entrenamiento y validación
data_train = rbind(data_cero_train, data_uno_train)
data_valid = rbind(data_cero_valid, data_uno_valid)





### III. Validación por conteo de filas -----
#Data general
table(data$resultado)
prop.table(table(data$resultado))

#Data_train
table(data_train$resultado)
prop.table(table(data_train$resultado))

#Data_valid
table(data_valid$resultado)
prop.table(table(data_valid$resultado))

#Data_test
table(data_test$resultado)
prop.table(table(data_test$resultado))

## 6.3. Eliminación de columnas no necesarias
#data_train = data_train[,-c(2,3,9)]
#data_valid = data_valid[,-c(2,3,9)]
#data_test  = data_test[,-c(2,3,9)]

data_train = data_train[,-c(1,2,8)]
data_valid = data_valid[,-c(1,2,8)]
data_test  = data_test[,-c(1,2,8)]

### Arbol C5.0----


data_train$resultado<-as.factor(data_train$resultado)
str(data_train$resultado)

arbol_c50 <- C5.0(
  formula = resultado ~ ., 
  data    = data_train,
  trials  = 1,
  rules   = FALSE,
  control = C5.0Control(seed = 611101)
)


summary(arbol_c50)


predicciones <- predict(arbol_c50, newdata = data_valid, type = "class")
table(predicho = predicciones, real = data_valid$resultado)


error_clas <- mean(predicciones != data_test$resultado)
paste(
  "El error de clasificación es del:", 100 * error_clas, "%.", 
  sum(predicciones == data_valid$resultado),
  "clasificaciones correctas de un total de",
  length(predicciones)
)

predicciones <- predict(arbol_c50, newdata = data_valid, type = "prob")
head(predicciones, 3)

modelo_c50_boost <- C5.0(
  formula = resultado ~ ., 
  data    = data_train,
  trials  = 100,
  rules   = FALSE,
  control = C5.0Control(seed = 611101)
)


predicciones <- predict(modelo_c50_boost, newdata = data_valid, type = "class")
table(predicho = predicciones, real = data_valid$resultado)

Predc50_boost <- predict(modelo_c50_boost, newdata = data_evaluacion)
solucion_C50_boost = cbind(id = data_evaluacion$id,
                    resultado = Predc50_boost)
write.csv(solucion_C50_boost, "solucion_c50Boost_Over.csv", row.names = FALSE)    


error_clas <- mean(predicciones != data_valid$resultado)
paste("El error de clasificación es del:", 100 * error_clas, "%.", 
      sum(predicciones == data_valid$resultado),
      "clasificaciones correctas de un total de", length(predicciones))




### Boosting-----


data_train <- xgb.DMatrix(
  data  = as.matrix(data_train %>% select(-resultado)),
  label = data_train$resultado
)

data_valid <- xgb.DMatrix(
  data  = as.matrix(data_valid %>% select(-resultado)),
  label = data_valid$resultado
)

datos_test <- xgb.DMatrix(
  data  = as.matrix(data_test %>% select(-resultado)),
  label = data_test$resultado
)

#####    Quitar ----

data_train <- xgb.DMatrix(
  data  = data_train %>% select(-resultado) %>% data.matrix(),
  label = data_train$resultado
)

data_validt <- xgb.DMatrix(
  data  = data_valid %>% select(-resultado) %>% data.matrix(),
  label = data_valid$resultado
)


datos_test <- xgb.DMatrix(
  data  = data_test %>% select(-resultado) %>% data.matrix(),
  label = data_test$resultado
)
### Boosting----  

set.seed(semilla)
modelo_xg <- xgb.train(
  data    = data_train,
  params  = list(max_depth = 2),
  nrounds = 10,
  
)
modelo_xg

predicciones <- predict( modelo_xg, newdata = data_valid)

matriz_evaluacion <- as.matrix(data_evaluacion)
matriz_evaluacion <- apply(matriz_evaluacion, 2, as.numeric)

# Crea un objeto xgb.DMatrix a partir de la matriz
datos_evaluacion <- xgb.DMatrix(data = matriz_evaluacion)

pred_xgb <- predict(modelo_xg, newdata = data_evaluacion)
solucion_C50_boost = cbind(id = data_evaluacion$id,
                           resultado = pred_xgb)
write.csv(solucion_C50_boost, "solucion_c50Boost_Over.csv", row.names = FALSE) 

test_rmse <- sqrt(mean((predicciones - getinfo(data_valid, "label"))^2))
paste("Error de test (rmse) del modelo:", round(test_rmse,2))
# Calcular el AUC
roc_values <- roc(data_valid$resultado, predicciones)
print(auc(roc_values))
