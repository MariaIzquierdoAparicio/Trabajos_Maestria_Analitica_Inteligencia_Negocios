# 2. Instalación de paquetes y descripción ----
library(readr)
library(readxl) #Para cargar bases de datos en Excel
library(psych) #Para estadistica descriptiva
library(e1071) #Offers quick and easy implementation of SVMs.
library("SmartEDA") #Para describir los datos (Análisis univariable) - punto 5.1
library(DataExplorer) #Para generar graphicos de los datos - punto 5.1 y 5.2
library("cluster") # Para calcular gap statistic
library("fpc")#Para evalur k-means cluster con ASW (average sillouethe width)
library(dplyr) #Para sacar muestras aleatorias de un dataset
library(ggplot2)#Para hacer graficos
library(caret)#para crear variables dummy
library(reshape2)#Para reestructurar y agregar data usado en el punto 8.3
library("vcd")#Para visualizar data categorica - usado en el punto 8.3 grafico de mosaico
library("corrplot")#Para realizar grafico de correlaciones
library(lmtest)
library(MASS)
library(leaps)
library(glmnet)
library(caret)

# 3. Definir carpeta en la que voy a trabajar ----
setwd("~/OneDrive - Pontificia Universidad Javeriana/MFIA - PUJ - Maestría/Semestre I/1 Métodos y Aplicaciones de analitica I/Taller Individual 2")

# 4. Carga de datos -----
gc() 
rm(list = ls())
data_evaluation = read_csv("test.csv")
data = read_csv("train.csv")


# 6. Transformación de datos---- 
# Base de training y de testing
semilla = 611101
set.seed(semilla) #Se deja la semilla para que el muestreo sea replicable

#Definirel tamaño de la muestra, en este caso entrenamiento tendrá el 80% de los casos
sample = sample.int(nrow(data), floor(.8*nrow(data)))
data_train = data[sample, ]
data_test = data[-sample, ]

# Comparación de Modelos
rows = 40
RMSE_table = data.frame(Iteración = rep(NA, rows), 
                        Modelo = rep(NA, rows),
                        r2= rep(NA, rows),
                        r2_Adj = rep(NA, rows),
                        AIC = rep(NA, rows),
                        RMSE_train = rep(NA,rows),
                        RMSE = rep(NA, rows),
                        Score_Kaggle = rep(NA,rows))

##Transformación de variables -------

### Dummys para Categoricas ------
data_train$idmercado_1 = ifelse(data_train$idmercado==1,1,0)
data_train$idmercado_2 = ifelse(data_train$idmercado==2,1,0)
data_train$idmercado_3 = ifelse(data_train$idmercado==3,1,0)
data_train$idmercado_4 = ifelse(data_train$idmercado==4,1,0)
data_train$idmercado_5 = ifelse(data_train$idmercado==5,1,0)  
data_train$idmercado_6 = ifelse(data_train$idmercado==6,1,0)  
data_train$idmercado_7 = ifelse(data_train$idmercado==7,1,0)  
data_train$idmercado_8 = ifelse(data_train$idmercado==8,1,0)

data_train$promo_1 = ifelse(data_train$promo==1,1,0)
data_train$promo_2 = ifelse(data_train$promo==2,1,0)

data_train$tamamer_Grande = ifelse(data_train$tamamer=="Grande",1,0)
data_train$tamamer_Median = ifelse(data_train$tamamer=="Median",1,0)

data_test$idmercado_1 = ifelse(data_test$idmercado==1,1,0)
data_test$idmercado_2 = ifelse(data_test$idmercado==2,1,0)
data_test$idmercado_3 = ifelse(data_test$idmercado==3,1,0)
data_test$idmercado_4 = ifelse(data_test$idmercado==4,1,0)
data_test$idmercado_5 = ifelse(data_test$idmercado==5,1,0)  
data_test$idmercado_6 = ifelse(data_test$idmercado==6,1,0)  
data_test$idmercado_7 = ifelse(data_test$idmercado==7,1,0)  
data_test$idmercado_8 = ifelse(data_test$idmercado==8,1,0)

data_test$promo_1 = ifelse(data_test$promo==1,1,0)
data_test$promo_2 = ifelse(data_test$promo==2,1,0)

data_test$tamamer_Grande = ifelse(data_test$tamamer=="Grande",1,0)
data_test$tamamer_Median = ifelse(data_test$tamamer=="Median",1,0)

data_evaluation$idmercado_1 = ifelse(data_evaluation$idmercado==1,1,0)
data_evaluation$idmercado_2 = ifelse(data_evaluation$idmercado==2,1,0)
data_evaluation$idmercado_3 = ifelse(data_evaluation$idmercado==3,1,0)
data_evaluation$idmercado_4 = ifelse(data_evaluation$idmercado==4,1,0)
data_evaluation$idmercado_5 = ifelse(data_evaluation$idmercado==5,1,0)  
data_evaluation$idmercado_6 = ifelse(data_evaluation$idmercado==6,1,0)  
data_evaluation$idmercado_7 = ifelse(data_evaluation$idmercado==7,1,0)  
data_evaluation$idmercado_8 = ifelse(data_evaluation$idmercado==8,1,0)

data_evaluation$promo_1 = ifelse(data_evaluation$promo==1,1,0)
data_evaluation$promo_2 = ifelse(data_evaluation$promo==2,1,0)

data_evaluation$tamamer_Grande = ifelse(data_evaluation$tamamer=="Grande",1,0)
data_evaluation$tamamer_Median = ifelse(data_evaluation$tamamer=="Median",1,0)

### Interacción de Correo y Paginas -----

data_test$correo_paginas = data_test$correo*data_test$paginas
data_train$correo_paginas = data_train$correo*data_train$paginas
data_evaluation$correo_paginas = data_evaluation$correo*data_evaluation$paginas

### Interacción de Impresa y IDMercado -----

data_test$impresa_idmercado = data_test$impresa * data_test$idmercado
data_train$impresa_idmercado = data_train$impresa * data_train$idmercado
data_evaluation$impresa_idmercado = data_evaluation$impresa * data_evaluation$idmercado

### X^2 de variables numericas ------ 

data_test$edadloc_sqr = (data_test$edadloc)^2
data_train$edadloc_sqr = (data_train$edadloc)^2
data_evaluation$edadloc_sqr = (data_evaluation$edadloc)^2

data_test$correo_sqr = (data_test$correo)^2
data_train$correo_sqr = (data_train$correo)^2
data_evaluation$correo_sqr = (data_evaluation$correo)^2

data_test$paginas_sqr = (data_test$paginas)^2
data_train$paginas_sqr = (data_train$paginas)^2
data_evaluation$paginas_sqr = (data_evaluation$paginas)^2

data_test$telefono_sqr = (data_test$telefono)^2
data_train$telefono_sqr = (data_train$telefono)^2
data_evaluation$telefono_sqr = (data_evaluation$telefono)^2

data_test$impresa_sqr = (data_test$impresa)^2
data_train$impresa_sqr = (data_train$impresa)^2
data_evaluation$impresa_sqr = (data_evaluation$impresa)^2

data_test$servicio_sqr = (data_test$servicio)^2
data_train$servicio_sqr = (data_train$servicio)^2
data_evaluation$servicio_sqr = (data_evaluation$servicio)^2

data_test$nomina_sqr = (data_test$nomina)^2
data_train$nomina_sqr = (data_train$nomina)^2
data_evaluation$nomina_sqr = (data_evaluation$nomina)^2

### √x raiz cuadrada de variables numericas ------ 

data_test$sqrt_edadloc = sqrt(data_test$edadloc)
data_train$sqrt_edadloc = sqrt(data_train$edadloc)
data_evaluation$sqrt_edadloc = sqrt(data_evaluation$edadloc)

data_test$sqrt_correo = sqrt(data_test$correo)
data_train$sqrt_correo = sqrt(data_train$correo)
data_evaluation$sqrt_correo = sqrt(data_evaluation$correo)

data_test$sqrt_paginas = sqrt(data_test$paginas)
data_train$sqrt_paginas = sqrt(data_train$paginas)
data_evaluation$sqrt_paginas = sqrt(data_evaluation$paginas)

data_test$sqrt_telefono = sqrt(data_test$telefono)
data_train$sqrt_telefono = sqrt(data_train$telefono)
data_evaluation$sqrt_telefono = sqrt(data_evaluation$telefono)

data_test$sqrt_impresa = sqrt(data_test$impresa)
data_train$sqrt_impresa = sqrt(data_train$impresa)
data_evaluation$sqrt_impresa = sqrt(data_evaluation$impresa)

data_test$sqrt_servicio = sqrt(data_test$servicio)
data_train$sqrt_servicio = sqrt(data_train$servicio)
data_evaluation$sqrt_servicio = sqrt(data_evaluation$servicio)

data_train$sqrt_nomina = sqrt(data_train$nomina)
data_test$sqrt_nomina = sqrt(data_test$nomina)
data_evaluation$sqrt_nomina = sqrt(data_evaluation$nomina)

### 1/X de variables numericas -------

data_test$edadloc_inversa = 1/(data_test$edadloc)
data_train$edadloc_inversa = 1/(data_train$edadloc)
data_evaluation$edadloc_inversa = 1/(data_evaluation$edadloc)

data_test$correo_inversa = 1/(data_test$correo)
data_train$correo_inversa = 1/(data_train$correo)
data_evaluation$correo_inversa = 1/(data_evaluation$correo)

data_test$paginas_inversa = 1/(data_test$paginas)
data_train$paginas_inversa = 1/(data_train$paginas)
data_evaluation$paginas_inversa = 1/(data_evaluation$paginas)

data_test$telefono_inversa = 1/(data_test$telefono)
data_train$telefono_inversa = 1/(data_train$telefono)
data_evaluation$telefono_inversa = 1/(data_evaluation$telefono)

data_test$impresa_inversa = 1/(data_test$impresa)
data_train$impresa_inversa = 1/(data_train$impresa)
data_evaluation$impresa_inversa = 1/(data_evaluation$impresa)

data_test$servicio_inversa = 1/(data_test$servicio)
data_train$servicio_inversa = 1/(data_train$servicio)
data_evaluation$servicio_inversa = 1/(data_evaluation$servicio)

data_train$nomina_inversa = 1/(data_train$nomina)
data_test$nomina_inversa = 1/(data_test$nomina)
data_evaluation$nomina_inversa = 1/(data_evaluation$nomina)

### 1/X^2 de variables numericas -------

data_test$edadloc_inversa_sqr = 1/(data_test$edadloc^2)
data_train$edadloc_inversa_sqr = 1/(data_train$edadloc^2)
data_evaluation$edadloc_inversa_sqr = 1/(data_evaluation$edadloc^2)

data_test$correo_inversa_sqr = 1/(data_test$correo^2)
data_train$correo_inversa_sqr = 1/(data_train$correo^2)
data_evaluation$correo_inversa_sqr = 1/(data_evaluation$correo^2)

data_test$paginas_inversa_sqr = 1/(data_test$paginas^2)
data_train$paginas_inversa_sqr = 1/(data_train$paginas^2)
data_evaluation$paginas_inversa_sqr = 1/(data_evaluation$paginas^2)

data_test$telefono_inversa_sqr = 1/(data_test$telefono^2)
data_train$telefono_inversa_sqr = 1/(data_train$telefono^2)
data_evaluation$telefono_inversa_sqr = 1/(data_evaluation$telefono^2)

data_test$impresa_inversa_sqr = 1/(data_test$impresa^2)
data_train$impresa_inversa_sqr = 1/(data_train$impresa^2)
data_evaluation$impresa_inversa_sqr = 1/(data_evaluation$impresa^2)

data_test$servicio_inversa_sqr = 1/(data_test$servicio^2)
data_train$servicio_inversa_sqr = 1/(data_train$servicio^2)
data_evaluation$servicio_inversa_sqr = 1/(data_evaluation$servicio^2)

data_test$nomina_inversa_sqr = 1/(data_test$nomina^2)
data_train$nomina_inversa_sqr = 1/(data_train$nomina^2)
data_evaluation$nomina_inversa_sqr = 1/(data_evaluation$nomina^2)

### LOG(X) de variables numericas ------ 
data_train$ln_edadloc = log(data_train$edadloc)
data_test$ln_edadloc = log(data_test$edadloc)
data_evaluation$ln_edadloc = log(data_evaluation$edadloc)

data_train$ln_correo = log(data_train$correo)
data_test$ln_correo = log(data_test$correo)
data_evaluation$ln_correo = log(data_evaluation$correo)

data_train$ln_paginas = log(data_train$paginas)
data_test$ln_paginas = log(data_test$paginas)
data_evaluation$ln_paginas = log(data_evaluation$paginas)

data_train$ln_telefono = log(data_train$telefono)
data_test$ln_telefono = log(data_test$telefono)
data_evaluation$ln_telefono = log(data_evaluation$telefono)

data_train$ln_impresa = log(data_train$impresa)
data_test$ln_impresa = log(data_test$impresa)
data_evaluation$ln_impresa = log(data_evaluation$impresa)

data_train$ln_servicio = log(data_train$servicio)
data_test$ln_servicio = log(data_test$servicio)
data_evaluation$ln_servicio = log(data_evaluation$servicio)

data_train$ln_nomina = log(data_train$nomina)
data_test$ln_nomina = log(data_test$nomina)
data_evaluation$ln_nomina = log(data_evaluation$nomina)

### e^X de variables numericas ------ 

data_train$e_edadloc = exp(data_train$edadloc)
data_test$e_edadloc = exp(data_test$edadloc)
data_evaluation$e_edadloc = exp(data_evaluation$edadloc)

# data_train$e_correo = exp(data_train$correo)
# data_test$e_correo = exp(data_test$correo)
# data_evaluation$e_correo = exp(data_evaluation$correo)

data_train$e_paginas = exp(data_train$paginas)
data_test$e_paginas = exp(data_test$paginas)
data_evaluation$e_paginas = exp(data_evaluation$paginas)

data_train$e_telefono = exp(data_train$telefono)
data_test$e_telefono = exp(data_test$telefono)
data_evaluation$e_telefono = exp(data_evaluation$telefono)

# data_train$e_impresa = exp(data_train$impresa)
# data_test$e_impresa = exp(data_test$impresa)
# data_evaluation$e_impresa = exp(data_evaluation$impresa)

data_train$e_servicio = exp(data_train$servicio)
data_test$e_servicio = exp(data_test$servicio)
data_evaluation$e_servicio = exp(data_evaluation$servicio)

# data_train$e_nomina = exp(data_train$nomina)
# data_test$e_nomina = exp(data_test$nomina)
# data_evaluation$e_nomina = exp(data_evaluation$nomina)



## MODELAJE ----

### Modelaje con todo y stepwise -----
N=31
# Regresión
modelo1 = lm(ropamujer ~.,data=data_train)
summary(modelo1)

modelo1<- step(modelo1,direction="both")
summary(modelo1)

RMSE_table$Iteración[N]=N
RMSE_table$Modelo[N] = c("Stepwise de modelo con todo")

# Comprobaciones de supuestos
AIC(modelo1)
RMSE_table$AIC[N]=AIC(modelo1)
dwtest(modelo1)
layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
plot(modelo1)

# Predicciones
predicciones1 = predict(modelo1, data_test,se.fit = TRUE)
RMSE1<-sqrt(mean((predicciones1$fit-data_test$ropamujer)^2))
RMSE1
RMSE_table$RMSE[N]=RMSE1

RMSE_train1<-sqrt(mean((predicciones1$fit-data_train$ropamujer)^2))
RMSE_train1
RMSE_table$RMSE_train[N]=RMSE_train1

#Evaluación/Solución
evaluacion1 = predict(modelo1, data_evaluation,se.fit = TRUE)
solucion1 = data_evaluation[,c(1,12)]
solucion1$ropamujer = evaluacion1$fit
write.csv(solucion1, "solucion_con_todo.csv", row.names = FALSE)

### Modelaje con todo pero sin IDLOC ----
N=32

data_train1 = data_train[,-1]
data_test1 = data_test[,-1]
data_evaluation1 = data_evaluation[,]

#Regresión
modelo12<-lm(ropamujer ~.,data=data_train1)
summary(modelo12)

modelo12<- step(modelo12,direction="both")
summary(modelo12)

RMSE_table$Iteración[N]=N
RMSE_table$Modelo[N] = c("modelo con todo sin idloc y stepwise")

# Comprobaciones de supuestos
AIC(modelo12)
RMSE_table$AIC[N]=AIC(modelo12)
dwtest(modelo12)
layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
plot(modelo12)

# Predicciones
predicciones12 = predict(modelo12, data_test1,se.fit = TRUE)
RMSE12<-sqrt(mean((predicciones12$fit-data_test1$ropamujer)^2))
RMSE12
RMSE_table$RMSE[N]=RMSE12

RMSE_train12<-sqrt(mean((predicciones12$fit-data_train1$ropamujer)^2))
RMSE_train12
RMSE_table$RMSE_train[N]=RMSE_train12

#Evaluación
evaluacion12 = predict(modelo12, data_evaluation1,se.fit = TRUE)
solucion12 = data_evaluation1[,c(1,12)]
solucion12$ropamujer = evaluacion12$fit
write.csv(solucion12, "solucion_con_todo_sin_idloc.csv", row.names = FALSE)

### RIdge y Lasso Modelaje con todo pero sin IDLOC ------
columns_to_keep <- c("ropamujer","edadloc","telefono","impresa","servicio","idmercado","impresa_idmercado","telefono_sqr","impresa_sqr","sqrt_edadloc","sqrt_correo","sqrt_paginas","sqrt_telefono","sqrt_impresa","sqrt_servicio","edadloc_inversa","paginas_inversa","telefono_inversa","impresa_inversa","servicio_inversa","edadloc_inversa_sqr","servicio_inversa_sqr","ln_edadloc","ln_correo","ln_telefono","ln_servicio","e_edadloc","ln_servicio","e_edadloc")
data_trainRD <- data_train[, (names(data_train) %in% columns_to_keep)]
data_testRD <- data_test[, (names(data_test) %in% columns_to_keep)]
data_evaluationRD <- data_evaluation[, (names(data_evaluation) %in% columns_to_keep)]

# Seprar predictores de variable a predecir
y.data_train = as.matrix(data_trainRD[,6])
y.data_test = as.matrix(data_testRD[,6])
y.data_evaluation = as.matrix(data_evaluationRD[,6])

# Estandarizar
data_trainA<-as.data.frame(scale(data_trainRD[,1:5]))
data_testA<-as.data.frame(scale(data_testRD[,1:5]))
data_evaluationA<-as.data.frame(scale(data_evaluationRD[,1:5]))

data_trainB<-scale(data_trainRD[,7:27])
data_testB<-as.data.frame(scale(data_testRD[,7:27]))
data_evaluationB<-as.data.frame(scale(data_evaluationRD[,7:27]))

x.data_train = as.matrix(cbind(data_trainA, data_trainB))
x.data_test = as.matrix(cbind(data_testA, data_testB))
x.data_evaluation = as.matrix(cbind(data_evaluationA, data_evaluationB))

# Modelo GLMnet con Ridge y Lasso
fitlasso<-glmnet(x.data_train,y.data_train,alpha = 1)
fitridge<-glmnet(x.data_train,y.data_train,alpha = 0)

plot(fitlasso, xvar="lambda")
plot(fitridge, xvar="lambda")

foundridge<-cv.glmnet(x.data_train, y.data_train,alpha=0,nfolds=5)
plot(foundridge)
foundlasso<-cv.glmnet(x.data_train, y.data_train,alpha=1,nfolds=5)
plot(foundlasso)

# Revisar los landas mínimosveo los lambda minimos
foundridge$lambda.1se
foundridge$lambda.min
coef(foundridge,s=foundridge$lambda.1se)# Solo se puede interpretar signo y magnitud en valo absoluto

foundlasso$lambda.1se
foundlasso$lambda.min
coef(fitlasso,s=foundlasso$lambda.min)

# Modelo de GLMnet con Redes elásticas
for (i in 1:10){
  assign(paste("found", i, sep=""), cv.glmnet(x.data_train, y.data_train, nfolds=5,
                                              alpha=i/10,))}
# Revisar mejor red elastica
min(foundridge$cvm)
min(found1$cvm)
min(found2$cvm)
min(found3$cvm)
min(found4$cvm)
min(found5$cvm)
min(found6$cvm)
min(found7$cvm)
min(found8$cvm)
min(found9$cvm)
min(foundlasso$cvm)  

# Selecciono redes elasticas
elastic5<-glmnet(x.data_train,y.data_train,alpha = 0.5)
coef(elastic5,s=found5$lambda.1se)

# Predicciones
# red elastica
predicciones1<-predict.glmnet(elastic5, x.data_test, s=elastic5$lambda.min)
#ridge
predicciones2<-predict.glmnet(fitridge, x.data_test, s=foundridge$lambda.min)
#lasso
predicciones3<-predict.glmnet(fitlasso, x.data_test, s=foundlasso$lambda.min)

# Calcular el MSE
erroreselastic=sqrt(mean((predicciones1-y.data_test[,1])^2))
erroresridge=sqrt(mean((predicciones2-y.data_test[,1])^2))
erroreslasso=sqrt(mean((predicciones3-y.data_test[,1])^2))

# Imprimir Errores 
erroreselastic
erroresridge
erroreslasso

#Evaluación 
# red elastica
evaluacion_rd1<-predict.glmnet(elastic5, x.data_evaluation, s=elastic5$lambda.min)
#ridge
evaluacion_ridge1<-predict.glmnet(fitridge, x.data_evaluation, s=foundridge$lambda.min)
#lasso
evaluacion_lasso1<-predict.glmnet(fitlasso, x.data_evaluation, s=foundlasso$lambda.min)

# Mandar a CSV
evaluacion_ridge1 = cbind(data_evaluation[,1],evaluacion_ridge1)
colnames(evaluacion_ridge1)[colnames(evaluacion_ridge1) == "s1"] <- "ropamujer"

evaluacion_lasso1 = cbind(data_evaluation[,1],evaluacion_lasso1)
colnames(evaluacion_lasso1)[colnames(evaluacion_lasso1) == "s1"] <- "ropamujer"


write.csv(evaluacion_ridge1, "evaluacion_ridge_contodo_sin_idloc.csv", row.names = FALSE)
write.csv(evaluacion_lasso1, "evaluacion_lasso_contodo_sin_idloc.csv", row.names = FALSE)

# Completamos tabla RMSE
# Ridge
RMSE_table$Iteración[N]=N
RMSE_table$Modelo[N] = "Modelo con todo pero sin idloc con regularización Ridge"
RMSE_table$RMSE[N] = erroresridge
# Lasso
RMSE_table$Iteración[N+1]=N+1
RMSE_table$Modelo[N+1] = "Modelo con todo pero sin idloc con regularización Lasso"
RMSE_table$RMSE[N+1] = erroreslasso
# Red Elastica
RMSE_table$Iteración[N+2]=N+2
RMSE_table$Modelo[N+2] = "Modelo con todo pero sin idloc con regularización redes elasticas con alpha = 0.5"
RMSE_table$RMSE[N+2] = erroreselastic


