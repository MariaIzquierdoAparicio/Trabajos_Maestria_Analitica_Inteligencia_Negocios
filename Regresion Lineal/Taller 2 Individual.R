#------------------------------------------------------------------#
# Taller 2 Individual - Métodos y Aplicaciones de Analítica I
# Fecha: 16 de Septiembre de 2023
# Elaborado por: Maria Fernanda Izquierdo Aparicio
#------------------------------------------------------------------#

# 1. Instalación de librerias y descripción ----
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
library(gridExtra)
library(ggthemes)

# 3. Definir carpeta en la que voy a trabajar ----
setwd("~/OneDrive - Pontificia Universidad Javeriana/MFIA - PUJ - Maestría/Semestre I/1 Métodos y Aplicaciones de analitica I/Taller Individual 2")

# 4. Carga de datos -----
gc() 
rm(list = ls())
data_evaluation = read_csv("test.csv")
data = read_csv("train.csv")

# 5. Descripción de la base de datos -----
## 5.1. Revision de variables inicial (análisis univariado) ----
  
  ### Estructura de la data ----
  Structure = ExpData(data=data,type=2, fun = c("mean", "median", "var"))
  View(Structure)
  plot_intro(data, title="Resumen variables data")
  plot_missing(data, title = "Resumen de missing values por columna")

  # Tabla descriptiva variables numericas
  Numerical_Vars = ExpNumStat(data,by="A",gp=NULL,Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2,Nlim=1)
  Numerical_Vars$ZeroPerc = (Numerical_Vars$nZero/47871)*100
  View(Numerical_Vars)
  
  # Tabla descriptiva variables categricas con 150 niveles o mas
  Categorical_Vars = ExpCTable(data,Target=NULL,margin=1,clim=150,nlim=3,round=2,bin=NULL,per=T)
  View(Categorical_Vars)

  # Graphical representation of all numeric features using Density plot (Univariate)
  # Note: Variable excluded (if unique value of variable which is less than or eaual to 10 [nlim=10])
  # Produces a PDF with the density plots and saves it in the defined folder above
  plot1 = ExpNumViz(data=data, target=NULL,type=2, nlim=100, 
                    Page=c(3,2), fname="Density de variables numéricas",
                    gtitle="Density de variables numéricas")

  # Hisotgrama de variables numericas (incluye las binarias)
  plot_histogram(data, binary_as_factor = FALSE, 
                 title = "Histograma variables numéricas y binarias",
                 nrow = 3, ncol = 4, ggtheme = theme_bw())
  
  plot(data$ropamujer, ggtheme = theme_bw())
 
 
  ### Scatterplots de variable Y con respecto a variables -----
  scatter_plot1 <- ggplot(data = data, aes(x = idloc, y = ropamujer , color = edadloc)) +
    geom_point() +
    labs(x = "idloc", y = "Ropa Mujer", 
         title = "Ventas millones COP durante el año por tamaño de antiguedad en zona")
  print(scatter_plot1)
  
  scatter_plot2 <- ggplot(data = data, aes(x = idloc, y = ropamujer , color = correo)) +
    geom_point() +
    labs(x = "idloc", y = "Ropa Mujer", 
         title = "Ventas millones COP durante el año por número de catálogos enviados durante el año")
  print(scatter_plot2)
  
  scatter_plot3 <- ggplot(data = data, aes(x = idloc, y = ropamujer , color = paginas)) +
    geom_point() +
    labs(x = "idloc", y = "Ropa Mujer", 
         title = "Ventas millones COP durante el año por número de páginas del catálogo")
  print(scatter_plot3)
  
  scatter_plot4 <- ggplot(data = data, aes(x = idloc, y = ropamujer , color = telefono)) +
    geom_point() +
    labs(x = "idloc", y = "Ropa Mujer", 
         title = "Ventas millones COP durante el año por número promedio de líneas para llamada abiertas para pedidos")
  print(scatter_plot4)
  
  scatter_plot5 <- ggplot(data = data, aes(x = idloc, y = ropamujer , color = impresa)) +
    geom_point() +
    labs(x = "idloc", y = "Ropa Mujer", 
         title = "Ventas millones COP durante el año por cantidad gastada en publicidad impresa")
  print(scatter_plot5)
  
  scatter_plot6 <- ggplot(data = data, aes(x = idloc, y = ropamujer , color = servicio)) +
    geom_point() +
    labs(x = "idloc", y = "Ropa Mujer", 
         title = "Ventas millones COP durante el año por número de representantes del servicio al cliente")
  print(scatter_plot6)
  
  scatter_plot7 <- ggplot(data = data, aes(x = idloc, y = ropamujer , color = idmercado)) +
    geom_point() +
    labs(x = "idloc", y = "Ropa Mujer", 
         title = "Ventas millones COP durante el año por tipo de mercado")
  print(scatter_plot7)
  
  scatter_plot8 <- ggplot(data = data, aes(x = idloc, y = ropamujer , color = tamamer)) +
    geom_point() +
    labs(x = "idloc", y = "Ropa Mujer", 
         title = "Ventas millones COP durante el año por tamaño potencial del mercado")
  print(scatter_plot8)
  
  scatter_plot9 <- ggplot(data = data, aes(x = idloc, y = ropamujer , color = promo)) +
    geom_point() +
    labs(x = "idloc", y = "Ropa Mujer", 
         title = "Ventas millones COP durante el año por tipo de promoción que se llevó a cabo durante el año")
  print(scatter_plot9)
  
  scatter_plot10 <- ggplot(data = data, aes(x = idloc, y = ropamujer , color = nomina)) +
    geom_point() +
    labs(x = "idloc", y = "Ropa Mujer", 
         title = "Ventas millones COP durante el año por valor total de la nómina durante el año")
  print(scatter_plot10)
  
  
#-----#
## 5.2. Revisión de variables (analisis multivariable)----
  ### Correlación ----
  plot_correlation(na.omit(data), maxcat = 5L)
  
  ### Graficos con respecto a la variable dependiente -----
  gg1 = ggplot(data = data, aes(x = idloc, y = ropamujer)) +
    geom_point() +
    labs(title = "Gráfico 1", 
         x = "ID de la tienda", 
         y = "Ventas millones COP durante el año")+
    theme_calc()+scale_fill_calc() 
  
  gg2 = ggplot(data = data, aes(x = edadloc, y = ropamujer)) +
    geom_point() +
    labs(title = "Gráfico 2", 
         x = "Años desde la llegada a la zona", 
         y = "Ventas millones COP durante el año")+
    theme_calc()+scale_fill_calc()
  
  gg3 = ggplot(data = data, aes(x = correo, y = ropamujer)) +
    geom_point() +
    labs(title = "Gráfico 3", 
         x = "Catálogos enviados durante el año", 
         y = "Ventas millones COP durante el año")+
    theme_calc() +scale_fill_calc()
  
  gg4 = ggplot(data = data, aes(x = paginas, y = ropamujer)) +
    geom_point() +
    labs(title = "Gráfico 4", 
         x = "Páginas del catálogo", 
         y = "Ventas millones COP durante el año")+
    theme_calc()+scale_fill_calc()

  gg5 = ggplot(data = data, aes(x = telefono, y = ropamujer)) +
    geom_point() +
    labs(title = "Gráfico 5", 
         x = "Promedio líneas para llamada", 
         y = "Ventas millones COP durante el año")+
    theme_calc()+scale_fill_calc()
  
  gg6 = ggplot(data = data, aes(x = impresa, y = ropamujer)) +
    geom_point() +
    labs(title = "Gráfico 6", 
         x = "Gasto en publicidad impresa.", 
         y = "Ventas millones COP durante el año")+
    theme_calc()+scale_fill_calc()
 
   gg7 = ggplot(data = data, aes(x = servicio, y = ropamujer)) +
    geom_point() +
    labs(title = "Gráfico 7", 
         x = "Representantes servicio al cliente", 
         y = "Ventas millones COP durante el año")+
     theme_calc()+scale_fill_calc()
  
   gg8 = ggplot(data = data, aes(x = idmercado, y = ropamujer)) +
     geom_point() +
     labs(title = "Gráfico 8", 
          x = "Tipo de marcado", 
          y = "Ventas millones COP durante el año")+
     theme_calc()+scale_fill_calc()
   
   gg9 = ggplot(data = data, aes(x = tamamer, y = ropamujer)) +
     geom_point() +
     labs(title = "Gráfico 9", 
          x = "Tamaño potencial del mercado", 
          y = "Ventas millones COP durante el año")+theme_calc()+scale_fill_calc()
   
   gg10 =  ggplot(data = data, aes(x = promo, y = ropamujer)) +
     geom_point() +
     labs(title = "Gráfico 10", 
          x = "Tipo de promoción durante el año", 
          y = "Ventas millones COP durante el año")+theme_calc()+scale_fill_calc()
   
   # Arrange the plots
   combined_ggplots = grid.arrange(gg1, gg2, gg3, gg4, gg5, gg6, gg7, gg8, gg9, gg10, nrow = 2, ncol = 5)
   
   
  ### Graficos entre variables explicativas ------
   gg11 = ggplot(data = data, aes(x = correo, y = paginas)) +
     geom_point() +
     labs(title = "Gráfico 11", 
          x = "Número de catálogos enviados durante el año", 
          y = "Páginas por catalogo")+
     theme_calc()+scale_fill_calc()
   
   gg12 = ggplot(data = data, aes(x = correo, y = telefono)) +
     geom_point() +
     labs(title = "Gráfico 12", 
          x = "Número de catálogos enviados durante el año", 
          y = "Lineas de atención telefónica")+
     theme_calc()+scale_fill_calc()
   
   gg13 = ggplot(data = data, aes(x = paginas, y = telefono)) +
     geom_point() +
     labs(title = "Gráfico 13", 
          x = "Páginas por catalogo", 
          y = "Lineas de atención telefónica")+
     theme_calc()+scale_fill_calc()
   
   gg14 =  ggplot(data = data, aes(x = servicio, y = nomina)) +
     geom_point() +
     labs(title = "Gráfico 14", 
          y = "Gasto en Nómina", 
          x = "Representantes servicio al cliente")+
     theme_calc()+scale_fill_calc()
   
   gg15 = ggplot(data = data, aes(x = edadloc, y = servicio)) +
     geom_point() +
     labs(title = "Gráfico 15", 
          x = "Antigüedad en la zona", 
          y = "Representantes servicio al cliente")+
     theme_calc()+scale_fill_calc()
   
   gg16 = ggplot(data = data, aes(x = tamamer, y = nomina)) +
     geom_point() +
     labs(title = "Gráfico 16", 
          x = "Tamaño potencial del mercado", 
          y = "Gasto en nomina")+
     theme_calc()+scale_fill_calc()
   
   
   # Arrange the plots
   combined_ggplots = grid.arrange(gg11, gg12, gg13, gg14, gg15, gg16, nrow = 2, ncol = 3)
   
  ### Combinación de todos los plots----
   combined_ggplots = grid.arrange(gg1, gg2, gg3, gg4, gg5, gg6, gg7, gg8, gg9, gg10,gg11, gg12, gg13, gg14, gg15, gg16, nrow = 4, ncol = 4)

# 6. Transformación de datos---- 
  # Base de training y de testing
  semilla = 611101
  set.seed(semilla) #Se deja la semilla para que el muestreo sea replicable
  
  #Definirel tamaño de la muestra, en este caso entrenamiento tendrá el 80% de los casos
  sample = sample.int(nrow(data), floor(.8*nrow(data)))
  data_train = data[sample, ]
  data_test = data[-sample, ]
  
# 7. Modeling de Regresiones Lineales-----
  # Comparación de Modelos
  rows = 40
  RMSE_table = data.frame(Iteración = rep(NA, rows), 
                          Modelo = rep(NA, rows),
                          r2= rep(NA, rows),
                          r2_Adj = rep(NA, rows),
                          AIC = rep(NA, rows),
                          DWTest = rep(NA, rows),
                          DWTest_pvalue = rep(NA, rows),
                          RMSE_train = rep(NA,rows),
                          RMSE = rep(NA, rows),
                          Score_Kaggle = rep(NA,rows))

## 7.1. Regresión con todas las variables y ninguna transformación ------
  N=1
  # Regresión
  modelo1 = lm(ropamujer ~.,data=data_train)
  summary(modelo1)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + idloc  + edadloc  +  correo +  paginas + teléfono + impresa + servicio + nomina + idmercado + tamamerMedian + tamamerPequeño + promo")
    
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
  write.csv(solucion1, "solucion1.csv", row.names = FALSE)
  
  RMSE_table$Score_Kaggle[1]=11072.41172

## 7.2. Regresión lineal sencilla con edadloc + correo + impresa + servicio + nomina -------
  N=2
  # Regresión
  modelo2 = lm(ropamujer ~ edadloc + correo + impresa + servicio + nomina
               ,data=data_train)
  summary(modelo2)
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + edadloc + correo + impresa + servicio + nomina")
  
  # Comprobaciones de supuestos
  AIC(modelo2)
  RMSE_table$AIC[N]=AIC(modelo2)
  dwtest(modelo2)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo2)
  
  # Predicciones
  predicciones2 = predict(modelo2, data_test,se.fit = TRUE)
  RMSE2<-sqrt(mean((predicciones2$fit-data_test$ropamujer)^2))
  RMSE2
  RMSE_table$RMSE[N]=RMSE2
  
  RMSE_train2<-sqrt(mean((predicciones2$fit-data_train$ropamujer)^2))
  RMSE_train2
  RMSE_table$RMSE_train[N]=RMSE_train2
  
  #Evaluación
  evaluacion2 = predict(modelo2, data_evaluation,se.fit = TRUE)
  solucion2 = data_evaluation[,c(1,12)]
  solucion2$ropamujer = evaluacion2$fit
  write.csv(solucion2, "solucion2.csv", row.names = FALSE)

## 7.3. Regresión lineal sencilla con ------
  N=3
  # Regresión
  modelo3 = lm(ropamujer ~ edadloc + correo + impresa + servicio + nomina + promo
               ,data=data_train)
  summary(modelo3)
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + edadloc + correo + impresa + servicio + nomina + promo")
  
  # Comprobaciones de supuestos
  AIC(modelo3)
  RMSE_table$AIC[N]=AIC(modelo3)
  dwtest(modelo3)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo3)
  
  # Predicciones
  predicciones3 = predict(modelo3, data_test,se.fit = TRUE)
  RMSE3<-sqrt(mean((predicciones3$fit-data_test$ropamujer)^2))
  RMSE3
  RMSE_table$RMSE[N]=RMSE3
  
  RMSE_train3<-sqrt(mean((predicciones3$fit-data_train$ropamujer)^2))
  RMSE_train3
  RMSE_table$RMSE_train[N]=RMSE_train3
  
  #Evaluación
  evaluacion3 = predict(modelo3, data_evaluation,se.fit = TRUE)
  solucion3 = data_evaluation[,c(1,12)]
  solucion3$ropamujer = evaluacion3$fit
  write.csv(solucion3, "solucion3.csv", row.names = FALSE)

## 7.4. Regresión con todas las variables numericas execpto id ------
  N=4
  # Regresión
  modelo4 = lm(ropamujer ~ edadloc + correo + paginas + telefono + impresa + servicio + nomina + idmercado + promo,data=data_train)
  summary(modelo4)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + edadloc + correo + paginas + teléfono + impresa + servicio + nomina + idmercado + promo")
  
  # Comprobaciones de supuestos
  AIC(modelo4)
  RMSE_table$AIC[N]=AIC(modelo4)
  dwtest(modelo4)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo4)
  
  # Predicciones
  predicciones4 = predict(modelo4, data_test,se.fit = TRUE)
  RMSE4<-sqrt(mean((predicciones4$fit-data_test$ropamujer)^2))
  RMSE4
  RMSE_table$RMSE[N]=RMSE4
  
  RMSE_train4<-sqrt(mean((predicciones4$fit-data_train$ropamujer)^2))
  RMSE_train4
  RMSE_table$RMSE_train[N]=RMSE_train4
  
  #Evaluación
  evaluacion4 = predict(modelo4, data_evaluation,se.fit = TRUE)
  solucion4 = data_evaluation[,c(1,12)]
  solucion4$ropamujer = evaluacion4$fit
  write.csv(solucion4, "solucion4.csv", row.names = FALSE)
  
## 7.5. Regresión con todas las variables excepto id -----
  N=5
  # Regresión
  modelo5 = lm(ropamujer ~ edadloc + correo + paginas + telefono + impresa + servicio + nomina + idmercado + promo + tamamer ,data=data_train)
  summary(modelo5)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + edadloc + correo + paginas + teléfono + impresa + servicio + nomina + idmercado + promo + tamamer")
  
  # Comprobaciones de supuestos
  AIC(modelo5)
  RMSE_table$AIC[N]=AIC(modelo5)
  dwtest(modelo5)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo5)
  
  # Predicciones
  predicciones5 = predict(modelo5, data_test,se.fit = TRUE)
  RMSE5<-sqrt(mean((predicciones5$fit-data_test$ropamujer)^2))
  RMSE5
  RMSE_table$RMSE[N]=RMSE5
  
  RMSE_train5<-sqrt(mean((predicciones5$fit-data_train$ropamujer)^2))
  RMSE_train5
  RMSE_table$RMSE_train[N]=RMSE_train5
  
  #Evaluación
  evaluacion5 = predict(modelo5, data_evaluation,se.fit = TRUE)
  solucion5 = data_evaluation[,c(1,12)]
  solucion5$ropamujer = evaluacion5$fit
  write.csv(solucion5, "solucion5.csv", row.names = FALSE)

## 7.6. Regresion con stepwise usando todas las variables como vienen ----
  N=6
  #uso modelo 1
  modelo6<- step(modelo1,direction="both")
  summary(modelo6)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + idloc + correo + paginas + impresa + servicio")
  
  # Comprobaciones de supuestos
  AIC(modelo6)
  RMSE_table$AIC[N]=AIC(modelo6)
  dwtest(modelo6)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo6)
  
  # Predicciones
  predicciones6 = predict(modelo6, data_test,se.fit = TRUE)
  RMSE6<-sqrt(mean((predicciones6$fit-data_test$ropamujer)^2))
  RMSE6
  RMSE_table$RMSE[N]=RMSE6
  
  RMSE_train6<-sqrt(mean((predicciones6$fit-data_train$ropamujer)^2))
  RMSE_train6
  RMSE_table$RMSE_train[N]=RMSE_train6
  
  #Evaluación
  evaluacion6 = predict(modelo6, data_evaluation,se.fit = TRUE)
  solucion6 = data_evaluation[,c(1,12)]
  solucion6$ropamujer = evaluacion6$fit
  write.csv(solucion6, "solucion6.csv", row.names = FALSE)

## 7.7. Regresion con best subsets usando todas las variables como vienen -----
  N=7
  #nbest (n?mero de modelos por cada k) es por defecto 1 y nvmax es por defecto 8 (k m?ximo)
  modelo7<-regsubsets(ropamujer ~.,data=data_train, nbest=1, nvmax=12, method = "exhaustive")
  summary(modelo7)
  summary(modelo7)$adjr2
  summary(modelo7)$cp
  summary(modelo7)$bic
  
  ## Evaluando el modelo sugerido es el mismo que por stepwise modelo 6
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + idloc + correo + paginas + impresa + servicio")
  
  # Comprobaciones de supuestos
  AIC(modelo6)
  RMSE_table$AIC[N]=AIC(modelo6)
  dwtest(modelo6)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo6)
  
  # Predicciones
  predicciones7 = predict(modelo6, data_test,se.fit = TRUE)
  RMSE7<-sqrt(mean((predicciones7$fit-data_test$ropamujer)^2))
  RMSE7
  RMSE_table$RMSE[N]=RMSE7
  
  RMSE_train7<-sqrt(mean((predicciones7$fit-data_train$ropamujer)^2))
  RMSE_train7
  RMSE_table$RMSE_train[N]=RMSE_train7
  
  #Evaluación
  evaluacion7 = predict(modelo6, data_evaluation,se.fit = TRUE)
  solucion7 = data_evaluation[,c(1,12)]
  solucion7$ropamujer = evaluacion7$fit
  write.csv(solucion7, "solucion7.csv", row.names = FALSE)
  
## 7.8. Regresión con variables todas las variables numericas (sin idloc) y variables binarias para categoricas ----
  N=8
  data_train1 = data_train[,-1]
  data_test1 = data_test[,-1]
  data_evaluation1 = data_evaluation[,]
  ### Create binary variables using dummyVars ----
  # data train
  data_train1$idmercado_1 = ifelse(data_train1$idmercado==1,1,0)
  data_train1$idmercado_2 = ifelse(data_train1$idmercado==2,1,0)
  data_train1$idmercado_3 = ifelse(data_train1$idmercado==3,1,0)
  data_train1$idmercado_4 = ifelse(data_train1$idmercado==4,1,0)
  data_train1$idmercado_5 = ifelse(data_train1$idmercado==5,1,0)  
  data_train1$idmercado_6 = ifelse(data_train1$idmercado==6,1,0)  
  data_train1$idmercado_7 = ifelse(data_train1$idmercado==7,1,0)  
  data_train1$idmercado_8 = ifelse(data_train1$idmercado==8,1,0)
  
  data_train1$promo_1 = ifelse(data_train1$promo==1,1,0)
  data_train1$promo_2 = ifelse(data_train1$promo==2,1,0)
  
  data_train1$tamamer_Grande = ifelse(data_train1$tamamer=="Grande",1,0)
  data_train1$tamamer_Median = ifelse(data_train1$tamamer=="Median",1,0)
  
  columnas_a_eliminar = c("idmercado", "promo", "tamamer") 
  data_train1 <- data_train1[, !(names(data_train1) %in% columnas_a_eliminar)]
  
  #data_test
  data_test1$idmercado_1 = ifelse(data_test1$idmercado==1,1,0)
  data_test1$idmercado_2 = ifelse(data_test1$idmercado==2,1,0)
  data_test1$idmercado_3 = ifelse(data_test1$idmercado==3,1,0)
  data_test1$idmercado_4 = ifelse(data_test1$idmercado==4,1,0)
  data_test1$idmercado_5 = ifelse(data_test1$idmercado==5,1,0)  
  data_test1$idmercado_6 = ifelse(data_test1$idmercado==6,1,0)  
  data_test1$idmercado_7 = ifelse(data_test1$idmercado==7,1,0)  
  data_test1$idmercado_8 = ifelse(data_test1$idmercado==8,1,0)
  
  data_test1$promo_1 = ifelse(data_test1$promo==1,1,0)
  data_test1$promo_2 = ifelse(data_test1$promo==2,1,0)
  
  data_test1$tamamer_Grande = ifelse(data_test1$tamamer=="Grande",1,0)
  data_test1$tamamer_Median = ifelse(data_test1$tamamer=="Median",1,0)
  
  columnas_a_eliminar = c("idmercado", "promo", "tamamer") 
  data_test1 <- data_test1[, !(names(data_test1) %in% columnas_a_eliminar)]
  
  # data_evaluation
  data_evaluation1$idmercado_1 = ifelse(data_evaluation1$idmercado==1,1,0)
  data_evaluation1$idmercado_2 = ifelse(data_evaluation1$idmercado==2,1,0)
  data_evaluation1$idmercado_3 = ifelse(data_evaluation1$idmercado==3,1,0)
  data_evaluation1$idmercado_4 = ifelse(data_evaluation1$idmercado==4,1,0)
  data_evaluation1$idmercado_5 = ifelse(data_evaluation1$idmercado==5,1,0)  
  data_evaluation1$idmercado_6 = ifelse(data_evaluation1$idmercado==6,1,0)  
  data_evaluation1$idmercado_7 = ifelse(data_evaluation1$idmercado==7,1,0)  
  data_evaluation1$idmercado_8 = ifelse(data_evaluation1$idmercado==8,1,0)
  
  data_evaluation1$promo_1 = ifelse(data_evaluation1$promo==1,1,0)
  data_evaluation1$promo_2 = ifelse(data_evaluation1$promo==2,1,0)
  
  data_evaluation1$tamamer_Grande = ifelse(data_evaluation1$tamamer=="Grande",1,0)
  data_evaluation1$tamamer_Median = ifelse(data_evaluation1$tamamer=="Median",1,0)
  
  columnas_a_eliminar = c("idmercado", "promo", "tamamer") 
  data_evaluation1 <- data_evaluation1[, !(names(data_evaluation1) %in% columnas_a_eliminar)]
  ### Process -----
  # Regresión
  modelo8 = lm(ropamujer ~.,data=data_train1)
  summary(modelo8)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + edadloc  +  correo +  paginas + teléfono + impresa + servicio + nomina + idmercado_1 + idmercado_2 + idmercado_3 + idmercado_4 + idmercado_5 + idmercado_6 + idmercado_7 + idmercado_8 + promo_1 + promo_2 + tamamer_Grande + tamamer_Median")
  
  # Comprobaciones de supuestos
  AIC(modelo8)
  RMSE_table$AIC[N]=AIC(modelo8)
  dwtest(modelo8)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo8)
  
  # Predicciones
  predicciones8 = predict(modelo8, data_test1,se.fit = TRUE)
  RMSE8<-sqrt(mean((predicciones8$fit-data_test1$ropamujer)^2))
  RMSE8
  RMSE_table$RMSE[N]=RMSE8
  
  RMSE_train8<-sqrt(mean((predicciones1$fit-data_train1$ropamujer)^2))
  RMSE_train8
  RMSE_table$RMSE_train[N]=RMSE_train1
  
  #Evaluación/Solución
  evaluacion8 = predict(modelo8, data_evaluation1,se.fit = TRUE)
  solucion8 = data_evaluation1[,c(1,9)]
  solucion8$ropamujer = evaluacion8$fit
  write.csv(solucion8, "solucion8.csv", row.names = FALSE)
  
## 7.9. Regresión usando stepwise con todas las variables numericas (sin idloc) y variables binarias para categoricas ----
  N=9
  #uso modelo 1
  modelo9<- step(modelo8,direction="both")
  summary(modelo9)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + correo + paginas + impresa + servicio + id_mercado7")
  
  # Comprobaciones de supuestos
  AIC(modelo9)
  RMSE_table$AIC[N]=AIC(modelo9)
  dwtest(modelo9)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo9)
  
  # Predicciones
  predicciones9 = predict(modelo9, data_test1,se.fit = TRUE)
  RMSE9<-sqrt(mean((predicciones9$fit-data_test1$ropamujer)^2))
  RMSE9
  RMSE_table$RMSE[N]=RMSE9
  
  RMSE_train9<-sqrt(mean((predicciones9$fit-data_train1$ropamujer)^2))
  RMSE_train9
  RMSE_table$RMSE_train[N]=RMSE_train9
  
  #Evaluación
  evaluacion9 = predict(modelo9, data_evaluation1,se.fit = TRUE)
  solucion9 = data_evaluation1[,c(1,9)]
  solucion9$ropamujer = evaluacion9$fit
  write.csv(solucion9, "solucion9.csv", row.names = FALSE)

## 7.10. Regresión con best subsets usando todas las variables numericas y categoricas binarias------ 
  N=10
  # nbest (n?mero de modelos por cada k) es por defecto 1 y nvmax es por defecto 8 (k m?ximo)
  modelo10<-regsubsets(ropamujer ~.,data=data_train1, nbest=1, nvmax=20, method = "exhaustive")
  summary(modelo10)
  summary(modelo10)$adjr2
  summary(modelo10)$cp
  summary(modelo10)$bic
  
  modelo10 = lm(ropamujer ~ correo + paginas + impresa + servicio,data=data_train)
  summary(modelo10)
  ## Evaluando el modelo sugerido es el mismo que por stepwise modelo 6
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + correo + paginas + impresa + servicio")
  
  # Comprobaciones de supuestos
  AIC(modelo10)
  RMSE_table$AIC[N]=AIC(modelo10)
  dwtest(modelo10)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo10)
  
  # Predicciones
  predicciones10 = predict(modelo10, data_test,se.fit = TRUE)
  RMSE10<-sqrt(mean((predicciones10$fit-data_test$ropamujer)^2))
  RMSE10
  RMSE_table$RMSE[N]=RMSE10
  
  RMSE_train10<-sqrt(mean((predicciones10$fit-data_train$ropamujer)^2))
  RMSE_train10
  RMSE_table$RMSE_train[N]=RMSE_train10
  
  #Evaluación
  evaluacion10 = predict(modelo10, data_evaluation,se.fit = TRUE)
  solucion10 = data_evaluation[,c(1,12)]
  solucion10$ropamujer = evaluacion10$fit
  write.csv(solucion10, "solucion10.csv", row.names = FALSE)

## 7.11. Regresión con combinación de variables: correo y paginas ------
  N=11
  data_test1$correo_paginas = data_test1$correo*data_test1$paginas
  data_train1$correo_paginas = data_train1$correo*data_train1$paginas
  data_evaluation1$correo_paginas = data_evaluation1$correo*data_evaluation1$paginas
  
  #Regresión
  modelo11<-lm(ropamujer ~.,data=data_train1)
  summary(modelo11)
  
  modelo11<- step(modelo11,direction="both")
  summary(modelo11)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + correo + impresa + servicio + idmercado_7 + correo_paginas")
  
  # Comprobaciones de supuestos
  AIC(modelo11)
  RMSE_table$AIC[N]=AIC(modelo11)
  dwtest(modelo11)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo11)
  
  # Predicciones
  predicciones11 = predict(modelo11, data_test1,se.fit = TRUE)
  RMSE11<-sqrt(mean((predicciones11$fit-data_test1$ropamujer)^2))
  RMSE11
  RMSE_table$RMSE[N]=RMSE11
  
  RMSE_train11<-sqrt(mean((predicciones11$fit-data_train1$ropamujer)^2))
  RMSE_train11
  RMSE_table$RMSE_train[N]=RMSE_train11
  
  #Evaluación
  evaluacion11 = predict(modelo11, data_evaluation1,se.fit = TRUE)
  solucion11 = data_evaluation1[,c(1,9)]
  solucion11$ropamujer = evaluacion11$fit
  write.csv(solucion11, "solucion11.csv", row.names = FALSE)
  
## 7.12. Regresión con combinación de variables: correo con paginas e impresa con idmercado------
  N=12
  data_train = data_train[,-1]
  data_test = data_test[,-1]
  data_evaluation = data_evaluation[,]
  
  data_test$correo_paginas = data_test$correo*data_test$paginas
  data_train$correo_paginas = data_train$correo*data_train$paginas
  data_evaluation$correo_paginas = data_evaluation$correo*data_evaluation$paginas
  
  data_test$impresa_idmercado = data_test$impresa * data_test$idmercado
  data_train$impresa_idmercado = data_train$impresa * data_train$idmercado
  data_evaluation$impresa_idmercado = data_evaluation$impresa * data_evaluation$idmercado
  
  #Regresión
  modelo12<-lm(ropamujer ~.,data=data_train)
  summary(modelo12)
  
  modelo12<- step(modelo12,direction="both")
  summary(modelo12)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + correo + impresa + servicio + idmercado + correo_paginas + impresa_idmercado")
  
  # Comprobaciones de supuestos
  AIC(modelo12)
  RMSE_table$AIC[N]=AIC(modelo12)
  dwtest(modelo12)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo12)
  
  # Predicciones
  predicciones12 = predict(modelo12, data_test,se.fit = TRUE)
  RMSE12<-sqrt(mean((predicciones12$fit-data_test$ropamujer)^2))
  RMSE12
  RMSE_table$RMSE[N]=RMSE12
  
  RMSE_train12<-sqrt(mean((predicciones12$fit-data_train$ropamujer)^2))
  RMSE_train12
  RMSE_table$RMSE_train[N]=RMSE_train12
  
  #Evaluación
  evaluacion12 = predict(modelo12, data_evaluation,se.fit = TRUE)
  solucion12 = data_evaluation[,c(1,12)]
  solucion12$ropamujer = evaluacion12$fit
  write.csv(solucion12, "solucion12.csv", row.names = FALSE)

## 7.13. Regresion con combinación de variables y variables al cuadrado y binarias ------  
  N=13
  ### Transformación de variables ---- 
  data_test$impresa_sqr = (data_test$impresa)^2
  data_train$impresa_sqr = (data_train$impresa)^2
  data_evaluation$impresa_sqr = (data_evaluation$impresa)^2
  
  data_test$correo_sqr = (data_test$correo)^2
  data_train$correo_sqr = (data_train$correo)^2
  data_evaluation$correo_sqr = (data_evaluation$correo)^2
  
  data_test$nomina_sqr = (data_test$nomina)^2
  data_train$nomina_sqr = (data_train$nomina)^2
  data_evaluation$nomina_sqr = (data_evaluation$nomina)^2
  
  data_train$promo_1 = ifelse(data_train$promo==1,1,0)
  data_train$promo_2 = ifelse(data_train$promo==2,1,0)
  data_test$promo_1 = ifelse(data_test$promo==1,1,0)
  data_test$promo_2 = ifelse(data_test$promo==2,1,0)
  data_evaluation$promo_1 = ifelse(data_evaluation$promo==1,1,0)
  data_evaluation$promo_2 = ifelse(data_evaluation$promo==2,1,0)
  
  data_train$tamamer_Grande = ifelse(data_train$tamamer=="Grande",1,0)
  data_train$tamamer_Median = ifelse(data_train$tamamer=="Median",1,0)
  data_test$tamamer_Grande = ifelse(data_test$tamamer=="Grande",1,0)
  data_test$tamamer_Median = ifelse(data_test$tamamer=="Median",1,0)
  data_evaluation$tamamer_Grande = ifelse(data_evaluation$tamamer=="Grande",1,0)
  data_evaluation$tamamer_Median = ifelse(data_evaluation$tamamer=="Median",1,0)
  
  columnas_a_eliminar = c("promo", "tamamer") 
  data_train <- data_train[, !(names(data_train) %in% columnas_a_eliminar)]
  data_test <- data_test[, !(names(data_test) %in% columnas_a_eliminar)]
  data_evaluation <- data_evaluation[, !(names(data_evaluation) %in% columnas_a_eliminar)]
  ### Proceso --------
  #Regresión
  modelo13<-lm(ropamujer ~.,data=data_train)
  summary(modelo13)
  
  modelo_step<- step(modelo13,direction="both")
  summary(modelo_step)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + edadloc + correo + paginas + impresa + servicio + nomina + idmercado + correo_paginas + impresa_idmercado + impresa^2 + correo^2 + nomina^2 + promo_1 + promo_2 + tamamer_Grande + tamamer_Median")
  
  # Comprobaciones de supuestos
  AIC(modelo13)
  RMSE_table$AIC[N]=AIC(modelo13)
  dwtest(modelo13)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo12)
  
  # Predicciones
  predicciones13 = predict(modelo13, data_test,se.fit = TRUE)
  RMSE13<-sqrt(mean((predicciones13$fit-data_test$ropamujer)^2))
  RMSE13
  RMSE_table$RMSE[N]=RMSE13
  
  RMSE_train13<-sqrt(mean((predicciones13$fit-data_train$ropamujer)^2))
  RMSE_train13
  RMSE_table$RMSE_train[N]=RMSE_train13
  
  #Evaluación
  evaluacion13 = predict(modelo13, data_evaluation,se.fit = TRUE)
  solucion13 = data_evaluation[,c(1,10)]
  solucion13$ropamujer = evaluacion13$fit
  write.csv(solucion13, "solucion13.csv", row.names = FALSE)
  
## 7.14. Regresión igual a la 13 pero incluyendo idloc ------
  ### Transformación de variables -----
  data_evaluation = read_csv("test.csv")
  data = read_csv("train.csv")
  
  set.seed(semilla) #Se deja la semilla para que el muestreo sea replicable
  #aquí se define el tamaño de la muestra, en este caso entrenamiento tendrá el 80% de los casos
  sample = sample.int(nrow(data), floor(.8*nrow(data)))
  data_train = data[sample, ]
  data_test = data[-sample, ]
  
  data_test$correo_paginas = data_test$correo*data_test$paginas
  data_train$correo_paginas = data_train$correo*data_train$paginas
  data_evaluation$correo_paginas = data_evaluation$correo*data_evaluation$paginas
  
  data_test$impresa_idmercado = data_test$impresa * data_test$idmercado
  data_train$impresa_idmercado = data_train$impresa * data_train$idmercado
  data_evaluation$impresa_idmercado = data_evaluation$impresa * data_evaluation$idmercado
  
  data_test$impresa_sqr = (data_test$impresa)^2
  data_train$impresa_sqr = (data_train$impresa)^2
  data_evaluation$impresa_sqr = (data_evaluation$impresa)^2
  
  data_test$correo_sqr = (data_test$correo)^2
  data_train$correo_sqr = (data_train$correo)^2
  data_evaluation$correo_sqr = (data_evaluation$correo)^2
  
  data_test$nomina_sqr = (data_test$nomina)^2
  data_train$nomina_sqr = (data_train$nomina)^2
  data_evaluation$nomina_sqr = (data_evaluation$nomina)^2
  
  data_train$promo_1 = ifelse(data_train$promo==1,1,0)
  data_train$promo_2 = ifelse(data_train$promo==2,1,0)
  data_test$promo_1 = ifelse(data_test$promo==1,1,0)
  data_test$promo_2 = ifelse(data_test$promo==2,1,0)
  data_evaluation$promo_1 = ifelse(data_evaluation$promo==1,1,0)
  data_evaluation$promo_2 = ifelse(data_evaluation$promo==2,1,0)
  
  data_train$tamamer_Grande = ifelse(data_train$tamamer=="Grande",1,0)
  data_train$tamamer_Median = ifelse(data_train$tamamer=="Median",1,0)
  data_test$tamamer_Grande = ifelse(data_test$tamamer=="Grande",1,0)
  data_test$tamamer_Median = ifelse(data_test$tamamer=="Median",1,0)
  data_evaluation$tamamer_Grande = ifelse(data_evaluation$tamamer=="Grande",1,0)
  data_evaluation$tamamer_Median = ifelse(data_evaluation$tamamer=="Median",1,0)
  
  columnas_a_eliminar = c("promo", "tamamer") 
  data_train <- data_train[, !(names(data_train) %in% columnas_a_eliminar)]
  data_test <- data_test[, !(names(data_test) %in% columnas_a_eliminar)]
  data_evaluation <- data_evaluation[, !(names(data_evaluation) %in% columnas_a_eliminar)]
  
  N=14
  ### Proceso --------
  #Regresión
  modelo14<-lm(ropamujer ~.,data=data_train)
  summary(modelo14)
  
  modelo14<- step(modelo14,direction="both")
  summary(modelo14)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + idloc + correo + impresa + servicio + idmercado + correo_paginas + impresa_idmercado")
  
  # Comprobaciones de supuestos
  AIC(modelo14)
  RMSE_table$AIC[N]=AIC(modelo14)
  dwtest(modelo14)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo14)
  
  # Predicciones
  predicciones14 = predict(modelo14, data_test,se.fit = TRUE)
  RMSE14<-sqrt(mean((predicciones14$fit-data_test$ropamujer)^2))
  RMSE14
  RMSE_table$RMSE[N]=RMSE14
  
  RMSE_train14<-sqrt(mean((predicciones14$fit-data_train$ropamujer)^2))
  RMSE_train14
  RMSE_table$RMSE_train[N]=RMSE_train14
  
  #Evaluación
  evaluacion14 = predict(modelo14, data_evaluation,se.fit = TRUE)
  solucion14 = data_evaluation[,c(1,10)]
  solucion14$ropamujer = evaluacion14$fit
  write.csv(solucion14, "solucion14.csv", row.names = FALSE)
  
  RMSE_table$Score_Kaggle[N] = 11009.9309

## 7.15. Variables con logaritmos ----
  ### Transformación de variables ----
  data_train$ln_edadloc = log(data_train$edadloc)
  data_train$ln_correo = log(data_train$correo)
  data_train$ln_paginas = log(data_train$paginas)
  data_train$ln_telefono = log(data_train$telefono)
  data_train$ln_servicio = log(data_train$servicio)
  data_train$ln_nomina = log(data_train$nomina)
  
  data_test$ln_edadloc = log(data_test$edadloc)
  data_test$ln_correo = log(data_test$correo)
  data_test$ln_paginas = log(data_test$paginas)
  data_test$ln_telefono = log(data_test$telefono)
  data_test$ln_servicio = log(data_test$servicio)
  data_test$ln_nomina = log(data_test$nomina)
  
  data_evaluation$ln_edadloc = log(data_evaluation$edadloc)
  data_evaluation$ln_correo = log(data_evaluation$correo)
  data_evaluation$ln_paginas = log(data_evaluation$paginas)
  data_evaluation$ln_telefono = log(data_evaluation$telefono)
  data_evaluation$ln_servicio = log(data_evaluation$servicio)
  data_evaluation$ln_nomina = log(data_evaluation$nomina)
  
  N=15
  ### Proceso --------
  #Regresión
  modelo15<-lm(ropamujer ~.,data=data_train)
  summary(modelo15)
  
  modelo15<- step(modelo15,direction="both")
  summary(modelo15)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + idloc + edadloc + correo + paginas + impresa + servicio +idmercado + impresa_idmercado + ln_edadloc + ln_paginas + ln_servicio ")
  
  # Comprobaciones de supuestos
  AIC(modelo15)
  RMSE_table$AIC[N]=AIC(modelo15)
  dwtest(modelo15)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo15)
  
  # Predicciones
  predicciones15 = predict(modelo15, data_test,se.fit = TRUE)
  RMSE15<-sqrt(mean((predicciones15$fit-data_test$ropamujer)^2))
  RMSE15
  RMSE_table$RMSE[N]=RMSE15
  
  RMSE_train15<-sqrt(mean((predicciones15$fit-data_train$ropamujer)^2))
  RMSE_train15
  RMSE_table$RMSE_train[N]=RMSE_train15
  
  #Evaluación
  evaluacion15 = predict(modelo15, data_evaluation,se.fit = TRUE)
  solucion15 = data_evaluation[,c(1,10)]
  solucion15$ropamujer = evaluacion15$fit
  write.csv(solucion15, "solucion15.csv", row.names = FALSE)
  
  RMSE_table$Score_Kaggle[15] = 10595.28202
  
## 7.16. Regresion modelo 15 pero sin idloc ------
  data_train2 = data_train[,-1]
  data_test2 = data_test[,-1]
  data_evaluation2 = data_evaluation[,]
  
  N=16
  #Regresión
  modelo16<-lm(ropamujer ~.,data=data_train2)
  summary(modelo16)
  
  modelo16<- step(modelo16,direction="both")
  summary(modelo16)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + edadloc + correo + paginas + impresa + servicio + idmercado + impresa_idmercado + ln_edadloc + ln_paginas + ln_servicio ")
  
  # Comprobaciones de supuestos
  AIC(modelo16)
  RMSE_table$AIC[N]=AIC(modelo16)
  dwtest(modelo16)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo16)
  
  # Predicciones
  predicciones16 = predict(modelo16, data_test2,se.fit = TRUE)
  RMSE16<-sqrt(mean((predicciones16$fit-data_test2$ropamujer)^2))
  RMSE16
  RMSE_table$RMSE[N]=RMSE16
  
  RMSE_train16<-sqrt(mean((predicciones16$fit-data_train2$ropamujer)^2))
  RMSE_train16
  RMSE_table$RMSE_train[N]=RMSE_train16
  
  #Evaluación
  evaluacion16 = predict(modelo16, data_evaluation2,se.fit = TRUE)
  solucion16 = data_evaluation2[,c(1,10)]
  solucion16$ropamujer = evaluacion16$fit
  write.csv(solucion16, "solucion16.csv", row.names = FALSE)

## 7.17. Variables con raiz cuadrada ----
  ### Transformación de variables ----
  data_train$sqrt_nomina = sqrt(data_train$nomina)
  data_test$sqrt_nomina = sqrt(data_test$nomina)
  data_evaluation$sqrt_nomina = sqrt(data_evaluation$nomina)
  
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
  
  N=17
  ### Proceso --------
  #Regresión
  modelo17<-lm(ropamujer ~.,data=data_train)
  summary(modelo17)
  
  modelo17<- step(modelo17,direction="both")
  summary(modelo17)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + idloc + edadloc + correo + impresa + servicio + idmercado + impresa_idmercado + ln_edadloc + ln_paginas + ln_servicio + sqrt_edadloc + sqrt_paginas + sqrt_servicio")
  
  # Comprobaciones de supuestos
  AIC(modelo17)
  RMSE_table$AIC[N]=AIC(modelo17)
  dwtest(modelo17)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo17)
  
  # Predicciones
  predicciones17 = predict(modelo17, data_test,se.fit = TRUE)
  RMSE17<-sqrt(mean((predicciones17$fit-data_test$ropamujer)^2))
  RMSE17
  RMSE_table$RMSE[N]=RMSE17
  
  RMSE_train17<-sqrt(mean((predicciones17$fit-data_train$ropamujer)^2))
  RMSE_train17
  RMSE_table$RMSE_train[N]=RMSE_train17
  
  #Evaluación
  evaluacion17 = predict(modelo17, data_evaluation,se.fit = TRUE)
  solucion17 = data_evaluation[,c(1,10)]
  solucion17$ropamujer = evaluacion17$fit
  write.csv(solucion17, "solucion17.csv", row.names = FALSE)

## 7.18. Regresión con variables inversas y dejando idloc-----
  ### Transformación de variables -----
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

  N=18
  ### Proceso -----
  #Regresión
  modelo18<-lm(ropamujer ~.,data=data_train)
  summary(modelo18)
  
  modelo18<- step(modelo18,direction="both")
  summary(modelo18)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + idloc + edadloc + telefono + impresa + impresa_sqr + ln_edadloc + ln_correo + ln_paginas + ln_telefono + ln_servicio + sqrt_edadloc + sqrt_correo + sqrt_telefono + sqrt_impresa + edadloc_inversa + correo_inversa + paginas_inversa + telefono_inversa + impresa_inversa + servicio_inversa")
  
  # Comprobaciones de supuestos
  AIC(modelo18)
  RMSE_table$AIC[N]=AIC(modelo18)
  dwtest(modelo18)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo18)
  
  # Predicciones
  predicciones18 = predict(modelo18, data_test,se.fit = TRUE)
  RMSE18<-sqrt(mean((predicciones18$fit-data_test$ropamujer)^2))
  RMSE18
  RMSE_table$RMSE[N]=RMSE18
  
  RMSE_train18<-sqrt(mean((predicciones18$fit-data_train$ropamujer)^2))
  RMSE_train18
  RMSE_table$RMSE_train[N]=RMSE_train18
  
  #Evaluación
  evaluacion18 = predict(modelo18, data_evaluation,se.fit = TRUE)
  solucion18 = data_evaluation[,c(1,10)]
  solucion18$ropamujer = evaluacion18$fit
  write.csv(solucion18, "solucion18.csv", row.names = FALSE)
  
  RMSE_table$Score_Kaggle[18]=10629.61369
  
## 7.19. Regresión con varibles inversas y sin idloc ------
  data_train2 = data_train[,-1]
  data_test2 = data_test[,-1]
  data_evaluation2 = data_evaluation[,-1]
  
  N=19
  #Regresión
  modelo19<-lm(ropamujer ~.,data=data_train2)
  summary(modelo19)
  
  modelo19<- step(modelo19,direction="both")
  summary(modelo19)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + edadloc + impresa + servicio + idmercado + impresa_idmercado + impresa_sqr + ln_edadloc + ln_correo + ln_paginas + ln_servicio + sqrt_edadloc + sqrt_correo + sqrt_impresa + sqrt_servicio + edadloc_inversa + paginas_inversa + impresa_inversa")
  
  # Comprobaciones de supuestos
  AIC(modelo19)
  RMSE_table$AIC[N]=AIC(modelo19)
  dwtest(modelo19)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo19)
  
  # Predicciones
  predicciones19 = predict(modelo19, data_test2,se.fit = TRUE)
  RMSE19<-sqrt(mean((predicciones19$fit-data_test2$ropamujer)^2))
  RMSE19
  RMSE_table$RMSE[N]=RMSE19
  
  RMSE_train19<-sqrt(mean((predicciones19$fit-data_train2$ropamujer)^2))
  RMSE_train19
  RMSE_table$RMSE_train[N]=RMSE_train19
  
  #Evaluación
  evaluacion19 = predict(modelo19, data_evaluation2,se.fit = TRUE)
  solucion19 = data_evaluation2[,c(1,9)]
  solucion19$ropamujer = evaluacion19$fit
  write.csv(solucion19, "solucion19.csv", row.names = FALSE)
  
## 7.20. Regresión con variables inversas al cuadrado y dejando idloc-----
  ### Transformación de variables -----
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
  
  N=20
  ### Proceso -----
  #Regresión
  modelo20<-lm(ropamujer ~.,data=data_train)
  summary(modelo20)
  
  modelo20<- step(modelo20,direction="both")
  summary(modelo20)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + idloc + edadloc + correo + impresa + servicio + nomina + idmercado + correo_paginas + impresa_idmercado + impresa_sqr + correo_sqr + nomina_sqr + ln_edadloc + ln_correo + ln_servicio + ln_nomina + sqrt_nomina + sqrt_edadloc + sqrt_correo + sqrt_impresa + sqrt_servicio + edadloc_inversa + correo_inversa + paginas_inversa + impresa_inversa + edadloc_inversa_sqr + correo_inversa_sqr")
  
  # Comprobaciones de supuestos
  AIC(modelo20)
  RMSE_table$AIC[N]=AIC(modelo20)
  dwtest(modelo20)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo20)
  
  # Predicciones
  predicciones20 = predict(modelo20, data_test,se.fit = TRUE)
  RMSE20<-sqrt(mean((predicciones20$fit-data_test$ropamujer)^2))
  RMSE20
  RMSE_table$RMSE[N]=RMSE20
  
  RMSE_train20<-sqrt(mean((predicciones20$fit-data_train$ropamujer)^2))
  RMSE_train20
  RMSE_table$RMSE_train[N]=RMSE_train20

  #Evaluación
  evaluacion20 = predict(modelo20, data_evaluation,se.fit = TRUE)
  solucion20 = data_evaluation[,c(1,10)]
  solucion20$ropamujer = evaluacion20$fit
  write.csv(solucion20, "solucion20.csv", row.names = FALSE)
  

## 7.21. Regresión con varibles inversas al cuadrado y sin idloc ------
  data_train2 = data_train[,-1]
  data_test2 = data_test[,-1]
  data_evaluation2 = data_evaluation[,-1]
  
  N=21
  #Regresión
  modelo21<-lm(ropamujer ~.,data=data_train2)
  summary(modelo21)
  
  modelo21<- step(modelo21,direction="both")
  summary(modelo21)
  
  RMSE_table$Iteración[N]=N
  RMSE_table$Modelo[N] = c("β + edadloc + correo + impresa + servicio + idmercado + correo_paginas + impresa_idmercado + impresa_sqr + correo_sqr + ln_edadloc + ln_correo + ln_servicio + sqrt_edadloc + sqrt_correo + sqrt_impresa + sqrt_servicio + edadloc_inversa + correo_inversa + paginas_inversa + impresa_inversa + servicio_inversa + edadloc_inversa_sqr + correo_inversa_sqr + servicio_inversa_sqr")
  
  # Comprobaciones de supuestos
  AIC(modelo21)
  RMSE_table$AIC[N]=AIC(modelo21)
  dwtest(modelo21)
  layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
  plot(modelo21)
  
  # Predicciones
  predicciones21 = predict(modelo21, data_test2,se.fit = TRUE)
  RMSE21<-sqrt(mean((predicciones21$fit-data_test2$ropamujer)^2))
  RMSE21
  RMSE_table$RMSE[N]=RMSE21
  
  RMSE_train21<-sqrt(mean((predicciones21$fit-data_train2$ropamujer)^2))
  RMSE_train21
  RMSE_table$RMSE_train[N]=RMSE_train21
  
  #Evaluación
  evaluacion21 = predict(modelo21, data_evaluation2,se.fit = TRUE)
  solucion21 = data_evaluation2[,c(1,9)]
  solucion21$ropamujer = evaluacion21$fit
  write.csv(solucion21, "solucion21.csv", row.names = FALSE)
  
## 7.22. Regularización Ridge y Lasso usando data modelo 20 -----

  N=22
  # Seprar predictores de variable a predecir
    y.data_train = as.matrix(data_train[,10])
    y.data_test = as.matrix(data_test[,10])
    y.data_evaluation = as.matrix(data_evaluation[,10])
  
  # Estandarizar
    data_trainA<-as.data.frame(scale(data_train[,1:9]))
    data_testA<-as.data.frame(scale(data_test[,1:9]))
    data_evaluationA<-as.data.frame(scale(data_evaluation[,1:9]))
    
    data_trainB<-scale(data_train[,11:44])
    data_testB<-as.data.frame(scale(data_test[,11:44]))
    data_evaluationB<-as.data.frame(scale(data_evaluation[,11:44]))
  
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
      evaluacion_rd1 = cbind(data_evaluation[,1], evaluacion_rd1[,2])
      colnames(evaluacion_rd1)[colnames(evaluacion_rd1) == "evaluacion_rd1[, 2]"] <- "ropamujer"
      
      evaluacion_ridge1 = cbind(data_evaluation[,1],evaluacion_ridge1)
      colnames(evaluacion_ridge1)[colnames(evaluacion_ridge1) == "s1"] <- "ropamujer"
      
      evaluacion_lasso1 = cbind(data_evaluation[,1],evaluacion_lasso1)
      colnames(evaluacion_lasso1)[colnames(evaluacion_lasso1) == "s1"] <- "ropamujer"
      
      write.csv(evaluacion_rd1, "evaluacion_rd1.csv", row.names = FALSE)
      write.csv(evaluacion_ridge1, "evaluacion_ridge1.csv", row.names = FALSE)
      write.csv(evaluacion_lasso1, "evaluacion_lasso1.csv", row.names = FALSE)
      
    # Completamos tabla RMSE
      # Ridge
        RMSE_table$Iteración[N]=N
        RMSE_table$Modelo[N] = "Modelo 20 con regularización Ridge"
        RMSE_table$RMSE[N] = erroresridge
      # Lasso
        RMSE_table$Iteración[N+1]=N+1
        RMSE_table$Modelo[N+1] = "Modelo 20 con regularización Lasso"
        RMSE_table$RMSE[N+1] = erroreslasso
      # Red Elastica
        RMSE_table$Iteración[N+2]=N+2
        RMSE_table$Modelo[N+2] = "Modelo 20 con regularización redes elasticas con alpha = 0.5"
        RMSE_table$RMSE[N+2] = erroreselastic
        
## 7.25. Regularización Ridge y Lasso usando data modelo 21 -----
    N=25
    # Seprar predictores de variable a predecir
      y.data_train = as.matrix(data_train2[,9])
      y.data_test = as.matrix(data_test2[,9])
      y.data_evaluation = as.matrix(data_evaluation2[,9])
    
    # Estandarizar
      data_trainA<-as.data.frame(scale(data_train2[,1:8]))
      data_testA<-as.data.frame(scale(data_test2[,1:8]))
      data_evaluationA<-as.data.frame(scale(data_evaluation2[,1:8]))
    
      data_trainB<-scale(data_train2[,10:43])
      data_testB<-as.data.frame(scale(data_test2[,10:43]))
      data_evaluationB<-as.data.frame(scale(data_evaluation2[,10:43]))
    
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
      elastic5<-glmnet(x.data_train,y.data_train,alpha = 0.1)
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
      evaluacion_rd1 = cbind(data_evaluation[,1], evaluacion_rd1[,2])
      colnames(evaluacion_rd1)[colnames(evaluacion_rd1) == "evaluacion_rd1[, 2]"] <- "ropamujer"
      
      evaluacion_ridge1 = cbind(data_evaluation[,1],evaluacion_ridge1)
      colnames(evaluacion_ridge1)[colnames(evaluacion_ridge1) == "s1"] <- "ropamujer"
      
      evaluacion_lasso1 = cbind(data_evaluation[,1],evaluacion_lasso1)
      colnames(evaluacion_lasso1)[colnames(evaluacion_lasso1) == "s1"] <- "ropamujer"
      
      write.csv(evaluacion_rd1, "evaluacion_rd2.csv", row.names = FALSE)
      write.csv(evaluacion_ridge1, "evaluacion_ridge2.csv", row.names = FALSE)
      write.csv(evaluacion_lasso1, "evaluacion_lasso2.csv", row.names = FALSE)
      
    # Completamos tabla RMSE
      # Ridge
      RMSE_table$Iteración[N]=N
      RMSE_table$Modelo[N] = "Modelo 20 data sin idloc con regularización Ridge"
      RMSE_table$RMSE[N] = erroresridge
      # Lasso
      RMSE_table$Iteración[N+1]=N+1
      RMSE_table$Modelo[N+1] = "Modelo 20 data sin idloc con regularización Lasso"
      RMSE_table$RMSE[N+1] = erroreslasso
      # Red Elastica
      RMSE_table$Iteración[N+2]=N+2
      RMSE_table$Modelo[N+2] = "Modelo 20 data sin idloc con regularización redes elasticas con alpha = 0.1"
      RMSE_table$RMSE[N+2] = erroreselastic

## 7.28. Regularizacion Ridge y Lasso con el mejor modelo stepwise --------
  # β + idloc + edadloc + correo + impresa + servicio + nomina + idmercado + correo_paginas + impresa_idmercado + impresa_sqr + correo_sqr + nomina_sqr + ln_edadloc + ln_correo + ln_servicio + ln_nomina + sqrt_nomina + sqrt_edadloc + sqrt_correo + sqrt_impresa + sqrt_servicio + edadloc_inversa + correo_inversa + paginas_inversa + impresa_inversa + edadloc_inversa_sqr + correo_inversa_sqr
      columns_to_keep <- c("ropamujer","idloc","edadloc","correo","impresa","servicio","nomina","idmercado","correo_paginas","impresa_idmercado","impresa_sqr","correo_sqr","nomina_sqr","ln_edadloc","ln_correo","ln_servicio","ln_nomina","sqrt_nomina","sqrt_edadloc","sqrt_correo","sqrt_impresa","sqrt_servicio","edadloc_inversa","correo_inversa","paginas_inversa","impresa_inversa","edadloc_inversa_sqr","correo_inversa_sqr")
      data_trainRD <- data_train[, (names(data_train) %in% columns_to_keep)]
      data_testRD <- data_test[, (names(data_test) %in% columns_to_keep)]
      data_evaluationRD <- data_evaluation[, (names(data_evaluation) %in% columns_to_keep)]
      
      N=28
      # Seprar predictores de variable a predecir
      y.data_train = as.matrix(data_trainRD[,8])
      y.data_test = as.matrix(data_testRD[,8])
      y.data_evaluation = as.matrix(data_evaluationRD[,8])
      
      # Estandarizar
      data_trainA<-as.data.frame(scale(data_trainRD[,1:7]))
      data_testA<-as.data.frame(scale(data_testRD[,1:7]))
      data_evaluationA<-as.data.frame(scale(data_evaluationRD[,1:7]))
      
      data_trainB<-scale(data_trainRD[,9:28])
      data_testB<-as.data.frame(scale(data_testRD[,9:28]))
      data_evaluationB<-as.data.frame(scale(data_evaluationRD[,9:28]))
      
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
      elastic5<-glmnet(x.data_train,y.data_train,alpha = 0.6)
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
      evaluacion_rd1 = cbind(data_evaluation[,1], evaluacion_rd1[,2])
      colnames(evaluacion_rd1)[colnames(evaluacion_rd1) == "evaluacion_rd1[, 2]"] <- "ropamujer"
      
      evaluacion_ridge1 = cbind(data_evaluation[,1],evaluacion_ridge1)
      colnames(evaluacion_ridge1)[colnames(evaluacion_ridge1) == "s1"] <- "ropamujer"
      
      evaluacion_lasso1 = cbind(data_evaluation[,1],evaluacion_lasso1)
      colnames(evaluacion_lasso1)[colnames(evaluacion_lasso1) == "s1"] <- "ropamujer"
      
      write.csv(evaluacion_rd1, "evaluacion_rd3.csv", row.names = FALSE)
      write.csv(evaluacion_ridge1, "evaluacion_ridge3.csv", row.names = FALSE)
      write.csv(evaluacion_lasso1, "evaluacion_lasso3.csv", row.names = FALSE)
      
      # Completamos tabla RMSE
      # Ridge
      RMSE_table$Iteración[N]=N
      RMSE_table$Modelo[N] = "Modelo 20 (28 vars seleccionadas a partir del stepwise) con regularización Ridge"
      RMSE_table$RMSE[N] = erroresridge
      # Lasso
      RMSE_table$Iteración[N+1]=N+1
      RMSE_table$Modelo[N+1] = "Modelo 20  (28 vars seleccionadas a partir del stepwise) data sin idloc con regularización Lasso"
      RMSE_table$RMSE[N+1] = erroreslasso
      # Red Elastica
      RMSE_table$Iteración[N+2]=N+2
      RMSE_table$Modelo[N+2] = "Modelo 20  (28 vars seleccionadas a partir del stepwise) data sin idloc con regularización redes elasticas con alpha = 0.1"
      RMSE_table$RMSE[N+2] = erroreselastic
      
## 7.31. Regresion con todo -------
  ### Limpiar el Enviroment -----
      # List the data frames you want to remove
      data_frames_to_remove <- c("data_evaluation", "data_test", "data_train")  # Specify the names of the data frames to remove
      # Remove the specified data frames from the environment
      rm(list = data_frames_to_remove)
  ### Crear nuevas tablas ----
      data_train = data[sample, ]
      data_test = data[-sample, ]
      data_evaluation = read_csv("test.csv")
  ###Transformación de variables -------
      #### Dummys para Categoricas ------
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
      
      #### Interacción de Correo y Paginas -----
      
      data_test$correo_paginas = data_test$correo*data_test$paginas
      data_train$correo_paginas = data_train$correo*data_train$paginas
      data_evaluation$correo_paginas = data_evaluation$correo*data_evaluation$paginas
      
      #### Interacción de Impresa y IDMercado -----
      
      data_test$impresa_idmercado = data_test$impresa * data_test$idmercado
      data_train$impresa_idmercado = data_train$impresa * data_train$idmercado
      data_evaluation$impresa_idmercado = data_evaluation$impresa * data_evaluation$idmercado
      
      #### X^2 de variables numericas ------ 
      
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
      
      #### √x raiz cuadrada de variables numericas ------ 
      
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
      
      #### 1/X de variables numericas -------
      
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
      
      #### 1/X^2 de variables numericas -------
      
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
      
      #### LOG(X) de variables numericas ------ 
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
      
      #### e^X de variables numericas ------ 
      
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
    
    N=31
    # Regresión
    modelo31 = lm(ropamujer ~.,data=data_train)
    summary(modelo31)
    
    modelo31<- step(modelo31,direction="both")
    summary(modelo31)
    
    RMSE_table$Iteración[N]=N
    RMSE_table$Modelo[N] = c("Stepwise de modelo con todo")
    
    # Comprobaciones de supuestos
    AIC(modelo31)
    RMSE_table$AIC[N]=AIC(modelo31)
    dwtest(modelo31)
    layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
    plot(modelo31)
    
    # Predicciones
    predicciones31 = predict(modelo31, data_test,se.fit = TRUE)
    RMSE31<-sqrt(mean((predicciones31$fit-data_test$ropamujer)^2))
    RMSE31
    RMSE_table$RMSE[N]=RMSE31
    
    RMSE_train31<-sqrt(mean((predicciones31$fit-data_train$ropamujer)^2))
    RMSE_train31
    RMSE_table$RMSE_train[N]=RMSE_train31
      
    #Evaluación/Solución
    evaluacion31 = predict(modelo31, data_evaluation,se.fit = TRUE)
    solucion31 = data_evaluation[,c(1,12)]
    solucion31$ropamujer = evaluacion31$fit
    write.csv(solucion31, "solucion_con_todo.csv", row.names = FALSE)

## 7.32. Modelaje con todo pero sin IDLOC ----
    N=32
    
    data_train1 = data_train[,-1]
    data_test1 = data_test[,-1]
    data_evaluation1 = data_evaluation[,]
    
    #Regresión
    modelo32<-lm(ropamujer ~.,data=data_train1)
    summary(modelo32)
    
    modelo32<- step(modelo32,direction="both")
    summary(modelo32)
    
    RMSE_table$Iteración[N]=N
    RMSE_table$Modelo[N] = c("Modelo con todo sin idloc y stepwise")
    
    # Comprobaciones de supuestos
    AIC(modelo32)
    RMSE_table$AIC[N]=AIC(modelo32)
    dwtest(modelo32)
    layout(matrix(c(1,2,3,4),2,2)) # opcional 4 graficos/pagina
    plot(modelo32)
    
    # Predicciones
    predicciones32 = predict(modelo32, data_test1,se.fit = TRUE)
    RMSE32<-sqrt(mean((predicciones12$fit-data_test1$ropamujer)^2))
    RMSE32
    RMSE_table$RMSE[N]=RMSE12
    
    RMSE_train32<-sqrt(mean((predicciones32$fit-data_train1$ropamujer)^2))
    RMSE_train32
    RMSE_table$RMSE_train[N]=RMSE_train32
    
    #Evaluación
    evaluacion32 = predict(modelo32, data_evaluation1,se.fit = TRUE)
    solucion32 = data_evaluation1[,c(1,12)]
    solucion32$ropamujer = evaluacion32$fit
    write.csv(solucion32, "solucion_con_todo_sin_idloc.csv", row.names = FALSE)
    
## 7.33. Ridge y Lasso Modelaje con todo pero sin IDLOC ------
    N=33
    
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
      
# 8.Completar la tabla con R2, R2 Ajustados y DW Tests ----
  RMSE_table$r2[1] = summary(modelo1)$r.squared
  RMSE_table$r2[2] = summary(modelo2)$r.squared
  RMSE_table$r2[3] = summary(modelo3)$r.squared
  RMSE_table$r2[4] = summary(modelo4)$r.squared
  RMSE_table$r2[5] = summary(modelo5)$r.squared
  RMSE_table$r2[6] = summary(modelo6)$r.squared
  RMSE_table$r2[7] = summary(modelo6)$r.squared
  RMSE_table$r2[8] = summary(modelo8)$r.squared
  RMSE_table$r2[9] = summary(modelo9)$r.squared
  RMSE_table$r2[10] = summary(modelo10)$r.squared
  RMSE_table$r2[11] = summary(modelo11)$r.squared
  RMSE_table$r2[12] = summary(modelo12)$r.squared
  RMSE_table$r2[13] = summary(modelo13)$r.squared
  RMSE_table$r2[14] = summary(modelo14)$r.squared
  RMSE_table$r2[15] = summary(modelo15)$r.squared
  RMSE_table$r2[16] = summary(modelo16)$r.squared
  RMSE_table$r2[17] = summary(modelo17)$r.squared
  RMSE_table$r2[18] = summary(modelo18)$r.squared
  RMSE_table$r2[19] = summary(modelo19)$r.squared
  RMSE_table$r2[20] = summary(modelo20)$r.squared
  RMSE_table$r2[21] = summary(modelo21)$r.squared
  RMSE_table$r2[31] = summary(modelo31)$r.squared
  RMSE_table$r2[32] = summary(modelo32)$r.squared  
  
  RMSE_table$r2_Adj[1] = summary(modelo1)$adj.r.squared
  RMSE_table$r2_Adj[2] = summary(modelo2)$adj.r.squared
  RMSE_table$r2_Adj[3] = summary(modelo3)$adj.r.squared
  RMSE_table$r2_Adj[4] = summary(modelo4)$adj.r.squared
  RMSE_table$r2_Adj[5] = summary(modelo5)$adj.r.squared
  RMSE_table$r2_Adj[6] = summary(modelo6)$adj.r.squared
  RMSE_table$r2_Adj[7] = summary(modelo6)$adj.r.squared
  RMSE_table$r2_Adj[8] = summary(modelo8)$adj.r.squared
  RMSE_table$r2_Adj[9] = summary(modelo9)$adj.r.squared
  RMSE_table$r2_Adj[10] = summary(modelo10)$adj.r.squared
  RMSE_table$r2_Adj[11] = summary(modelo11)$adj.r.squared
  RMSE_table$r2_Adj[12] = summary(modelo12)$adj.r.squared
  RMSE_table$r2_Adj[13] = summary(modelo13)$adj.r.squared
  RMSE_table$r2_Adj[14] = summary(modelo14)$adj.r.squared
  RMSE_table$r2_Adj[15] = summary(modelo15)$adj.r.squared
  RMSE_table$r2_Adj[16] = summary(modelo16)$adj.r.squared
  RMSE_table$r2_Adj[17] = summary(modelo17)$adj.r.squared
  RMSE_table$r2_Adj[18] = summary(modelo18)$adj.r.squared
  RMSE_table$r2_Adj[19] = summary(modelo19)$adj.r.squared
  RMSE_table$r2_Adj[20] = summary(modelo20)$adj.r.squared
  RMSE_table$r2_Adj[21] = summary(modelo21)$adj.r.squared
  RMSE_table$r2_Adj[31] = summary(modelo31)$adj.r.squared
  RMSE_table$r2_Adj[32] = summary(modelo32)$adj.r.squared  
  
  RMSE_table$DWTest[1] = dwtest(modelo1)$statistic
  RMSE_table$DWTest[2] = dwtest(modelo2)$statistic
  RMSE_table$DWTest[3] = dwtest(modelo3)$statistic
  RMSE_table$DWTest[4] = dwtest(modelo4)$statistic
  RMSE_table$DWTest[5] = dwtest(modelo5)$statistic
  RMSE_table$DWTest[6] = dwtest(modelo6)$statistic
  RMSE_table$DWTest[7] = dwtest(modelo6)$statistic
  RMSE_table$DWTest[8] = dwtest(modelo8)$statistic
  RMSE_table$DWTest[9] = dwtest(modelo9)$statistic
  RMSE_table$DWTest[10] = dwtest(modelo10)$statistic
  RMSE_table$DWTest[11] = dwtest(modelo11)$statistic
  RMSE_table$DWTest[12] = dwtest(modelo12)$statistic
  RMSE_table$DWTest[13] = dwtest(modelo13)$statistic
  RMSE_table$DWTest[14] = dwtest(modelo14)$statistic
  RMSE_table$DWTest[15] = dwtest(modelo15)$statistic
  RMSE_table$DWTest[16] = dwtest(modelo16)$statistic
  RMSE_table$DWTest[17] = dwtest(modelo17)$statistic
  RMSE_table$DWTest[18] = dwtest(modelo18)$statistic
  RMSE_table$DWTest[19] = dwtest(modelo19)$statistic
  RMSE_table$DWTest[20] = dwtest(modelo20)$statistic
  RMSE_table$DWTest[21] = dwtest(modelo21)$statistic
  RMSE_table$DWTest[31] = dwtest(modelo31)$statistic
  RMSE_table$DWTest[32] = dwtest(modelo32)$statistic  
  
  RMSE_table$DWTest_pvalue[1] = dwtest(modelo1)$p.value
  RMSE_table$DWTest_pvalue[2] = dwtest(modelo2)$p.value
  RMSE_table$DWTest_pvalue[3] = dwtest(modelo3)$p.value
  RMSE_table$DWTest_pvalue[4] = dwtest(modelo4)$p.value
  RMSE_table$DWTest_pvalue[5] = dwtest(modelo5)$p.value
  RMSE_table$DWTest_pvalue[6] = dwtest(modelo6)$p.value
  RMSE_table$DWTest_pvalue[7] = dwtest(modelo6)$p.value
  RMSE_table$DWTest_pvalue[8] = dwtest(modelo8)$p.value
  RMSE_table$DWTest_pvalue[9] = dwtest(modelo9)$p.value
  RMSE_table$DWTest_pvalue[10] = dwtest(modelo10)$p.value
  RMSE_table$DWTest_pvalue[11] = dwtest(modelo11)$p.value
  RMSE_table$DWTest_pvalue[12] = dwtest(modelo12)$p.value
  RMSE_table$DWTest_pvalue[13] = dwtest(modelo13)$p.value
  RMSE_table$DWTest_pvalue[14] = dwtest(modelo14)$p.value
  RMSE_table$DWTest_pvalue[15] = dwtest(modelo15)$p.value
  RMSE_table$DWTest_pvalue[16] = dwtest(modelo16)$p.value
  RMSE_table$DWTest_pvalue[17] = dwtest(modelo17)$p.value
  RMSE_table$DWTest_pvalue[18] = dwtest(modelo18)$p.value
  RMSE_table$DWTest_pvalue[19] = dwtest(modelo19)$p.value
  RMSE_table$DWTest_pvalue[20] = dwtest(modelo20)$p.value
  RMSE_table$DWTest_pvalue[21] = dwtest(modelo21)$p.value
  RMSE_table$DWTest_pvalue[31] = dwtest(modelo31)$p.value
  RMSE_table$DWTest_pvalue[32] = dwtest(modelo32)$p.value
  
  
  RMSE_table$semilla = semilla
  View(RMSE_table)
  print(semilla)


# 9. Resultados de residuales en graficos ----------
  #layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),W,5))
  # Seleccionar el modelo a graficar
  #modelo_to_plot = modelo18
  # Correr los residuales y las variables (Ojo con el dataset elegido - data_train o data_train1 o data_train2)
  # plot(data_train$idloc,modelo_to_plot$residuals)
  # plot(data_train$edadloc,modelo_to_plot$residuals)
  # plot(data_train$correo,modelo_to_plot$residuals)
  # plot(data_train$paginas,modelo_to_plot$residuals)
  # plot(data_train$telefono,modelo_to_plot$residuals)
  # plot(data_train$impresa,modelo_to_plot$residuals)
  # plot(data_train$servicio,modelo_to_plot$residuals)
  # plot(data_train$nomina,modelo_to_plot$residuals)
  # plot(data_train$idmercado,modelo_to_plot$residuals)
  # plot(data_train$ropamujer,modelo_to_plot$residuals)
  # 
  # plot(data_train$correo_paginas,modelo_to_plot$residuals)
  # plot(data_train$impresa_idmercado,modelo_to_plot$residuals)
  # plot(data_train$impresa_sqr,modelo_to_plot$residuals)
  # plot(data_train$correo_sqr,modelo_to_plot$residuals)
  # plot(data_train$nomina_sqr,modelo_to_plot$residuals)
  # 

#-----#