#------------------------------------------------------------------#
# Taller 1 Individual - Métodos y Aplicaciones de Analítica I
# Fecha: 25 de Agosto de 2023
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
  
# 2. Instalación de paquetes y descripción ----
library(readxl) #Para cargar bases de datos en Excel
library(psych) #Para estadistica descriptiva
library(e1071) #Offers quick and easy implementation of SVMs.
library("SmartEDA") #Para describir los datos (Análisis univariable) - punto 5.1
library(DataExplorer) #Para generar graphicos de los datos - punto 5.1 y 5.2
library("cluster") # Para calcular gap statistic
library("fpc")#Para evalur k-means cluster con ASW (average sillouethe width)
library(dplyr) #Para sacar muestras aleatorias de un dataset
library(ggplot2)#Para hacer graficos
library(reshape2)#Para reestructurar y agregar data usado en el punto 8.3
library("vcd")#Para visualizar data categorica - usado en el punto 8.3 grafico de mosaico
library("corrplot")#Para realizar grafico de correlaciones

# 3. Definir carpeta en la que voy a trabajar ----
setwd("~/OneDrive - Pontificia Universidad Javeriana/MFIA - PUJ - Maestría/Semestre I/1 Métodos y Aplicaciones de analitica I/Taller Individual 1")

# 4. Carga de datos -----
data_final = read_excel("infoclientebanca.xlsx")
columnas_a_eliminar = c("Consumo_consolidado")
data <- data_final[, !(names(data_final) %in% columnas_a_eliminar)]

# 5. Descripción de la base de datos -----
## 5.1. Revision de variables inicial (análisis univariado) ----
  
  # Estructura de la data
  Structure = ExpData(data=data,type=2, fun = c("mean", "median", "var"))
  View(Structure)
  plot_intro(data, title="Resumen variables data")
  plot_missing(data, title = "Resumen de missing values por columna")
  
  # Tabla descriptiva variables numericas
  Numerical_Vars = ExpNumStat(data,by="A",gp=NULL,Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2,Nlim=10)
  Numerical_Vars$ZeroPerc = (Numerical_Vars$nZero/47871)*100
  View(Numerical_Vars)
  
  # Tabla descriptiva variables categricas con 150 niveles o mas
  Categorical_Vars = ExpCTable(data,Target=NULL,margin=1,clim=150,nlim=3,round=2,bin=NULL,per=T)
  View(Categorical_Vars)
  
  # Graphical representation of all numeric features using Density plot (Univariate)
  # Note: Variable excluded (if unique value of variable which is less than or eaual to 10 [nlim=10])
  # Produces a PDF with the density plots and saves it in the defined folder above
  plot1 = ExpNumViz(data=data, target=NULL,type=2, nlim=100, 
                     Page=c(3,3), fname="Density de variables numéricas",
                    gtitle="Density de variables numéricas")
  
  # Hisotgrama de variables numericas (incluye las binarias)
  plot_histogram(data, binary_as_factor = FALSE, 
                 title = "Histograma variables numéricas y binarias",
                 nrow = 3, ncol = 3)
  
  # Graphical representation of categorical variables using Barplots
  # Produces a PDF with the barpplots and saves it in the defined folder above
  plot2 = ExpCatViz(data=data, target=NULL, clim=120, 
                   Page=c(2,1), fname="Barplot de variables categoricas", 
                   gtitle="Barplot de variables categoricas")
  plot_bar(data, maxcat=50, 
           title = "BARPLOT variables numericas y binarias",
           nrow =2, ncol = 2)
  plot_bar(data$Sitio_consumo_masfrecuente, maxcat = 109)
  plot_bar(data_final$Consumo_consolidado, maxcat = 109)
  
  #Boxplot de variables de transacciones y valor
  boxplot_plot1 <- ggplot(data, aes(x = CLIENTE, y = promedio_por_transaccion)) +
    geom_boxplot() +
    labs(title = "Promedio por transacción", x = "Clientes", y = "Promedio por transacción")
  boxplot_plot2 <- ggplot(data, aes(x = CLIENTE, y = Numero_de_transacciones)) +
    geom_boxplot() +
    labs(title = "Número de transacciones", x = "Clientes", y = "Número de transacciones")
  boxplot_plot3 <- ggplot(data, aes(x = CLIENTE, y = desviacion_estandar_por_transaccion)) +
    geom_boxplot() +
    labs(title = "Desviación estándar por transacción", x = "Clientes", y = "Desviación estándar por transacción")
  
  # Crear una rejilla de boxplots en una misma imagen
  multi_boxplot <- cowplot::plot_grid(boxplot_plot1, boxplot_plot2, boxplot_plot3, ncol = 3)
  
  # Mostrar la rejilla de boxplots
  print(multi_boxplot)
  
## 5.2. Revisión de variables (analisis multivariable)----
  # Correlación
  plot_correlation(na.omit(data), maxcat = 5L)
  
  # Boxplot entre tipo de cliente y promedio de transacción
  plot_boxplot(data = data, by="grupo_de_cliente", title = "Boxplots por grupo de cliente",
               nrow = 2L, ncol=2L)
  
  #hay datos atípicos????????
  ### si hay presencia de datos atípicos 
  
  
  
# 6. Transformación de datos---- 
# (seleccionar datos a usar eliminación de columnas, transformación de datos)
# para fines practicos creo un nuevo dataframe con el que voy a realizar el análisis de clustering

  # VAMOS A HACER CLUSTERING BASANDONOS EN EL USO QUE LE DAN A LA TC (PORCENTAJE DE USO)
  
## 6.1. Eliminar variables que no me sirven  -----
  # Eliminación de variables que a principio no me dicen mucho o no las voy a usar en
  # clustering
  columnas_a_eliminar = c("CLIENTE", "grupo_de_cliente", # no me dice nada del cliente
                          "transaccion_minima", "transaccion_maxima", "desviacion_estandar_por_transaccion",# alta correlacion con promedio y ds
                          "porcentaje_manana","porcentaje_tarde","porcentaje_noche", #sera que el momento del dia me dice algo?
                          "Sitio_consumo_masfrecuente", "Consumo_consolidado"#variable categorica con muchos niveles que no necesito en el clustering
                          ) # Se elimina porque suma 100 con porcentaje nacional total
  data1 <- data_final[, !(names(data_final) %in% columnas_a_eliminar)]
  
## 6.2. Transformación de la data -----
  
  ### A. Crear una columna del promedio total de compras por cliente --------
  data1$promedio_total = (data1$promedio_por_transaccion*data1$Numero_de_transacciones)
  
  ### B. Indicador VISA o MC ------
  data1$visa       = (data1$porcentaje_visa_nacional+data$porcentaje_visa_internacional)*data1$promedio_total
  data1$mastercard = (data1$porcentaje_mastercard_nacional+data$porcentaje_mastercard_internacional)*data1$promedio_total
  
  ### C. Indicador compra nacional o internacional -------
  data1$total_nacional      = data1$porcentaje_nacional_total*data1$promedio_total
  data1$total_internacional = data1$porcentaje_internacional_total*data1$promedio_total
  
  columnas_a_eliminar = c("porcentaje_nacional_total", "porcentaje_internacional_total")
  data1 <- data1[, !(names(data1) %in% columnas_a_eliminar)]
  
  ### D. Indicador de momento en la semana -----
  data1$fin_de_semana = (data1$porcDOMINGO+data1$porcSABADO)*data1$promedio_total
  # data1$entre_semana  = (data1$porcLUNES+data1$porcMARTES+data1$porcMIERCOLES+data1$porcJUEVES+data1$porcVIERNES)*data1$promedio_total
  
  columnas_a_eliminar = c("porcLUNES", "porcMARTES", "porcMIERCOLES", "porcJUEVES",
                          "porcVIERNES", "porcSABADO", "porcDOMINGO")
  data1 <- data1[, !(names(data1) %in% columnas_a_eliminar)]
  
  ### E. Crear columna con total de tarjetas por cliente ----
  # Usando los porcentajes de las tarjetas de credito puedo saber si el cliente tiene 1, 2 o 3 tarjetas
  data1$visa_count       = ifelse(data1$porcentaje_visa_nacional+data$porcentaje_visa_internacional>0,1,0)
  data1$mastercard_count = ifelse(data1$porcentaje_mastercard_nacional+data$porcentaje_mastercard_internacional>0,1,0)
  data1$otra_tc_count    = ifelse(data1$Porcentaje_otrafranquicia_nacional+data$porcentaje_otrafranquicia_internacional>0,1,0)
  data1$tc_usadas  = data1$visa_count + data1$mastercard_count + data1$otra_tc_count

  columnas_a_eliminar = c("porcentaje_visa_nacional", "porcentaje_visa_internacional", 
                          "porcentaje_mastercard_nacional", "porcentaje_mastercard_internacional", 
                          "Porcentaje_otrafranquicia_nacional", "porcentaje_otrafranquicia_internacional",
                          "Numero_de_transacciones", "promedio_por_transaccion",
                          "visa_count", "mastercard_count", "otra_tc_count")
  data1 <- data1[, !(names(data1) %in% columnas_a_eliminar)]
  
  ### F. Remover valores Atípicos usando función de atipicos ----

  #Función para quitar atípicos del subconjunto de datos (var Total Compras)
  remove_outliers <- function(x, na.rm = TRUE, ...) { 
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
  }
  
  #Quitamos atípicos utilizando la función
  promtx<-remove_outliers(data1$promedio_total) #Quitar atípicos
  
  #Agregar columna sin atipicos al subconjunto original
  data1<-cbind(data1,promtx)
  data_final = cbind(data_final,promtx)
  
  #Contamos el numero de atipicos
  sapply(data1, function(x) sum(is.na(data1$promtx)))
  
  #Quitar atípicos de la base original
  data1         = data1[!is.na(data1$promtx),]
  data_atipicos = data_final[is.na(data_final$promtx),]
  data_final    = data_final[!is.na(data_final$promtx),]
  
  columnas_a_eliminar = c("promedio_total","promtx","total_internacional")
  data1 <- data1[, !(names(data1) %in% columnas_a_eliminar)] 
  
  ## 6.3. Revisión de datos antes de hacer clustering -------
  # Hisotgrama de variables numericas (incluye las binarias)
  plot_histogram(data1, binary_as_factor = FALSE, 
                 title = "Histograma variables numericas transformadas",
                 nrow = 3, ncol = 3)
  
  # Tabla descriptiva variables numericas (incluye las binarias)
  Summary_vars_to_use = ExpNumStat(data1, by="A",gp=NULL,Qnt=seq(0,1,0.1),
                                   MesofShape=2,Outlier=TRUE,round=2,Nlim=2)
  View(Summary_vars_to_use)
  
  # Correlación
  base<-cor(data1)
  corrplot(base, type="upper",method="circle",title="Matriz de correlaciones", tl.cex=0.6,order="hclust", addCoef.col = "black", number.cex=0.5)

 ## 6.4. Transformación de variables con logaritmo y normalización -----
  # Variables a aplicar logaritmo
  vars_to_log = data1[,c(1,2,3,4,5)]
  
  #obtener el logaritmo de 1+x
  vars_logged = apply(vars_to_log,2,log1p)
  
  # Reemplazar las variables originales por las transformadas con logaritmo
  data1[, c(1,2,3,4,5)] <- vars_logged
  
  # Hisotgrama de variables numericas (incluye las binarias)
  plot_histogram(data1, binary_as_factor = FALSE, 
                 title = "Histograma variables numericas trasnformadas con logaritmo",
                 nrow = 3, ncol = 3)

  
## 6.5. Estandarización de variables-------
  # Estandarizar
  data1<-as.data.frame(scale(data1))
  
  # Revisar descriptivos
  psych::describe(data1)
  
  # Hisotgrama de variables numericas (incluye las binarias)
  plot_histogram(data1, binary_as_factor = FALSE, 
                 title = "Histograma variables numericas estandarizadas",
                 nrow = 3, ncol = 3)
  
# 7. Cálculo y definición del número de clusters a utilizar por medio de K-MEANS -----
## 7.1. Selección de K y gráfico de codo----
  # Definición de la semilla para replicar resultados
  set.seed(5935)
  
  # Cálculo la suma de cuadrados total
  suma_cuadrados <- (nrow(data1)-1)*sum(apply(data1,2,var))
  
  # Cálculo para cada solución de clustering
  for (i in 2:15) suma_cuadrados[i] <- sum(kmeans(data1,
                                       centers=i, nstart=20)$withinss)
  plot(1:15, suma_cuadrados, type="b", xlab="Número de Clústeres",
       ylab="Suma de cuadrados within")

## 7.2. Evaluar usando el criterio ASW (average sillouethe width) -----
  # # Definición de la semilla para replicar resultados
  set.seed(2) #Para evitar aleatoriedad en los resultados

  # Obtener una muestra aleatoria del 70% de los datos
  sample_size <- round(nrow(data1) * 0.7)
  sample_data <- data1 %>% sample_n(sample_size)

  #Cálculo de la silueta
  clustering.asw <- kmeansruns(sample_data,krange=2:8,criterion="asw",iter.max=100, runs= 100,critout=TRUE)
  clustering.asw$bestk

## 7.3. Evaluar usando  gap statistic -----
  # Definición de la semilla para replicar resultados
  set.seed(2) #Para evitar aleatoriedad en los resultados
  
  #mira el minimo k tal que el gap sea mayor que el gap de k+1 restado de su desviacion
  gscar<-clusGap(data1,FUN=kmeans,K.max=8,B=50)
  gscar
 
  
# 8. Ejecución de clustering (ahora si a agrupar) ----
## 8.1. De acuerdo al grafico de codo y el gap statistic defino el total de clusters a realizar ----
  total_clusters = 4
  
## 8.2. Ejecutar clusters-----
  # Definir semilla
  set.seed(45390)
  
  # Correr kmeans 
  data_cluster<-kmeans(data1,centers=total_clusters,nstart=10,iter.max=20)
  
  # Identificar el tamaño de cada grupo
  data_cluster$size
  
  # Obtener número de iteraciones
  data_cluster$iter
  
  # Centros de grupos
  data_cluster$centers
  # esto esa estandarizado entonces toca leerlo con respecto a desviacnes estandar

## 8.3. Revisar cómo quedaron los clusters ------
  # Realizar HeatMap
  centros<-as.data.frame(data_cluster$centers)
  
  centros$grupo<-as.factor(rownames(centros))
  
  centrosheat<-reshape2::melt(centros)
  
  colnames(centrosheat)<-c("grupo","variable","centroide")
  
  #Creación de mosaico
  ggplot(centrosheat,
         aes(x=grupo,y=variable,fill=centroide, label=sprintf("%0.2f", round(centroide, digits=2))))+
    geom_tile()+scale_fill_distiller(palette="RdBu")+
    geom_text()
  
## 8.4. Validar Clusters a partir de consistencia ----
  # Validar resultados- consistencia
  kclusters <- clusterboot(data1,B=10,clustermethod=kmeansCBI,k=total_clusters,seed=5)
  
  #la validacion del resultado. >0.75 o .85 muy bueno; <.6 malo
  kclusters$bootmean
  
# 9. Perfilamiento de Clusters ------ 
  # Guardar el cluster de pertenencia
  data_final$grupo<-data_cluster$cluster

## 9.1. Revisión de Mosaicos ------ 
  mosaic(~grupo + grupo_de_cliente ,data=data_final,
         legend=TRUE, shade=TRUE)
  
  mosaic(~grupo + Consumo_consolidado ,data=data_final,
         legend=TRUE, shade=TRUE)
  
## 9.2. Revisión Boxplots
  # Clusters
  data_final$grupo = factor(x = data_final$grupo)
  plot_boxplot(data = data_final, by="grupo", title = "Boxplots por clusters",
               nrow = 2L, ncol=2L, parallel=TRUE)
  
  ggplot(data_final, aes(x = Consumo_consolidado, fill = grupo)) +
    geom_bar(position = "dodge") +
    labs(title = "Grouped Bar Chart",
         x = "Consolidado",
         y = "Conteo de Observaciones",
         fill = "Grupos")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(data_final, aes(x =grupo , fill = Consumo_consolidado)) +
    geom_bar(position = "dodge") +
    labs(title = "Grouped Bar Chart",
         x = "grupo",
         y = "Conteo de Observaciones",
         fill = "Tipo de comercio")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Atipicos
  data_atipicos$grupo = factor(4)
  plot_boxplot(data = data_atipicos, by="grupo", title = "Boxplots por clusters",
               nrow = 2L, ncol=2L, parallel=TRUE)
  
## 9.3 Append de todo
  data_resulatado = rbind(data_final,data_atipicos)
  data_resulatado$grupo = factor(x = data_resulatado$grupo)  
  
  columnas_a_eliminar = c("promtx")
  data_resulatado <- data_resulatado[, !(names(data_resulatado) %in% columnas_a_eliminar)]
  
  # Boxplots
  plot_boxplot(data = data_resulatado, by="grupo", title = "Boxplots por clusters",
               nrow = 2L, ncol=2L, parallel=TRUE)
  
  #Boxplot de variables de transacciones y valor
  
  data_resulatado$entre_semana  = (data_resulatado$porcLUNES+data_resulatado$porcMARTES+
                                     data_resulatado$porcMIERCOLES+data_resulatado$porcJUEVES+
                                     data_resulatado$porcVIERNES)
  boxplot_plot1 <- ggplot(data_resulatado, aes(x = grupo, y = promedio_por_transaccion)) +
    geom_boxplot() +
    labs(title = "Promedio por transacción", x = "grupo", y = "Promedio por transacción")
  
  boxplot_plot2 <- ggplot(data_resulatado, aes(x = grupo, y = Numero_de_transacciones)) +
    geom_boxplot() +
    labs(title = "Número de transacciones", x = "grupo", y = "Número de transacciones")
  
  boxplot_plot3 <- ggplot(data_resulatado, aes(x = grupo, y = entre_semana)) +
    geom_boxplot() +
    labs(title = "% entre_semana", x = "grupo", y = "entre_semana")
  
  # Crear una rejilla de boxplots en una misma imagen
  multi_boxplot <- cowplot::plot_grid(boxplot_plot1, boxplot_plot2, boxplot_plot3, ncol = 3)
  
  # Mostrar la rejilla de boxplots
  print(multi_boxplot)
  
  
  ## Bar Chart de Tipo de comercio
  ggplot(data_resulatado, aes(x =grupo , fill = Consumo_consolidado)) +
    geom_bar(position = "dodge") +
    labs(title = "Grouped Bar Chart",
         x = "grupo",
         y = "Conteo de Observaciones",
         fill = "Tipo de comercio")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Segmented bar chart de tipo de comercio
  ggplot(data_resulatado, aes(x = grupo, fill = Consumo_consolidado)) +
    geom_bar(position = "fill") +
    labs(title = "Segmented Bar Chart",
         x = "Grupo",
         y = "Proporción de Observaciones",
         fill = "Tipo de Comercio") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  