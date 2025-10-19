#-------------------------------------------------------------------------------#
# Taller 1 Individual - Métodos y Aplicaciones de Analítica I
# Fecha: 18 de Septiembre de 2023
# Elaborado por: Maria Fernanda Izquierdo Aparicio y Alvaro Javier Acero Camargo  
#-------------------------------------------------------------------------------#

# 1. Instalación de librerias y descripción ----
install.packages(c("psych","e1071")) #psych Para estadistica descriptiva y e1071 para quick and easy implementation of SVMs.
install.packages("SmartEDA") #Para describir los datos (Análisis univariable)
install.packages("DataExplorer") #Para generar graphicos de los datos - punto 5.1 y 5.2
install.packages("reshape2") #Para reestructurar y agregar data
install.packages("vcd") #Para visualizar data categorica - grafico de mosaico
install.packages("corrplot") #Para realizar grafico de correlaciones
install.packages("ggplot2") #Generación de graficos
install.packages("tidyverse")
install.packages("arules")
install.packages("arulesViz")
install.packages("gmodels")
install.packages("igraph")
install.packages("factoextra")
install.packages("dendextend")
install.packages("grDevices")


# 2. Instalación de paquetes y descripción ----
library(readxl) #Para cargar bases de datos en Excel
library(readr) #Para cargar datos en csv
library(psych) #Para estadistica descriptiva
library(e1071) #Offers quick and easy implementation of SVMs.
library("SmartEDA") #Para describir los datos (Análisis univariable) - punto 5.1
library(DataExplorer) #Para generar graphicos de los datos - punto 5.1 y 5.2
library(dplyr) #Para sacar muestras aleatorias de un dataset
library(ggplot2)#Para hacer graficos
library(reshape2)#Para reestructurar y agregar data - usado en la creación de matrices
library("vcd")#Para visualizar data categorica - grafico de mosaico
library("corrplot")#Para realizar grafico de correlaciones
library(tidyverse)
library(arules)
library(arulesViz)
library(cluster)
library(factoextra)
library(dendextend)
library(grDevices)
library(data.table)
library(knitr)
library(stringr)
library(DT)
library("methods")
library("Matrix")


# 3. Definir carpeta en la que voy a trabajar ----
setwd("~/OneDrive - Pontificia Universidad Javeriana/MFIA - PUJ - Maestría/Semestre I/1 Métodos y Aplicaciones de analitica I/Taller Grupal 1")

# 4. Carga y Preparación de datos -----
## Data Dictionary: https://gist.github.com/jeremystan/c3b39d947d9b88b3ccff3147dbcf6c6b  
## Recursos: https://rpubs.com/aephidayatuloh/instacartmba 
## 4.1. Crear tabla transaccional de ordenes y productos -----
 # Importar tablas
 start_time <- Sys.time()
 order_products_prior = read_csv("order_products__prior.csv")
 products             = read_csv("products.csv")
 # Tabla de orden id y producto id
 data_orders = data.frame( order_id = order_products_prior$order_id,
                            product_id = order_products_prior$product_id)
 # Tabla producto id y nombre del producto
 data_products = data.frame(product_id = products$product_id,
                           products = products$product_name)
 # Unión de orden id, producto id y nombre del producto
 merged_data <- merge(data_orders, data_products, by = "product_id", all.x = TRUE)
 # Eliminar columnas que no son necesarias
 columnas_a_eliminar = c("product_id")
 merged_data <- merged_data[, !(names(merged_data) %in% columnas_a_eliminar)]
 # Guardar la tabla en un CSV
 file_path = "ordenes_y_productos.csv"
 write.csv(merged_data, file_path, row.names = FALSE)
 cat("Data frame saved to CSV:", file_path, "\n")
 end_time <- Sys.time()
 elapsed_time <- end_time - start_time
 print(elapsed_time)
 
## 4.2. Crear tabla transaccional de ordenes y pasillos -----
 start_time <- Sys.time()
 # Importar tablas
 aisles = read_csv("aisles.csv")
 # Tabla producto id y pasillo id
 data_products = data.frame(product_id = products$product_id,
                            aisle_id = products$aisle_id)
 # Tabla de producto y pasillo
 merged_data <- merge(data_products, aisles, by = "aisle_id", all.x = TRUE)
 # Unión de orden id, producto id y nombre del pasillo
 merged_data1 <- merge(data_orders, merged_data, by = "product_id", all.x = TRUE)
 # Eliminar columnas que no son necesarias
 columnas_a_eliminar = c("product_id", "aisle_id")
 merged_data1 <- merged_data1[, !(names(merged_data1) %in% columnas_a_eliminar)]
 # Guardar la tabla en un CSV
 file_path = "ordenes_y_pasillos.csv"
 write.csv(merged_data1, file_path, row.names = FALSE)
 cat("Data frame saved to CSV:", file_path, "\n")
 end_time <- Sys.time()
 elapsed_time <- end_time - start_time
 print(elapsed_time)
 
## 4.3. Crear tabla transaccional de ordenes y departamentos -----
 start_time <- Sys.time()
 # Importar tablas
 departments = read_csv("departments.csv")
 # Tabla producto id y departamento id
 data_products = data.frame(product_id = products$product_id,
                            department_id = products$department_id)
 # Tabla de producto y departamento
 merged_data <- merge(data_products, departments, by = "department_id", all.x = TRUE)
 # Unión de orden id, producto id y nombre del departamento
 merged_data1 <- merge(data_orders, merged_data, by = "product_id", all.x = TRUE)
 # Eliminar columnas que no son necesarias
 columnas_a_eliminar = c("product_id", "department_id")
 merged_data1 <- merged_data1[, !(names(merged_data1) %in% columnas_a_eliminar)]
 # Guardar la tabla en un CSV
 file_path = "ordenes_y_departamentos.csv"
 write.csv(merged_data1, file_path, row.names = FALSE)
 cat("Data frame saved to CSV:", file_path, "\n")
 end_time <- Sys.time()
 elapsed_time <- end_time - start_time
 print(elapsed_time)
 
## 4.4. Limpiar Consola y Enviroment y solo cargar lo que necesitamos ------
 # Remove all objects from the environment
 rm(list = ls())
 # Message indicating successful environment cleanup
 cat("Environment cleaned.\n")

## 4.5. Crear las vistas de matriz ------
 
 # ## Ordenes y Productos
 # ordenes_y_productos     = read_csv("ordenes_y_productos.csv")
 # binary_matrix <- ordenes_y_productos %>%
 #   distinct(order_id, products) %>%
 #   pivot_wider(names_from = products, values_from = products,
 #               values_fn = length, values_fill = 0)
 # # Guardar la tabla en un CSV
 # file_path = "matriz_ordenes_productos.csv"
 # write.csv(merged_data1, file_path, row.names = FALSE)
 # cat("Data frame saved to CSV:", file_path, "\n")
 
 rm(list = ls())
 
 ## Ordenes y Pasillos
 start_time <- Sys.time()
 ordenes_y_pasillos      = read_csv("ordenes_y_pasillos.csv")
 binary_matrix <- ordenes_y_pasillos %>%
   distinct(order_id, aisle) %>%
   pivot_wider(names_from = aisle, values_from = aisle,
               values_fn = length, values_fill = 0)
 # Guardar la tabla en un CSV
 file_path = "matriz_ordenes_pasillos.csv"
 write.csv(binary_matrix, file_path, row.names = FALSE)
 cat("Data frame saved to CSV:", file_path, "\n")
 end_time <- Sys.time()
 elapsed_time <- end_time - start_time
 print(elapsed_time)
 
 rm(list = ls())
 
 ## Ordenes y departamentos
 start_time <- Sys.time()
 ordenes_y_departamentos = read_csv("ordenes_y_departamentos.csv")
 binary_matrix <- ordenes_y_departamentos %>%
   distinct(order_id, department) %>%
   pivot_wider(names_from = department, values_from = department,
               values_fn = length, values_fill = 0)
 # Guardar la tabla en un CSV
 file_path = "matriz_ordenes_departamentos.csv"
 write.csv(binary_matrix, file_path, row.names = FALSE)
 cat("Data frame saved to CSV:", file_path, "\n")
 end_time <- Sys.time()
 elapsed_time <- end_time - start_time
 print(elapsed_time)
 
## 4.6. Jerarquia Ordenes, Pasillos y Departamentos ---- 
 departments = read_csv("departments.csv")
 products             = read_csv("products.csv")
 aisles = read_csv("aisles.csv")
 merged_data <- merge(products, aisles, by = "aisle_id", all.x = TRUE)
 merged_data = merge(merged_data, departments, by = "department_id", all.x = TRUE)
 
 # Define the desired column order
 desired_order <- c("department_id", "department", 
                    "aisle_id", "aisle",
                    "product_id","product_name")
 
 # Reorder columns in the data frame
 merged_data <- merged_data %>%
   select(all_of(desired_order))
 # Guardar la tabla en un CSV
 file_path = "jerarquia_productos_pasillos_departamentos.csv"
 write.csv(merged_data, file_path, row.names = FALSE)
 cat("Data frame saved to CSV:", file_path, "\n")
 
## 4.7. Cargar datos definitivos ------
 rm(list = ls())
 Jerarquia_productos_pasillos_departamentos = read_excel("jerarquia_productos_pasillos_departamentos.xlsx", 
                                                         +     sheet = "jerarquia_productos_pasillos_de")
 ordenes_y_pasillos  = read_csv("matriz_ordenes_pasillos.csv")
 ordenes_y_departamentos = read_csv("matriz_ordenes_departamentos.csv")
 
# 5. Descripción de la base de datos -----
 order_products_prior = read_csv("order_products__prior.csv")
 orders               = read_csv("orders.csv")
 products             = read_csv("products.csv")
 aisles               = read_csv("aisles.csv")
 departments          = read_csv("departments.csv")
 
 orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
 products <- products %>% mutate(product_name = as.factor(product_name))
 aisles <- aisles %>% mutate(aisle = as.factor(aisle))
 departments <- departments %>% mutate(department = as.factor(department))
 
## 5.1. Revision de variables inicial (análisis univariado) ----

   ### Hora del dia para la compra
orders %>% 
     ggplot(aes(x=order_hour_of_day)) + 
     geom_histogram(stat="count",fill="red")+
     ggtitle("Distribución de ordenes durante el dia")
 
       # Obtener los valores del histograma
       hist_data <- ggplot_build(hist_plot)$data[[1]]
       
       # Crear una tabla a partir de los valores del histograma
       tabla <- data.frame(
         Hora_del_día = hist_data$x,
         Conteo_de_órdenes = hist_data$y
       )

    promedio_hora = mean(orders$order_hour_of_day)
    curtosis_hora = kurtosis(orders$order_hour_of_day)
    sesgo_hora = skewness(orders$order_hour_of_day)
   
   ### Dia de la semana para la compra
   orders %>% 
     ggplot(aes(x=order_dow)) + 
     geom_histogram(stat="count",fill="blue")+
     ggtitle("Distribución de ordenes durante los días de la semana")+
     labs(caption = "NOTA: No sabemos sabe que dia corresponde a cada dígito")
   
   promedio_dia = mean(orders$order_dow)
   
     # Tabla
     table_data <- orders %>%
       group_by(order_dow) %>%
       count() %>%
       arrange(order_dow)
     # Rename the columns for clarity
     colnames(table_data) <- c("Day of the Week", "Frequency")
   
   ### Numero de ordenes por usuario
   table_data <- orders %>%
     group_by(user_id) %>%
     count() %>%
     arrange(user_id)
   
   ### Dias que pasan desde la compra para volver a comprar
  orders %>% 
     ggplot(aes(x=days_since_prior_order)) + 
     geom_histogram(stat="count",fill="green")+
     ggtitle("Distribución de ordenes de acuerdo a dias que días que pasan desde la anterior compra")
   
   # Obtener los valores del histograma
   hist_data <- ggplot_build(hist)$data[[1]]
   
   # Crear una tabla a partir de los valores del histograma
   tabla_days_since <- data.frame(
     dias_desde_ultima_orden = hist_data$x,
     Conteo_de_órdenes = hist_data$y)
   
   na_days_since = sum(is.na(orders$days_since_prior_order))
   promedio_days_since = mean(orders$days_since_prior_order, na.rm = TRUE)
   curtosis_days_since = kurtosis(orders$days_since_prior_order, na.rm = TRUE)
   sesgo_days_since = skewness(orders$days_since_prior_order, na.rm = TRUE)
   
   ### ¿Cuántos pedidos previos hay?
     orders %>% filter(eval_set=="prior") %>% count(order_number) %>% 
       ggplot(aes(order_number,n)) + geom_line(color="darkmagenta", size=1)+geom_point(size=2, color="darkmagenta")+
       ggtitle("Distribución de ordenes de acuerdo a pedidos previos")
     
   ### Numero de productos pedidos por orden
     order_products_prior %>% 
       group_by(order_id) %>% 
       summarize(n_items = last(add_to_cart_order)) %>%
       ggplot(aes(x=n_items))+
       geom_histogram(stat="count",fill="blue4") + 
       geom_rug()+
       geom_text(
         aes(label = after_stat(count)), 
         stat = "count", 
         vjust = -0.5, 
         size = 3
       ) +
       coord_cartesian(xlim=c(0,80))+
       ggtitle("Distribución del número de productos pedidos por orden")
     
     table_productos_por_orden <- order_products_prior %>%
       group_by(order_id) %>%
       summarize(n_items = last(add_to_cart_order)) %>%
       group_by(n_items) %>%
       count() %>%
       arrange(n_items)
     
     
     table_productos_por_orden <- order_products_prior %>%
       group_by(order_id) %>%
       summarize(n_items = last(add_to_cart_order))
     
     promedio_productos = mean(table_productos_por_orden$n_items)
    curtosis_productos = kurtosis(table_productos_por_orden$n_items)
     sesgo_productos = skewness(table_productos_por_orden$n_items)
     
     # Rename the columns for clarity
     colnames(table_data) <- c("Number of Items", "Frequency")
     

  ### Productos más vendidos Top 50
     top50_productos <- order_products_prior %>% 
       group_by(product_id) %>% 
       summarize(count = n()) %>% 
       top_n(50, wt = count) %>%
       left_join(select(products,product_id,product_name),by="product_id") %>%
       arrange(desc(count)) 
     kable(top50_productos)
     
     top50_productos %>% 
       ggplot(aes(x=reorder(product_name,-count), y=count))+
       geom_bar(stat="identity",fill="coral")+
       theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+
       ggtitle("Top 50 de productos mas vendidos")
     
     orden_productos <- order_products_prior %>% 
       group_by(product_id) %>% 
       summarize(count = n()) %>%
       left_join(select(products,product_id,product_name),by="product_id")
     
   ### Pasillos mas usados top 50
     ordenes_y_pasillos <- read_csv("ordenes_y_pasillos.csv")
     top50_pasillos <- ordenes_y_pasillos %>% 
       group_by(aisle) %>%  
       summarize(count = n()) %>% 
       top_n(50, wt = count) %>%
       arrange(desc(count)) 
     kable(top50_pasillos)
     
     top50_pasillos %>% 
       ggplot(aes(x=reorder(aisle,-count), y=count))+
       geom_bar(stat="identity",fill="darkturquoise")+
       theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+
       ggtitle("Top 50 de pasillos mas usados")
     
   ### Departamentos mas usados 
     ordenes_y_departamentos <- read_csv("ordenes_y_departamentos.csv")
     
     top_departamentos <- ordenes_y_departamentos %>% 
       group_by(department) %>%  
       summarize(count = n()) %>% 
       arrange(desc(count)) 
     kable(top_departamentos)
     
     top_departamentos %>% 
       ggplot(aes(x=reorder(department,-count), y=count))+
       geom_bar(stat="identity",fill="darkgreen")+
       theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+
       ggtitle("Orden de departamentos mas usados")
 
   ### Portafolio de Productos
     install.packages("treemap")
     library(treemap)
     
     tmp <- products %>% group_by(department_id, aisle_id) %>% summarize(n=n())
     tmp <- tmp %>% left_join(departments,by="department_id")
     tmp <- tmp %>% left_join(aisles,by="aisle_id")
     
     tmp2<-order_products_prior %>% 
       group_by(product_id) %>% 
       summarize(count=n()) %>% 
       left_join(products,by="product_id") %>% 
       ungroup() %>% 
       group_by(department_id,aisle_id) %>% 
       summarize(sumcount = sum(count)) %>% 
       left_join(tmp, by = c("department_id", "aisle_id")) %>% 
       mutate(onesize = 1)
     
     # Pasillos dentro de departamentos
     treemap(tmp2,index=c("department","aisle"),vSize="onesize",vColor="department",
             palette="Set3",title="Pasillos por departamentos",sortID="-sumcount", border.col="#FFFFFF",
             type="categorical", fontsize.legend = 0,bg.labels = "#FFFFFF")
    
     # ¿Cuántos productos únicos se ofrecen en cada departamento/pasillo?
     #  El tamaño de las cajas muestra la cantidad de productos de cada categoría.
     treemap(tmp,index=c("department","aisle"),vSize="n",title="Productos por departamento",palette="Set3",
             border.col="#FFFFFF")
    
     # ¿Con qué frecuencia se venden los productos del departamento/pasillo?
     #   El tamaño de las cajas muestra el número de ventas. 
     treemap(tmp2,index=c("department","aisle"),vSize="sumcount",title="¿Con qué frecuencia se venden los productos del departamento/pasillo?",palette="Set3",border.col="#FFFFFF")
    
     

     
# 6. Modelado productos saludables----
## 6.1. Carga de dataset final productos saludables -----
  rm(list = ls())
  data <- read_csv("dataset_1_aisles.csv")
  data = data[1:3363687,]
  
  # Revisar conteo distintivo de pasillos
  unique_aisle = unique(data$aisle)
  count_unique_aisle = length(unique_aisle)
  print(count_unique_aisle)
  # Revisar conteo distintivo de ordens
  unique_order = unique(data$order_id)
  count_unique_order = length(unique_order)
  print(count_unique_order)
  
  ## 6.2. Creación matriz binaria -----
  binary_matrix <- data %>%
    distinct(order_id, aisle) %>%
    pivot_wider(names_from = aisle, values_from = aisle,
                values_fn = length, values_fill = 0)
  # Remover la columna de order_id para que no cause problema en la generación de la matriz transaccional
  binary_matrix = binary_matrix[,-1]
  
## 6.3. Creación matriz transaccional ----
  binary_matrix1<-as.matrix(binary_matrix)
  binary_matrix1<-as(binary_matrix1,"transactions") 

## 6.4 Aplicación algoritmo A-Priori ------
  
  # Revisar frecuencia de cada producto (Support)
  itemFrequencyPlot(binary_matrix1, topN=50,  cex.names=.5)
    # Frutas y vegetales estan en mas del 50% de las transacciones 
  
  #Obtener reglas
  # Support = 10% y Confidence = 50%
  rulesA<-apriori(binary_matrix1,parameter=list(support=0.1,confidence=0.5))
  summary(rulesA)  
   # Support = 10% y Confidence = 60%
  rulesB<-apriori(binary_matrix1,parameter=list(support=0.1,confidence=0.6))
  summary(rulesB)  
  # Support = 10% y Confidence = 70%
  rulesC<-apriori(binary_matrix1,parameter=list(support=0.1,confidence=0.7))
  summary(rulesC) 

  #Revisar primeras 20 reglas de la combinación C
  inspect(head(sort(rulesC,by="lift"),50))
  
  #Gráficos para ver potenciales asociaciones de interés
  plot(rulesC, measure=c("support","lift"), shading="confidence")
  
  plot(rules, measure=c("support","lift"), shading="confidence", control=list(main="Reglas de MBA"))  
  inspect(head(sort(rules,by="support"),20))  
  subrules<-subset(rules,quality(rules)$confidence>0.7)
  plot (subrules, method="matrix", measure="lift", control=list(reorder='support/confidence'))  
  plot (subrules, method="matrix", measure=c("lift", "support"), control=list(reorder='support/confidence'))  
  subrules2 <- head(sort(rules, by="lift"), 10)
  str(rules)  
  plot(subrules2, method="graph")  
  plot(subrules2, method="graph", arrowSize=.01)  
  
  
# 7. Modelación productos diff. a saludables ------
  ## 7.1. Carga de dataset final productos saludables -----
  rm(list = ls())
  data <- read_csv("dataset_2_aisles.csv")
  data = data[1:980647,]
  
  # Revisar conteo distintivo de pasillos
  unique_aisle = unique(data$aisle)
  count_unique_aisle = length(unique_aisle)
  print(count_unique_aisle)
  # Revisar conteo distintivo de ordens
  unique_order = unique(data$order_id)
  count_unique_order = length(unique_order)
  print(count_unique_order)
  
  ## 7.2. Creación matriz binaria -----
  binary_matrix <- data %>%
    distinct(order_id, aisle) %>%
    pivot_wider(names_from = aisle, values_from = aisle,
                values_fn = length, values_fill = 0)
  # Remover la columna de order_id para que no cause problema en la generación de la matriz transaccional
  binary_matrix = binary_matrix[,-1]
  
  ## 7.3. Creación matriz transaccional ----
  binary_matrix1<-as.matrix(binary_matrix)
  binary_matrix1<-as(binary_matrix1,"transactions") 
  
  ## 7.4 Aplicación algoritmo A-Priori ------
  
  # Revisar frecuencia de cada producto (Support)
  itemFrequencyPlot(binary_matrix1, topN=50,  cex.names=.5)
  # Estrategia Nicho todo esta en menos del 30% de las transacciones
  
  #Obtener reglas
  # Support = 10% y Confidence = 40%
  rulesA<-apriori(binary_matrix1,parameter=list(support=0.01,confidence=0.4))
  summary(rulesA)  
  # Support = 10% y Confidence = 60%
  rulesB<-apriori(binary_matrix1,parameter=list(support=0.01,confidence=0.5))
  summary(rulesB)  
  # Support = 10% y Confidence = 70%
  rulesC<-apriori(binary_matrix1,parameter=list(support=0.01,confidence=0.6))
  summary(rulesC) 
  
  #Revisar primeras 20 reglas de la combinación A
  inspect(head(sort(rulesA,by="lift"),20))
  
  #Gráficos para ver potenciales asociaciones de interés
  plot(rulesA, measure=c("support","lift"), shading="confidence")
  
  plot(rules, measure=c("support","lift"), shading="confidence", control=list(main="Reglas de MBA"))  
  inspect(head(sort(rules,by="support"),20))  
  subrules<-subset(rules,quality(rules)$confidence>0.7)
  plot (subrules, method="matrix", measure="lift", control=list(reorder='support/confidence'))  
  plot (subrules, method="matrix", measure=c("lift", "support"), control=list(reorder='support/confidence'))  
  subrules2 <- head(sort(rules, by="lift"), 10)
  str(rules)  
  plot(subrules2, method="graph")  
  plot(subrules2, method="graph", arrowSize=.01)  