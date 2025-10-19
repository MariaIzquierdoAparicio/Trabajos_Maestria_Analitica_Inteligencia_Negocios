#-------------------------------------------------------------------------#
# Taller 2 Grupal - Métodos y Aplicaciones de Analítica I
# Fecha: 29 de Octubre de 2023
# Elaborado por: Maria Fernanda Izquierdo Aparicio & Juan Pablo Cuellar 
#-------------------------------------------------------------------------#

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
install.packages("randomForest")
install.packages("xgboost")
# 2. Instalación de librerias y descripción ----
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
library(ROCR)
#library("lift")
library(ROSE)
library(randomForest)
library(xgboost)
# 3. Definir carpeta en la que voy a trabajar ----
setwd("~/OneDrive - Pontificia Universidad Javeriana/MFIA - PUJ - Maestría/Semestre I/1 Métodos y Aplicaciones de analitica I/Taller Grupal 2")

# 4. Carga de datos -----
gc() 
rm(list = ls())
data_evaluacion = read_excel("testelco.xlsx")
data = read_excel("traintelco.xlsx")

# 5. Descripción de la base de datos -----
  ## 5.1. Revision de variables inicial (análisis univariado) ----
    ### Estructura de la data ----
    Structure = ExpData(data=data,type=2, fun = c("mean", "median", "var"))
    View(Structure)
    plot_intro(data, title="Resumen variables data")
    plot_missing(data, title = "Resumen de missing values por columna")
    
    # Tabla descriptiva variables numericas
    Numerical_Vars = ExpNumStat(data,by="A",gp=NULL,Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2,Nlim=1)
    Numerical_Vars$ZeroPerc = (Numerical_Vars$nZero/8243)*100
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
                   nrow = 2, ncol = 5, ggtheme = theme_bw())

    ### Scatterplots de variable Y con respecto a variables -----
    scatter_plot1 = ggplot(data = data, aes(x =tipo_cliente , y = resultado , color = tipo_cliente)) +
      geom_point() +
      labs(x = "tipo_cliente", y = "Resultado", 
           title = "Resultado por tipo de cliente")
    print(scatter_plot1)
    
    scatter_plot2 = ggplot(data = data, aes(x = Factura_online, y = resultado , color = Factura_online)) +
      geom_point() +
      labs(x = "factura_online", y = "Resultado", 
           title = "Resultado por factura online")
    print(scatter_plot2)
    
    scatter_plot3 = ggplot(data = data, aes(x = Antigüedad_Equipo, y = resultado , color = Antigüedad_Equipo)) +
      geom_point() +
      labs(x = "Antiguedad Equipo", y = "Resultado", 
           title = "Resultado por antiguedad equipo")
    print(scatter_plot3)
    
    scatter_plot4 = ggplot(data = data, aes(x = Plan_de_datos, y = resultado , color = Plan_de_datos)) +
      geom_point() +
      labs(x = "Plan_de_datos", y = "Resultado", 
           title = "Resultado por plan de datos")
    print(scatter_plot4)
    
    scatter_plot5 = ggplot(data = data, aes(x = facturación, y = resultado , color = facturación)) +
      geom_point() +
      labs(x = "facturación", y = "Resultado", 
           title = "Resultado por facturacion")
    print(scatter_plot5)
    
    scatter_plot6 = ggplot(data = data, aes(x = mora, y = resultado , color = mora)) +
      geom_point() +
      labs(x = "mora", y = "Resultado", 
           title = "Resultado por mora")
    print(scatter_plot6)
    
    scatter_plot7 = ggplot(data = data, aes(x = minutos, y = resultado , color = minutos)) +
      geom_point() +
      labs(x = "minutos", y = "Resultado", 
           title = "Resultado por minutos")
    print(scatter_plot7)
    
    scatter_plot8 = ggplot(data = data, aes(x = Edad, y = resultado , color = Edad)) +
      geom_point() +
      labs(x = "Edad", y = "Resultado", 
           title = "Resultado por Edad")
    print(scatter_plot8)
    
    scatter_plot9 = ggplot(data = data, aes(x = Antigüedad_contrato_años, y = resultado , color = Antigüedad_contrato_años)) +
      geom_point() +
      labs(x = "Antigüedad_contrato_años", y = "Resultado", 
           title = "Resultado por Antigüedad_contrato_años")
    print(scatter_plot9)
#-----#
    
    
  ## 5.2. Revisión de variables (analisis multivariable)----
    ### Correlación ----
    plot_correlation(na.omit(data), maxcat = 5L)
    
    ### Graficos con respecto a la variable dependiente -----
    gg1 = ggplot(data = data, aes(x = resultado, y = id)) +
      geom_point() +
      labs(title = "Gráfico 1", 
           y = "ID ", 
           x = "Resultado")+
      theme_calc()+scale_fill_calc() 
    
    gg2 = ggplot(data = data, aes(x = resultado, y = tipo_cliente)) +
      geom_point() +
      labs(title = "Gráfico 2", 
           y = "tipo_cliente", 
           x = "Resultado")+
      theme_calc()+scale_fill_calc()
    
    gg3 = ggplot(data = data, aes(x = resultado, y = Factura_online)) +
      geom_point() +
      labs(title = "Gráfico 3", 
           y = "Factura_online", 
           x = "Resultado")+
      theme_calc() +scale_fill_calc()
    
    gg4 = ggplot(data = data, aes(x = resultado, y = Antigüedad_Equipo)) +
      geom_point() +
      labs(title = "Gráfico 4", 
           y = "Antigüedad_equipo", 
           x  = "Resultado")+
      theme_calc()+scale_fill_calc()
    
    gg5 = ggplot(data = data, aes(x = resultado, y = Plan_de_datos)) +
      geom_point() +
      labs(title = "Gráfico 5", 
           y = "Plan_de_datos", 
           x = "Resultado")+
      theme_calc()+scale_fill_calc()
    
    gg6 = ggplot(data = data, aes(x = resultado, y = facturación)) +
      geom_point() +
      labs(title = "Gráfico 6", 
           y = "facturación", 
           x = "Resultado")+
      theme_calc()+scale_fill_calc()
    
    gg7 = ggplot(data = data, aes(x = resultado, y = mora)) +
      geom_point() +
      labs(title = "Gráfico 7", 
           y = "mora", 
           x = "Resultado")+
      theme_calc()+scale_fill_calc()
    
    gg8 = ggplot(data = data, aes(x = resultado, y = minutos)) +
      geom_point() +
      labs(title = "Gráfico 8", 
           y = "minutos", 
           x = "Resultado")+
      theme_calc()+scale_fill_calc()
    
    gg9 = ggplot(data = data, aes(x = resultado, y = Edad)) +
      geom_point() +
      labs(title = "Gráfico 9", 
           y = "Edad", 
           x = "Resultado")+theme_calc()+scale_fill_calc()
    
    gg10 =  ggplot(data = data, aes(x = resultado, y = Antigüedad_contrato_años)) +
      geom_point() +
      labs(title = "Gráfico 10", 
           y = "Antigüedad_contrato_años", 
           y = "Resultado")+theme_calc()+scale_fill_calc()
    
    # Arrange the plots
    combined_ggplots = grid.arrange(gg1, gg2, gg3, gg4, gg5, gg6, gg7, gg8, gg9, gg10, nrow = 2, ncol = 5)
    
    
    ### Graficos entre variables explicativas ------
    gg11 = ggplot(data = data, aes(x = tipo_cliente, y = Antigüedad_Equipo)) +
      geom_point() +
      labs(title = "Gráfico 11", 
           x = "tipo_cliente", 
           y = "Antigüedad_Equipo")+
      theme_calc()+scale_fill_calc()
    
    gg12 = ggplot(data = data, aes(x = tipo_cliente, y = facturación)) +
      geom_point() +
      labs(title = "Gráfico 12", 
           x = "tipo_cliente", 
           y = "facturación")+
      theme_calc()+scale_fill_calc()
    
    gg13 = ggplot(data = data, aes(x = tipo_cliente, y = mora)) +
      geom_point() +
      labs(title = "Gráfico 13", 
           x = "tipo_cliente", 
           y = "mora")+
      theme_calc()+scale_fill_calc()
    
    gg14 =  ggplot(data = data, aes(x = tipo_cliente, y = minutos)) +
      geom_point() +
      labs(title = "Gráfico 14", 
           y = "tipo_cliente", 
           x = "minutos")+
      theme_calc()+scale_fill_calc()
    
    gg15 = ggplot(data = data, aes(x = tipo_cliente, y = Edad)) +
      geom_point() +
      labs(title = "Gráfico 15", 
           x = "tipo_cliente", 
           y = "edad")+
      theme_calc()+scale_fill_calc()
    
    gg16 = ggplot(data = data, aes(x = tipo_cliente, y = Antigüedad_contrato_años)) +
      geom_point() +
      labs(title = "Gráfico 16", 
           x = "tipo_cliente", 
           y = "Antigüedad_contrato_años")+
      theme_calc()+scale_fill_calc()
    
    gg17 = ggplot(data = data, aes(x = Factura_online, y = Antigüedad_Equipo)) +
      geom_point() +
      labs(title = "Gráfico 17", 
           x = "Factura_online", 
           y = "Antigüedad_Equipo")+
      theme_calc()+scale_fill_calc()
    
    gg18 = ggplot(data = data, aes(x = Factura_online, y = facturación)) +
      geom_point() +
      labs(title = "Gráfico 18", 
           x = "Factura_online", 
           y = "facturación")+
      theme_calc()+scale_fill_calc()
    
    gg19 = ggplot(data = data, aes(x = Factura_online, y = mora)) +
      geom_point() +
      labs(title = "Gráfico 19", 
           x = "Factura_online", 
           y = "mora")+
      theme_calc()+scale_fill_calc()
    
    gg20 =  ggplot(data = data, aes(x = Factura_online, y = minutos)) +
      geom_point() +
      labs(title = "Gráfico 20", 
           y = "Factura_online", 
           x = "minutos")+
      theme_calc()+scale_fill_calc()
    
    gg21 = ggplot(data = data, aes(x = Factura_online, y = Edad)) +
      geom_point() +
      labs(title = "Gráfico 21", 
           x = "Factura_online", 
           y = "edad")+
      theme_calc()+scale_fill_calc()
    
    gg22 = ggplot(data = data, aes(x = Factura_online, y = Antigüedad_contrato_años)) +
      geom_point() +
      labs(title = "Gráfico 22", 
           x = "Factura_online", 
           y = "Antigüedad_contrato_años")+
      theme_calc()+scale_fill_calc()
    
    gg23 = ggplot(data = data, aes(x = facturación, y = minutos)) +
      geom_point() +
      labs(title = "Gráfico 23", 
               x = "Facturación", 
                y = "minutos")+
           theme_calc()+scale_fill_calc()
    
    gg24 = ggplot(data = data, aes(x = facturación, y = mora)) +
      geom_point() +
      labs(title = "Gráfico 24", 
           x = "Facturación", 
           y = "mora")+
      theme_calc()+scale_fill_calc()
    
    gg25 = ggplot(data = data, aes(x = facturación, y = Edad)) +
      geom_point() +
      labs(title = "Gráfico 25", 
           x = "Facturación", 
           y = "Edad")+
      theme_calc()+scale_fill_calc()
    
    gg26 = ggplot(data = data, aes(x = facturación, y = Antigüedad_contrato_años)) +
      geom_point() +
      labs(title = "Gráfico 26", 
           x = "Facturación", 
           y = "Antigüedad_contrato_años")+
      theme_calc()+scale_fill_calc()
    
    gg27 = ggplot(data = data, aes(x = facturación, y = Antigüedad_Equipo)) +
      geom_point() +
      labs(title = "Gráfico 27", 
           x = "Facturación", 
           y = "Antigüedad_Equipo")+
      theme_calc()+scale_fill_calc()
    
    gg28 = ggplot(data = data, aes(x = facturación, y = Plan_de_datos)) +
      geom_point() +
      labs(title = "Gráfico 28", 
           x = "Facturación", 
           y = "Plan_de_datos")+
      theme_calc()+scale_fill_calc()
    
    
    gg29 = ggplot(data = data, aes(x = mora, y = minutos)) +
      geom_point() +
      labs(title = "Gráfico 29", 
           x = "mora", 
           y = "minutos")+
      theme_calc()+scale_fill_calc()
    
    gg30 = ggplot(data = data, aes(x = mora, y = Edad)) +
      geom_point() +
      labs(title = "Gráfico 30", 
           x = "mora", 
           y = "Edad")+
      theme_calc()+scale_fill_calc()
    
    gg31 = ggplot(data = data, aes(x = mora, y = Antigüedad_contrato_años)) +
      geom_point() +
      labs(title = "Gráfico 31", 
           x = "mora", 
           y = "Antigüedad_contrato_años")+
      theme_calc()+scale_fill_calc()
    
    gg32 = ggplot(data = data, aes(x = mora, y = Antigüedad_Equipo)) +
      geom_point() +
      labs(title = "Gráfico 32", 
           x = "mora", 
           y = "Antigüedad_Equipo")+
      theme_calc()+scale_fill_calc()
    
    gg33 = ggplot(data = data, aes(x = mora, y = Plan_de_datos)) +
      geom_point() +
      labs(title = "Gráfico 33", 
           x = "mora", 
           y = "Plan_de_datos")+
      theme_calc()+scale_fill_calc()
    
    gg34 = ggplot(data = data, aes(x = minutos, y = Edad)) +
      geom_point() +
      labs(title = "Gráfico 34", 
           x = "minutos", 
           y = "Edad")+
      theme_calc()+scale_fill_calc()
    
    gg35 = ggplot(data = data, aes(x = minutos, y = Antigüedad_contrato_años)) +
      geom_point() +
      labs(title = "Gráfico 35", 
           x = "minutos", 
           y = "Antigüedad_contrato_años")+
      theme_calc()+scale_fill_calc()
    
    gg36 = ggplot(data = data, aes(x = minutos, y = Antigüedad_Equipo)) +
      geom_point() +
      labs(title = "Gráfico 36", 
           x = "minutos", 
           y = "Antigüedad_Equipo")+
      theme_calc()+scale_fill_calc()
    
    gg37 = ggplot(data = data, aes(x = minutos, y = Plan_de_datos)) +
      geom_point() +
      labs(title = "Gráfico 37", 
           x = "minutos", 
           y = "Plan_de_datos")+
      theme_calc()+scale_fill_calc()
    
    gg38 = ggplot(data = data, aes(x = Edad, y = Antigüedad_contrato_años)) +
      geom_point() +
      labs(title = "Gráfico 38", 
           x = "Edad", 
           y = "Antigüedad_contrato_años")+
      theme_calc()+scale_fill_calc()
    
    gg39 = ggplot(data = data, aes(x = Edad, y = Antigüedad_Equipo)) +
      geom_point() +
      labs(title = "Gráfico 39", 
           x = "Edad", 
           y = "Antigüedad_Equipo")+
      theme_calc()+scale_fill_calc()
    
    gg40 = ggplot(data = data, aes(x = Edad, y = Plan_de_datos)) +
      geom_point() +
      labs(title = "Gráfico 40", 
           x = "Edad", 
           y = "Plan_de_datos")+
      theme_calc()+scale_fill_calc()
    
    gg41 = ggplot(data = data, aes(x = Antigüedad_contrato_años, y = Antigüedad_Equipo)) +
      geom_point() +
      labs(title = "Gráfico 41", 
           x = "Antigüedad_contrato_años", 
           y = "Antigüedad_Equipo")+
      theme_calc()+scale_fill_calc()
    
    gg42 = ggplot(data = data, aes(x = Antigüedad_contrato_años, y = Plan_de_datos)) +
      geom_point() +
      labs(title = "Gráfico 42", 
           x = "Antigüedad_contrato_años", 
           y = "Plan_de_datos")+
      theme_calc()+scale_fill_calc()
    
    ### Combinación de todos los plots y exportación a pdf----
    ggplot_list <- list( gg1, gg2, gg3, gg4, gg5, gg6, gg7, gg8, gg9, gg10,
                         gg11, gg12, gg13, gg14, gg15, gg16, gg17, gg18, gg19, gg20,
                         gg21, gg22, gg23, gg24, gg25, gg26, gg27, gg28, gg29, gg30,
                         gg31, gg32, gg33, gg34, gg35, gg36, gg37, gg38, gg39, gg40,
                         gg41, gg42)
    
    arrange_plots <- function(plots) {
      do.call("grid.arrange", c(plots, ncol = 3))
    }
    
    page1 <- arrange_plots(ggplot_list[1:6])
    page2 <- arrange_plots(ggplot_list[7:12])
    page3 <- arrange_plots(ggplot_list[13:18])
    page4 <- arrange_plots(ggplot_list[19:24])
    page5 <- arrange_plots(ggplot_list[25:30])
    page6 <- arrange_plots(ggplot_list[31:36])
    page7 <- arrange_plots(ggplot_list[37:42])
    
    pdf("arranged_plots.pdf", width = 12, height = 9)

    # Print each page to the pdf file
    grid.newpage()
    grid.draw(page1)
    
    grid.newpage()
    grid.draw(page2)
    
    grid.newpage()
    grid.draw(page3)
    
    grid.newpage()
    grid.draw(page4)
    
    grid.newpage()
    grid.draw(page5)
    
    grid.newpage()
    grid.draw(page6)
    
    grid.newpage()
    grid.draw(page7)
    
    # Close the pdf device
    dev.off()
    
    
    
    
# 6. Transformación de datos------
  ## 6.1. Creación Variables dummy hombres, mujeres y empresas con base en tipo de cliente -----
    data$hombres=ifelse(data$tipo_cliente==1,1,0)
    data$mujeres=ifelse(data$tipo_cliente==2,1,0)
    data$empresas=ifelse(data$tipo_cliente==3,1,0)
    
    data_evaluacion$hombres=ifelse(data_evaluacion$tipo_cliente==1,1,0)
    data_evaluacion$mujeres=ifelse(data_evaluacion$tipo_cliente==2,1,0)
    data_evaluacion$empresas=ifelse(data_evaluacion$tipo_cliente==3,1,0)
    
    data <- data[, -1]
    # data_evaluacion <- data[data$facturación >= 0, ]
    
  ## 6.2. Determinar variables categoricas como factores -----
    data$tipo_cliente=as.factor(data$tipo_cliente)
    data_evaluacion$tipo_cliente=as.factor(data_evaluacion$tipo_cliente)
    
    # data$resultado = as.factor(data$resultado)

 ## 6.3. Balanceo con Undersampling ------
  ### I. Dataset test -----
    # Separación de resultado=0 y resultado=1
    data_cero=subset(data,data$resultado==0)
    data_uno=subset(data,data$resultado==1)
    
    # Creación de Semilla
    semilla = 611101
    set.seed(semilla) #Se deja la semilla para que el muestreo sea replicable
    
    # Creación dataset test (20% de ceros)
    sample = sample.int(nrow(data_cero), round(.2*nrow(data_cero)))
    data_cero_test = data_cero[sample, ]
    data_cero_rest = data_cero[-sample, ]

    # Creación dataset test (20% de unos)
    sample = sample.int(nrow(data_uno), round(.2*nrow(data_uno)))
    data_uno_test = data_uno[sample, ]
    data_uno_rest = data_uno[-sample, ]
    
    # Fundo las dos
    data_test = rbind(data_cero_test,data_uno_test)
    nrow(data_test)

  ### II. Dataset Train y Validation -----
    # Creación dataset train (50% de unos)
    sample = sample.int(nrow(data_uno_rest), round(.5*nrow(data_uno_rest)))
    data_uno_train = data_uno_rest[sample, ]
    data_uno_valid = data_uno_rest[-sample, ]
    
    # Pongo la misma cantidad de ceros y unos en entrenamiento:
    sample = sample.int(nrow(data_cero_rest), nrow(data_uno_train))
    data_cero_train = data_cero_rest[sample, ]
    data_cero_valid = data_cero_rest[-sample, ]
    
    # Creo entrenamiento y validación
    data_train = rbind(data_cero_train,data_uno_train)
    data_valid = rbind(data_cero_valid,data_uno_valid)
    
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
      data_train = data_train[,-c(1,2,8)]
      data_valid = data_valid[,-c(1,2,8)]
      data_test  = data_test[,-c(1,2,8)]

# 7. Modeling de Regresiones Logaritmica-----
  # Comparación de Modelos
  rows = 40
  COMP_table = data.frame(Iteración = rep(NA, rows), 
                          Modelo = rep(NA, rows),
                          punto_corte= rep(NA, rows),
                          AIC = rep(NA, rows),
                          Presicion_train = rep(NA, rows),
                          Exhaustividad_train = rep(NA, rows),
                          F_Score_train = rep(NA,rows),
                          AUC_train= rep(NA,rows),
                          Presicion_valid = rep(NA, rows),
                          Exhaustividad_valid  = rep(NA, rows),
                          F_Score_valid  = rep(NA,rows),
                          AUC_valid  = rep(NA,rows))

 ## 7.1. Regresión con todas las variables y ninguna transformación -------
  N=1
  ### I. Regresión Logaritmica -------
    # Por si solos los coeficientos solamente dicen la dirección del impacto
    # El coeficiente e a la -beta es el odd ratio
      modelo1 =glm(resultado~.,family=binomial,data_train)
      summary(modelo1)
      
      COMP_table$Iteración[N]=N
      COMP_table$Modelo[N] = c("β + Factura_online + Antigüedad_Equipo + Plan_de_datos + facturación + mora + minutos + Edad + Antiguedad_contrato_años + hombres + mujeres")

  ### II. Criterio de Akaike -----
  AIC(modelo1)
  COMP_table$AIC[N]=AIC(modelo1)

  ### III. Punto de corte -----
  punto_corte = 0.5
  COMP_table$punto_corte[N] = punto_corte
  
    
  ### IV. Prediccion con TRAIN ----
   # Tabla de confusión  
    pronostico1_train = ifelse(modelo1$fitted.values > punto_corte,1,0)
    conf_train = confusionMatrix(as.factor(pronostico1_train), 
                                    as.factor(data_train$resultado), 
                                   positive = "1")
    conf_train$table
    conf_train$byClass
    
    COMP_table$Presicion_train[N] = conf_train$byClass[5]
    COMP_table$Exhaustividad_train[N] = conf_train$byClass[6]
    COMP_table$F_Score_train[N] = conf_train$byClass[7]
    
    
   # Creacion Curva ROC
    pr_train1 = prediction(pronostico1_train,data_train$resultado)
    curvaROC_train1 = performance(pr_train1,measure="tpr",x.measure="fpr")
    plot(curvaROC_train1)
    
   # Calcular el AUC
    auc_train = performance(pr_train1,measure = "auc")
    auc_train = auc_train@y.values[[1]]
    COMP_table$AUC_train[N] = auc_train
    auc_train

  ### V. Prediccion con TEST -------
    probabilidad1_valid = predict(modelo1,newdata = data_test,type='response')
    predicciones1_valid = ifelse(probabilidad1_valid > punto_corte,1,0)
    conf_valid =confusionMatrix(as.factor(predicciones1_valid),
                               as.factor(data_test$resultado), 
                               positive = "1")
    conf_valid$table
    conf_valid$byClass
    
    COMP_table$Presicion_valid[N] = conf_valid$byClass[5]
    COMP_table$Exhaustividad_valid[N] = conf_valid$byClass[6]
    COMP_table$F_Score_valid[N] = conf_valid$byClass[7]
  
    # Creacion Curva ROC
    pr_valid1=prediction(probabilidad1_valid,data_test$resultado)
    curvaROC_valid1=performance(pr_valid1,measure="tpr",x.measure="fpr")
    plot(curvaROC_valid1)
    
    # Calcular el AUC
    auc_valid =performance(pr_valid1,measure = "auc")
    auc_valid = auc_valid@y.values[[1]]
    COMP_table$AUC_valid[N] = auc_valid
    auc_valid

  ### VI.Evaluación/Solución -------
    evaluacion1=predict(modelo1,newdata = data_evaluacion,type='response')
    #predicciones_sol1=ifelse(evaluacion1 > punto_corte,1,0)
    solucion1 = cbind(id = data_evaluacion$id,
                      resultado = evaluacion1)
    write.csv(solucion1, "solucion1_prob.csv", row.names = FALSE)

    
 ## 7.2. Stepwise Regresión con todas las variables y ninguna transformación ------
    N=2
  ### I. Regresión Logaritmica -------
    # Por si solos los coeficientos solamente dicen la dirección del impacto
    # El coeficiente e a la -beta es el odd ratio
    modelo2 = step(modelo1, direction="both")
    summary(modelo2)

    COMP_table$Iteración[N]=N
    COMP_table$Modelo[N] = c("β + facturación + mora + Edad + Antiguedad_contrato_años + hombres + mujeres")
    
  ### II. Criterio de Akaike -----
    AIC(modelo2)
    COMP_table$AIC[N]=AIC(modelo2)
    
  ### III. Punto de corte -----
    punto_corte = 0.5
    COMP_table$punto_corte[N] = punto_corte
    
    
  ### IV. Prediccion con TRAIN ----
    # Tabla de confusión  
    pronostico2_train = ifelse(modelo2$fitted.values > punto_corte,1,0)
    conf_train = confusionMatrix(as.factor(pronostico2_train), 
                                 as.factor(data_train$resultado), 
                                 positive = "1")
    conf_train$table
    conf_train$byClass
    
    COMP_table$Presicion_train[N] = conf_train$byClass[5]
    COMP_table$Exhaustividad_train[N] = conf_train$byClass[6]
    COMP_table$F_Score_train[N] = conf_train$byClass[7]
    
    # Creacion Curva ROC
    pr_train = prediction(pronostico2_train,data_train$resultado)
    curvaROC_train = performance(pr_train,measure="tpr",x.measure="fpr")
    plot(curvaROC_train)
    
    # Calcular el AUC
    auc_train = performance(pr_train,measure = "auc")
    auc_train = auc_train@y.values[[1]]
    COMP_table$AUC_train[N] = auc_train
    auc_train
    
  ### V. Prediccion con TEST -------
    probabilidad1_valid = predict(modelo2,newdata = data_test,type='response')
    predicciones1_valid = ifelse(probabilidad1_valid > punto_corte,1,0)
    conf_valid =confusionMatrix(as.factor(predicciones1_valid),
                                as.factor(data_test$resultado), 
                                positive = "1")
    conf_valid$table
    conf_valid$byClass
    
    COMP_table$Presicion_valid[N] = conf_valid$byClass[5]
    COMP_table$Exhaustividad_valid[N] = conf_valid$byClass[6]
    COMP_table$F_Score_valid[N] = conf_valid$byClass[7]
    
    # Creacion Curva ROC
    pr_valid=prediction(probabilidad1_valid,data_test$resultado)
    curvaROC_valid=performance(pr_valid,measure="tpr",x.measure="fpr")
    plot(curvaROC_valid)
    
    # Calcular el AUC
    auc_valid =performance(pr_valid,measure = "auc")
    auc_valid = auc_valid@y.values[[1]]
    COMP_table$AUC_valid[N] = auc_valid
    auc_valid
    
  ### VI.Evaluación/Solución -------
    evaluacion2=predict(modelo2,newdata = data_evaluacion,type='response')
    #predicciones_sol2=ifelse(evaluacion2 > punto_corte,1,0)
    solucion2 = cbind(id = data_evaluacion$id,
                      resultado = evaluacion2)
    write.csv(solucion2, "solucion2_prob.csv", row.names = FALSE)
    
  ### Best Subsets Regresión con todas las variables y ninguna transformación ------
    # Por si solos los coeficientos solamente dicen la dirección del impacto
    # El coeficiente e a la -beta es el odd ratio
  
    data_test_y = data_test[,c(8)]
    data_test_x = data_test[,-c(8)]
    Xy = as.data.frame(cbind(data_test_x,data_test_y))
    
    library("bestglm")
    modelo3 = bestglm(family=binomial, IC = "AIC", method = "exhaustive",
                      Xy = Xy)
    
    summary(modelo3)
    best_subset <- modelo3$BestModel
    best_subset$coefficients
    
    #---- NOTA: Con best Subsets da lo mismo que con stepwise!!!! ----#
    
 ## 7.3. Aplicando KNN -----  
    # Balanceo con AUC
    levels(data_train$resultado) <- make.names(levels(factor(data_train$resultado)))
    
    # creo parámetros de validación cruzada
    cross<-trainControl(method="cv",number=5,
                        classProbs = TRUE,
                        summaryFunction =prSummary)
    
    modeloknn2<-train(resultado ~.,method="knn",
                      tuneGrid=expand.grid(k=1:50),
                      trControl=cross, 
                      metric="AUC",
                      data= data_train)
    modeloknn2
    plot(modeloknn2)    
    
    # el modelo sintonizado
    predmod2<-predict(modeloknn2,data_test,type="prob")
    pronknn2<-ifelse(predmod2$X1 > 0.5,1,0)
    confknn2<-confusionMatrix(as.factor(pronknn2),
                              data_test$resultado, positive = "1")
    confknn2$table
    confknn2$byClass
    
    #crear objeto de predicciones
    pr2<-prediction(pronknn2,data_test$resultado)
    #creacion del objeto de la curva
    curvaROC2<-performance(pr2,measure="tpr",x.measure="fpr")
    #grafico de la curva
    plot(curvaROC2)
    #calcular el AUC
    auc2<-performance(pr2,measure = "auc")
    auc2 <- auc2@y.values[[1]]
    #ver el AUC
    auc2
    
    ### VI.Evaluación/Solución -------
    evaluacion3=predict(modeloknn2,newdata = data_evaluacion,type='prob')
    predicciones_sol3=ifelse(evaluacion3 > punto_corte,1,0)
    solucion3 = cbind(id = data_evaluacion$id,
                      resultado = predicciones_sol3)
    solucion3 = as.data.frame(solucion3)
    solucion3 = solucion3[,-2]
    colnames(solucion3)[2] ="resultado"
    write.csv(solucion3, "solucion3.csv", row.names = FALSE)
    
    
 ## 7.4. Transformación de variables numericas con stepwise -------
    #### Transformaciones al cuadrado  -----
    data_train$Antigüedad_Equipo_sqr = (data_train$Antigüedad_Equipo^2)
    data_valid$Antigüedad_Equipo_sqr = (data_valid$Antigüedad_Equipo^2)
    data_test$Antigüedad_Equipo_sqr = (data_test$Antigüedad_Equipo^2)
    data_evaluacion$Antigüedad_Equipo_sqr = (data_evaluacion$Antigüedad_Equipo^2)
    
    data_train$facturación_sqr = (data_train$facturación^2)
    data_valid$facturación_sqr = (data_valid$facturación^2)
    data_test$facturación_sqr = (data_test$facturación^2)
    data_evaluacion$facturación_sqr = (data_evaluacion$facturación^2)
    
    data_train$mora_sqr = (data_train$mora^2)
    data_valid$mora_sqr = (data_valid$mora^2)
    data_test$mora_sqr = (data_test$mora^2)
    data_evaluacion$mora_sqr = (data_evaluacion$mora^2)
    
    data_train$minutos_sqr = (data_train$minutos^2)
    data_valid$minutos_sqr = (data_valid$minutos^2)
    data_test$minutos_sqr = (data_test$minutos^2)
    data_evaluacion$minutos_sqr = (data_evaluacion$minutos^2)
    
    data_train$Edad_sqr = (data_train$Edad^2)
    data_valid$Edad_sqr = (data_valid$Edad^2)
    data_test$Edad_sqr = (data_test$Edad^2)
    data_evaluacion$Edad_sqr = (data_evaluacion$Edad^2)
    
    data_train$Antigüedad_contrato_años_sqr = (data_train$Antigüedad_contrato_años^2)
    data_valid$Antigüedad_contrato_años_sqr = (data_valid$Antigüedad_contrato_años^2)
    data_test$Antigüedad_contrato_años_sqr = (data_test$Antigüedad_contrato_años^2)
    data_evaluacion$Antigüedad_contrato_años_sqr = (data_evaluacion$Antigüedad_contrato_años^2)
    
    #### Transformaciones sacando logaritmo natural -----
    data_train$Antigüedad_Equipo_ln = log(data_train$Antigüedad_Equipo)
    data_valid$Antigüedad_Equipo_ln = log(data_valid$Antigüedad_Equipo)
    data_test$Antigüedad_Equipo_ln = log(data_test$Antigüedad_Equipo)
    data_evaluacion$Antigüedad_Equipo_ln = log(data_evaluacion$Antigüedad_Equipo)
    
    # data_train$facturación_ln = log(data_train$facturación)
    # data_valid$facturación_ln = log(data_valid$facturación)
    # data_test$facturación_ln = log(data_test$facturación)
    
    # data_train$mora_ln = log(data_train$mora)
    # data_valid$mora_ln = log(data_valid$mora)
    # data_test$mora_ln = log(data_test$mora)
    
    data_train$minutos_ln = log(data_train$minutos)
    data_valid$minutos_ln = log(data_valid$minutos)
    data_test$minutos_ln = log(data_test$minutos)
    data_evaluacion$minutos_ln = log(data_evaluacion$minutos)
    
    data_train$Edad_ln = log(data_train$Edad)
    data_valid$Edad_ln = log(data_valid$Edad)
    data_test$Edad_ln = log(data_test$Edad)
    data_evaluacion$Edad_ln = log(data_evaluacion$Edad)
    
    # data_train$Antigüedad_contrato_años_ln = log(data_train$Antigüedad_contrato_años)
    # data_valid$Antigüedad_contrato_años_ln = log(data_valid$Antigüedad_contrato_años)
    # data_test$Antigüedad_contrato_años_ln = log(data_test$Antigüedad_contrato_años)
  
    #### Transformaciones sacando inversal -----
    data_train$Antigüedad_Equipo_inversa = 1/(data_train$Antigüedad_Equipo)
    data_valid$Antigüedad_Equipo_inversa = 1/(data_valid$Antigüedad_Equipo)
    data_test$Antigüedad_Equipo_inversa = 1/(data_test$Antigüedad_Equipo)
    data_evaluacion$Antigüedad_Equipo_inversa = 1/(data_evaluacion$Antigüedad_Equipo)
    
    data_train$facturación_inversa = 1/(data_train$facturación)
    data_valid$facturación_inversa = 1/(data_valid$facturación)
    data_test$facturación_inversa = 1/(data_test$facturación)
    data_evaluacion$facturación_inversa = 1/(data_evaluacion$facturación)
    
    # data_train$mora_inversa = 1/(data_train$mora)
    # data_valid$mora_inversa = 1/(data_valid$mora)
    # data_test$mora_inversa = 1/(data_test$mora)
    
    data_train$minutos_inversa = 1/(data_train$minutos)
    data_valid$minutos_inversa = 1/(data_valid$minutos)
    data_test$minutos_inversa = 1/(data_test$minutos)
    data_evaluacion$minutos_inversa = 1/(data_evaluacion$minutos)
    
    data_train$Edad_inversa = 1/(data_train$Edad)
    data_valid$Edad_inversa = 1/(data_valid$Edad)
    data_test$Edad_inversa = 1/(data_test$Edad)
    data_evaluacion$Edad_inversa = 1/(data_evaluacion$Edad)
    
    data_train$Antigüedad_contrato_años_inversa = 1/(data_train$Antigüedad_contrato_años)
    data_valid$Antigüedad_contrato_años_inversa = 1/(data_valid$Antigüedad_contrato_años)
    data_test$Antigüedad_contrato_años_inversa = 1/(data_test$Antigüedad_contrato_años)
    data_evaluacion$Antigüedad_contrato_años_inversa = 1/(data_evaluacion$Antigüedad_contrato_años)
    
    ### I. Regresión Logaritmica -------
    N=4
    
    # Por si solos los coeficientos solamente dicen la dirección del impacto
    # El coeficiente e a la -beta es el odd ratio
    modelo4 =glm(resultado~.,family=binomial,data_train)
    summary(modelo4)
    
    modelo4 = step(modelo4, direction="both")
    summary(modelo4)
    
    COMP_table$Iteración[N]=N
    COMP_table$Modelo[N] = c("β + facturación + mora + Edad + Antiguedad_contrato_años + hombres + muejeres + mora_sqr + Antigüedad_contrato_años_sqr + Edad_ln")
    
    ### II. Criterio de Akaike -----
    AIC(modelo4)
    COMP_table$AIC[N]=AIC(modelo4)
    
    ### III. Punto de corte -----
    punto_corte = 0.5
    COMP_table$punto_corte[N] = punto_corte
    
    ### IV. Prediccion con TRAIN ----
    # Tabla de confusión  
    pronostico_train = ifelse(modelo4$fitted.values > punto_corte,1,0)
    conf_train = confusionMatrix(as.factor(pronostico_train), 
                                 as.factor(data_train$resultado), 
                                 positive = "1")
    conf_train$table
    conf_train$byClass
    
    COMP_table$Presicion_train[N] = conf_train$byClass[5]
    COMP_table$Exhaustividad_train[N] = conf_train$byClass[6]
    COMP_table$F_Score_train[N] = conf_train$byClass[7]
    
    # Creacion Curva ROC
    pr_train = prediction(pronostico_train,data_train$resultado)
    curvaROC_train = performance(pr_train,measure="tpr",x.measure="fpr")
    plot(curvaROC_train)
    
    # Calcular el AUC
    auc_train = performance(pr_train,measure = "auc")
    auc_train = auc_train@y.values[[1]]
    COMP_table$AUC_train[N] = auc_train
    auc_train
    
    ### V. Prediccion con TEST -------
    probabilidad1_valid = predict(modelo4,newdata = data_test,type='response')
    predicciones1_valid = ifelse(probabilidad1_valid > punto_corte,1,0)
    conf_valid =confusionMatrix(as.factor(predicciones1_valid),
                                as.factor(data_test$resultado), 
                                positive = "1")
    conf_valid$table
    conf_valid$byClass
    
    COMP_table$Presicion_valid[N] = conf_valid$byClass[5]
    COMP_table$Exhaustividad_valid[N] = conf_valid$byClass[6]
    COMP_table$F_Score_valid[N] = conf_valid$byClass[7]
    
    # Creacion Curva ROC
    pr_valid=prediction(probabilidad1_valid,data_test$resultado)
    curvaROC_valid=performance(pr_valid,measure="tpr",x.measure="fpr")
    plot(curvaROC_valid)
    
    # Calcular el AUC
    auc_valid =performance(pr_valid,measure = "auc")
    auc_valid = auc_valid@y.values[[1]]
    COMP_table$AUC_valid[N] = auc_valid
    auc_valid
    
    ### VI.Evaluación/Solución -------
    evaluacion4=predict(modelo4,newdata = data_evaluacion,type='response')
    #predicciones_sol4=ifelse(evaluacion4 > punto_corte,1,0)
    solucion4 = cbind(id = data_evaluacion$id,
                      resultado = evaluacion4)
    write.csv(solucion4, "solucion4_prob.csv", row.names = FALSE)
    
 ## 7.5. Interacciones entre variables: mora/minutos,facturación,edad-----
    
    data_train$mora_minutos = data_train$mora/data_train$minutos
    data_valid$mora_minutos = data_valid$mora/data_valid$minutos
    data_test$mora_minutos = data_test$mora/data_test$minutos
    data_evaluacion$mora_minutos = data_evaluacion$mora/ data_evaluacion$minutos
    
    data_train$mora_facturacion = data_train$mora/data_train$facturación
    data_valid$mora_facturacion = data_valid$mora/data_valid$facturación
    data_test$mora_facturacion = data_test$mora/data_test$facturación
    data_evaluacion$mora_facturacion = data_evaluacion$mora/data_evaluacion$facturación
    
    data_train$mora_edad = data_train$mora/data_train$Edad
    data_valid$mora_edad = data_valid$mora/data_valid$Edad
    data_test$mora_edad = data_test$mora/data_test$Edad
    data_evaluacion$mora_edad = data_evaluacion$mora/data_evaluacion$Edad
    
    ### I. Regresión Logaritmica -------
    N=5
    
    # Por si solos los coeficientos solamente dicen la dirección del impacto
    # El coeficiente e a la -beta es el odd ratio
    modelo5 =glm(resultado~.,family=binomial,data_train)
    summary(modelo5)
    
    modelo5 = step(modelo5, direction="both")
    summary(modelo5)
    
    COMP_table$Iteración[N]=N
    COMP_table$Modelo[N] = c("β + facturación + mora + Edad + Antiguedad_contrato_años + hombres + muejres + Antigüedad_contrato_años_sqr + Edad_ln + mora_edad")
    
    ### II. Criterio de Akaike -----
    AIC(modelo5)
    COMP_table$AIC[N]=AIC(modelo4)
    
    ### III. Punto de corte -----
    punto_corte = 0.5
    COMP_table$punto_corte[N] = punto_corte
    
    ### IV. Prediccion con TRAIN ----
    # Tabla de confusión  
    pronostico_train = ifelse(modelo5$fitted.values > punto_corte,1,0)
    conf_train = confusionMatrix(as.factor(pronostico_train), 
                                 as.factor(data_train$resultado), 
                                 positive = "1")
    conf_train$table
    conf_train$byClass
    
    COMP_table$Presicion_train[N] = conf_train$byClass[5]
    COMP_table$Exhaustividad_train[N] = conf_train$byClass[6]
    COMP_table$F_Score_train[N] = conf_train$byClass[7]
    
    # Creacion Curva ROC
    pr_train = prediction(pronostico_train,data_train$resultado)
    curvaROC_train = performance(pr_train,measure="tpr",x.measure="fpr")
    plot(curvaROC_train)
    
    # Calcular el AUC
    auc_train = performance(pr_train,measure = "auc")
    auc_train = auc_train@y.values[[1]]
    COMP_table$AUC_train[N] = auc_train
    auc_train
    
    ### V. Prediccion con TEST -------
    probabilidad1_valid = predict(modelo5,newdata = data_test,type='response')
    predicciones1_valid = ifelse(probabilidad1_valid > punto_corte,1,0)
    conf_valid =confusionMatrix(as.factor(predicciones1_valid),
                                as.factor(data_test$resultado), 
                                positive = "1")
    conf_valid$table
    conf_valid$byClass
    
    COMP_table$Presicion_valid[N] = conf_valid$byClass[5]
    COMP_table$Exhaustividad_valid[N] = conf_valid$byClass[6]
    COMP_table$F_Score_valid[N] = conf_valid$byClass[7]
    
    # Creacion Curva ROC
    pr_valid=prediction(probabilidad1_valid,data_test$resultado)
    curvaROC_valid=performance(pr_valid,measure="tpr",x.measure="fpr")
    plot(curvaROC_valid)
    
    # Calcular el AUC
    auc_valid =performance(pr_valid,measure = "auc")
    auc_valid = auc_valid@y.values[[1]]
    COMP_table$AUC_valid[N] = auc_valid
    auc_valid
    
    ### VI.Evaluación/Solución -------
    evaluacion5=predict(modelo5,newdata = data_evaluacion,type='response')
    #predicciones_sol5=ifelse(evaluacion5 > punto_corte,1,0)
    solucion5 = cbind(id = data_evaluacion$id,
                      resultado = evaluacion5)
    write.csv(solucion5, "solucion5_prob.csv", row.names = FALSE)
    
    ### Best Subsets Regresión con todas las variables y ninguna transformación ------
    # Por si solos los coeficientos solamente dicen la dirección del impacto
    # El coeficiente e a la -beta es el odd ratio
  
    data_test_y = data_test[,c(9)]
    data_test_x = data_test[,-c(1,2,9,15,16,17,18,19)] # BestGLM solo permite 15 variables por lo que toca eliminar varias
    Xy = as.data.frame(cbind(data_test_x,data_test_y))
    
    #install.packages("bestglm")
    library("bestglm")
    
    modelo6 = bestglm(family=binomial, IC = "AIC", method = "exhaustive",
                      Xy = Xy)
    summary(modelo6)
    best_subset <- modelo6$BestModel
    best_subset$coefficients
    
    #---- NOTA: Con best Subsets da lo mismo que con stepwise!!!! ----#

 ## 7.6.Creación de grupos de clientes "Comunidad" ------
    
    # Creación variable Comunidad Representada por niveles según el porcentaje de relación
    # del cliente con todos sus contactos, calculado en función
    # a llamadas, ejemplo: grado 1 (0-25%), grado 2
    # (26-50%), grado 3 (51-75%), grado 4 (76-100%)
    
    quartiles <- quantile(data$minutos, probs = c(0.25, 0.5, 0.75))
    
    Q1 <- quartiles[1] # First quartile (25th percentile)
    Q2 <- quartiles[2] # Second quartile (median, 50th percentile)
    Q3 <- quartiles[3] # Third quartile (75th percentile)
    
    data_train$comunidad_baja       = ifelse(data_train$minutos<= Q1,1,0)
    data_train$comunidad_media      = ifelse(data_train$minutos>Q1 & data_train$minutos<= Q2,1,0)
    data_train$comunidad_media_alta = ifelse(data_train$minutos>Q2 & data_train$minutos<= Q3,1,0)
    data_train$comunidad_alta       = ifelse(data_train$minutos> Q3,1,0)
    
    data_valid$comunidad_baja       = ifelse(data_valid$minutos<= Q1,1,0)
    data_valid$comunidad_media      = ifelse(data_valid$minutos>Q1 & data_valid$minutos<= Q2,1,0)
    data_valid$comunidad_media_alta = ifelse(data_valid$minutos>Q2 & data_valid$minutos<= Q3,1,0)
    data_valid$comunidad_alta       = ifelse(data_valid$minutos> Q3,1,0)
    
    data_test$comunidad_baja       = ifelse(data_test$minutos<= Q1,1,0)
    data_test$comunidad_media      = ifelse(data_test$minutos>Q1 & data_test$minutos<= Q2,1,0)
    data_test$comunidad_media_alta = ifelse(data_test$minutos>Q2 & data_test$minutos<= Q3,1,0)
    data_test$comunidad_alta       = ifelse(data_test$minutos> Q3,1,0)
    
    data_evaluacion$comunidad_baja       = ifelse(data_evaluacion$minutos<= Q1,1,0)
    data_evaluacion$comunidad_media      = ifelse(data_evaluacion$minutos>Q1 & data_evaluacion$minutos<= Q2,1,0)
    data_evaluacion$comunidad_media_alta = ifelse(data_evaluacion$minutos>Q2 & data_evaluacion$minutos<= Q3,1,0)
    data_evaluacion$comunidad_alta       = ifelse(data_evaluacion$minutos> Q3,1,0) 
    
    ### I. Regresión Logaritmica -------
    N=6
    
    # Por si solos los coeficientos solamente dicen la dirección del impacto
    # El coeficiente e a la -beta es el odd ratio
    modelo6 =glm(resultado~.,family=binomial,data_train)
    summary(modelo6)
    
    COMP_table$Iteración[N]=N
    COMP_table$Modelo[N] = c("β + Factura_online + Antigüedad_Equipo + Plan_de_datos + facturación + mora + minutos + Edad + Antigüedad_contrato_años + hombres + mujeres + empresas + Antigüedad_Equipo_sqr + facturación_sqr + mora_sqr + minutos_sqr + Edad_sqr + Antigüedad_contrato_años_sqr + Antigüedad_Equipo_ln + minutos_ln + Edad_ln + Antigüedad_Equipo_inversa + facturación_inversa + minutos_inversa + Edad_inversa + Antigüedad_contrato_años_inversa + mora_minutos + mora_facturacion + mora_edad + comunidad_baja + comunidad_media + comunidad_media_alta")
    
    ### II. Criterio de Akaike -----
    AIC(modelo6)
    COMP_table$AIC[N]=AIC(modelo6)
    
    ### III. Punto de corte -----
    punto_corte = 0.5
    COMP_table$punto_corte[N] = punto_corte
    
    ### IV. Prediccion con TRAIN ----
    # Tabla de confusión  
    pronostico_train = ifelse(modelo6$fitted.values > punto_corte,1,0)
    conf_train = confusionMatrix(as.factor(pronostico_train), 
                                 as.factor(data_train$resultado), 
                                 positive = "1")
    conf_train$table
    conf_train$byClass
    
    COMP_table$Presicion_train[N] = conf_train$byClass[5]
    COMP_table$Exhaustividad_train[N] = conf_train$byClass[6]
    COMP_table$F_Score_train[N] = conf_train$byClass[7]
    
    # Creacion Curva ROC
    pr_train = prediction(pronostico_train,data_train$resultado)
    curvaROC_train = performance(pr_train,measure="tpr",x.measure="fpr")
    plot(curvaROC_train)
    
    # Calcular el AUC
    auc_train = performance(pr_train,measure = "auc")
    auc_train = auc_train@y.values[[1]]
    COMP_table$AUC_train[N] = auc_train
    auc_train
    
    ### V. Prediccion con TEST -------
    probabilidad1_valid = predict(modelo6,newdata = data_test,type='response')
    predicciones1_valid = ifelse(probabilidad1_valid > punto_corte,1,0)
    conf_valid =confusionMatrix(as.factor(predicciones1_valid),
                                as.factor(data_test$resultado), 
                                positive = "1")
    conf_valid$table
    conf_valid$byClass
    
    COMP_table$Presicion_valid[N] = conf_valid$byClass[5]
    COMP_table$Exhaustividad_valid[N] = conf_valid$byClass[6]
    COMP_table$F_Score_valid[N] = conf_valid$byClass[7]
    
    # Creacion Curva ROC
    pr_valid=prediction(probabilidad1_valid,data_test$resultado)
    curvaROC_valid=performance(pr_valid,measure="tpr",x.measure="fpr")
    plot(curvaROC_valid)
    
    # Calcular el AUC
    auc_valid =performance(pr_valid,measure = "auc")
    auc_valid = auc_valid@y.values[[1]]
    COMP_table$AUC_valid[N] = auc_valid
    auc_valid
    
    ### VI.Evaluación/Solución -------
    evaluacion6=predict(modelo6,newdata = data_evaluacion,type='response')
    #predicciones_sol6=ifelse(evaluacion6 > punto_corte,1,0)
    solucion6 = cbind(id = data_evaluacion$id,
                      resultado = evaluacion6)
    write.csv(solucion6, "solucion6_prob.csv", row.names = FALSE)

 ## 7.7. Stepwise GLM del Modelo 6 ----
    N=7
  ### I. Compute the stepwise logistic regression ----
    modelo7 <- glm(resultado ~  Plan_de_datos + mora + Antigüedad_contrato_años + hombres + mujeres + facturación_sqr + Edad_sqr + Antigüedad_contrato_años_sqr + mora_minutos + comunidad_alta, data = data_train, family = binomial)
    summary(modelo7)
    
    COMP_table$Iteración[N]=N
    COMP_table$Modelo[N] = c("β + Plan_de_datos + mora + Antigüedad_contrato_años + hombres + mujeres + facturación_sqr + Edad_sqr + Antigüedad_contrato_años_sqr + mora_minutos + comunidad_alta")
    
    ### II. Criterio de Akaike -----
    AIC(modelo7)
    COMP_table$AIC[N]=AIC(modelo7)
    
    ### III. Punto de corte -----
    punto_corte = 0.5
    COMP_table$punto_corte[N] = punto_corte
    
    ### IV. Prediccion con TRAIN ----
    # Tabla de confusión  
    pronostico_train = ifelse(modelo7$fitted.values > punto_corte,1,0)
    conf_train = confusionMatrix(as.factor(pronostico_train), 
                                 as.factor(data_train$resultado), 
                                 positive = "1")
    conf_train$table
    conf_train$byClass
    
    COMP_table$Presicion_train[N] = conf_train$byClass[5]
    COMP_table$Exhaustividad_train[N] = conf_train$byClass[6]
    COMP_table$F_Score_train[N] = conf_train$byClass[7]
    
    # Creacion Curva ROC
    pr_train = prediction(pronostico_train,data_train$resultado)
    curvaROC_train = performance(pr_train,measure="tpr",x.measure="fpr")
    plot(curvaROC_train)
    
    # Calcular el AUC
    auc_train = performance(pr_train,measure = "auc")
    auc_train = auc_train@y.values[[1]]
    COMP_table$AUC_train[N] = auc_train
    auc_train
    
    ### V. Prediccion con TEST -------
    probabilidad1_valid = predict(modelo7,newdata = data_test,type='response')
    predicciones1_valid = ifelse(probabilidad1_valid > punto_corte,1,0)
    conf_valid =confusionMatrix(as.factor(predicciones1_valid),
                                as.factor(data_test$resultado), 
                                positive = "1")
    conf_valid$table
    conf_valid$byClass
    
    COMP_table$Presicion_valid[N] = conf_valid$byClass[5]
    COMP_table$Exhaustividad_valid[N] = conf_valid$byClass[6]
    COMP_table$F_Score_valid[N] = conf_valid$byClass[7]
    
    # Creacion Curva ROC
    pr_valid=prediction(probabilidad1_valid,data_test$resultado)
    curvaROC_valid=performance(pr_valid,measure="tpr",x.measure="fpr")
    plot(curvaROC_valid)
    
    # Calcular el AUC
    auc_valid =performance(pr_valid,measure = "auc")
    auc_valid = auc_valid@y.values[[1]]
    COMP_table$AUC_valid[N] = auc_valid
    auc_valid
    
    ### VI.Evaluación/Solución -------
    evaluacion7=predict(modelo7,newdata = data_evaluacion,type='response')
    #predicciones_sol7=ifelse(evaluacion7 > punto_corte,1,0)
    solucion7 = cbind(id = data_evaluacion$id,
                      resultado = evaluacion7)
    write.csv(solucion7, "solucion7_prob.csv", row.names = FALSE)
    
 ##7.8. Relación facturación minutos -----
    data_train$minutos_facturacion = data_train$minutos/data_train$facturación
    data_valid$minutos_facturacion = data_valid$minutos/data_valid$facturación
    data_test$minutos_facturacion = data_test$minutos/data_test$facturación
    data_evaluacion$minutos_facturacion = data_evaluacion$minutos/ data_evaluacion$facturación
    
    N=8
    ### I. Compute the stepwise logistic regression ----
    modelo8 <- glm(resultado ~ facturación + mora + Antigüedad_contrato_años + hombres + mujeres + Antigüedad_contrato_años_sqr + mora_edad + minutos_facturacion , data = data_train, family = binomial)
    summary(modelo8)
    
    
    COMP_table$Iteración[N]=N
    COMP_table$Modelo[N] = c("β + facturación + mora + Antigüedad_contrato_años + hombres + mujeres + Antigüedad_contrato_años_sqr + mora_edad + minutos_facturacion ")
    
    ### II. Criterio de Akaike -----
    AIC(modelo8)
    COMP_table$AIC[N]=AIC(modelo8)
    
    ### III. Punto de corte -----
    punto_corte = 0.5
    COMP_table$punto_corte[N] = punto_corte
    
    ### IV. Prediccion con TRAIN ----
    # Tabla de confusión  
    pronostico_train = ifelse(modelo8$fitted.values > punto_corte,1,0)
    conf_train = confusionMatrix(as.factor(pronostico_train), 
                                 as.factor(data_train$resultado), 
                                 positive = "1")
    conf_train$table
    conf_train$byClass
    
    COMP_table$Presicion_train[N] = conf_train$byClass[5]
    COMP_table$Exhaustividad_train[N] = conf_train$byClass[6]
    COMP_table$F_Score_train[N] = conf_train$byClass[7]
    
    # Creacion Curva ROC
    pr_train = prediction(pronostico_train,data_train$resultado)
    curvaROC_train = performance(pr_train,measure="tpr",x.measure="fpr")
    plot(curvaROC_train)
    
    # Calcular el AUC
    auc_train = performance(pr_train,measure = "auc")
    auc_train = auc_train@y.values[[1]]
    COMP_table$AUC_train[N] = auc_train
    auc_train
    
    ### V. Prediccion con TEST -------
    probabilidad1_valid = predict(modelo8,newdata = data_test,type='response')
    predicciones1_valid = ifelse(probabilidad1_valid > punto_corte,1,0)
    conf_valid =confusionMatrix(as.factor(predicciones1_valid),
                                as.factor(data_test$resultado), 
                                positive = "1")
    conf_valid$table
    conf_valid$byClass
    
    COMP_table$Presicion_valid[N] = conf_valid$byClass[5]
    COMP_table$Exhaustividad_valid[N] = conf_valid$byClass[6]
    COMP_table$F_Score_valid[N] = conf_valid$byClass[7]
    
    # Creacion Curva ROC
    pr_valid=prediction(probabilidad1_valid,data_test$resultado)
    curvaROC_valid=performance(pr_valid,measure="tpr",x.measure="fpr")
    plot(curvaROC_valid)
    
    # Calcular el AUC
    auc_valid =performance(pr_valid,measure = "auc")
    auc_valid = auc_valid@y.values[[1]]
    COMP_table$AUC_valid[N] = auc_valid
    auc_valid
    
    ### VI.Evaluación/Solución -------
    evaluacion8=predict(modelo8,newdata = data_evaluacion,type='response')
    #predicciones_sol8=ifelse(evaluacion8 > punto_corte,1,0)
    solucion8 = cbind(id = data_evaluacion$id,
                      resultado = evaluacion8)
    write.csv(solucion8, "solucion8_prob.csv", row.names = FALSE)

 ## 7.9. Efecto Marginal del año de nacimiento ------
    
  ## Segun este paper el efecto de el consumo (promedio) de minutos y la duración sobre 
  ## la probabilidad de abandono no es el mismo dependiendo de los valores del año de nacimiento
  ## https://www.czso.cz/documents/10180/88506448/32019719q2_129_mandak_analyses.pdf/550e60c0-149c-42ce-9bfa-b449002b10c6?version=1.0
    
    data_train$ME_Minutos_BirthYear = (data_train$Edad + 2018)*data_train$minutos
    data_valid$ME_Minutos_BirthYear = (data_valid$Edad + 2018)*data_valid$minutos
    data_test$ME_Minutos_BirthYear = (data_test$Edad + 2018)*data_test$minutos
    data_evaluacion$ME_Minutos_BirthYear = (data_evaluacion$Edad + 2018)*data_evaluacion$minutos
    
    data_train$ME_duracion_BirthYear = (data_train$Edad + 2018)*data_train$Antigüedad_contrato_años
    data_valid$ME_duracion_BirthYear = (data_valid$Edad + 2018)*data_valid$Antigüedad_contrato_años
    data_test$ME_duracion_BirthYear = (data_test$Edad + 2018)*data_test$Antigüedad_contrato_años
    data_evaluacion$ME_duracion_BirthYear = (data_evaluacion$Edad + 2018)*data_evaluacion$Antigüedad_contrato_años
    
    N=9
    ### I. Compute the stepwise logistic regression ----
    modelo9 <- stepAIC(glm(resultado ~ ., data = data_train, family = binomial), direction = "both")
    summary(modelo9)
    
    COMP_table$Iteración[N]=N
    COMP_table$Modelo[N] = c("β + facturación + mora + Edad + hombres + mujeres + Antigüedad_contrato_años_sqr + Edad_ln + mora_edad + ME_duracion_BirthYear")
    
    ### II. Criterio de Akaike -----
    AIC(modelo9)
    COMP_table$AIC[N]=AIC(modelo9)
    
    ### III. Punto de corte -----
    punto_corte = 0.5
    COMP_table$punto_corte[N] = punto_corte
    
    ### IV. Prediccion con TRAIN ----
    # Tabla de confusión  
    pronostico_train = ifelse(modelo8$fitted.values > punto_corte,1,0)
    conf_train = confusionMatrix(as.factor(pronostico_train), 
                                 as.factor(data_train$resultado), 
                                 positive = "1")
    conf_train$table
    conf_train$byClass
    
    COMP_table$Presicion_train[N] = conf_train$byClass[5]
    COMP_table$Exhaustividad_train[N] = conf_train$byClass[6]
    COMP_table$F_Score_train[N] = conf_train$byClass[7]
    
    # Creacion Curva ROC
    pr_train = prediction(pronostico_train,data_train$resultado)
    curvaROC_train = performance(pr_train,measure="tpr",x.measure="fpr")
    plot(curvaROC_train)
    
    # Calcular el AUC
    auc_train = performance(pr_train,measure = "auc")
    auc_train = auc_train@y.values[[1]]
    COMP_table$AUC_train[N] = auc_train
    auc_train
    
    ### V. Prediccion con TEST -------
    probabilidad1_valid = predict(modelo9,newdata = data_test,type='response')
    predicciones1_valid = ifelse(probabilidad1_valid > punto_corte,1,0)
    conf_valid =confusionMatrix(as.factor(predicciones1_valid),
                                as.factor(data_test$resultado), 
                                positive = "1")
    conf_valid$table
    conf_valid$byClass
    
    COMP_table$Presicion_valid[N] = conf_valid$byClass[5]
    COMP_table$Exhaustividad_valid[N] = conf_valid$byClass[6]
    COMP_table$F_Score_valid[N] = conf_valid$byClass[7]
    
    # Creacion Curva ROC
    pr_valid=prediction(probabilidad1_valid,data_test$resultado)
    curvaROC_valid=performance(pr_valid,measure="tpr",x.measure="fpr")
    plot(curvaROC_valid)
    
    # Calcular el AUC
    auc_valid =performance(pr_valid,measure = "auc")
    auc_valid = auc_valid@y.values[[1]]
    COMP_table$AUC_valid[N] = auc_valid
    auc_valid
    
    ### VI.Evaluación/Solución -------
    evaluacion9=predict(modelo9,newdata = data_evaluacion,type='response')
    #predicciones_sol9=ifelse(evaluacion8 > punto_corte,1,0)
    solucion9 = cbind(id = data_evaluacion$id,
                      resultado = evaluacion9)
    write.csv(solucion9, "solucion9_prob.csv", row.names = FALSE)

 ## 7.10. AdaBoost ----
    install.packages('rgl', type = 'source')
    Sys.setenv(RGL_USE_NULL=TRUE)
    install.packages('adabag') # for fitting the adaboost model
    library(adabag)

    #data_train$resultado = as.factor(data_train$resultado)
    model_adaboost <- boosting(resultado ~., data=data_train, boos=TRUE, mfinal=5)
    summary(model_adaboost) # optional: view the model summary
    
# 8. Random forest y modelos alternos -----
  library(rpart)
  
  # Crear el modelo de árbol de decisión
  tree_model <- rpart(resultado ~ ., data = data_train)
  
  # Realizar predicciones en el conjunto de prueba
  predictions_tree <- predict(tree_model, newdata = data_evaluacion)
  
  # Modelo de Random Forest
  rf_model <- randomForest(resultado ~ facturación+mora+Edad+Antigüedad_contrato_años+hombres+mujeres+mora_sqr+Antigüedad_contrato_años_sqr+Edad_ln, data = data_train, ntree = 100)
  rf_pred <- predict(rf_model, newdata = data_evaluacion)
  solucion_rf = cbind(id = data_evaluacion$id,
                      resultado = rf_pred)
  write.csv(solucion_rf, "solucion_rf_mafe_2.csv", row.names = FALSE)  
  
  # Modelo de XGBoost  - No sé por què me genera error 
  xgb_model <- xgboost(data = as.matrix(data_train[, -which(names(data_train) == "resultado")]), label = data_train$resultado, nrounds = 100, objective = "reg:linear")
  xgb_pred <- predict(xgb_model, newdata = as.matrix(data_evaluacion[, -which(names(data_evaluacion) == "resultado")]))
  solucion_xgb = cbind(id = data_evaluacion$id,
                       resultado = xgb_pred)
  write.csv(solucion1, "solucion_xgb.csv", row.names = FALSE)    
  
