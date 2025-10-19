import numpy as np
import pandas as pd
import MaDatos
import Create_report
import os
import matplotlib.pyplot as plt
import Model
from pycaret.classification import *
#Opciones 
pd.options.display.max_columns = None
pd.set_option('display.max_rows', None)
pd.set_option('display.max_colwidth', None)
pd.set_option('display.width', 1000)
pd.set_option('display.expand_frame_repr', False)
#Version de python necesaria 3.11.9
#Necesario tener instalado en entorno virtual  pip install openpyxl
#Procesamiento de datos
path = os.getcwd() + '\\bank_churn.xlsx'
datos = MaDatos.Manupilacion_datos(path,0,"attrition_flag")
df = datos.Cargue_archivos_Excel()
#df.info()
#Combinar categorias menos frecuentes
df = datos.CombineRareCategories(df, 10)
df['clientnum'] = df['clientnum'].astype(str) #para evitar que se tome como variable continua
#imputar datos
df = datos.imputar_datos(df, ['attrition_flag', 'clientnum'])
#Vista minable columnas transoformadas, imputadas y combinadas
df_filtred = datos.seleccionar_columnas(df)
#tratamiento atipicos
tipos_de_datos = df_filtred.dtypes
#df_filtred = datos.tratar_atipicos(df_filtred) #esto afecta los resultados de los modelos
#categorias transformadas
Categoria_columnas = tipos_de_datos[tipos_de_datos == 'object'].index.tolist()
df_filtred = datos.Modificar_Tipo(df_filtred,Categoria_columnas,'category','1')
#dumificar datos
df_filtred = datos.dumificar_datos(df_filtred,['attrition_flag', 'clientnum'])
#df_filtred.info()
#quitar idenficacion del cliente -- verificar si es necesario 
#tranformacion variable objetivo 'Attrited Customer' lo que me interesa es saber si se va o no
df_filtred['attrition_flag'] = df_filtred['attrition_flag'].replace({'Attrited Customer': 1, 'Existing Customer': 0})
#eliminar columnas que no aportan
#df_filtred = df_filtred.drop(['clientnum','credit_limit','total_trans_ct','customer_age'], axis=1)
df_filtred = df_filtred.drop(['clientnum'], axis=1)
#Eda 
Reporte = Create_report.DataReport('bank_churn.xlsx')
Reporte.generate_html()
# modelos
#Definicion de variables para el modelo    
y = df_filtred['attrition_flag']
X = df_filtred.drop(columns = 'attrition_flag')
#balancer datos 
#X_balanced, y_balanced =datos.balanceo_oversampling(X, y) #se sobreajusta el modelo
#X_balanced, y_balanced =datos.balanceo_smote(X, y) #crea faltntes en la data
#parametros para los modelos
Parametros_svm = {'SVM__C': [0.1, 1, 10],'SVM__kernel': ['rbf' , 'poly']}
Parametros_knn = {'KNN__n_neighbors': [1,5,10,15,20,25,30]}
#objeto para comparar modelos
Model_Compare = Model.Model(X=X,y=y,Parametros_svm=Parametros_svm,Parametros_knn=Parametros_knn,Column_target='attrition_flag',df=df_filtred) 
#split de datos y creacion de semilla
Model_Compare.train_test_split(0.2)
#modelo pycaret -------- para comparar con los modelos de sklearn
Model_Compare.Set_up_pycaret()
#Modelo 1  svm con 5 folds 
Model_Compare.Classifier_SVM_grid_search(5)
Model_Compare.plot_results_svm()
Model_Compare.grid.best_params_
#Modelo 2  Knn con 5 folds
Model_Compare.Classifier_Knn_Best_k_grid_search(5)
Model_Compare.plot_results_knn()
Model_Compare.grid_knn.best_params_
#Modelo 3 Para redes neuronales dado la complegidad de usar un grid search se realizara un metodo para comprarar dos configuraciones
Parametros_rnn_1 = [(1000, 'relu'), (500, 'relu'), (250, 'relu'),(75, 'relu'),(25, 'relu')]
Parametros_rnn_2 = [(500, 'leaky_relu'), (250, 'leaky_relu'), (125, 'leaky_relu'),(75, 'leaky_relu')]
#Se crea el mejor modelo con la configuracion ganadadora dentro del objeto para la configuracion final
Model_Compare.create_model_rnn(
Model_Compare.compare_rnn_models(config1=Parametros_rnn_1, config2=Parametros_rnn_2,epochs=30, batch_size=None, validation_split=0.2))
#Entrenamiento ramdom forest
Model_Compare.Classifier_RandomForest_grid_search(5)
Model_Compare.plot_results_rf()
Model_Compare.grid_rf.best_params_
#Entrenamiento AdaBoost
Model_Compare.Classifier_AdaBoost_grid_search(5)
Model_Compare.plot_results_ada()
Model_Compare.grid_ada.best_params_
#comparacion final de modelos
Model_Compare.compare_models()