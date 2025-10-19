import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.impute import SimpleImputer
import seaborn as sns
from sklearn.ensemble import IsolationForest
from imblearn.over_sampling import RandomOverSampler
from imblearn.over_sampling import SMOTE


class  Manupilacion_datos:

    def __init__(self,Path_Archivo,Tiene_Encabezado,Column_target):
        self.Path_Archivo = Path_Archivo
        self.Tiene_Encabezado = Tiene_Encabezado
        self.Column_target = Column_target

    def Cargue_archivos_csv(self):
        self.df = pd.read_csv(self.Path_Archivo, header = self.Tiene_Encabezado)
        return self.df

    def Cargue_archivos_Excel(self):
        self.df = pd.read_excel(self.Path_Archivo, header = self.Tiene_Encabezado)
        return self.df
    
    def Modificar_Tipo(self,df,Columnas,Tipo_dato,Tipo_transformacion):
        if Tipo_transformacion == '1':
            for columna in Columnas:
                if columna in df.columns:
                    df[columna] = df[columna].astype(Tipo_dato)
        if Tipo_transformacion == '2':
            for columna in Columnas:
                if columna in df.columns:
                    df[columna] = pd.to_datetime(df[columna], unit='D', origin='1899-12-30')
        return df

    def CombineRareCategories(self,df, mincount):
        for col in df.columns:
            if (type(df[col][0]) == str):
                for index, row in pd.DataFrame(df[col].value_counts()).iterrows():
                    if ( row[0] < mincount):
                        df[col + '_surrogate'] = df[col]
                        df[col + '_surrogate'] = df[col + '_surrogate'].replace(index, 'Other_' + col)
                        #data[col].replace(index, 'Other_' + col, inplace = True)
                    else:
                        None
        return df
    
    def encontrar_columnas_por_cadena(self,df, cadena):
        columnas_encontradas = []
        for columna in df.columns:
            if cadena in columna:
                columnas_encontradas.append(columna)
        return columnas_encontradas
    
    def imputar_datos(self, df, columnas_a_ignorar=[]):
        for col in df.select_dtypes(exclude=['datetime64[ns]']):
            if col in columnas_a_ignorar:
                continue  # Si la columna está en la lista de ignorados, saltar al siguiente ciclo
            
            # Verificar si hay valores nulos en la columna original
            if df[col].isna().any():
                surrogate_col_name = col + '_surrogate'
                # Verificar si la columna '_surrogate' ya existe
                if surrogate_col_name not in df:
                    # Si no existe, crear una nueva columna '_surrogate' con el mismo tipo de datos que la columna original
                    if pd.api.types.is_categorical_dtype(df[col]):
                        df[surrogate_col_name] = pd.Categorical(df[col].isna().astype(int))
                    else:
                        df[surrogate_col_name] = df[col].isna().astype(df[col].dtype)
                else:
                    # Si ya existe, llenar los valores nulos en la columna '_surrogate' sin alterar el tipo de datos
                    if pd.api.types.is_categorical_dtype(df[surrogate_col_name]):
                        # Si la columna '_surrogate' es categórica
                        categories = df[surrogate_col_name].cat.categories.tolist()
                        if "1" not in categories:
                            categories.append("1")
                            df[surrogate_col_name] = df[surrogate_col_name].cat.set_categories(categories)
                    df.loc[df[col].isna(), surrogate_col_name] = "1"

        # Procesar columnas categóricas
        categoricas = [col for col in df.select_dtypes(exclude=['int64', 'float64','datetime64[ns]']).columns if col not in columnas_a_ignorar]
        if categoricas:
            imputer_categoricos = SimpleImputer(missing_values=np.nan, strategy='constant')
            imputer_categoricos.fit(df[categoricas])
            df[categoricas] = imputer_categoricos.transform(df[categoricas])

        # Procesar columnas numéricas
        numericas = [col for col in df.select_dtypes(include=['int64', 'float64']).columns if col not in columnas_a_ignorar]
        if numericas:
            imputer_numericos = SimpleImputer(missing_values=np.nan, strategy='median')
            imputer_numericos.fit(df[numericas])
            df[numericas] = imputer_numericos.transform(df[numericas])
        
        return df
    
    def graficar_columnas(self,df, columnas):
        for columna in columnas:
            if df[columna].dtype == 'object':
                # Graficar variables categóricas
                plt.figure(figsize=(8, 6))
                sns.countplot(data=df, x=columna)
                plt.title(f'Conteo de valores para {columna}')
                plt.xticks(rotation=45)
                plt.show()
            else:
                # Graficar variables numéricas
                plt.figure(figsize=(8, 6))
                sns.histplot(data=df, x=columna, kde=True)
                plt.title(f'Distribución de valores para {columna}')
                plt.show()
     
    def seleccionar_columnas(self,df):
        # Obtener todas las columnas del DataFrame
        todas_columnas = set(df.columns)
    
        # Obtener todas las columnas que terminan en '_surrogate'
        columnas_surrogate = {col for col in todas_columnas if col.endswith('_surrogate')}
    
        # Obtener todas las columnas que tienen su respectiva columna original sin '_surrogate'
        columnas_originales = {col.replace('_surrogate', '') for col in columnas_surrogate}
    
        # Obtener las columnas '_surrogate' que no tienen su respectiva columna original sin '_surrogate'
        columnas_filtradas_surrogate = columnas_surrogate - columnas_originales
    
        # Combinar las columnas seleccionadas
        columnas_seleccionadas = list(columnas_filtradas_surrogate) + list(todas_columnas - columnas_originales)

        columnas_seleccionadas = list(set(columnas_seleccionadas))  

        df = df[columnas_seleccionadas]
        
        return df
    
    def dumificar_datos(self, df, columnas_a_ignorar=[]):
        # Seleccionar columnas categóricas, excluyendo las columnas que no son de tipo numérico ni datetime,
        # y también excluyendo las columnas específicas a ignorar.
        columnas_para_dumificar = [col for col in df.select_dtypes(exclude=['int64', 'float64', 'datetime64[ns]']).columns 
                                if col not in columnas_a_ignorar]
        
        # Aplicar get_dummies solo a las columnas seleccionadas
        df = pd.get_dummies(df, columns=columnas_para_dumificar, drop_first=True)
        
        return df
    
    from sklearn.ensemble import IsolationForest


    
    def tratar_atipicos(self,df):
        df_tratado = df.copy()
        tipos_de_datos = df_tratado.dtypes
        columnas = []
        for columna, tipo in tipos_de_datos.items():
            if tipo == 'int64' or tipo == 'float64':
                columnas.append(columna)
                
        columnas = [col for col in columnas if col if col not in [self.Column_target, 'clientnum']]
        # Crear y ajustar el modelo Isolation Forest para cada columna
        for col in columnas:
            modelo_iforest = IsolationForest(contamination='auto', random_state=42)
            modelo_iforest.fit(df_tratado[[col]])

            # Identificar los valores atípicos
            outliers = modelo_iforest.predict(df_tratado[[col]])
            
            # Reemplazar los valores atípicos por la mediana
            mediana = df_tratado[col].median()
            df_tratado.loc[outliers == -1, col] = mediana

        return df_tratado

    def balanceo_oversampling(self, X, y):
    # Inicializar el oversampler
        oversampler = RandomOverSampler(random_state=42)

    # Aplicar oversampling
        X_balanceado, y_balanceado = oversampler.fit_resample(X, y)

        return X_balanceado, y_balanceado
    


    def balanceo_smote(self, X, y):
        smote = SMOTE(random_state=42)
        X_balanceado, y_balanceado = smote.fit_resample(X, y)
        return X_balanceado, y_balanceado

    def imprimir_matriz_correlacion(self,df):
        df_numerico = df.select_dtypes(include=['int64', 'float64'])

        # Calcular la matriz de correlación
        matriz_correlacion = df_numerico.corr()

        # Crear un mapa de calor con seaborn
        plt.figure(figsize=(12, 10))
        sns.heatmap(matriz_correlacion, annot=True, cmap='coolwarm', fmt=".2f", linewidths=.5)
        plt.title('Matriz de correlación de características numéricas')
        plt.show()

    def eliminar_correlaciones_altas(self,df, umbral=0.5):
        df_numerico = df.select_dtypes(include=['int64', 'float64'])
        # Calcular la matriz de correlación
        matriz_correlacion = df_numerico.corr()

        # Crear una matriz de correlación absoluta para identificar correlaciones altas (positivas o negativas)
        matriz_correlacion_abs = matriz_correlacion.abs()

        # Crear una máscara booleana para identificar las correlaciones altas
        mascara = np.triu(matriz_correlacion_abs, k=1)  # excluye la diagonal principal y la mitad inferior
        correlaciones_altas = matriz_correlacion_abs.mask(mascara > umbral)

        # Encontrar las características altamente correlacionadas
        variables_correlacionadas = [columna for columna in correlaciones_altas.columns if any(correlaciones_altas[columna] > umbral)]
        # Eliminar las características altamente correlacionadas del DataFrame
        variables_correlacionadas = [col for col in variables_correlacionadas if col not in [self.Column_target, 'clientnum']]

        df_filtrado = df.drop(variables_correlacionadas, axis=1)

        return df_filtrado
    
    def fechas_a_int(self,df, columnas_fechas):

        for columna_fecha in columnas_fechas:
            # Reemplazar los valores NaN por la fecha 1900-01-01
            df[columna_fecha].fillna(pd.to_datetime('1900-01-01'), inplace=True)
            # Convertir la columna de fechas a enteros en formato YYYYMMDD
            df[columna_fecha] = df[columna_fecha].dt.strftime('%Y%m%d').astype(int)
        return df
    
