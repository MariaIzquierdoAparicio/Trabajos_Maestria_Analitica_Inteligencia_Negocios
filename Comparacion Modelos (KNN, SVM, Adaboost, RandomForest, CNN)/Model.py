import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn import svm
import seaborn as sns
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
from sklearn.model_selection import train_test_split
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import (accuracy_score, precision_score, recall_score,
                             f1_score, confusion_matrix, roc_auc_score,roc_curve,
                             make_scorer,auc)
from sklearn.linear_model import LogisticRegression
from sklearn.neighbors import KNeighborsClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier
from pycaret.classification import *
import sweetviz as sv

import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import scikeras
from scikeras.wrappers import KerasClassifier
from tensorflow.keras.utils import to_categorical
from tensorflow.keras.callbacks import EarlyStopping

class Model:
    def __init__(self,X,y,Column_target,df,Parametros_svm={'SVM__C': [0.1, 1, 10],'SVM__kernel': ['linear', 'rbf' , 'poly']},Parametros_knn={'KNN__n_neighbors': [1,5,10,15,20,25,30]}):
        self.X = X
        self.y = y 
        self.Parametros_svm = Parametros_svm
        #self.Parametros_rnn = Parametros_rnn
        self.Parametros_knn = Parametros_knn
        self.Column_target = Column_target
        self.df = df


    def train_test_split(self, test_size):

        np.random.seed(77300)
        self.X_train, self.X_test, self.y_train, self.y_test = train_test_split(self.X,self.y, test_size = test_size, stratify=self.y)
        self.y_train_categori = to_categorical(self.y_train)
        self.y_test_categori = to_categorical(self.y_test)

        return self.X_train, self.X_test, self.y_train, self.y_test
        # ROC curve
    
    def plot_roc(y_test, y_pred):
        fpr, tpr, thresholds = roc_curve(y_test, y_pred, pos_label=1, drop_intermediate = False)
        roc_auc = auc(fpr, tpr)
        plt.figure()
        lw = 2
        plt.plot(fpr, tpr, color='darkorange',
                lw=lw, label='ROC curve (AUC = %0.2f)' % roc_auc)
        plt.plot([0, 1], [0, 1], color='navy', lw=lw, linestyle='--')
        plt.xlim([-0.001, 1.001])
        plt.ylim([-0.001, 1.001])
        plt.xlabel('1-Specificity (False Negative Rate)')
        plt.ylabel('Sensitivity (True Positive Rate)')
        plt.title('ROC curve')
        plt.legend(loc="lower right")
        plt.show()

    # Confusion Matrix returns in the format: cm[0,0], cm[0,1], cm[1,0], cm[1,1]: tn, fp, fn, tp

    # Sensitivity
    def custom_sensitivity_score(y_test, y_pred):
        cm = confusion_matrix(y_test, y_pred)
        tn, fp, fn, tp = cm[0][0], cm[0][1], cm[1][0], cm[1][1]
        return (tp/(tp+fn))

    # Specificity
    def custom_specificity_score(y_test, y_pred):
        cm = confusion_matrix(y_test, y_pred)
        tn, fp, fn, tp = cm[0][0], cm[0][1], cm[1][0], cm[1][1]
        return (tn/(tn+fp))

    # Positive Predictive Value
    def custom_ppv_score(y_test, y_pred):
        cm = confusion_matrix(y_test, y_pred)
        tn, fp, fn, tp = cm[0][0], cm[0][1], cm[1][0], cm[1][1]
        return (tp/(tp+fp))

    # Negative Predictive Value
    def custom_npv_score(y_test, y_pred):
        cm = confusion_matrix(y_test, y_pred)
        tn, fp, fn, tp = cm[0][0], cm[0][1], cm[1][0], cm[1][1]
        return (tn/(tn+fn))

    # Accuracy
    def custom_accuracy_score(y_test, y_pred):
        cm = confusion_matrix(y_test, y_pred)
        tn, fp, fn, tp = cm[0][0], cm[0][1], cm[1][0], cm[1][1]
        return ((tn+tp)/(tn+tp+fn+fp))
    
    def Classifier_SVM_default(self):
        svm_estimators = []
        svm_estimators.append(('standardize', StandardScaler())) # scale the data
        svm_estimators.append(('SVM', svm.SVC(probability=True))) # define SVM with probabilities (recall, SVM be default does not predict probabilities)
        self.Classifier_SVM = Pipeline(svm_estimators, verbose=False)
        self.Classifier_SVM.fit(self.X_train, self.y_train)
        return self.Classifier_SVM
    
    def Classifier_SVM_grid_search(self,Number_of_folds=5):
        score_func = make_scorer(accuracy_score, greater_is_better=True)
        self.Classifier_SVM_PIPE = Pipeline([('standardize', StandardScaler()), ('SVM', svm.SVC(probability=True))])
        self.grid = GridSearchCV(self.Classifier_SVM_PIPE , param_grid = self.Parametros_svm, cv = Number_of_folds, scoring = score_func, refit = True, verbose = 3, return_train_score = True)
        self.grid.fit(self.X_train, self.y_train)
        self.results_df = pd.DataFrame(self.grid.cv_results_)
        return self.grid
    
    def plot_results_svm(self):
        """
        Genera gráficos de los resultados de GridSearchCV mostrando la precisión tanto del conjunto de prueba como del conjunto de entrenamiento para cada combinación de 'C' y 'kernel'.
        """
        # Asegurarse de que los nombres de los kernels estén correctos
        self.results_df['param_SVM__kernel'] = self.results_df['param_SVM__kernel'].replace('lineal', 'linear')

        # Filtrar los resultados para excluir filas donde mean_test_score o mean_train_score es NaN
        filtered_results = self.results_df.dropna(subset=['mean_test_score', 'mean_train_score'])

        # Iterar sobre cada tipo de kernel y generar un gráfico
        kernels = filtered_results['param_SVM__kernel'].unique()
        for kernel in kernels:
            # Filtrar los resultados por kernel
            kernel_df = filtered_results[filtered_results['param_SVM__kernel'] == kernel]

            # Crear el gráfico para Test Accuracy
            plt.figure(figsize=(10, 6))
            sns.lineplot(data=kernel_df, x='param_SVM__C', y='mean_test_score', marker='o', label=f'Test Accuracy: {kernel}')
            
            # Crear el gráfico para Train Accuracy en el mismo plot
            sns.lineplot(data=kernel_df, x='param_SVM__C', y='mean_train_score', marker='^', label=f'Train Accuracy: {kernel}', linestyle='--')

            plt.title(f'Accuracy of SVM Varying C for {kernel} Kernel')
            plt.xlabel('C (Regularization Parameter)')
            plt.ylabel('Mean Accuracy')
            plt.xscale('log')  # Escala logarítmica para el eje X
            plt.legend()
            plt.grid(True)
            plt.show()

    def Classifier_LogisticRegression_default(self):

        self.Classifier_LogisticRegression = LogisticRegression(max_iter=1000)
        self.Classifier_LogisticRegression.fit(self.X_train, self.y_train)
        return self.Classifier_LogisticRegression
    
    def Classifier_Knn_Best_k_grid_search(self, Number_of_folds=5):
        self.knn_pipeline = Pipeline([('standardize', StandardScaler()), ('KNN', KNeighborsClassifier())])
        score_func = make_scorer(accuracy_score, greater_is_better=True)
        self.grid_knn = GridSearchCV(self.knn_pipeline, param_grid=self.Parametros_knn, cv=Number_of_folds, scoring=score_func, refit=True, verbose=3, return_train_score=True)
        self.grid_knn.fit(self.X_train, self.y_train)
        self.results_df_knn = pd.DataFrame(self.grid_knn.cv_results_)
        
        return self.grid_knn
    
    def plot_results_knn(self):

        # Asegurarse de que los resultados están limpios y listos para ser graficados
        self.results_df_knn['param_KNN__n_neighbors'] = self.results_df_knn['param_KNN__n_neighbors'].astype(int)  # Asegurar que los valores sean enteros

        # Filtrar los resultados para excluir filas donde mean_test_score o mean_train_score es NaN
        filtered_results = self.results_df_knn.dropna(subset=['mean_test_score', 'mean_train_score'])

        # Crear el gráfico para Test Accuracy
        plt.figure(figsize=(10, 6))
        sns.lineplot(data=filtered_results, x='param_KNN__n_neighbors', y='mean_test_score', marker='o', label='Test Accuracy')

        # Crear el gráfico para Train Accuracy en el mismo plot
        sns.lineplot(data=filtered_results, x='param_KNN__n_neighbors', y='mean_train_score', marker='^', label='Train Accuracy', linestyle='--')

        plt.title('Accuracy of KNN Varying n_neighbors')
        plt.xlabel('Number of Neighbors (k)')
        plt.ylabel('Mean Accuracy')
        plt.legend()
        plt.grid(True)
        plt.show()
    
    def Classifier_RandomForest_grid_search(self, Number_of_folds=5):
        # Crear pipeline con la etapa de estandarización y el clasificador RandomForest
        rf_pipeline = Pipeline([('standardize', StandardScaler()), ('RandomForest', RandomForestClassifier())])

        # Definir los parámetros a probar en el grid search
        param_grid = {
            'RandomForest__n_estimators': [10, 50, 100],  # Número de árboles en el bosque
            'RandomForest__max_depth': [1, 10, 20],  # Profundidad máxima de los árboles
            'RandomForest__min_samples_split': [2, 5, 10]  # Número mínimo de muestras requeridas para dividir un nodo interno
        }
        
        # Definir la función de puntuación a utilizar
        score_func = make_scorer(accuracy_score, greater_is_better=True)

        # Crear el objeto GridSearchCV
        self.grid_rf = GridSearchCV(estimator=rf_pipeline, param_grid=param_grid, cv=Number_of_folds, scoring=score_func, refit=True, verbose=3, return_train_score=True)

        # Ajustar el GridSearchCV con los datos de entrenamiento
        self.grid_rf.fit(self.X_train, self.y_train)

        self.results_df_rf = pd.DataFrame(self.grid_rf.cv_results_)

        self.best_rf_model = self.grid_rf.best_estimator_

        return self.grid_rf
    
    def plot_results_rf(self):
    # Asegurarse de que los resultados estén limpios y listos para ser graficados
        self.results_df_rf['param_RandomForest__n_estimators'] = self.results_df_rf['param_RandomForest__n_estimators'].astype(int)

        # Puede haber múltiples gráficos, uno por cada combinación de max_depth y min_samples_split
        fig, axes = plt.subplots(nrows=1, ncols=3, figsize=(18, 6), sharey=True)
        fig.suptitle('Random Forest Performance Varying n_estimators')

        depths = self.results_df_rf['param_RandomForest__max_depth'].unique()

        for i, depth in enumerate(depths):
            # Filtrar los resultados para la profundidad actual
            depth_data = self.results_df_rf[self.results_df_rf['param_RandomForest__max_depth'] == depth]

            sns.lineplot(ax=axes[i], data=depth_data, x='param_RandomForest__n_estimators', y='mean_test_score', marker='o', label='Test Accuracy')
            sns.lineplot(ax=axes[i], data=depth_data, x='param_RandomForest__n_estimators', y='mean_train_score', marker='^', label='Train Accuracy', linestyle='--')

            axes[i].set_title(f'Max Depth: {depth}')
            axes[i].set_xlabel('Number of Estimators')
            axes[i].set_ylabel('Mean Accuracy')
            axes[i].legend()
            axes[i].grid(True)

        plt.tight_layout(rect=[0, 0.03, 1, 0.95])
        plt.show()

    def Classifier_AdaBoost_grid_search(self, Number_of_folds=5):
        ada_pipeline = Pipeline([('standardize', StandardScaler()), ('AdaBoost', AdaBoostClassifier())])
        param_grid = {
            'AdaBoost__n_estimators': [50, 100, 200],  # Número de estimadores en el ensemble
            'AdaBoost__learning_rate': [0.01, 0.1, 1.0]  # Tasa de aprendizaje
        }
        
        score_func = make_scorer(accuracy_score, greater_is_better=True)
        
        self.grid_ada = GridSearchCV(ada_pipeline, param_grid=param_grid, cv=Number_of_folds, scoring=score_func, refit=True, verbose=3, return_train_score=True)
        self.grid_ada.fit(self.X_train, self.y_train)
        self.results_df_ada = pd.DataFrame(self.grid_ada.cv_results_)
        
        self.best_ada_model = self.grid_ada.best_estimator_
        
        return self.grid_ada
    
    def plot_results_ada(self):
    # Asegurarse de que los resultados están limpios y listos para ser graficados
        self.results_df_ada['param_AdaBoost__n_estimators'] = self.results_df_ada['param_AdaBoost__n_estimators'].astype(int)
        self.results_df_ada['param_AdaBoost__learning_rate'] = self.results_df_ada['param_AdaBoost__learning_rate'].astype(float)

        # Puede haber múltiples gráficos, uno por cada tasa de aprendizaje
        fig, axes = plt.subplots(nrows=1, ncols=len(self.results_df_ada['param_AdaBoost__learning_rate'].unique()), figsize=(18, 6), sharey=True)
        fig.suptitle('AdaBoost Performance Varying n_estimators and Learning Rate')

        learning_rates = self.results_df_ada['param_AdaBoost__learning_rate'].unique()

        for i, rate in enumerate(sorted(learning_rates)):
            # Filtrar los resultados para la tasa de aprendizaje actual
            rate_data = self.results_df_ada[self.results_df_ada['param_AdaBoost__learning_rate'] == rate]

            sns.lineplot(ax=axes[i], data=rate_data, x='param_AdaBoost__n_estimators', y='mean_test_score', marker='o', label='Test Accuracy')
            sns.lineplot(ax=axes[i], data=rate_data, x='param_AdaBoost__n_estimators', y='mean_train_score', marker='^', label='Train Accuracy', linestyle='--')

            axes[i].set_title(f'Learning Rate: {rate}')
            axes[i].set_xlabel('Number of Estimators')
            axes[i].set_ylabel('Mean Accuracy')
            axes[i].legend()
            axes[i].grid(True)

        plt.tight_layout(rect=[0, 0.03, 1, 0.95])
        plt.show()

    def create_model_rnn(self,Parametros_rnn, optimizer='adam', loss='categorical_crossentropy',learning_rate=0.1):
        self.model = keras.Sequential()
        self.model.add(keras.layers.InputLayer( input_shape=(self.X_train.shape[1],),batch_size=None ,name=f"Input-layer"))
        # Configurar las capas basado en los parámetros dados
        for i, (neurons, activation) in enumerate(Parametros_rnn, start=1):
            #if i == 1:
                # La primera capa incluye la dimensión de entrada
                #self.model.add(layers.Dense(neurons, activation=activation, input_dim=self.X_train.shape[1], name=f"hidden-dense-{neurons}-layer-{i}"))
            #    self.model.add(keras.layers.InputLayer( input_shape=self.X_train.shape[1],batch_size=None ,name=f"Input-layer"))
            #else:
            self.model.add(layers.Dense(neurons, activation=activation, name=f"hidden-dense-{neurons}-layer-{i}"))
            self.model.add(layers.Dropout(0.3 if i % 2 == 1 else 0.2))

        # Añadir la capa de salida con softmax para clasificación multiclase
        self.model.add(layers.Dense(self.y_train_categori.shape[1], activation='softmax', name="output-layer"))

        # Configurar el optimizador
        optimizer = {'adam': tf.keras.optimizers.Adam(),
                     'sgd': tf.keras.optimizers.SGD(learning_rate=learning_rate),
                     'rmsprop': tf.keras.optimizers.RMSprop()}.get(optimizer, tf.keras.optimizers.Adam())

        # Compilar el modelo
        self.model.compile(loss=loss, optimizer=optimizer, metrics=["categorical_accuracy"])
        return self.model
    
    def train_model_rnn(self, epochs=30, batch_size=None, validation_split=0.2):
        # Configurar el pipeline con preprocesamiento y el modelo Keras
        early_stopping = EarlyStopping(monitor='val_loss', patience=10, verbose=1, mode='min', restore_best_weights=True)

        nn_estimators = [
            ('standardize', StandardScaler()),  # Estandarización de los datos de entrada
            ('mlp', KerasClassifier(model=self.model, epochs=epochs, batch_size=batch_size, validation_split=validation_split, callbacks=early_stopping))
        ]
        self.Clasificador_RN = Pipeline(nn_estimators, verbose=False)
        
        self.Clasificador_RN.fit(self.X_train, self.y_train_categori)  # Entrenar el modelo
        self.history_df_rnn = pd.DataFrame(self.Clasificador_RN.named_steps['mlp'].model.history.history)
        return self.history_df_rnn
    
    def plot_results_rnn(self, history_df):
        # Graficar los resultados de la pérdida y la precisión
        history_df[['loss', 'val_loss']].plot(title='Model Loss', xlabel='Epoch', ylabel='Loss')
        plt.show()
        #self.history_df[['accuracy', 'val_accuracy']].plot(title='Model Accuracy', xlabel='Epoch', ylabel='Accuracy')
        history_df[['categorical_accuracy', 'val_categorical_accuracy']].plot(title='Model categorical accuracy', xlabel='Epoch', ylabel='categorical accuracy')
        plt.show()
        return None
    
    def compare_rnn_models(self, config1, config2, epochs=30, batch_size=None, validation_split=0.2):
    # Entrenar el primer modelo
        self.create_model_rnn(Parametros_rnn=config1)
        history1 = self.train_model_rnn(epochs=epochs, batch_size=batch_size, validation_split=validation_split)
        max_val_acc1 = max(history1['val_categorical_accuracy'])
        
        # Entrenar el segundo modelo
        self.create_model_rnn(Parametros_rnn=config2)
        history2 = self.train_model_rnn(epochs=epochs, batch_size=batch_size, validation_split=validation_split)
        max_val_acc2 = max(history2['val_categorical_accuracy'])
        
        # Comparar los resultados y seleccionar el mejor
        if max_val_acc1 > max_val_acc2:
            print("La configuración 1 es mejor.")
            best_config = config1
            best_history = history1
        else:
            print("La configuración 2 es mejor.")
            best_config = config2
            best_history = history2
        
        # Opcionalmente, graficar resultados del mejor modelo
        self.plot_results_rnn(best_history)
        return best_config
    
    def compare_models(self):
        # Predecir con el modelo SVM Grid
        y_pred_svm = self.grid.predict(self.X_test)
        y_proba_svm = self.grid.predict_proba(self.X_test)[:, 1]
        auc_svm = roc_auc_score(self.y_test, y_proba_svm)
        f1_svm = f1_score(self.y_test, y_pred_svm)
        accuracy_svm = accuracy_score(self.y_test, y_pred_svm)

        # Predecir con el modelo kNN Grid
        y_pred_knn = self.grid_knn.predict(self.X_test)
        y_proba_knn = self.grid_knn.predict_proba(self.X_test)[:, 1]
        auc_knn = roc_auc_score(self.y_test, y_proba_knn)
        f1_knn = f1_score(self.y_test, y_pred_knn)
        accuracy_knn = accuracy_score(self.y_test, y_pred_knn)

        # Predecir con el modelo Random Forest Grid
        y_pred_rf = self.best_rf_model.predict(self.X_test)
        y_proba_rf = self.best_rf_model.predict_proba(self.X_test)[:, 1]
        auc_rf = roc_auc_score(self.y_test, y_proba_rf)
        f1_rf = f1_score(self.y_test, y_pred_rf)
        accuracy_rf = accuracy_score(self.y_test, y_pred_rf)

        # Predecir con el modelo AdaBoost Grid
        y_pred_ada = self.best_ada_model.predict(self.X_test)
        y_proba_ada = self.best_ada_model.predict_proba(self.X_test)[:, 1]
        auc_ada = roc_auc_score(self.y_test, y_proba_ada)
        f1_ada = f1_score(self.y_test, y_pred_ada)
        accuracy_ada = accuracy_score(self.y_test, y_pred_ada)

        # Predecir con el modelo RNN
        y_pred_prob = self.Clasificador_RN.predict_proba(self.X_test)[:,1]
        y_pred_rnn = np.where(y_pred_prob > 0.5, 1, 0) 
        accuracy_rnn = accuracy_score(self.y_test_categori[:,1], y_pred_rnn)
        f1_rnn = f1_score(self.y_test_categori[:,1], y_pred_rnn)
        auc_rnn = roc_auc_score(self.y_test_categori[:,1], y_pred_prob)

        # Graficar las comparaciones de precisión
        labels = ['SVM Grid', 'kNN Grid', 'Random Forest Grid', 'AdaBoost Grid','RNN']
        Metrics_value = [f1_svm, f1_knn, f1_rf, f1_ada,f1_rnn]

        plt.figure(figsize=(12, 6))
        plt.bar(labels, Metrics_value, color=['blue', 'green', 'orange', 'red','purple'])
        plt.title('Comparación de F1 entre SVM Grid, kNN Grid, Random Forest Grid, AdaBoost Grid ,RNN')
        plt.xlabel('Modelo')
        plt.ylabel('F1 Score')
        plt.ylim(0, 1)
        plt.show()

        # Mostrar resultados en consola
        print(f'F1 del modelo SVM Grid: {f1_svm}')
        print(f'F1 del modelo kNN Grid: {f1_knn}')
        print(f'F1 del modelo Random Forest Grid: {f1_rf}')
        print(f'F1 del modelo AdaBoost Grid: {f1_ada}')
        print(f'F1 del modelo RNN: {f1_rnn}')


        print(f'Auc del modelo SVM Grid: {auc_svm}')
        print(f'Auc del modelo kNN Grid: {auc_knn}')
        print(f'Auc del modelo Random Forest Grid: {auc_rf}')
        print(f'Auc del modelo AdaBoost Grid: {auc_ada}')
        print(f'Auc del modelo RNN: {auc_rnn}')

        print(f'Precisión del modelo SVM Grid: {accuracy_svm}')
        print(f'Precisión del modelo kNN Grid: {accuracy_knn}')
        print(f'Precisión del modelo Random Forest Grid: {accuracy_rf}')
        print(f'Precisión del modelo AdaBoost Grid: {accuracy_ada}')
        print(f'Precisión del modelo RNN: {accuracy_rnn}')

        print(f'Sensibilidad del modelo SVM Grid: {Model.custom_sensitivity_score(self.y_test, y_pred_svm)}')
        print(f'Sensibilidad del modelo KNN Grid: {Model.custom_sensitivity_score(self.y_test, y_pred_knn)}')
        print(f'Sensibilidad del modelo Random Forest Grid: {Model.custom_sensitivity_score(self.y_test, y_pred_rf)}')
        print(f'Sensibilidad del modelo AdaBoost Grid: {Model.custom_sensitivity_score(self.y_test, y_pred_ada)}')
        print(f'Sensibilidad del modelo RNN: {Model.custom_sensitivity_score(self.y_test_categori[:,1], y_pred_rnn)}')     

        print(f'Especificidad del modelo SVM Grid: {Model.custom_specificity_score(self.y_test, y_pred_svm)}')
        print(f'Especificidad del modelo KNN Grid: {Model.custom_specificity_score(self.y_test, y_pred_knn)}')
        print(f'Especificidad del modelo Random Forest Grid: {Model.custom_specificity_score(self.y_test, y_pred_rf)}')
        print(f'Especificidad del modelo AdaBoost Grid: {Model.custom_specificity_score(self.y_test, y_pred_ada)}')
        print(f'Especificidad del modelo RNN: {Model.custom_specificity_score(self.y_test_categori[:,1], y_pred_rnn)}')

        print(f'Postive rate del modelo SVM Grid: {Model.custom_ppv_score(self.y_test, y_pred_svm)}')
        print(f'Postive rate del modelo KNN Grid: {Model.custom_ppv_score(self.y_test, y_pred_knn)}')
        print(f'Postive rate del modelo Random Forest Grid: {Model.custom_ppv_score(self.y_test, y_pred_rf)}')
        print(f'Postive rate del modelo AdaBoost Grid: {Model.custom_ppv_score(self.y_test, y_pred_ada)}')
        print(f'Postive rate del modelo RNN: {Model.custom_ppv_score(self.y_test_categori[:,1], y_pred_rnn)}')

        print(f'Negative rate del modelo SVM Grid: {Model.custom_npv_score(self.y_test, y_pred_svm)}')
        print(f'Negative rate del modelo KNN Grid: {Model.custom_npv_score(self.y_test, y_pred_knn)}')
        print(f'Negative rate del modelo Random Forest Grid: {Model.custom_npv_score(self.y_test, y_pred_rf)}')
        print(f'Negative rate del modelo AdaBoost Grid: {Model.custom_npv_score(self.y_test, y_pred_ada)}')
        print(f'Negative rate del modelo RNN: {Model.custom_npv_score(self.y_test_categori[:,1], y_pred_rnn)}')
    
    def Set_up_pycaret(self):

        train_data = pd.concat([self.X_train, self.y_train], axis=1)
        test_data = pd.concat([self.X_test, self.y_test], axis=1)
        s = setup(data=train_data, target=self.Column_target, session_id=42, train_size=0.8, fix_imbalance=False)
        compare_models(sort='F1')
        best_model = compare_models(sort='F1')
        # Finalizar el modelo: entrenar en todo el conjunto de entrenamiento
        final_model = finalize_model(best_model)
        # Predecir sobre el conjunto de prueba
        predictions = predict_model(final_model, data=test_data)
        y_test = test_data['attrition_flag']
        y_pred = predictions['prediction_label']
        y_pred_proba = predictions['prediction_score']
        print("Accuracy:", accuracy_score(y_test, y_pred))
        print("F1 Score:", f1_score(y_test, y_pred, average='binary'))  # Ajusta 'average' según tu caso
        print("AUC Score:", roc_auc_score(y_test, y_pred_proba))
        return predictions