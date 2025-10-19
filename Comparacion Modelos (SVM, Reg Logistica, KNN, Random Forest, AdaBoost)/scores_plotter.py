import matplotlib.pyplot as plt
import numpy as np

class ScoresPlotter:
    def __init__(self, results):
        self.results = results

    def plot_train_scores(self):
        # 2.1 Obtener los valores de SVM__C y los scores de train para cada kernel
        svm_c_values = self.results['param_SVM__C'].data
        svm_kernel_values = self.results['param_SVM__kernel'].data
        split0_train_scores = self.results['split0_train_score']
        split1_train_scores = self.results['split1_train_score']
        split2_train_scores = self.results['split2_train_score']
        split3_train_scores = self.results['split3_train_score']
        split4_train_scores = self.results['split4_train_score']

        # 2.2 Graficar cada punto para cada combinación de kernel
        for i in range(len(svm_c_values)):
            plt.scatter([svm_c_values[i]]*5, [split0_train_scores[i], split1_train_scores[i], split2_train_scores[i], split3_train_scores[i], split4_train_scores[i]], label=svm_kernel_values[i])

        plt.xlabel('SVM__C')
        plt.ylabel('Train Score')
        plt.title('Scores de Train para cada SVM__kernel (por pliegue)')
        plt.legend()
        plt.show()

        # 3.1. Obtener los valores de SVM__C y los scores de train para cada kernel
        svm_c_values = self.results['param_SVM__C'].data
        svm_kernel_values = self.results['param_SVM__kernel'].data
        split0_train_scores = self.results['split0_test_score']
        split1_train_scores = self.results['split1_test_score']
        split2_train_scores = self.results['split2_test_score']
        split3_train_scores = self.results['split3_test_score']
        split4_train_scores = self.results['split4_test_score']

        # 3.2. Graficar cada punto para cada combinación de kernel
        for i in range(len(svm_c_values)):
            plt.scatter([svm_c_values[i]]*5, [split0_train_scores[i], split1_train_scores[i], split2_train_scores[i], split3_train_scores[i], split4_train_scores[i]], label=svm_kernel_values[i])

        plt.xlabel('SVM__C')
        plt.ylabel('Train Score')
        plt.title('Scores de Test para cada SVM__kernel (por pliegue)')
        plt.legend()
        plt.show()

    def plot_average_train_scores(self):
        svm_c_values = np.unique(self.results['param_SVM__C'].data)
        svm_kernel_values = np.unique(self.results['param_SVM__kernel'].data)
        train_scores = self.results['mean_train_score']

        for kernel in svm_kernel_values:
            kernel_scores = train_scores[self.results['param_SVM__kernel'].data == kernel]
            plt.plot(svm_c_values, kernel_scores, label=kernel)

        plt.xlabel('SVM__C')
        plt.ylabel('Promedio de Train Score')
        plt.title('Promedio de Train Scores para cada SVM__kernel')
        plt.legend()
        plt.show()

    def plot_average_test_scores(self):
        svm_c_values = np.unique(self.results['param_SVM__C'].data)
        svm_kernel_values = np.unique(self.results['param_SVM__kernel'].data)
        test_scores = self.results['mean_test_score']

        for kernel in svm_kernel_values:
            kernel_scores = test_scores[self.results['param_SVM__kernel'].data == kernel]
            plt.plot(svm_c_values, kernel_scores, label=kernel)

        plt.xlabel('SVM__C')
        plt.ylabel('Promedio de Test Score')
        plt.title('Promedio de Test Scores para cada SVM__kernel')
        plt.legend()
        plt.show()

class LogisticRegressionResultsVisualizer:
    def __init__(self, results):
        self.results = results

    def plot_train_scores(self):
        logit_C_values = np.unique(self.results['param_logistic_regression__C'].data)
        logit_penalty_values = np.unique(self.results['param_logistic_regression__penalty'].data)
        train_scores = self.results['mean_train_score']

        for penalty in logit_penalty_values:
            penalty_scores = train_scores[self.results['param_logistic_regression__penalty'].data == penalty]
            if penalty == 'l1':
                penalty_label = 'L1 Lasso'
            elif penalty == 'l2':
                penalty_label = 'L2 Ridge'
            else:
                penalty_label = penalty
            plt.plot(logit_C_values, penalty_scores, label=penalty_label)

        plt.xlabel('Logistic_Regression__C')
        plt.ylabel('Promedio de Train Score')
        plt.title('Promedio de Train Scores para cada penalidad')
        plt.legend()
        plt.show()

    def plot_test_scores(self):
        logit_C_values = np.unique(self.results['param_logistic_regression__C'].data)
        logit_penalty_values = np.unique(self.results['param_logistic_regression__penalty'].data)
        test_scores = self.results['mean_test_score']

        for penalty in logit_penalty_values:
            penalty_scores = test_scores[self.results['param_logistic_regression__penalty'].data == penalty]
            if penalty == 'l1':
                penalty_label = 'L1 Lasso'
            elif penalty == 'l2':
                penalty_label = 'L2 Ridge'
            else:
                penalty_label = penalty
            plt.plot(logit_C_values, penalty_scores, label=penalty_label)

        plt.xlabel('Logistic_Regression__C')
        plt.ylabel('Promedio de Test Score')
        plt.title('Promedio de Test Scores para cada penalidad')
        plt.legend()
        plt.show()


import numpy as np
import matplotlib.pyplot as plt

class KNNResultsVisualizer:
    def __init__(self, results):
        self.results = results

    def plot_train_scores(self):
        knn_n_neighbors_values = np.unique(self.results['param_knn__n_neighbors'].data)
        knn_metric_values = np.unique(self.results['param_knn__metric'].data)
        train_scores = self.results['mean_train_score']

        for metric in knn_metric_values:
            metric_scores = train_scores[self.results['param_knn__metric'].data == metric]
            plt.plot(knn_n_neighbors_values, metric_scores, label=metric)

        plt.xlabel('Número de vecinos (k)')
        plt.ylabel('Promedio de Train Score')
        plt.title('Promedio de Train Scores para cada métrica')
        plt.legend()
        plt.show()

    def plot_test_scores(self):
        knn_n_neighbors_values = np.unique(self.results['param_knn__n_neighbors'].data)
        knn_metric_values = np.unique(self.results['param_knn__metric'].data)
        test_scores = self.results['mean_test_score']

        for metric in knn_metric_values:
            metric_scores = test_scores[self.results['param_knn__metric'].data == metric]
            plt.plot(knn_n_neighbors_values, metric_scores, label=metric)

        plt.xlabel('Número de vecinos (k)')
        plt.ylabel('Promedio de Test Score')
        plt.title('Promedio de Test Scores para cada métrica')
        plt.legend()
        plt.show()