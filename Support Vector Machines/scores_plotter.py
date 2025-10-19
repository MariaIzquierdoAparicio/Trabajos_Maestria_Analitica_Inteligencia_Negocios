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