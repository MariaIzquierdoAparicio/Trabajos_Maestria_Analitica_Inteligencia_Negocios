from sklearn.metrics import confusion_matrix, roc_curve, auc
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from sklearn.metrics import roc_auc_score

# ROC curve
def plot_roc(y_test, y_pred):
    fpr, tpr, thresholds = roc_curve(y_test, y_pred, pos_label=1, drop_intermediate=False)
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

def evaluate_model(model, model_name, X_test, y_test, mean_retention):
    class_threshold = mean_retention

    y_pred_prob = model.predict_proba(X_test)[:,1] # probabilities
    y_pred = np.where(y_pred_prob > class_threshold, 1, 0) # classification

    print('Metrics of the model:')
    cm = np.transpose(confusion_matrix(y_test, y_pred))
    print("Confusion matrix:\n", cm)
    print("Accuracy:", custom_accuracy_score(y_test, y_pred))
    print("Sensitivity (Recall):", custom_sensitivity_score(y_test, y_pred))
    print("Specificity (Fall-Out):", custom_specificity_score(y_test, y_pred))
    print("Positive Predictive Value (Precision):", custom_ppv_score(y_test, y_pred))
    print("Negative Predictive Value:", custom_npv_score(y_test, y_pred))

    plot_roc(y_test, y_pred_prob)
    print("AUC:", roc_auc_score(y_test, y_pred_prob))

    metrics_data = {
        "Model": [model_name],
        "Accuracy": [custom_accuracy_score(y_test, y_pred)],
        "Sensitivity (Recall)": [custom_sensitivity_score(y_test, y_pred)],
        "Specificity (Fall-Out)": [custom_specificity_score(y_test, y_pred)],
        "Positive Predictive Value (Precision)": [custom_ppv_score(y_test, y_pred)],
        "Negative Predictive Value": [custom_npv_score(y_test, y_pred)],
        "AUC": [roc_auc_score(y_test, y_pred_prob)]
    }

    return pd.DataFrame(metrics_data, index=[0])

