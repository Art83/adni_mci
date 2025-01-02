from sklearn.preprocessing import PolynomialFeatures
import pandas as pd
from sklearn.metrics import confusion_matrix, roc_auc_score, recall_score,precision_score
import numpy as np


def prprc(df):
  poly = PolynomialFeatures(degree=2, interaction_only=False, include_bias=False)
  poly_features = poly.fit_transform(df[['age', 'LDELTOTAL', 'LIMMTOTAL']])

  # Convert the poly_features array to a DataFrame
  poly_features_df = pd.DataFrame(poly_features, columns=poly.get_feature_names_out(['age', 'LDELTOTAL', 'LIMMTOTAL']), index=df.index)
  poly_features_df.drop(['age', 'age^2', 'LDELTOTAL', 'LIMMTOTAL'], axis=1, inplace=True)
  # Concatenate the new polynomial features with the original DataFrame
  df = pd.concat([df, poly_features_df], axis=1)
  return df

def metrics_merged(y_test, predictions, predictions_proba):
  conf_matrix = confusion_matrix(y_test, predictions)
  class_labels = range(len(conf_matrix))  # Assuming classes are 0, 1, ..., n-1
  mapping = {0: "Control", 1: "MCI", 2: "AD"}
  class_labels_txt = [mapping[x] for x in class_labels]
  specificity_scores = []
  npv_scores = []
  recall_scores = []
  auc_scores = []
  prec_scores = []

# Assuming predictions_prob contains the predicted probabilities (e.g., from a classifier with predict_proba)
  for i in class_labels:
      tn = conf_matrix.sum() - (conf_matrix[i].sum() + conf_matrix[:, i].sum() - conf_matrix[i, i])  # True Negatives
      fp = conf_matrix[:, i].sum() - conf_matrix[i, i]  # False Positives
      fn = conf_matrix[i].sum() - conf_matrix[i, i]  # False Negatives
      tp = conf_matrix[i, i]  # True Positives
    
    # Calculating Specificity
      specificity = tn / (tn + fp) if (tn + fp) > 0 else 0
    
    # Calculating NPV
      npv = tn / (tn + fn) if (tn + fn) > 0 else 0

    # calculating PPV/precision
      prec = tp / (tp + fp)
    
    # Calculating Recall (Sensitivity)
      recall = tp / (tp + fn) if (tp + fn) > 0 else 0
    
    # AUC for the current class (using predicted probabilities)
    # Make sure predictions_prob contains probability estimates for all classes
      auc_n = roc_auc_score((y_test == i).astype(int), predictions_proba[:, i]) if len(np.unique(y_test)) > 1 else np.nan

    # Append results to lists
      specificity_scores.append(specificity)
      npv_scores.append(npv)
      prec_scores.append(prec)
      recall_scores.append(recall)
      auc_scores.append(auc_n)

# Create DataFrame to store results
  metrics_df = pd.DataFrame({
      'Class': class_labels_txt,
      'Recall': recall_scores,
      "Precision": prec_scores,
      'Specificity': specificity_scores,
      'NPV': npv_scores,
      'AUC': auc_scores
  })
  return metrics_df

def metrics_binary(y_test, predictions, predictions_proba):
    # Compute confusion matrix for binary classification
    conf_matrix = confusion_matrix(y_test, predictions)
    tn, fp, fn, tp = conf_matrix.ravel()
    
    # Specificity
    specificity = tn / (tn + fp) if (tn + fp) > 0 else 0
    
    # Negative Predictive Value (NPV)
    npv = tn / (tn + fn) if (tn + fn) > 0 else 0
    
    # Precision (Positive Predictive Value)
    precision = precision_score(y_test, predictions)
    
    # Recall (Sensitivity)
    recall = recall_score(y_test, predictions)
    
    # AUC (using probability for the positive class)
    auc = roc_auc_score(y_test, predictions_proba[:, 1]) if len(np.unique(y_test)) > 1 else np.nan
    
    # Create a DataFrame to store results
    metrics_df = pd.DataFrame({
        'Metric': ['Recall', 'Precision', 'Specificity', 'NPV', 'AUC'],
        'Score': [recall, precision, specificity, npv, auc]
    })
    
    return metrics_df