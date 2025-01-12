# Breast_cancer_wisconsin
##Breast_cancer_wisconsin_classification_task

Objective: 
The aim of this project is to build a robust machine learning model to classify breast cancer diagnosis (Malignant or Benign) based on the Breast Cancer Wisconsin Dataset.

##Dataset Overview:
Source: UCI Machine Learning Repository and https://www.kaggle.com/datasets/uciml/breast-cancer-wisconsin-data .
Dataset contains 569 samples with 30 features representing various diagnostic measurements. Features are computed from a digitized image of a fine needle aspirate (FNA) of a breast mass. They describe characteristics of the cell nuclei present in the image.

Structure:

project-name/
│
├── notebooks/         # Jupyter Notebooks (.ipynb)
│   ├── Breast_cancer_wisconsin_1.ipynb
│   ├── 2_breast_cancer_wisconsin_rf.ipynb
│   ├── 3_breast_cancer_wisconsi_SVR.ipynb
│   ├── 4_breast_cancer_wisconsin_XGBoost.ipynb
│
├── scripts/           # Python and R scripts
│   ├── 1_breast_cancer_wisconsin_eda.R
│   ├── 2_breast_cancer_wisconsin_random_forest.R
│   ├── 3_breast_cancer_wisconsin_SVC.R
│   ├── 4_breast_cancer_wisconsin_XGBoost.R
│
├── data/                 # some obtained csv. statistical summaries, skewness, kurtosis, t_test results
│   ├── README.md         # Original data was downloaded from kaggle
│   ├── numeric_summary_breast_cancer.csv
│   ├── skewness_kurtosis_breast_cancer.csv
│   ├── t_test_results_breast_cancer.csv
│   ├── 
│

├── results/           # Stored results like metrics or plots
│   ├── All_boxplots_breast_cancer.png
│   ├── All_violinplot_breast_cancer.png
│   ├── All_densityplots_breast_cancer.png
│   ├── All_histograms_cancer_diagnosis.png
│   ├── Heatmap_best_breast_cancer.png
│   ├── barplot_breast_cancer_diagnosis.png
│

├── README.md          # Project overview
├── requirements.txt   # Python dependencies
├── LICENSE            # Licensing info (if 
