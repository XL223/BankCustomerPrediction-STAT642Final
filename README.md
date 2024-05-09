# Data Mining Course Final Project #
## Introduction ##
An individual data mining ranking competition on STAT-642 course (Data Mining for Business) on bank customer dataset, the competition was presented on Kaggle (https://www.kaggle.com/competitions/stat642-datamining-2023). I used R to clean, preproess, visualize, and predict if customer will open a new credict card or not. The model can be applied in the financial industry that distribute the customers two or more categories based on basic information and other behaviors.
## Dataset ##
The datasets from the Kaggle too, which include train set and test set, the train set has 28831 records, the test set has 12357 records.
## Data Preparation ##
1. Imputed missing values by kNN algorithm
2. Used one-hot-encoding on categorical variables, created new dummy variables
3. Normalized continuous variables
## Descriptive Analysis ##
Visualize dataset to display the trend and relationship between variables
## Model Prediction and Evaluation ##
1. Used training set to feed five different algorithms respectively
2. Evaluated models based on confusion matrix
