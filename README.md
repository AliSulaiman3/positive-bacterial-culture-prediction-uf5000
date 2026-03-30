# Prediction of Positive Bacterial Cultures using UF-5000

This repository contains the R code used in a master's thesis for the development and validation of a predictive model based on UF-5000 flow cytometry data.

## Important note
The model predicts **positive bacterial cultures**, not clinically confirmed urinary tract infections (UTIs). Due to the absence of detailed clinical data, microbiological culture results were used as the reference standard.

The use of culture positivity as a proxy for UTI diagnosis represents a limitation and may not fully reflect clinical infection status.

## Contents
- Data preparation
- ROC analyses
- Cut-off determination
- Predictive modeling
- Model evaluation

## Data availability
The original clinical dataset is not publicly available due to privacy and data protection restrictions.

To run the analysis, a dataset with equivalent structure and variable names is required.
## How to run the analysis

1. Open R or RStudio
2. Install required packages:
```r
install.packages(c("tidyverse", "dplyr", "openxlsx", "readxl", "glue", "pROC", "randomForest", "rpart", "rpart.plot", "e1071", "xgboost", "tabnet", "caret"))
