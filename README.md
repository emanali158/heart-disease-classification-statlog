# heart-disease-classification-statlog
Classification of heart disease using the StatLog dataset. Includes data exploration, model development, validation, and interpretation.
# Predicting Coronary Heart Disease Using Machine Learning

This project applies a **Random Forest classification model** to predict the presence of **Coronary Heart Disease (CHD)** using the *StatLog Heart dataset*.  
It demonstrates practical skills in **data wrangling, exploratory analysis, model development, and validation** using R.

📄 Full report available here: [Heart_Disease_Classification_Report.pdf](./Heart_Disease_Classification_Report.pdf)

---

## Overview

Cardiovascular disease remains a leading cause of death globally.  
This project explores how machine learning can identify complex, non-linear patterns in clinical data to improve early prediction of CHD risk.

---

##  Methods

- **Algorithm:** Random Forest  
- **Software:** R (v4.4.1)  
- **Libraries:** `randomForest`, `caret`, `pROC`, `ggplot2`, `dplyr`  
- **Validation:** 5-fold cross-validation and test-set evaluation  

---

##  Key Findings

- The model achieved an **AUC of 0.86** and **accuracy of ~80%**, with slightly higher specificity than sensitivity.  
- Top predictive features included **maximum heart rate**, **ST depression**, **chest pain type**, and **number of major vessels**.  
- Findings aligned with established clinical indicators of CHD.
