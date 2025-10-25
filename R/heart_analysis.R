# Read the .dat file
data <- read.table("heart.dat", header = FALSE)  
#Set column names 
colnames(data) <- c("age", "sex", "cp", "restbps", "chol", "fbs", "restecg",
                    "maxhr", "exang", "oldpeak", "slope", "majorvessels", "thal", "target")


# Load all necessary packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(caret)
library(randomForest)
library(pROC)
library(boot)



# Summary statistics and data structure
summary(data)
str(data)


#Understand the dataset through some graphical expolrations 
#Create a correlation plot


# Convert the 'target' variable to numeric to be able to use it in correlation plot
data$target_numeric <- as.numeric(as.character(data$target))

#Keep a factored version of the target variable for ggplots plots 
data$target <- factor(data$target, levels = c(1, 2), labels = c("Absent", "Present"))


# Select only numeric variables from the dataset for correlation analysis
numeric_vars <- data %>% select_if(is.numeric)
correlation_matrix <- cor(numeric_vars)

corrplot(correlation_matrix, 
         method = "circle", 
         type = "upper",
         tl.col = "black", 
         tl.srt = 45,
         addCoef.col = "black", # Adds correlation coefficients
         number.cex = 0.7,         
         number.digits = 2)

## Add a custom title to the correlation plot
title("Correlation Matrix Plot", 
      cex.main = 1.2,  
      line = 2.4)       




# Distribution of the target variable
ggplot(data, aes(x = target, fill = target)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  
  labs(title = "Distribution of Heart Disease", 
       x = "Heart Disease Diagnosis", 
       y = "Count",
       fill = "Presence of Heart Disease") +
  scale_fill_manual(values = c("skyblue", "salmon"), 
                    labels = c("Absent", "Present")) +
  theme_classic()


# Age distribution by target
ggplot(data, aes(x = age, fill = target)) +
  geom_density(alpha = 0.5) +
  labs(title = "Age Distribution by Heart Disease Status", 
       x = "Age", y = "Density",
       fill = "Presence of Heart Disease") +
  scale_fill_manual(values = c("skyblue", "salmon"), 
                    labels = c("Absent", "Present")) +
  theme_classic()




#Categcorical variables 

# Create a dataset with clinical labels for visualization
heart_clinical <- data %>% mutate(
  # Convert target to factor with clinical labels
  target = factor(target_numeric, 
                  levels = c(1, 2),
                  labels = c("Absent", "Present")),
  
  # Sex: 0 = female, 1 = male
  sex_label = factor(sex,
                     levels = c(0, 1),
                     labels = c("Female", "Male")),
  
  # Chest pain type: 1-4
  cp_label = factor(cp,
                    levels = c(1, 2, 3, 4),
                    labels = c("Typical Angina", "Atypical Angina", 
                               "Non-Anginal Pain", "Asymptomatic")),
  
  # Fasting blood sugar > 120 mg/dl: 0 = false, 1 = true
  fbs_label = factor(fbs,
                     levels = c(0, 1),
                     labels = c("≤ 120 mg/dl", "> 120 mg/dl")),
  
  # Resting ECG results: 0-2
  restecg_label = factor(restecg,
                         levels = c(0, 1, 2),
                         labels = c("Normal", "ST-T Abnormality", "LV Hypertrophy")),
  
  # Exercise induced angina: 0 = no, 1 = yes
  exang_label = factor(exang,
                       levels = c(0, 1),
                       labels = c("No", "Yes")),
  
  # ST segment slope: 1-3
  slope_label = factor(slope,
                       levels = c(1, 2, 3),
                       labels = c("Upsloping", "Flat", "Downsloping")),
  
  # Number of major vessels: 0-3
  majorvessels_label = factor(majorvessels,
                              levels = c(0, 1, 2, 3),
                              labels = c("0 Vessels", "1 Vessel", "2 Vessels", "3 Vessels")),
  
  # Thalassemia: 3 = normal, 6 = fixed defect, 7 = reversible defect
  thal_label = factor(thal,
                      levels = c(3, 6, 7),
                      labels = c("Normal", "Fixed Defect", "Reversible Defect"))
)

# Create a longer dataset for faceting with all labeled variables
heart_long <- heart_clinical %>%
  select(target, 
         "Sex" = sex_label,
         "Chest Pain Type" = cp_label,
         "Fasting Blood Sugar" = fbs_label,
         "Resting ECG Results" = restecg_label,
         "Exercise-Induced Angina" = exang_label,
         "ST Segment Slope" = slope_label,
         "Major Vessels (Fluoroscopy)" = majorvessels_label,
         "Thalassemia Type" = thal_label) %>%
  pivot_longer(-target, 
               names_to = "Clinical Factor", 
               values_to = "Value")

# Create the faceted plot with correct clinical labels
ggplot(heart_long, aes(x = Value, fill = target)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ `Clinical Factor`, scales = "free_x", ncol = 3) +
  labs(title = "Clinical Factors Associated with Heart Disease",
       subtitle = "Distribution of key cardiac risk factors by disease status",
       x = NULL,
       y = "Count",
       fill = "Heart Disease") +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "lightblue", color = "navy"),
    strip.text = element_text(face = "bold", color = "black"),
    legend.position = "bottom"
  )




##Numeric Variables 
#select numerical variables of interest with labels
heart_num_long <- heart_clinical %>%
  select(target,
         Age = age,
         "Resting BP" = restbps,
         Cholesterol = chol,
         "Max Heart Rate" = maxhr,
         "ST Depression (Oldpeak)" = oldpeak) %>%
  pivot_longer(-target, names_to = "Clinical Measure", values_to = "Value")

# Create faceted boxplots
ggplot(heart_num_long, aes(x = target, y = Value, fill = target)) +
  geom_boxplot(alpha = 0.8) +
  facet_wrap(~ `Clinical Measure`, scales = "free_y", ncol = 3) +
  labs(title = "Distribution of Clinical Measures by Heart Disease Status",
       subtitle = "Boxplots of continuous variables grouped by presence of heart disease",
       x = "Heart Disease",
       y = NULL,
       fill = "Heart Disease") +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, face = "italic"),
    strip.background = element_rect(fill = "lightblue", color = "navy"),
    strip.text = element_text(face = "bold", color = "black"),
    legend.position = "bottom"
  )




# Random Forest set up and data preparation
#set seed for reproducibility
set.seed(123)

# Split data: 70% training, 30% testing
train_idx <- sample(1:nrow(data), size = 0.7 * nrow(data))
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]


# CROSS-VALIDATION (INTERNAL VALIDATION)

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = "final"
)

rf_cv <- train(
  target ~ age + sex + cp + restbps + chol + fbs + restecg +
    maxhr + exang + oldpeak + slope + majorvessels + thal,
  data = train_data,
  method = "rf",
  trControl = ctrl
)

# Extract predictions
preds <- rf_cv$pred[rf_cv$pred$mtry == rf_cv$bestTune$mtry, ]
cv_cm <- confusionMatrix(preds$pred, preds$obs, positive = "Present")

# Performance metrics
cv_roc <- roc(preds$obs, preds$Present)
cv_auc <- round(auc(cv_roc), 3)
cv_ci <- ci.auc(cv_roc)

cv_performance <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "F1 Score", "AUC"),
  Value = c(
    round(cv_cm$overall["Accuracy"], 3),
    round(cv_cm$byClass["Sensitivity"], 3),
    round(cv_cm$byClass["Specificity"], 3),
    round(2 * ((cv_cm$byClass["Pos Pred Value"] * cv_cm$byClass["Sensitivity"]) /
                 (cv_cm$byClass["Pos Pred Value"] + cv_cm$byClass["Sensitivity"])), 3),
    cv_auc
  ),
  CI_Lower = c(NA, NA, NA, NA, round(cv_ci[1], 3)),
  CI_Upper = c(NA, NA, NA, NA, round(cv_ci[3], 3))
)

print(cv_performance) 


# FINAL MODEL ON TRAINING DATA

final_rf <- randomForest(
  target ~ age + sex + cp + restbps + chol + fbs + restecg +
    maxhr + exang + oldpeak + slope + majorvessels + thal,
  data = train_data,
  importance = TRUE
)

print(final_rf)

#Missclassifcation Rates Plot
plot(final_rf,
     main = "Misclassification Rates Over 500 Trees in the Random Forest Model")

legend("topright",
       legend = c("Out-of-Bag Error (Overall)",
                  "Heart Disease Absent",
                  "Heart Disease Present"),
       col = 1:3, lty = 1, bty = "n")



# TEST SET EVALUATION

# Generate predictions
test_pred <- predict(final_rf, newdata = test_data)
test_prob <- predict(final_rf, newdata = test_data, type = "prob")[, "Present"]

# Calculate direct metrics
test_cm <- confusionMatrix(test_pred, test_data$target, positive = "Present")
test_roc <- roc(test_data$target, test_prob)
test_auc <- auc(test_roc)
test_ci <- ci.auc(test_roc)

# Calculate F1 score
precision <- test_cm$byClass["Pos Pred Value"]
recall <- test_cm$byClass["Sensitivity"]
f1_score <- 2 * ((precision * recall) / (precision + recall))

# Create direct metrics table (including AUC)
performance_table <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", 
             "Positive Predictive Value", "Negative Predictive Value", "F1 Score", "AUC"),
  Value = c(
    round(test_cm$overall["Accuracy"], 3),
    round(test_cm$byClass["Sensitivity"], 3),
    round(test_cm$byClass["Specificity"], 3),
    round(test_cm$byClass["Pos Pred Value"], 3),
    round(test_cm$byClass["Neg Pred Value"], 3),
    round(f1_score, 3),
    round(test_auc, 3)
  )
)
print(performance_table)


# BOOTSTRAP CONFIDENCE INTERVALS

# Create a single function that returns all metrics at once
set.seed(123) 

boot_metrics <- function(data, indices) {
  sample_data <- data[indices,]
  preds <- predict(final_rf, newdata = sample_data)
  probs <- predict(final_rf, newdata = sample_data, type = "prob")[,"Present"]
  
  cm <- confusionMatrix(preds, sample_data$target, positive = "Present")
  roc_obj <- roc(sample_data$target, probs)
  
  prec <- cm$byClass["Pos Pred Value"]
  rec <- cm$byClass["Sensitivity"]
  f1 <- 2 * ((prec * rec) / (prec + rec))
  
  return(c(
    Accuracy = cm$overall["Accuracy"],
    Sensitivity = cm$byClass["Sensitivity"],
    Specificity = cm$byClass["Specificity"],
    PPV = cm$byClass["Pos Pred Value"],
    NPV = cm$byClass["Neg Pred Value"],
    F1 = f1,
    AUC = as.numeric(auc(roc_obj))
  ))
}

# Run bootstrap once for all metrics
set.seed(123)
boot_results <- boot(test_data, boot_metrics, R = 1000)

# Create table with results
metrics <- c("Accuracy", "Sensitivity", "Specificity", 
             "Positive Predictive Value", "Negative Predictive Value", "F1 Score", "AUC")

# Create combined performance table with direct values and bootstrap CIs
performance_table_with_ci <- data.frame(
  Metric = metrics,
  Value = performance_table$Value,  # Use direct calculation values
  CI_Lower = round(apply(boot_results$t, 2, quantile, probs = 0.025), 3),
  CI_Upper = round(apply(boot_results$t, 2, quantile, probs = 0.975), 3)
)
print(performance_table_with_ci)



# ROC CURVE FOR TEST SET
roc_df <- data.frame(
  specificity = test_roc$specificities,
  sensitivity = test_roc$sensitivities
)

# Use the direct AUC calculation in the plot with bootstrap CIs
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "#2E86C1", size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "ROC Curve for Heart Disease Prediction",
    subtitle = paste0("AUC = ", round(test_auc, 3),
                      ", 95% CI: ", 
                      performance_table_with_ci$CI_Lower[performance_table_with_ci$Metric == "AUC"], "-", 
                      performance_table_with_ci$CI_Upper[performance_table_with_ci$Metric == "AUC"]),
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  coord_equal() +
  theme_classic()


# VARIABLE IMPORTANCE
importance_df <- as.data.frame(importance(final_rf))
importance_df$Variable <- rownames(importance_df)
importance_df$ClinicalName <- sapply(importance_df$Variable, function(x) {
  switch(x,
         "age" = "Age",
         "sex" = "Sex",
         "cp" = "Chest Pain Type",
         "restbps" = "Resting Blood Pressure",
         "chol" = "Serum Cholesterol",
         "fbs" = "Fasting Blood Sugar",
         "restecg" = "Resting ECG",
         "maxhr" = "Maximum Heart Rate",
         "exang" = "Exercise-Induced Angina",
         "oldpeak" = "ST Depression",
         "slope" = "ST Segment Slope",
         "majorvessels" = "Major Vessels",
         "thal" = "Thalassemia Type", x)
})

ggplot(importance_df, aes(x = reorder(ClinicalName, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Clinical Risk Factors for Heart Disease",
    x = NULL,
    y = "Variable Importance"
  ) +
  theme_classic()
