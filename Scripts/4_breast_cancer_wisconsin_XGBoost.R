# Load necessary libraries
library(xgboost)
library(e1071)         # For SVC
library(caret)         # For splitting and evaluation
library(pROC)          # For ROC Curve and AUC
library(ggplot2)       # For visualizations

data <- br_data

# Convert the target variable to a factor (important for classification)
data$diagnosis <- as.factor(data$diagnosis)

# Exchange the spaces in column names with dots:
colnames(data) <- make.names(colnames(data))

# Split the data (same as before)
train_index <- createDataPartition(data$diagnosis, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Convert the data into a matrix (XGBoost expects matrix input)
train_matrix <- data.matrix(train_data[, -which(names(train_data) == "diagnosis")])
test_matrix <- data.matrix(test_data[, -which(names(test_data) == "diagnosis")])

# Convert target variable to numeric (1 = benign, 0 = malignant, for binary classification)
train_label <- as.numeric(train_data$diagnosis) - 1
test_label <- as.numeric(test_data$diagnosis) - 1

# Train the XGBoost model
xgb_model <- xgboost(
  data = train_matrix, label = train_label, 
  nrounds = 100, objective = "binary:logistic", 
  eval_metric = "logloss", 
  max_depth = 6, eta = 0.3, gamma = 0, 
  colsample_bytree = 0.8, subsample = 0.8
)
# Make predictions
xgb_predictions <- predict(xgb_model, test_matrix)
xgb_predictions <- ifelse(xgb_predictions > 0.5, 1, 0)  # Convert probabilities to classes

# Generate confusion matrix
confusionMatrix(as.factor(xgb_predictions), as.factor(test_label))

# Generate and print confusion matrix
confusion <- confusionMatrix(xgb_predictions, test_data$diagnosis)
print(confusion)
# Convert confusion matrix to a data frame for plotting
conf_matrix <- as.data.frame(confusion$table)
# Plot confusion matrix
ggplot(conf_matrix, aes(Prediction, Reference, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  labs(title = "Confusion Matrix", x = "Predicted", y = "Actual") +
  theme_minimal()+theme(
    # Title font size and face (bold, italic, etc.)
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    
    # Axis title font size and face
    axis.title.x = element_text( size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    
    # Axis text font size
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    
    # Legend title and text font size
    legend.title = element_text(size = 18, face = "italic"),
    legend.text = element_text(size = 12))

# Save the plot as an image
ggsave("confusion_matrix_plot_XGB_R.png", width = 6, height = 4, bg='white')

# Extract metrics from confusion matrix
metrics <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity"),
  Value = c(
    confusion$overall['Accuracy'],
    confusion$byClass['Sensitivity'],
    confusion$byClass['Specificity']
  )
)
# Print metrics
print(metrics)
# Save metrics as a CSV
write.csv(metrics, "xgb_model_metrics_breast_cancer_R.csv", row.names = FALSE)


# Calculate AUC
library(pROC)
roc_curve <- roc(test_label, xgb_predictions)
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 4)))

# Plot the ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve for XGBoost")

