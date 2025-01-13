# Load necessary libraries
library(e1071)         # For SVC
library(caret)         # For splitting and evaluation
library(pROC)          # For ROC Curve and AUC
library(ggplot2)       # For visualizations

data <- br_data

# Convert the target variable to a factor (important for classification)
data$diagnosis <- as.factor(data$diagnosis)

# Exchange the spaces in column names with dots:
colnames(data) <- make.names(colnames(data))

# Split the dataset into training and test sets
set.seed(42)  # For reproducibility
train_index <- createDataPartition(data$diagnosis, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Scale the features (excluding the target variable) in both training and test sets
train_data_scaled <- train_data
test_data_scaled <- test_data

# Apply scaling to the features (excluding the 'diagnosis' column)
train_data_scaled[, -which(names(train_data) == "diagnosis")] <- scale(train_data[, -which(names(train_data) == "diagnosis")])
test_data_scaled[, -which(names(test_data) == "diagnosis")] <- scale(test_data[, -which(names(test_data) == "diagnosis")])


# Train the Support Vector Classifier model
set.seed(42)  # For reproducibility
svc_model <- svm(diagnosis ~ ., data = train_data_scaled, kernel = "radial", cost = 1, scale = TRUE, probability=TRUE)

# Print model summary
print(svc_model)

# Predict on the test set
svc_predictions <- predict(svc_model, test_data_scaled)

# Generate a confusion matrix
confusion <- confusionMatrix(svc_predictions, test_data_scaled$diagnosis)
print(confusion)

# Generate and print confusion matrix
confusion <- confusionMatrix(svc_predictions, test_data_scaled$diagnosis)
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
ggsave("confusion_matrix_plot_svc_R.png", width = 6, height = 4, bg='white')

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
write.csv(metrics, "svc_model_metrics_breast_cancer_R.csv", row.names = FALSE)


# Calculate AUC and plot ROC curve
svc_probabilities <- attr(predict(svc_model, test_data_scaled, probability = TRUE), "probabilities")[, 2]
roc_curve <- roc(test_data_scaled$diagnosis, svc_probabilities, levels = rev(levels(test_data_scaled$diagnosis)))
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 4)))
  
# Plot the ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve for SVC")

# Feature importance (via SVM coefficients)
# Extracting feature importance from the coefficients of the trained SVC model
importance <- abs(svc_model$coefs) * apply(svc_model$SV, 2, function(x) x %*% svc_model$coefs)
feature_importance <- colSums(importance)

# OPTIONAL: Visualize feature importance with ggplot2
importance_df <- data.frame(Feature = names(feature_importance), Importance = feature_importance)
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance (SVC)", x = "Features", y = "Importance")

#Tune-grid  defining of hyperparameters
# Split the dataset into training and test sets
set.seed(42)  # For reproducibility
train_index <- createDataPartition(data$diagnosis, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ] 


# Train the SVC model with grid search using the caret package svmRadial
set.seed(42)
svc_tune <- train(
  diagnosis ~ ., 
  data = train_data_scaled, 
  method = "svmRadial",            # Using radial kernel
  cost = c(0.1, 1, 10),           # Cost parameter values to try
  gamma = c(0.1, 0.5, 1),  # Hyperparameter grid
  trControl = trainControl(method = "cv", number = 5)  # Cross-validation
)

# View the best tuning parameters
print(svc_tune$bestTune)

# Evaluate the model on test set
svc_tune_predictions <- predict(svc_tune, test_data_scaled)
confusionMatrix(svc_tune_predictions, test_data$diagnosis)


# Train the SVC model with grid search using the caret package svmPoly:
set.seed(42)
svc_tune <- train(
  diagnosis ~ ., 
  data = train_data_scaled, 
  method = "svmPoly",   
  probability=TRUE, # Using radial kernel
  cost = c(0.1, 1, 10),           # Cost parameter values to try
  gamma = c(0.1, 0.5, 1, 3),  # Hyperparameter grid
  trControl = trainControl(method = "cv", number = 5)  # Cross-validation
)

# View the best tuning parameters
print(svc_tune$bestTune)

# Evaluate the model on test set
svc_tune_predictions <- predict(svc_tune, test_data_scaled)
confusionMatrix(svc_tune_predictions, test_data$diagnosis)


#Errors analysis:
# Make predictions using the trained SVC model
svc_predictions <- predict(svc_tune, newdata = test_data_scaled)

# Generate the confusion matrix
confusion <- confusionMatrix(svc_predictions, test_data_scaled$diagnosis)

# Print confusion matrix to the console
print(confusion)

# Access confusion matrix components
conf_matrix <- confusion$table
print(conf_matrix)

# Extract TP, TN, FP, FN
TP <- conf_matrix[2, 2]  # True Positives
TN <- conf_matrix[1, 1]  # True Negatives
FP <- conf_matrix[1, 2]  # False Positives
FN <- conf_matrix[2, 1]  # False Negatives

# Print detailed error analysis
cat("True Positives (TP):", TP, "\n")
cat("True Negatives (TN):", TN, "\n")
cat("False Positives (FP):", FP, "\n")
cat("False Negatives (FN):", FN, "\n")

# Calculate additional error metrics (optional)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)  # Accuracy
precision <- TP / (TP + FP)  # Precision (Positive Predictive Value)
recall <- TP / (TP + FN)  # Recall (Sensitivity or True Positive Rate)
f1_score <- 2 * (precision * recall) / (precision + recall)  # F1 Score

cat("\nAdditional Metrics:\n")
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

# Optionally: Get predictions with probabilities (if needed for ROC or AUC)
svc_probabilities <- predict(svc_tune, newdata = test_data, type = "prob")

# Create a data frame of true vs predicted labels
error_df <- data.frame(Actual = test_data$diagnosis, Predicted = svc_predictions)

# Create a confusion matrix plot
library(ggplot2)
ggplot(error_df, aes(x = Actual, y = Predicted)) +
  geom_point(width = 0.1, height = 0.1, color = "red") +
  labs(title = "True vs Predicted Labels (Errors)", x = "Actual", y = "Predicted")
# Make predictions using the trained SVC model
svc_predictions <- predict(svc_tune, newdata = test_data_scaled)

# Create a logical vector indicating where errors occurred
errors <- svc_predictions != test_data$diagnosis

# Subset the original test data where errors occurred
error_data <- test_data[errors, ]

# Print out the rows where errors occurred
print(error_data)

# Optionally, you can also check the actual predictions vs. true labels
error_comparison <- data.frame(
  Actual = test_data$diagnosis[errors],
  Predicted = svc_predictions[errors]
)

print(error_comparison)

# Custom summary function to include Accuracy, Precision, Recall, etc.
cv_control <- trainControl(method = "cv", number = 5,
                           savePredictions = "all", 
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary) 

# Example: To include more metrics such as Accuracy, you can use:
print(svc_tune$results)


# Plot Accuracy vs C and Scale
ggplot(svc_tune$results, aes(x = C, y = Accuracy, color = factor(scale))) +
  geom_line() +
  geom_point() +
  labs(title = "Accuracy vs. C and Scale", x = "C Parameter", y = "Accuracy") +
  theme_minimal()
# Train an SVC model on the training data
svc_model <- train(diagnosis ~ ., data = train_data_scaled, method = "svmPoly", trControl = trainControl(method = "cv", number = 5))

# Now perform 5-fold cross-validation
cv_results <- train(diagnosis ~ ., data = train_data_scaled, method = "svmPoly", trControl = trainControl(method = "cv", number = 5))

# Print results
print(cv_results)

