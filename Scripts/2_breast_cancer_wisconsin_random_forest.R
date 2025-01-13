# Load necessary libraries
library(randomForest)  # For Random Forest
library(caret)         # For splitting and evaluation
library(pROC)          # For ROC Curve and AUC
library(ggplot2)       # For visualizations

data <- br_data

# Convert the target variable to a factor (important for classification)
data$diagnosis <- as.factor(data$diagnosis)

#exchange the spaces in columnn naems with dots:
colnames(data) <- make.names(colnames(data))

# Split the dataset into training and test sets
set.seed(42)  # For reproducibility
train_index <- createDataPartition(data$diagnosis, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train the Random Forest model
set.seed(42)  # For reproducibility
rf_model <- randomForest(diagnosis ~ ., data = train_data, ntree = 500, mtry = sqrt(ncol(train_data) - 1),
                         importance = TRUE)

# Print model summary
print(rf_model)

# Predict on the test set
rf_predictions <- predict(rf_model, test_data)

# Generate a confusion matrix
confusion <- confusionMatrix(rf_predictions, test_data$diagnosis)
print(confusion)

# Calculate AUC and plot ROC curve
rf_probabilities <- predict(rf_model, test_data, type = "prob")[, 2]
roc_curve <- roc(test_data$diagnosis, rf_probabilities, levels = rev(levels(test_data$diagnosis)))
auc <- auc(roc_curve)
print(paste("AUC:", round(auc, 4)))

# Plot the ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve for Random Forest")


# Generate and print confusion matrix
confusion <- confusionMatrix(rf_predictions, test_data$diagnosis)
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
ggsave("confusion_matrix_plot_rf_R.png", width = 6, height = 4, bg='white')


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
write.csv(metrics, "rf_model_metrics_breast_cancer_R.csv", row.names = FALSE)

# Feature importance
importance <- importance(rf_model)
varImpPlot(rf_model)

# OPTIONAL: Visualize feature importance with ggplot2
importance_df <- data.frame(Feature = rownames(importance), Importance = importance[, "MeanDecreaseGini"])
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance (Random Forest)", x = "Features", y = "Importance")

