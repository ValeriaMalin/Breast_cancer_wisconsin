library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)
library(ggcorrplot)
library(reshape2)
library(RColorBrewer)
library(randomForest)
library(corrplot)
library(officer)

df <- br_data

#missing values
colSums((is.na(df)))
print(colSums)

#The counts of diagnoses:
counts <- df %>% group_by(diagnosis) %>% summarise (count=n(), concavity=mean(concavity_mean, na.rm=TRUE), smoothless_mean=mean(smoothness_mean, na.rm =TRUE))
print(counts)

#Column names:
colnames(df)

#Generating statistical summary
summary_data <- summary(df)

#Create a dataframe from summary -  Custom summary function:
custom_summary <- function(column) {
  if (is.numeric(column)) {
    # Numeric summary
    data.frame(
      Min = min(column, na.rm = TRUE),
      Q1 = quantile(column, 0.25, na.rm = TRUE),
      Median = median(column, na.rm = TRUE),
      Mean = mean(column, na.rm = TRUE),
      Q3 = quantile(column, 0.75, na.rm = TRUE),
      Max = max(column, na.rm = TRUE),
      NA_Count = sum(is.na(column)),
      stringsAsFactors = FALSE
    )
  } else if (is.factor(column) || is.character(column)) {
    # Categorical summary
    levels <- table(column, useNA = "ifany")
    na_count <- sum(is.na(column))
    summary_df <- as.data.frame(levels, stringsAsFactors = FALSE)
    summary_df$NA_Count <- na_count
    names(summary_df) <- c("Category", "Count", "NA_Count")
    return(summary_df)
  } else {
    # For other types
    data.frame(
      Unique_Values = length(unique(column)),
      NA_Count = sum(is.na(column)),
      stringsAsFactors = FALSE
    )
  }
}
# Apply the custom summary function to each column
summary_list <- lapply(df, custom_summary)
# Combine numeric summaries into a single data frame
numeric_summary <- do.call(rbind, lapply(df[sapply(df, is.numeric)], custom_summary))
numeric_summary$Variable <- names(df)[sapply(df, is.numeric)]
# Reorder columns to move 'Variable' to the first position
numeric_summary <- numeric_summary[, c("Variable", setdiff(names(numeric_summary), "Variable"))]
# Categorical summary: Keep as a list since levels may vary
categorical_summary <- summary_list[sapply(df, function(col) is.factor(col) || is.character(col))]
# Display the numeric summary
print(numeric_summary)
#  Save the numeric summary as a CSV
write.csv(numeric_summary, "numeric_summary_breast_cancer.csv", row.names = FALSE)
#  Flextable for numeric summary
flex_numeric <- flextable(numeric_summary) %>%
  theme_vanilla() %>%
  autofit()
flex_numeric

# Calculate skewness and kurtosis
stats_table <- data.frame(
  Feature = colnames(df[, sapply(df, is.numeric)]),
  Skewness = apply(df[, sapply(df, is.numeric)], 2, skewness),
  Kurtosis = apply(df[, sapply(df, is.numeric)], 2, kurtosis)
)
write.csv(stats_table, 'skewness_kurtosis_breast_cancer.csv')
#or save as a felxtable:
flex_table <- flextable(stats_table)
print(flex_table)
doc <- read_docx()
doc <- body_add_flextable(doc, flex_table)
print(doc, target = "statistical_summary_breast_cancer.docx")


#Correlation matrix:
cor_matrix <- cor(df[, sapply(df, is.numeric)])
print(cor_matrix)
# Open a PNG device to save the plot
png("corrplot_output_breast_cancer.png", width = 800, height = 800)
# Create the corrplot
corrplot(cor_matrix, method = "circle")
# Close the device to save the plot
dev.off()

#Exchange the spaces in column names with dots:
colnames(df) <- make.names(colnames(df))

#Bar plot of Diagnosis Counts
p <- ggplot(df, aes(x = diagnosis, fill = diagnosis)) +
  geom_bar() +
  ggtitle('Count of Malignant vs Benign')+
  theme(
    # Title font size and face (bold, italic, etc.)
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    
    # Axis title font size and face
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    
    # Axis text font size
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    
    # Legend title and text font size
    legend.title = element_text(size = 18, face = "italic"),
    legend.text = element_text(size = 12)
  )
print(p)
ggsave("barplot_breast_cancer_diganosis.png",  plot=p, width = 6, height = 4)

#Create a numeric variable for diagnosis
df <- df %>%
  mutate(diagnosis_numeric = ifelse(diagnosis == "M", 1, 0))

# Ensure 'diagnosis' is a factor
df$diagnosis <- as.factor(df$diagnosis)

# Exclude 'id' and 'diagnosis' from numeric columns
num_cols <- setdiff(names(df)[sapply(df, is.numeric)], c("id", "diagnosis_numeric"))
#Creating the boxplots for every features
# Reshape the data into long format
long_df <- df %>%
  pivot_longer(
    cols = all_of(num_cols), # Ensure only numeric columns are included
    names_to = "Variable",
    values_to = "Value"
  )
# Create the boxplot
p <- ggplot(long_df, aes(x = diagnosis, y = Value, fill = diagnosis)) +
  geom_boxplot(color = 'black', outlier.size = 0.7, outlier.color = 'red') +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, face = 'bold'), # Adjust x-axis label font size
    axis.text.y = element_text(size = 9, face = 'bold'), # Adjust y-axis label font size
    plot.title = element_text(size = 14, face = "bold"), # Adjust title font size
    legend.position = "top" # Place legend at the top
  ) +
  scale_fill_manual(values = c("B" = "steelblue", "M" = "salmon")) + # Custom colors for the groups
  labs(
    title = "Boxplots of All Numerical Features by Diagnosis",
    x = "Diagnosis",
    y = "Value",
    fill = "Diagnosis" # Legend title
  )
# Display the plot
print(p)
# Save the plot
ggsave("All_boxplots_breast_cancer.png", plot = p, width = 13, height = 13, bg = 'white')

# Create the violins plot
p <- ggplot(long_df, aes(x = diagnosis, y = Value, fill = diagnosis)) +
  geom_violin(color = 'black') +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, face = 'bold'), # Adjust x-axis label font size
    axis.text.y = element_text(size = 9, face = 'bold'), # Adjust y-axis label font size
    plot.title = element_text(size = 14, face = "bold"), # Adjust title font size
    legend.position = "top" # Place legend at the top
  ) +
  scale_fill_manual(values = c("B" = "steelblue", "M" = "salmon")) + # Custom colors for the groups
  labs(
    title = "Violinplots of All Numerical Features by Diagnosis",
    x = "Diagnosis",
    y = "Value",
    fill = "Diagnosis" # Legend title
  )
# Display the plot
print(p)
# Save the plot
ggsave("All_violinplots_breast_cancer.png", plot = p, width = 13, height = 13, bg = 'white')

# Create the density plots
p <- ggplot(long_df, aes(x = Value,  fill = diagnosis, color=diagnosis)) +
  geom_density(alpha=0.4) +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, face = 'bold'), # Adjust x-axis label font size
    axis.text.y = element_text(size = 9, face = 'bold'), # Adjust y-axis label font size
    plot.title = element_text(size = 14, face = "bold"), # Adjust title font size
    legend.position = "top" # Place legend at the top
  ) +
  scale_fill_manual(values = c("B" = "steelblue", "M" = "salmon")) + # Custom colors for the groups
  scale_color_manual(values = c("B" = "steelblue", "M" = "salmon")) + # Custom colors for the lines
  labs(
    title = "Density Plots for All the Numerical Features by Diagnosis",
    x = "Diagnosis",
    y = "Value",
    fill = "Diagnosis", # Legend title
    color= 'Diagnosis'
  )
# Display the plot
print(p)
# Save the plot
ggsave("All_densityplots_breast_cancer.png", plot = p, width = 13, height = 13, bg = 'white')



# Loop to create and save all histograms in separate files:
for (col in num_cols) {
  p <- ggplot(df, aes(x = .data[[col]])) +  # Use .data[[col]] for proper column referencing
    geom_histogram(bins = 20, fill = 'blue', color = 'black') +
    ggtitle(paste("Distribution of", col))
  
  # Save the plot to a file
  ggsave(filename = paste("plot_", col, ".png"), plot = p, width = 6, height = 4)
}
#Other way to show histograms -  faceting with ggplot2 - as one plot
long_df <- df %>% pivot_longer(cols = all_of(num_cols), names_to = "Variable", values_to = "Value")
p <- ggplot(long_df, aes(x = Value)) +
     geom_histogram(bins = 20, fill = 'blue', color = 'black') +
     facet_wrap(~ Variable, scales = "free") +
     theme_minimal() +
     theme(
      axis.text.x = element_text(size = 8, face='bold'),  # Rotate x-axis labels
      axis.text.y = element_text(size = 9, face='bold'),                        # Adjust y-axis label font size
      plot.title = element_text(size = 12, face = "bold")           # Adjust title font size
      )+
      labs(title = "Histograms of the all numerical features", x = "", y = "")
p
ggsave("All_histograms_cancer_diganosis.png",  plot=p, width = 13, height = 13, bg='white')   


#We need num_cols again - Introducing  num_cols contains the names of numeric columns:
num_cols <- names(df)[sapply(df, is.numeric)]  # Get all numeric columns (including id this time)
#Heatmap:
corr <- cor(df[, num_cols])   
p <- ggcorrplot(corr, method = "circle", lab = TRUE, title = "Correlation Heatmap")
ggsave("Heatmap_breast_cancer_diganosis.png",  plot=p, width = 13, height = 13, bg='white')


#Other heatmap wit ggplot:
# Calculate correlation matrix
correlation_matrix <- cor(df[sapply(df, is.numeric)])
# Melt the matrix for ggplot
melted_corr <- melt(correlation_matrix)
# Plot heatmap
ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  geom_text(aes(label=round(value,2)), size=3)+
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face='bold'),  # Rotate x-axis labels
    axis.text.y = element_text(size = 12, face='bold'),                        # Adjust y-axis label font size
    plot.title = element_text(size = 16, face = "bold")           # Adjust title font size
  ) +
  labs(title = "Correlation Heatmap", x = "", y = "")
ggsave("Heatmap_best_breast_cancer_diganosis.png", width = 12, height = 12, bg='white')

#Statistical test for Significance:
t_test_result <- t.test(df$radius_mean ~ df$diagnosis)
print (t_test_radius_mean)
#Mann-Whitney U test (if the data not normally distributed)
wilcox.test(df$radius_mean ~ df$diagnosis)

# Perform the Welch Two Sample t-test
t_test_result <- t.test(df$radius_mean ~ df$diagnosis)

# Extract the key values into a dataframe
t_test_summary <- data.frame(
  Variable = "radius_mean",
  t_statistic = round(t_test_result$statistic, 2),
  Degrees_of_Freedom = round(t_test_result$parameter, 2),
  p_value = format.pval(t_test_result$p.value, digits = 4, eps = 1e-4),
  Mean_Group_B = round(t_test_result$estimate[1], 2),
  Mean_Group_M = round(t_test_result$estimate[2], 2),
  CI_Lower = round(t_test_result$conf.int[1], 2),
  CI_Upper = round(t_test_result$conf.int[2], 2),
  Interpretation = ifelse(t_test_result$p.value < 0.05, 
                          "Significant difference", 
                          "No significant difference")
)
# View the summary
print(t_test_summary)
write.csv(t_test_summary, "t_test_summary_radius_mean.csv", row.names = FALSE)
#For all the feautres:
# Identify numerical features (excluding 'diagnosis')
num_cols <- names(df)[sapply(df, is.numeric)]
num_cols <- setdiff(num_cols, c("diagnosis_numeric", "id"))  # Exclude non-relevant columns

# Create an empty dataframe to store the results
t_test_results <- data.frame(
  Variable = character(),
  t_statistic = numeric(),
  Degrees_of_Freedom = numeric(),
  p_value = character(),
  Mean_Group_B = numeric(),
  Mean_Group_M = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  Interpretation = character(),
  stringsAsFactors = FALSE
)
# Iterate through each numerical feature
for (col in num_cols) {
  # Perform Welch Two Sample t-test
  t_test <- t.test(df[[col]] ~ df$diagnosis)
  
  # Append the results to the dataframe
  t_test_results <- rbind(
    t_test_results,
    data.frame(
      Variable = col,
      t_statistic = round(t_test$statistic, 2),
      Degrees_of_Freedom = round(t_test$parameter, 2),
      p_value = format.pval(t_test$p.value, digits = 4, eps = 1e-4),
      Mean_Group_B = round(t_test$estimate[1], 2),
      Mean_Group_M = round(t_test$estimate[2], 2),
      CI_Lower = round(t_test$conf.int[1], 2),
      CI_Upper = round(t_test$conf.int[2], 2),
      Interpretation = ifelse(t_test$p.value < 0.05, 
                              "Significant difference", 
                              "No significant difference")
    )
  )
}
# View the dataframe
print(t_test_results)
# Save the results to a CSV file
write.csv(t_test_results, "t_test_results_breast_cancer.csv", row.names = FALSE)



#Example of Pairplot - separate:
ggpairs(df, columns =c("radius_mean", "perimeter_mean", "area_mean"), aes (color=diagnosis))

#Example of Violin Plot - separate:
ggplot(df, aes(x = diagnosis, y = texture_mean, fill = diagnosis)) +
  geom_violin() +
  theme_minimal() +
  ggtitle('Violin Plot of Texture Mean by Diagnosis')

#Example of Density Plot - separate:
ggplot(df, aes(x = radius_mean, fill = diagnosis)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  ggtitle('Density Plot of Radius Mean by Diagnosis')

#Features Importances Plots:
# Exclude 'id' and 'diagnosis_numeric' 
df_excluded <- df[, !colnames(df) %in% c("id", "diagnosis")]
#Feature Importances with Random Forest:to use taget - diagnosis_numeric
model <- randomForest(diagnosis_numeric ~ ., data = df_excluded)
importance <- importance(model)
importance_df <- data.frame(
  feature = rownames(importance),
  importance = importance[, 1]
)
ggplot(importance_df, aes(x = reorder(feature, importance), y = importance)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  coord_flip() +
  ggtitle('Feature Importance')
ggsave("Feature_importances_breast_cancer_r.png", width = 13, height = 13, bg='white')

df_excluded <- df[, !colnames(df) %in% c("id", "diagnosis_numeric")] 
#Feature Importances with Random Forest: to use target - diagnosis as a factor (preferrable for classification tasks)
model <- randomForest(diagnosis ~ ., data = df_excluded)
importance <- importance(model)
importance_df <- data.frame(
  feature = rownames(importance),
  importance = importance[, 1]
)
ggplot(importance_df, aes(x = reorder(feature, importance), y = importance)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  coord_flip() +
  ggtitle('Feature Importance')+
  theme(
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
ggsave("Feature Importances Plot-breast_cancer_random_forest.png", width = 18, height = 12, bg='white')

print(model)
importance(model)
plot(model)
#Or the same but t´with the legend
# Extract error rates
error_rates <- model$err.rate
# Plot the error rates with a legend
matplot(error_rates, type = "l", lty = 1, col = c("black", "red", "green"),
        xlab = "Number of Trees", ylab = "Error Rate", main = "Error Rates by Class")
legend("topright", legend = colnames(error_rates), col = c("black", "red", "green"), lty = 1)


# Extract error rates
error_rates <- model$err.rate
#Feature importances:
# Open a PNG device to save the plot
png("cvarImpPlot_breast_cancer.png", width = 800, height = 800)
# Create the corrplot
varImpPlot(model)
# Close the device to save the plot
dev.off()

#PCA
pca_result <- prcomp(df_excluded[sapply(df_excluded, is.numeric)], center = TRUE, scale. = TRUE)
pca_data <- data.frame(pca_result$x, diagnosis = df_excluded$diagnosis)

ggplot(pca_data, aes(x = PC1, y = PC2, color = diagnosis)) +
  geom_point() +
  ggtitle('PCA of the Features')
