# Load necessary libraries
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071")
}

library(caret)
library(dplyr)
library(ROCR)
library(naivebayes)
library(e1071)
library(corrplot)
library(pROC)

# Load data set
getwd()
setwd("/Users/audreyvolle/Desktop/School/MachineLearning/CropRecommendation/CRDatasetswithLabels")

#data <- read.csv("Original_Labels.csv")
#data <- read.csv("Cucurbitales.csv")
#data <- read.csv("Fabales.csv")
#data <- read.csv("Faboideae.csv")
#data <- read.csv("Sapindales.csv")
#data <- read.csv("Vigna.csv")
#data <- read.csv("poales.csv"
#data <- read.csv("Order_Labels.csv")
data <- read.csv("Order_Labels_2.csv")

#features_of_interest <- c("N", "P", "K", "temperature")
features_of_interest <- c("N", "P", "K", "temperature", "humidity", "ph", "rainfall") 

summary(data)

# Classification

# Split the data set into training and testing sets
set.seed(123)
splitIndex <- createDataPartition(data$label, p = 0.8, list = FALSE)
train_data <- data[splitIndex, ]
test_data <- data[-splitIndex, ]

# Remove highly correlated features
cor_threshold <- 0.7
highly_cor_features <- findCorrelation(cor(train_data[, -8]), cutoff = cor_threshold)

# Remove highly correlated features from the training and test sets
train_data_filtered <- train_data[, c(features_of_interest, "label")]
test_data_filtered <- test_data[, c(features_of_interest, "label")]

# Train a Naive Bayes model on filtered data
nb_model_filtered <- naiveBayes(label ~ ., data = train_data_filtered)

# Make predictions on the filtered test set
predictions_filtered <- predict(nb_model_filtered, newdata = test_data_filtered)

# Convert predictions and reference to factors with the same levels
predictions_filtered_factor <- factor(predictions_filtered, levels = levels(as.factor(test_data_filtered$label)))

# Evaluate the model using confusion matrix
confusion_matrix_filtered <- confusionMatrix(predictions_filtered_factor, as.factor(test_data_filtered$label))
print(confusion_matrix_filtered)

# If you want to visualize ROC curve on filtered data for each class
roc_curves <- lapply(unique(test_data_filtered$label), function(class) {
  actual_class <- as.numeric(as.factor(test_data_filtered$label) == class)
  predicted_class <- as.numeric(predictions_filtered_factor == class)
  roc_values <- roc(actual_class, predicted_class)
  return(roc_values)
})

# Plot ROC curves for each class
colors <- rainbow(length(roc_curves))
legend_labels <- levels(as.factor(test_data_filtered$label))
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curves (Filtered Data)")

for (i in seq_along(roc_curves)) {
  lines(roc_curves[[i]], col = colors[i], lwd = 2)
}

legend("bottomright", legend = legend_labels, col = colors, lwd = 2)


# Calculate AUC on filtered data for each class
auc_values_filtered <- sapply(roc_curves, function(roc_curve) auc(roc_curve))
cat("AUC for each class (Filtered Data):\n")
print(auc_values_filtered)


# Extract confusion matrix
conf_matrix <- confusionMatrix(predictions_filtered_factor, as.factor(test_data_filtered$label))

# Extract precision, recall, and F1 score for each class
precision <- conf_matrix$byClass[, "Pos Pred Value"]
recall <- conf_matrix$byClass[, "Recall"]
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the metrics for each class
metrics <- data.frame(Precision = precision, Recall = recall, F1_Score = f1_score)
print(metrics)

# Calculate mean F1 score
mean_f1_score <- mean(metrics$F1_Score, na.rm = TRUE)

cat("Mean F1 Score for the entire dataset:\n")
print(mean_f1_score)
