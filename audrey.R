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

# Load your dataset
getwd()
setwd("/Users/audreyvolle/Desktop/School/MachineLearning/CropRecommendation/")
data <- read.csv("Crop_recommendation.csv")

# Check for missing values
sum(is.na(data))

# Explore the dataset
summary(data)

# Check for correlations between variables
correlation_matrix <- cor(data[, -8])  # Exclude the 'label' column
corrplot::corrplot(correlation_matrix, method = "number")

# Sample a subset of observations for each label
set.seed(123)  # for reproducibility
sample_size_per_label <- 10  # Adjust as needed

sampled_data <- data %>%
  group_by(label) %>%
  sample_n(sample_size_per_label) %>%
  ungroup()

# Perform hierarchical clustering based on features
dist_matrix <- dist(sampled_data[, -8])  # Exclude the 'label' column
hclust_result <- hclust(dist_matrix, method = "ward.D2")

# Cut the dendrogram to get clusters
num_clusters <- 3  # You can adjust this based on your desired number of clusters
cluster_labels <- cutree(hclust_result, k = num_clusters)

# Combine labels based on clusters
cluster_combinations <- list(
  c("muskmelon", "watermelon"),
  c("pigeonpeas", "kidneybeans"),
  c("papaya", "jute"),
  c("mothbeans", "chickpeas", "lentil", "mungbean"),
  c("apple", "grapes"),
  c("pomegranate", "orange")
  # Add more clusters as needed based on your domain knowledge
)

# Update labels based on clusters
for (cluster_combination in cluster_combinations) {
  combined_label <- paste(cluster_combination, collapse = "_")
  sampled_data$label[sampled_data$label %in% cluster_combination] <- combined_label
}

# Check the distribution of labels after combining
table(sampled_data$label)

# Plot the hierarchical clustering dendrogram with custom labels
plot(hclust_result, main = "Hierarchical Clustering Dendrogram", sub = "", xlab = "", cex = 0.6, labels = sampled_data$label)

# Split the dataset into training and testing sets
set.seed(123)  # for reproducibility
splitIndex <- createDataPartition(data$label, p = 0.8, list = FALSE)
train_data <- data[splitIndex, ]
test_data <- data[-splitIndex, ]

# Remove highly correlated features
cor_threshold <- 0.7  # Adjust as needed
highly_cor_features <- findCorrelation(cor(train_data[, -8]), cutoff = cor_threshold)

# Remove highly correlated features from the training and test sets
train_data_filtered <- train_data[, -highly_cor_features]
test_data_filtered <- test_data[, -highly_cor_features]

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


# Calculate AUC on filtered data
auc_value_filtered <- performance(prediction_obj_filtered, "auc")@y.values[[1]]
cat("AUC (Filtered Data):", auc_value_filtered, "\n")

