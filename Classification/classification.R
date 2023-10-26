# Classification with Naive Bayes Algorithm
# https://www.r-bloggers.com/2021/04/naive-bayes-classification-in-r/

library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

# Load the CSV file into a data frame
getwd()
setwd("/Users/audreyvolle/Desktop/School/MachineLearning/CropRecommendation/")
your_data <- read.csv("Crop_recommendation.csv")

head(your_data)

#xtabs(~N+label, data = your_data)

# Split the data into training and testing sets (70% training, 30% testing)
set.seed(123) # for reproducibility
sample_index <- sample(1:nrow(your_data), 0.7*nrow(your_data))
train_data <- your_data[sample_index, ]
test_data <- your_data[-sample_index, ]

# Build Naive Bayes model
nb_model <- naive_bayes(label ~ N + P + K + temperature + humidity + ph + rainfall, data = train_data)

# Extract only the features used in the model from the test_data
features <- c("N", "P", "K", "temperature", "humidity", "ph", "rainfall")
test_features <- test_data[features]

# Make predictions using the Naive Bayes model
predictions <- predict(nb_model, test_features)

# Evaluate the accuracy of the model
accuracy <- sum(predictions == test_data$label) / nrow(test_data) * 100
print(paste("Accuracy of Naive Bayes model: ", accuracy, "%"))

# Extract predicted class labels
predicted_labels <- as.character(predictions)

# Generate confusion matrix
confusion_matrix <- table(Actual = test_data$label, Predicted = predicted_labels)
print("Confusion Matrix:")
print(confusion_matrix)

# Convert confusion matrix to data frame and set column names
confusion_matrix_df <- as.data.frame.matrix(confusion_matrix)
confusion_matrix_df <- cbind(Actual = rownames(confusion_matrix_df), confusion_matrix_df)
rownames(confusion_matrix_df) <- NULL
colnames(confusion_matrix_df) <- c("Actual", "Predicted", "Freq")

# Plot the confusion matrix
ggplot(confusion_matrix_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(x = "Actual", y = "Predicted", fill = "Frequency") +
  ggtitle("Confusion Matrix")

# You can also calculate precision, recall, and F1-score if needed
precision <- confusion_matrix["rice", "rice"] / sum(confusion_matrix["rice", ])
recall <- confusion_matrix["rice", "rice"] / sum(confusion_matrix["rice", ])
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Precision: ", precision))
print(paste("Recall: ", recall))
print(paste("F1-score: ", f1_score))
