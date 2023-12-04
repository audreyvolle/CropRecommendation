# Clustering (Hierarchical)
# https://www.r-bloggers.com/2016/01/hierarchical-clustering-in-r-2/

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

data <- read.csv("Original_Labels.csv")

features_of_interest <- c("N", "P", "K", "temperature", "humidity", "ph", "rainfall") 

summary(data)

# Check for correlations between variables
correlation_matrix <- cor(data[, -8])
corrplot::corrplot(correlation_matrix, method = "number")

# Sample a subset of observations for each label
set.seed(123)
sample_size_per_label <- 10

sampled_data <- data %>%
  group_by(label) %>%
  sample_n(sample_size_per_label) %>%
  ungroup()

# Perform hierarchical clustering based on features
dist_matrix <- dist(sampled_data[, -8]) 
hclust_result <- hclust(dist_matrix, method = "ward.D2")

# Cut the dendrogram to get clusters
num_clusters <- 3
cluster_labels <- cutree(hclust_result, k = num_clusters)

# Combine labels based on clusters
# cluster_combinations <- list(
#   c("muskmelon", "watermelon"),
#   c("pigeonpeas", "kidneybeans"),
#   c("papaya", "jute"),
#   c("mothbeans", "chickpeas", "lentil", "mungbean"),
#   c("apple", "grapes"),
#   c("pomegranate", "orange")
#   # Add more clusters
# )
# 
# # Update labels based on clusters
# for (cluster_combination in cluster_combinations) {
#   combined_label <- paste(cluster_combination, collapse = "_")
#   sampled_data$label[sampled_data$label %in% cluster_combination] <- combined_label
# }

# Combine labels based on clusters
cluster_combinations <- list(
  c("muskmelon", "watermelon"), #cucurbitoidaea
  c("pigeonpeas", "lentil", "kidneybeans", "blackgram", "mothbeans", "mungbean"), #Fabiodeae
  c("blackgram", "mothbeans", "mungbean"), #Vigna
  c("pigeonpeas", "lentil", "kidneybeans", "blackgram", "mothbeans", "mungbeans", "mungbean", "chickpea") #Fabales
  # Add more clusters
)

# Update labels based on clusters
for (cluster_combination in cluster_combinations) {
  combined_label <- paste(cluster_combination, collapse = "_")
  sampled_data$label[sampled_data$label %in% cluster_combination] <- combined_label
}


table(sampled_data$label)

plot(hclust_result, main = "Hierarchical Clustering Dendrogram", sub = "", xlab = "", cex = 0.6, labels = sampled_data$label)