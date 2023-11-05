# Install all the required packages (if not already installed)
install.packages("Rtsne")
install.packages("ggplot2")

# Load the required packages
library(Rtsne)
library(ggplot2)

# Load the CSV file into a data frame
getwd()
setwd("/Users/audreyvolle/Desktop/School/MachineLearning/CropRecommendation/")
your_data <- read.csv("Crop_recommendation.csv")

# Perform t-SNE on your_data for different combinations of features

# original combination
tsne_out <- Rtsne(as.matrix(your_data[, c("N", "P", "K", "temperature")]))

# all features
tsne_out <- Rtsne(as.matrix(your_data[, c("N", "P", "K", "temperature", "humidity" , "ph", "rainfall")]))

# non NPK features
tsne_out <- Rtsne(as.matrix(your_data[, c("humidity" , "ph", "rainfall")]))

# soil characteristics 
tsne_out <- Rtsne(as.matrix(your_data[, c("N", "P", "K", "ph")]))

# weather conditions
tsne_out <- Rtsne(as.matrix(your_data[, c("humidity", "rainfall", "temperature")]))

# one feature test
tsne_out <- Rtsne(as.matrix(your_data[, c("ph")]))

# NPK elimination test
tsne_out <- Rtsne(as.matrix(your_data[, c("N", "P", "temperature", "humidity" , "ph", "rainfall")]))
tsne_out <- Rtsne(as.matrix(your_data[, c("N", "K", "temperature", "humidity" , "ph", "rainfall")]))
tsne_out <- Rtsne(as.matrix(your_data[, c("P", "K", "temperature", "humidity" , "ph", "rainfall")]))
tsne_out <- Rtsne(as.matrix(your_data[, c("N", "P", "K", "temperature", "humidity" , "ph", "rainfall")]))

# Create a data frame for t-SNE output
tsne_plot <- data.frame(x = tsne_out$Y[, 1], y = tsne_out$Y[, 2])

# Add 'label' column from the original data to tsne_plot
tsne_plot$label <- your_data$label

# Plotting the t-SNE plot using ggplot2
ggplot(tsne_plot, aes(x = x, y = y, color = label)) +
  geom_point(size = 3) +
  theme_minimal() +
  ggtitle("t-SNE Visualization") +
  xlab("t-SNE Dimension 1") +
  ylab("t-SNE Dimension 2")




