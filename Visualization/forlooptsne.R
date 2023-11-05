# For
# Install and load required packages (if not already installed)
if (!requireNamespace("Rtsne", quietly = TRUE)) {
  install.packages("Rtsne")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(Rtsne)
library(ggplot2)

# Load the CSV file into a data frame
your_data <- read.csv("Crop_recommendation.csv")

# Define different combinations of features
feature_combinations <- list(
  c("N", "P", "K", "temperature"),
  c("N", "P", "K", "temperature", "humidity", "ph", "rainfall"),
  c("humidity", "ph", "rainfall"),
  c("N", "P", "K", "ph"),
  c("humidity", "rainfall", "temperature"),
  c("ph"),
  c("N", "P", "temperature", "humidity", "ph", "rainfall"),
  c("N", "K", "temperature", "humidity", "ph", "rainfall"),
  c("P", "K", "temperature", "humidity", "ph", "rainfall"),
  c("N", "P", "K", "temperature", "humidity", "ph", "rainfall")
)

# Create and save t-SNE plots for different feature combinations
for (i in 1:length(feature_combinations)) {
  tsne_out <- Rtsne(as.matrix(your_data[, feature_combinations[[i]]]))
  tsne_plot <- data.frame(x = tsne_out$Y[, 1], y = tsne_out$Y[, 2])
  tsne_plot$label <- your_data$label
  plot_title <- paste("t-SNE Visualization - Features:", paste(feature_combinations[[i]], collapse = ", "))
  
  # Save the plot as an image file (optional)
  g <- ggplot(tsne_plot, aes(x = x, y = y, color = label)) +
    geom_point(size = 3) +
    theme_minimal() +
    ggtitle(plot_title) +
    xlab("t-SNE Dimension 1") +
    ylab("t-SNE Dimension 2")
  
  # Save the plot as an image file (optional)
  ggsave(paste0("tsne_plot_", i, ".png"), plot = g, width = 8, height = 6)
  
  # Print the plot (optional, can be removed if saving as an image)
  print(g)
}
