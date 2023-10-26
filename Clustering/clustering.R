# Clustering (Heirarchical)
# https://www.r-bloggers.com/2016/01/hierarchical-clustering-in-r-2/

# Load the CSV file into a data frame
getwd()
setwd("/Users/audreyvolle/Desktop/School/MachineLearning/CropRecommendation/")
your_data <- read.csv("Crop_recommendation.csv")

clusters <- hclust(dist(your_data[, 3:4]))
plot(clusters)

#clusterCut <- cutree(clusters, 3)
