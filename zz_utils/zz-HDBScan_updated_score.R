# 20230726
# Get the new score based on distance to centroid.
# This code is used after topic modeling with HDBSCan.
# https://colab.research.google.com/drive/15px5kU-oi7cV0Q6AKk1lkv-cZpBqMawB#scrollTo=H38MhBhnUeGx

# The output of HDBSCan includes:
# - X_C the topic
# - score the probability of a paper of belonging to the assigned cluster
# - outlier the probability of a paper of being an outlier despite its assignation
# - x and y, being the coordinates computed with UMAP.

# However, this `score` is different from the LDA score, in that MANY papers 
# within a cluster get a high score of 1. This is due the way HDBSCan work (idland metaphore)
# and thus, the score is not useful as a ranking method, given that potentially 100s of papers 
# will be 1. 

# It is said that for HDBScan the concept of a distance to centorids does not makes sense
# because it does not use the concept of dots (Thats for PCA, UMAP, etc.)
# However, not having other means I'm using the centroids here as reference based on their UMAP 
# values (after all centroids make sense for UMAP)

# The formula is:
# new_score = score - outlier - distance to the cluster's centroid

# The distance to the centroid is normalized from 0 to 1, where 0 is the closest to the center.
# The normalization is relative to the cluster.

# This maximizes papers with high score, little chance of being outlier, and 
# that are near the center of the cluster in the 2D UMAP.

# INPUTS
# the dataset with the above mentioned columns, and the UT column.

# Utils
linMap <- function(x, from, to) {
  (x - min(x)) / max(x - min(x)) * (to - from) + from
}


# Initialize
dataset$X_C_centroid_distance <- 0

# Find centroids and distances per paper
for(cluster in c(1:max(dataset$X_C))) {
  cluster_data <- dataset[dataset$X_C == cluster, c("UT", "x", "y")]
  # Find the centroid
  centx <- mean(cluster_data$x, na.rm=TRUE)
  centy <- mean(cluster_data$y, na.rm=TRUE)
  # Compute distance to the centroid
  cluster_data$centroid <- sqrt((cluster_data$x - centx)**2 + (cluster_data$y - centy)**2)
  # Assign normalized values to main dataset
  dataset$X_C_centroid_distance[match(cluster_data$UT, dataset$UT)] <- linMap(cluster_data$centroid, 0, 1)
}

# distance to centroid
dataset$new_score <- dataset$score - dataset$outliers - dataset$X_C_centroid_distance
