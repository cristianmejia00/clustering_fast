# 20210812 Last updated

# This part was merged with "05_heatmap_keywords_optimized.R".
# Here, I separate it because there is no point the previous part multiple times
# If it comes from the same dataset.
# We just need to load the right objects.
print("###################### reports/05_heatmap_keywords_part_3.R")

#########################################
#########################################
print("-- Updating X_C in the sample")
#########################################
# The exploration is base on a sampling of the dataset, because it takes too long to compute in the complete set


# Subsequent sampling must be based on the current clustering solution
# And is dependent of the variable


if (level_report == 1) {
  myDataCorrect_SAMPLE$X_C <- n_cluster_listed[myDataCorrect_SAMPLE$subcluster_label1]
}
if (level_report == 2) {
  myDataCorrect_SAMPLE$X_C <- n_cluster_listed[myDataCorrect_SAMPLE$subcluster_label2]
}
if (level_report == 3) {
  myDataCorrect_SAMPLE$X_C <- n_cluster_listed[myDataCorrect_SAMPLE$subcluster_label3]
}


# Sampling sizes vary depending on level
my_thres <- c(100, 50, 10, 10)

if (nrow(myDataCorrect) > 10000) {
  myDataCorrect_SAMPLE2 <- myDataCorrect_SAMPLE %>%
    group_by(X_C) %>%
    top_n(my_thres[level_report + 1], X_E) %>%
    ungroup() # optionally use fractions %>% top_frac_ceiling(0.05, X_E)
} else {
  myDataCorrect_SAMPLE2 <- myDataCorrect_SAMPLE
}

# Create the assignation vector based on the current X_C
cluster_assignation_vector <- myDataCorrect_SAMPLE2$X_C
list_of_clusters <- cluster_assignation_vector %>%
  unique() %>%
  sort()
a_top_limit <- 100 # Number of keywords to write on report


#########################################
#########################################
print("-- Cluster aggregation and cluster keywords")
#########################################
clustersText <- myDataCorrect_SAMPLE2$papersText %>%
  clusterBulkText(
    cluster_assignation_vector = cluster_assignation_vector,
    list_of_clusters = list_of_clusters
  ) %>%
  unlist()

cluster_contents_corpus <- tidyText(clustersText,
  useStemming = FALSE,
  myStopWords = settings$stopwords$myStopWords
)

# Cluster Keywords
cluster_keywords_tf <- cluster_contents_corpus %>%
  tdmTf() %>%
  listOfKeywords(
    .,
    list_of_clusters,
    a_top_limit
  )

cluster_keywords_tfidf <- cluster_contents_corpus %>%
  tdmTfidf() %>%
  listOfKeywords(
    .,
    list_of_clusters,
    a_top_limit
  )


#########################################
#########################################
print("-- Saving cluster keywords")
#########################################
# Save as R object
save(cluster_keywords_tf,
  cluster_keywords_tfidf,
  file = rn$PROJECTKeywords
)
