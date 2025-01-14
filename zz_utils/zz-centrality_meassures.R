# 20211227
# Add additional measures of centrality if requested
# Verify that the X_N in dataset_minimal corresponds to the names of the vectors in Page Rank.
# The length should correspond to the number of nodes in Fukan System, but not necesessarily with the rows
# in the dataset_minimal because we have added orphans or made other modifications to it.


## Create the object
centrality_scores <- data.frame(
  "X_N" = V(g1)$name,
  stringsAsFactors = FALSE
)

## Page Rank
if (settings$addons$page_rank) {
  print("...computing Page Rank")
  p_rank <- page.rank(g1, directed = FALSE)
  centrality_scores$Page_Rank <- p_rank$vector
}

## Eigen
if (settings$addons$eigen_centrality) {
  print("...computing Eigen centrality")
  eigen_score <- eigen_centrality(g1, directed = FALSE)
  centrality_scores$Eigen <- eigen_score$vector
}

## Closeness
if (settings$addons$closeness_centrality) {
  print("...computing Closeness centrality")
  closeness_score <- closeness(g1)
  centrality_scores$Closeness <- closeness_score
}

## Betweenness
if (settings$addons$betweeness_centrality) {
  print("...computing Betweenness centrality")
  betweenness_score <- betweenness(g1, directed = FALSE)
  centrality_scores$Betweenness <- betweenness_score
}


centrality_scores <- merge(centrality_scores,
  dataset_minimal[, c("X_N", "UT")],
  by = "X_N",
  all.y = TRUE
)
centrality_scores <- centrality_scores[order(centrality_scores$UT), ]
centrality_scores[is.na(centrality_scores)] <- 0


write.csv(centrality_scores,
  file = file.path(
    settings$analysis_metadata$bibliometrics_folder,
    settings$analysis_metadata$project_folder,
    settings$analysis_metadata$analysis_folder,
    "additional_centrality_scores.csv"
  ),
  row.names = FALSE
)
