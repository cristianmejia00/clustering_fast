print("###################### reports/08_all_keywords_report.R")

# Auxiliar function normalizer
linMap <- function(x, from, to) {
  # Shifting the vector so that min(x) == 0
  x <- x - min(x)
  # Scaling to the range of [0, 1]
  x <- x / max(x)
  # Scaling to the needed amplitude
  x <- x * (to - from)
  # Shifting to the needed level
  x + from
}


####################################################
# AUTHOR KEYWORDS -- NOT POSSIBLE FOR PATENT DATA -- INSTEAD DO NGRAMS BELLOW
###################################################
# author_keywords <- lapply(list_of_clusters, function(x){
#   cluster_data = subset(myDataCorrect, cluster_assignation_vector == x)
#   keywords <- TopKeywords(cluster_data, 15)
#   return (keywords)
# })
# names(author_keywords) <- list_of_clusters

###################################################
# NGRAMS
###################################################
# Get the top TI_AB Keywords by cluster
# ngrams_per_cluster <- lapply(list_of_clusters, function(x){
#   cluster_text_content <- paste(myText[cluster_assignation_vector==x], collapse = " ")
#   # cluster_ngram4 <- get.phrasetable(ngram(cluster_text_content, n = 4)) %>% .[.$freq >= 3,] %>% .[1:50,]
#   # if(nrow(cluster_ngram4) > 0) {
#   #   cluster_ngram4$ngram <- 4
#   # cluster_ngram4$normalized <- linMap(cluster_ngram4[,2], 1,100)}
#
#   cluster_ngram3 <- get.phrasetable(ngram(cluster_text_content, n = 3)) %>% .[.$freq >= 3,] %>% .[1:50,]
#   if(nrow(cluster_ngram3) > 0) {cluster_ngram3$ngram <- 3
#   cluster_ngram3$normalized <- linMap(cluster_ngram3[,2], 1,100)}
#
#   cluster_ngram2 <- get.phrasetable(ngram(cluster_text_content, n = 2)) %>% .[.$freq >= 3,] %>% .[1:50,]
#   if(nrow(cluster_ngram2) > 0) {cluster_ngram2$ngram <- 2
#   cluster_ngram2$normalized <- linMap(cluster_ngram2[,2], 1,100)}
#
#   cluster_ngram_total <-  rbind.fill(cluster_ngram3, cluster_ngram2)
#   cluster_ngram_total$ngrams <- trimws(cluster_ngram_total$ngrams)
#   cluster_ngram_total$type <- "ngram"
#   cluster_ngram_total$cluster <- x
#   return(cluster_ngram_total)
# })
#
# ngrams_per_cluster_df <-  rbind.fill(ngrams_per_cluster)
# names(ngrams_per_cluster_df) <- c("term", "freq", "prop",  "word count", "normalized", "type" ,"cluster")

# write.csv(ngrams_per_cluster_df, file=PROJECTngrams, row.names = FALSE)


##################################################
# Unify values
##################################################
# tf
cluster_keywords_tf_DF <- lapply(list_of_clusters, function(x) {
  temp <- cluster_keywords_tf[[x]]
  names(temp) <- c("term", "freq")
  temp$cluster <- x
  temp$type <- "tf"
  temp$normalized <- linMap(temp[, 2], 1, 50)
  return(temp)
})
cluster_keywords_tf_DF <- rbindlist(cluster_keywords_tf_DF)

# tfidf
cluster_keywords_tfidf_exclu_DF <- lapply(list_of_clusters, function(x) {
  temp <- cluster_keywords_tfidf[[x]]
  names(temp) <- c("term", "freq")
  temp$cluster <- x
  temp$type <- "tfidf"
  temp$normalized <- linMap(temp[, 2], 1, 50)
  return(temp)
})
cluster_keywords_tfidf_exclu_DF <- rbindlist(cluster_keywords_tfidf_exclu_DF)

# probable
ALL_Topics_DF <- lapply(c(1:length(ALL_Topics)), function(x) {
  temp <- ALL_Topics[[x]]
  temp$cluster <- x
  temp$type <- "probable"
  temp$normalized <- linMap(temp$relevance, 1, 100)
  temp$relevance <- NULL
  return(temp)
})
ALL_Topics_DF <- rbindlist(ALL_Topics_DF)

# exclusive
ALL_Topics_exclu_DF <- lapply(c(1:length(ALL_Topics)), function(x) {
  temp <- ALL_Topics_exclu[[x]]
  temp$cluster <- x
  temp$type <- "exclusive"
  temp$normalized <- linMap(temp$relevance, 1, 100)
  temp$relevance <- NULL
  return(temp)
})
ALL_Topics_exclu_DF <- rbindlist(ALL_Topics_exclu_DF)

# # Top documents
# top_papers_df <- lapply(list_of_clusters, function(x) {
#   temp <- myDataCorrect[cluster_assignation_vector == x, c("TI", "Z9")]
#   temp <- temp[order(-temp$"Z9"),][1:10,]
#   temp$cluster <- x
#   temp$type <- "title"
#   return(temp)
# })
# top_papers_df <- rbindlist(top_papers_df)
# setnames(top_papers_df, "TI","term")

# Unified report
unified_keywords <- rbind.fill(
  cluster_keywords_tf_DF,
  cluster_keywords_tfidf_exclu_DF,
  ALL_Topics_DF,
  ALL_Topics_exclu_DF
) # , top_papers_df, ngrams_per_cluster_df)


# setnames(unified_keywords, c("term", "freq", "cluster", "type", "normalized"), c("Term", "Freq", "Cluster", "Type", "Normalized"))

# Write report
write.csv(unified_keywords,
  file = rn$PROJECTKeywords_report,
  col.names = c("Term", "Freq", "Cluster", "Type", "Normalized"),
  row.names = FALSE
)
