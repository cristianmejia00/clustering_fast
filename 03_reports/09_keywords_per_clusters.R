# Code to create a single keyword summary sheet per cluster
# Outputs 1 file per topic, with all kind of keywords displayed in a single sheet.

for (i in c(1:length(ALL_Topics))) {
  probable <- subset(ALL_Topics_DF, cluster == i)
  probable$topic <- probable$relevance <- probable$type <- NULL
  setnames(probable,"term", "probable")
  
  exclu <- subset(ALL_Topics_exclu_DF, cluster == i)
  exclu$topic <- exclu$relevance <- exclu$type <- NULL
  setnames(exclu,"term", "exclusive")
 
  # ngrams <- subset(ngrams_per_cluster_df, cluster == i)
  # ngrams$topic <- ngrams$freq <- ngrams$prop <-NULL
  # 
  # ngram2 <- subset(ngrams, ngrams$`word count` == 2)
  # ngram2$type <- ngram2$`word count` <- NULL
  # setnames(ngram2, "term", "ngram-2")
  # ngram3 <- subset(ngrams, ngrams$`word count` == 3)
  # ngram3$type <- ngram3$`word count` <- NULL
  # setnames(ngram3, "term", "ngram-3")

  blank_col <- c("","","")
  names(blank_col) <- "   "
  
  temp <- cbind.fill(probable, blank_col, 
                     exclu,  blank_col,
                     # ngram2, blank_col, 
                     # ngram3, blank_col, 
                     fill = '')
  
  cluster_name <- paste(PROJECT, "_keyword_report.csv", sep = "")
  write.csv(temp, file = cluster_name, row.names = FALSE)
}
