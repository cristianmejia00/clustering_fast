# 20190702

print("###################### reports/10_rcs_keywords.R")

# Per Ranjit petition we are creating a file where keyword are easier to read.
# To do so, we are concatenating the keywords as single string, and each keyword will be separated by a semicolon.
# The resulting file will be a dataframe 
# nrow = number of topics. At this point is 100
# ncol = one per each type of keywords: exclusive, probable, ngram2.

# Additionally the names of frequent companies in each topic.

# We load the environment.
# The environment is saved in the one drive.
###################################################################
# Concatenate keywords
# Using the unified_keywords data frame
# Use those keywords with a normalized score of 0.5 or more

# Prepare data frame, as data table class
unified_keywords2 <- as.data.table(unified_keywords)
setnames(unified_keywords2, "word count", "word_count", skip_absent = TRUE)
unified_keywords2$normalized <- as.numeric(unified_keywords2$normalized)
unified_keywords2$word_count <- as.numeric(unified_keywords2$word_count)
unified_keywords2$word_count[is.na(unified_keywords2$word_count)] <- 0

# auxiliary function to create the concatenated strings of keywords
# 1 per topic in the data table.
# Parameters:
# a_unified_dt = a data table. Must have the column topic, term, normalized, word count, and type. 
#               "normalized" goes from 1 to 100
#               "word count" is 2, 3, or 4 (the size of the ngram) or NA
# score = The threshold of the normalized. Keywords with a normalization score higher than this will be concatenated.
# type = the type of keyword, can be "exclusive", "probable", or "ngram"
paste_keywords <- function(a_unified_dt, score = 60, ktype = "exclusive") {
  list_of_clusters <- sort(unique(a_unified_dt$cluster))
  word_c <- if (ktype == "ngram") {2} else {0}
  
  temp_keywords <- sapply(list_of_clusters, function(x) {
    temp <- a_unified_dt[cluster == x][type == ktype][normalized >= score][word_count == word_c]$term
    concatenated_str <- if (length(temp) > 0) {
      paste(temp, collapse = "; ")
    } else {" "}
    return(concatenated_str)
  })
  return(temp_keywords)
}



############################################################################
concat_exclusive <- paste_keywords(unified_keywords2, 70, ktype = "exclusive")
concat_exclusive
concat_probable <- paste_keywords(unified_keywords2, 50, ktype = "probable")
concat_probable
#concat_ngram <- paste_keywords(unified_keywords2, 50, ktype = "ngram")
#concat_ngram
#rcs_complete$ngram2_keywords <- concat_ngram

rcs_complete <- rcs
rcs_complete <- rcs[order(rcs_complete$cluster),]
rcs_complete$most_unique_keywords <- concat_exclusive
rcs_complete$most_frequent_keywords <- concat_probable

rcs_complete$temp <- gsub("---$", "", rcs_complete$cluster_code)
rcs_complete$temp <- gsub("--$", "", rcs_complete$temp)

if (any(grepl("edges_level", ls()))) {
  if (level_report == 0) {rcs_complete <- merge(rcs_complete, edges_level1, by.x = "temp", by.y = "row.names", all.x = TRUE)}
  if (level_report == 1) {rcs_complete <- merge(rcs_complete, edges_level2, by.x = "temp", by.y = "row.names", all.x = TRUE)}
  if (level_report == 2) {rcs_complete <- merge(rcs_complete, edges_level3, by.x = "temp", by.y = "row.names", all.x = TRUE)}
  if (level_report == 3) {rcs_complete <- merge(rcs_complete, edges_level4, by.x = "temp", by.y = "row.names", all.x = TRUE)}
  
  rcs_complete$"E/N" <- round(rcs_complete$edg / rcs_complete$vert, 2)
} else {
  rcs_complete$edg <- 1
  rcs_complete$"E/N" <- 1
}

rcs_complete$cluster_year <- round(rcs_complete$cluster_year, 1)

rcs_complete$cluster_level <- level_report
rcs_complete <- rcs_complete[,intersect(colnames(rcs_complete),c("cluster", "cluster_level", "cluster_code", "cluster_year", "cluster_size", "edg", "E/N", "ave_cites", "label", 
                                "hub_year", "dmax", "hub_title",
                                "most_frequent_keywords", "most_unique_keywords", "WC", "SO", "AU", "Countries"))]

setnames(rcs_complete, 
         c("cluster", "cluster_level", "cluster_code", "cluster_year", "cluster_size", "edg", "E/N", "ave_cites", "label", 
           "hub_year", "dmax", "hub_title",
           "most_frequent_keywords", "most_unique_keywords", "WC", "SO", "AU", "Countries"),
         c("Cluster", "Clustering Level", "Cluster Code", "Ave. Year", "Nodes", "Edges", "E/N", "Ave. Cites", "RCS Classification", 
           "Hub Year", "Hub Cites", "Hub Title",
           "Frequent Keywords", "Unique Keywords", "WOS Categories", "Journals", "Authors", "Countries"),
         skip_absent=TRUE)


rcs_complete <- rcs_complete[order(rcs_complete$Cluster),]

write.csv(rcs_complete, 
          file = file.path(output_folder_level, "rcs_keywords.csv"),
          row.names = FALSE)
