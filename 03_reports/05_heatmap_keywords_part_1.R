# 20210812 Last updated
# 20180109 Helper function for heatmaps
print("###################### reports/05_heatmap_keywords_part_1.R")

#########################################
#########################################
# Functions for preparation and text tokenization
#########################################
#' @description
#' Agreggate textual contents by clusters. Ideal to merge the documents by clusters.
#' @param documents_vector LIST[STRINGS] The dataset column with the document text to be aggregated
#' @param cluster_assignation_vector LIST[INTEGERS] The dataset column with the cluster per document. usually X_C.
#' @param list_of_clusters LIST[INTEGERS] The list of cluster ids to use
#' @returns LIST[STRINGS] A list of size `list_of_clusters` with the aggregated text.
clusterBulkText <- function(documents_vector, cluster_assignation_vector, list_of_clusters) {
  lapply(list_of_clusters, function(i){
    paste(documents_vector[cluster_assignation_vector==i], collapse = " ")
  })
}

# Transforms text from dataset to a tm corpus
# Inputs:
# content_vector = a column, or character vector containing the text to clean
# useSteamming = Boolean, whether to use porter stemmer
# my_stopwords = A list of stopwords of my choice. like in c("this", "that")
# Outputs:
# A tm corpus object.
tidyText <- function(content_vector, useStemming = TRUE, myStopWords) {
  text <- SimpleCorpus(VectorSource(enc2utf8(content_vector)))
  text <- tm_map(text, content_transformer(tolower))
  text <- tm_map(text, removeWords, stopwords("english"))
  #text <- tm_map(text, removeWords, myStopWords)
  text <- tm_map(text, removePunctuation)
  text <- tm_map(text, removeNumbers)
  text <- tm_map(text, stripWhitespace)
  text <- tm_map(text, stripWhitespace)
  if (useStemming) {text <- tm_map(text, stemDocument, language = "english")}
  #text <- tm_map(text, removeWords, myStopWords)
  return(text)
}

# Transform back a tm corpus to normal text. Ideal to use after cleaning.
# Inputs:
# a_corpus = a tm corpus (e.g. created with function tidyText)
# Output:
# A character vector with the text re-assembled
corpusToText <- function(a_corpus){
  text <- unlist(sapply(1:length(a_corpus), function(x){return(a_corpus[[x]]$content)}))
  return(text)
}


#########################################
#########################################
# Functions for improved text mining
#########################################
# Splitting function
get_keywords_by_stopword_method <- function(content_vector, useStemming = TRUE, myStopWords) {
  
  # Patterns for the split
  my_pattern <- "--- | --- | ---|- | - | -|\\.|,|;|:|\\(|\\)|!|#|\\$|%|\\&|\\'|\\*|\\+|/|<|=|>|\\?|@|\\[|\\]|\\^|_|`|~|\\{|\\}|\\|"
  my_stops_space <- paste(" ", stopwords("english"), " ", sep = "") %>% paste(collapse = "|")
  my_stops_starts <- paste("^", stopwords("english"), " ", sep = "") %>% paste(collapse = "|")
  my_stops_ends <- paste(" ", stopwords("english"), "$", sep = "") %>% paste(collapse = "|")
  my_stops_colon <- paste(" ", stopwords("english"), ";", sep = "") %>% paste(collapse = "|")
  
  # Patterns to be removed
  my_specials <- "\\\\\\u|\\\\\\;"
  
  text <- SimpleCorpus(VectorSource(enc2utf8(content_vector)))
  text <- tm_map(text, content_transformer(tolower))
  
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_stops_space)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "", x)), my_stops_starts)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "", x)), my_stops_ends)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_pattern)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_specials)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_stops_colon)
  
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_stops_space)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "", x)), my_stops_starts)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "", x)), my_stops_ends)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_pattern)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_specials)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_stops_colon)
  
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_stops_space)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "", x)), my_stops_starts)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "", x)), my_stops_ends)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_pattern)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_specials)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_stops_colon)
  
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_stops_space)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "", x)), my_stops_starts)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "", x)), my_stops_ends)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_pattern)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_specials)
  text <- tm_map(text, content_transformer(function(x, a_pattern) gsub(a_pattern, "; ", x)), my_stops_colon)
  
  text <- tm_map(text, removeWords, myStopWords)
  text <- tm_map(text, removeNumbers)
  text <- tm_map(text, stripWhitespace)
  text <- tm_map(text, stripWhitespace)
  if (useStemming) {text <- tm_map(text, stemDocument, language = "english")}
  text <- tm_map(text, removeWords, myStopWords)
  return(text)
}

#########################################
#########################################
# Functions for working with corpus
#########################################
# Create Document Term Matrix TFIDF
# Input: a tm corpus object
# Output: a tdm object (sparse matrix from the slam package)
tdmTfidf <- function(a_corpus) {
  #return(TermDocumentMatrix(a_corpus, control = list(weighting = weightTfIdf, dictionary = custom_dictionary)))
  return(TermDocumentMatrix(a_corpus, control = list(weighting = weightTfIdf, bounds = list(global = c(5, Inf)))))
}


# Create Document Term Matrix TF
# Input: a tm corpus object
# Output: a tdm object (sparse matrix from the slam package)
tdmTf <- function(a_corpus) {
  #return(TermDocumentMatrix(a_corpus, control = list(weighting = weightTf, dictionary = custom_dictionary)))
  return(TermDocumentMatrix(a_corpus, control = list(weighting = weightTf, bounds = list(global = c(5, Inf)))))
}

# Gets a tdm and creates a list of clusters with its keywords sorted from the most relevant (either TF or Tfidf score)
# Inputs:
# a_tdm = a tdm object (e.g. from functions tdmTfifd, tdmTf, TermDocumentMatrix)
# list_of_clusters = the clusters to use
# a_top_limit = A threshold of the number of keyword to show
# Outputs:
# a list of data frames. The list is of length list_of_clusters. The data frames are of dimmension a_top_limit x 2. (the keyword and its score)
listOfKeywords <- function(a_tdm, list_of_clusters, a_top_limit) {
  lapply(list_of_clusters, function(i) {
    colindexes <- a_tdm$j == i
    keywordindexes <- a_tdm$i[colindexes]
    keywords <- a_tdm$dimnames$Terms[keywordindexes]
    # Back to normal. Remove the artificial separator
    keywords <- gsub("zqzq", " ", keywords)
    keywords <- trimws(keywords)
    # Scores
    tfidf_score <- a_tdm$v[colindexes]
    table_values <- data.frame(keywords, tfidf_score)
    table_values <- table_values[table_values[,1] != "circular economi",]
    table_values <- table_values[order(table_values[,2], decreasing = TRUE)[1:a_top_limit],]
    return(table_values)
  })
}


# A changing to "top_frac()" from dplyr. By default it rounds down.
# With this it rounds up, which is better for sampling because all clusters will have ate least 1 record in consideration
top_frac_ceiling <- function (x, n, wt) 
{
  top_n(x, ceiling({
    {
      n
    }
  } * n()), {
    {
      wt
    }
  })
}

