# 20180109.
# Helper function for heatmaps

##################################
##################################
# Helper Functions
##################################
##################################
# Agreggate textual contents by clusters. Ideal to merge the documents by clusters.
# documents_vector = the column containing the text to use
# Inputs:
# cluster_assignation_vector = The column having the cluster ID per document
# list_of_cluster = The list of clusters to use
# Outputs:
# A list of type character of size list_of_clusters.
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
  text <- Corpus(VectorSource(content_vector))
  text <- tm_map(text, content_transformer(tolower))
  text <- tm_map(text, removeWords, stopwords("english"))
  text <- tm_map(text, removeWords, settings$stopwords$myStopWords)
  text <- tm_map(text, removePunctuation)
  text <- tm_map(text, removeNumbers)
  text <- tm_map(text, stripWhitespace)
  text <- tm_map(text, stripWhitespace)
  if (useStemming) {text <- tm_map(text, stemDocument, language = "english")}
  text <- tm_map(text, removeWords, myStopWords)
  return(text)
}

# Transfor back a tm corpus to normal text. Ideal to use after cleaning.
# Inputs:
# a_corpus = a tm corpus (e.g. created with function tidyText)
# Output:
# A character vector with the text re-assembled
corpusToText <- function(a_corpus){
  text <- unlist(sapply(1:length(a_corpus), function(x){return(a_corpus[[x]]$content)}))
  return(text)
}


# Create Document Term Matrix TFIDF
# Input: a tm corpus object
# Output: a tdm object (sparse matrix from the slam package)
tdmTfidf <- function(a_corpus) {
  #return(TermDocumentMatrix(a_corpus, control = list(weighting = weightTfIdf, dictionary = custom_dictionary)))
  return(TermDocumentMatrix(a_corpus, control = list(weighting = weightTfIdf)))
}


# Create Document Term Matrix TF
# Input: a tm corpus object
# Output: a tdm object (sparse matrix from the slam package)
tdmTf <- function(a_corpus) {
  #return(TermDocumentMatrix(a_corpus, control = list(weighting = weightTf, dictionary = custom_dictionary)))
  return(TermDocumentMatrix(a_corpus, control = list(weighting = weightTf)))
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

##################################
##################################
# 20191118
# Auxiliary functions for text preparation in keyword analysis
#######################################################################
library(plyr)
library(tm)
library(ngram)

#######################################################################
# Function to clean text.
# Input: A character string, or a character vector (e.g. a column of text)
# Output: The same input but cleaned. (No numbers, no punctuation, stopwords removed, lowercased)
# Dependencies: library(tm)
# Params: rm_punt. Loguc. "Remove punctuation"
#         useSteaming. Logic.
#         settings$stopwords$myStopWords. A Character vector with my choosen stopwords
get_tidy_text <- function(a_char_vector, rm_punct = TRUE, useStemming = TRUE, myStopWords = c()) {
  text <- Corpus(VectorSource(a_char_vector))
  text <- tm_map(text, content_transformer(tolower))
  text <- tm_map(text, removeWords, stopwords("english"))
  text <- tm_map(text, removeWords, settings$stopwords$myStopWords)
  if (rm_punct) {text <- tm_map(text, removePunctuation)}
  text <- tm_map(text, removeNumbers)
  text <- tm_map(text, stripWhitespace)
  text <- tm_map(text, stripWhitespace)
  if (useStemming) {text <- tm_map(text, stemDocument, language = "english")}
  text <- tm_map(text, removeWords, myStopWords)
  text <- unlist(sapply(1:length(text), function(x){return(text[[x]]$content)}))
  return(text)
}

#######################################################################
# Function to remove the copyright from abstracts.
# Input: A char string or vector. Ususally the abstracts from WOS
# Output: The imput without the copyritgh statements.
# Dependencies: None.
remove_copyright_statements <- function(a_text) {
  return(gsub(" [A-z0-9 ]*?\\(C\\).*$", "", a_text))
}

#######################################################################
# Simple Keyword extractor by spliting sentences in stopwords and punctuation.
# Input: A character vector
# Output: A character vector with the keywords parsed (They will be in lowercase)
# Dependencies: Library(tm)
get_keywords_by_stopword_method <- function(a_text){
  temp <- strsplit(tolower(a_text), " ")[[1]]
  sw_idx <- temp %in% stopwords("english")
  temp[sw_idx] <- "---"
  temp <- paste(temp, collapse = " ")
  temp <- strsplit(temp, "--- | --- | ---|- | - | -|\\.|,|;|:|\\(|\\)")
  temp <- sapply(temp, trimws)
  temp <- temp[temp != ""]
  return(temp)
}


##############################################################################################
##############################################################################################
##############################################################################################
# SAMPLE

# When the dataset is too large., instead of computing this vector based on all tex, we use a sample.
# The sample is the top 10% most cited articles. (We are NOT using a random sample!)
if (nrow(myDataCorrect) > 10000) {
  indd <- round(table(myDataCorrect$X_C) * 0.1, 0 )
  myDataCorrect_SAMPLE <- lapply(as.numeric(names(indd)), function(x) {
    cluster_data <- subset(myDataCorrect, X_C == x)
    if (nrow(cluster_data) > 1100) {
      cluster_data <- cluster_data[order(cluster_data$X_E, decreasing = TRUE),][c(1:indd[x]),]
    }
    return(cluster_data)
  }) %>% rbind.fill
} else {
  myDataCorrect_SAMPLE <- myDataCorrect
}

# Parameters
cluster_assignation_vector <- myDataCorrect_SAMPLE$X_C
list_of_clusters <- cluster_assignation_vector %>% unique %>% sort
a_top_limit <- 100 #Number of keywords to write on report

##############################################################################################
##############################################################################################
##############################################################################################
# We are using title, abstract, DE, and ID keywords only.
# To use Abstract keywords we need to merge them with the title.
# In the case of using abstracs, is advised to remove copyright statements

#if (level_report == 0) {
# Append Title and Abstract keywords
title_text <- tolower(iconv(myDataCorrect_SAMPLE$TI, "UTF-8", "UTF-8", sub=''))
ab_text <- remove_copyright_statements(tolower(iconv(myDataCorrect_SAMPLE$AB, "UTF-8", "UTF-8",sub='')))

title_keywords <- paste(title_text, ab_text, sep = ". ") %>% tolower()
title_keywords <- lapply(title_keywords, get_keywords_by_stopword_method)
title_keywords <- sapply(title_keywords, function(x) {
  return(paste(x, collapse = "; "))
})

# Create the vector containing the keywords for each paper. By combining DE and ID
all_keywords <- paste(myDataCorrect_SAMPLE$DE, myDataCorrect_SAMPLE$ID, sep = "; ") %>% tolower()
all_keywords <- paste(all_keywords, title_keywords, sep = "; ")

# Custom corrections: Remove hyphens and replace abbreviations.
all_keywords <- gsub("[[:digit:]]", "", all_keywords)
all_keywords <- gsub("\\'", "", all_keywords)
all_keywords <- gsub('\\"', "", all_keywords)
all_keywords <- gsub("-", " ", all_keywords)
all_keywords <- gsub("   ", " ", all_keywords)
all_keywords <- gsub("  ", " ", all_keywords)
all_keywords <- gsub(" ;", ";", all_keywords)
all_keywords <- gsub("; ce;", "; circular economy;", all_keywords)
all_keywords <- gsub("; lca;", "; life cycle assessment;", all_keywords)
all_keywords <- gsub("; life cycle assessment lca;", "; life cycle assessment;", all_keywords)

##############################################################################################
# Correspondence vectors
# Create the stem-to-normal conversion vector
all_unique_raw_keywords <- strsplit(all_keywords, "; ") %>% unlist %>% table
all_unique_raw_keywords <- data.frame(all_unique_raw_keywords, stringsAsFactors = FALSE)
colnames(all_unique_raw_keywords) <- c("keyword", "counts")

from_raw_to_stem <- get_tidy_text(all_unique_raw_keywords$keyword)
names(from_raw_to_stem) <- all_unique_raw_keywords$keyword

from_stem_to_raw <- all_unique_raw_keywords$keyword[order(all_unique_raw_keywords$counts, decreasing = TRUE)]
from_stem_to_raw <- from_stem_to_raw[!duplicated(from_stem_to_raw)]
names(from_stem_to_raw) <- from_raw_to_stem[from_stem_to_raw]

##############################################################################################
# Tidy vector
# THIS PART IS SLOW!!!
clean_keywords <- strsplit(all_keywords, "; ")
clean_keywords <- lapply(clean_keywords, function(x) {
  temp <- c(x)
  temp <- unique(temp)
  temp <- from_raw_to_stem[temp]
  # Custom correction can also be done here
  #temp <- mesh_conversion_table$mesh_root_stem[match(temp, mesh_conversion_table$mesh_syn_stem)]
  temp <- temp[!is.na(temp)]
  temp <- paste(temp, collapse = "; ")
  return(temp)
})
clean_keywords <- unlist(clean_keywords)

# A posteriori custom corrections.
# Repair keywords we found after running all code
clean_keywords <- gsub("life cycl assess lca|assess lca", "life cycl assess", clean_keywords)
clean_keywords <- gsub("; lca;", "; life cycl assess;", clean_keywords)
clean_keywords <- gsub("^lca;", "life cycl assess;", clean_keywords)
clean_keywords <- gsub("; lca$", "; life cycl assess", clean_keywords)

# Preparation steps needed for topic model map
papersText <- clean_keywords
papersText <- gsub("; ", "wxwxw", papersText)
papersText <- gsub(" ", "zqzq", papersText)
papersText <- gsub("wxwxw", " ", papersText)
papersText <- gsub(";", "", papersText)
papersText <- gsub("NA ", "", papersText)


##############################################################################################
# Execution
# Cluster contents
#papersText2 <- enc2utf8(papersText)
#settings$stopwords$myStopWords <- c()

cluster_contents <- papersText %>%
  clusterBulkText(cluster_assignation_vector = cluster_assignation_vector,
                  list_of_clusters = list_of_clusters) %>%
  tidyText(useStemming = FALSE,
           myStopWords = settings$stopwords$myStopWords)


#Cluster Keywords
cluster_keywords_tf <- cluster_contents %>%
  tdmTf() %>%
  listOfKeywords(.,
                 list_of_clusters,
                 a_top_limit)

cluster_keywords_tfidf <- cluster_contents %>%
  tdmTfidf() %>%
  listOfKeywords(.,
                 list_of_clusters,
                 a_top_limit)

##################################
##################################
# Save as R object
save(cluster_keywords_tf,
     cluster_keywords_tfidf,
     file = rn$PROJECTKeywords)
