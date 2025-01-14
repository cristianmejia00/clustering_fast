# 20210812 Last updated

# 20181203
# Plot the keywords of the clusters like in the topic model visualizer
print("###################### reports/06_citation_to_topic_model_converter.R")

# From the codes, we need:
# papersText OR myDataCorrect (to paste TI and AB)
# cluster_keywords_tf OR cluster_keywords_tfidf (Depending of the one decided)
# cluster_ID_label1_backup (When doing retrospective)

###############################################
# Get the clean text to feed the topic model
# Notes:
# - useStemming must be FALSE because "papersText" is the zqzq special vector with keywords already stemmed and pasted with "zqzq"
# - settings$stopwords$myStopWords is passed as legacy parameter, here we do not need them and not removal is done.
# myText <- papersText %>% tidyText(., useStemming = FALSE, settings$stopwords$myStopWords = settings$stopwords$myStopWords)

# For this to work it only makes sense to use the Term Frequencies, hence this to be forced to be FALSE.
# I tried experimentally and the results of using tfidf are not interpretable
# This assignation is done beyond this file, in 01_execute_and_reports.R; but here we forced to FALSE.
use_tfidf <- FALSE

#########################################
#########################################
print("-- Creating PHI table (Topics vs Keywords)")
#########################################
if (use_tfidf) {
  PHI <- lapply(cluster_keywords_tfidf, function(x) {
    x <- x[!is.na(x$tfidf_score), ]
    temp <- x[, 2] * 10000
    names(temp) <- x[, 1]
    temp <- temp[names(temp) != ""]
    temp <- as.data.frame(t(temp))
    if (ncol(temp) == 0) {
      temp$wildcard <- 0.00001
    }
    return(temp)
  }) %>% rbind.fill()
} else {
  PHI <- lapply(cluster_keywords_tf, function(x) {
    x <- x[!is.na(x$tfidf_score), ]
    temp <- x[, 2]
    names(temp) <- x[, 1]
    temp <- temp[names(temp) != ""]
    temp <- as.data.frame(t(temp))
    if (ncol(temp) == 0) {
      temp$wildcard <- 1
    }
    return(temp)
  })  %>%
    data.table::rbindlist(., fill = TRUE) %>%
    as.data.frame()
}

PHI[is.na(PHI)] <- 0

temp_dict <- colnames(PHI)

# To show natural text, use this. To show stem comment and do not run this.
my_vocab <- from_stem_to_raw[temp_dict] %>% unname()

# Remove my Stopwords
PHI <- PHI[, !(my_vocab %in% settings$stopwords$myStopWords)]
temp_dict <- colnames(PHI)


temp_dict <- gsub(" ", "zqzq", temp_dict)
colnames(PHI) <- temp_dict

#########################################
#########################################
print("-- Computing term frequency")
#########################################
# Creating the Document X Keywords table
ttt <- SimpleCorpus(VectorSource(enc2utf8(papersText)))
ttdm <- DocumentTermMatrix(ttt, control = list(weighting = weightTf, dictionary = temp_dict, bounds = list(global = c(5, Inf))))
term.frequency <- col_sums(ttdm) %>% sort(., decreasing = TRUE)
term.frequency <- term.frequency + 1 # To avoid inf values later

valid_names <- intersect(names(term.frequency), colnames(PHI))
term.frequency <- term.frequency[valid_names]
ttdm <- ttdm[, valid_names]
PHI <- PHI[, valid_names]

#########################################
#########################################
print("-- Creating THETA table (Documents vs Keywords)")
#########################################
# Creating the Document X Keywords table
list_of_clusters
THETA <- lapply(list_of_clusters, function(x) {
  sss <- as.character(cluster_keywords_tf[[x]]$keywords)
  sss <- gsub(" ", "zqzq", sss)
  colindexes <- which(ttdm$dimnames$Terms %in% sss)
  positions <- ttdm$j %in% colindexes
  documents <- ttdm$i[positions]
  scores <- ttdm$v[positions]
  vectt <- rep(0, dim(ttdm)[1])
  vectt[as.numeric(documents)] <- scores
  return(vectt)
})

THETA <- data.frame(THETA)
THETA[is.na(THETA)] <- 0
THETA <- THETA + 0.001
PHI <- PHI + 0.001

#########################################
#########################################
print("-- Visualization parameters")
#########################################

theta <- THETA / row_sums(THETA, na.rm = TRUE) # Doc x Topic, document topic matrix
phi <- PHI / row_sums(PHI, na.rm = TRUE) # Topic x Keywords

#########################################
# The vocabulary to be used in the visualization
# Here we get the stemed text that is in PHI columns
my_vocab <- gsub("zqzq", " ", colnames(PHI))

# To show natural text, use this. To show stem comment and do not run this.
my_vocab <- from_stem_to_raw[my_vocab] %>% unname()

my_vocab <- gsub("^a |^the ", "", my_vocab)
my_vocab <- gsub("^sustain$", "sustainability", my_vocab)
my_vocab <- gsub("^by product$", "production", my_vocab)
my_vocab <- gsub("^did models$", "models", my_vocab)
my_vocab <- gsub("[[:punct:]]", "", my_vocab) %>% trimws()


if (any(is.na(my_vocab))) {
  my_vocab[is.na(my_vocab)] <- paste("--", as.character(c(1:length(which(is.na(my_vocab))))), sep = "")
}

#########################################
#########################################
print("-- Creating visualization")
#########################################
# Use TSNE for multidimensional scalling when we have too many clusters as levels 1 or above
# For level 0 we use the default scaling
if (level_report <= 4) {
  myMethod <- if (level_report == 0 | nrow(myDataCorrect_SAMPLE2) <= 1500) {
    function(x) LDAvis::jsPCA(x)
  } else {
    function(x) Rtsne(as.matrix(x), perplexity = floor((nrow(as.matrix(x)) - 1) / 3), dims = 2)$Y
  }
  myScale <- if (level_report == 0 | nrow(myDataCorrect_SAMPLE2) <= 1500) {
    100
  } else {
    1000
  }

  # create the JSON object to feed the visualization
  json <- createJSON_cnet(
    phi = phi,
    theta = theta,
    doc.length = row_sums(ttdm)[as.numeric(rownames(theta))],
    vocab = my_vocab,
    term.frequency = col_sums(ttdm),
    mds.method = myMethod,
    plot.opts = list(xlab = "", ylab = ""),
    reorder.topics = FALSE
  )

  # Manually change values in the JSON.
  # I also could for instance, change the "createJSON" function instead.
  ttt <- RJSONIO::fromJSON(json)
  xxx <- table(myDataCorrect_SAMPLE$"X_C") %>% unname()
  xxx <- xxx / sum(xxx, na.rm = TRUE)
  ttt$mdsDat$Freq <- sqrt(xxx * myScale)
  json <- RJSONIO::toJSON(ttt)

  # Create the files
  serVis(json,
    out.dir = file.path(output_folder_level, 'keyword_explorer'), 
    open.browser = FALSE
  ) 
}

