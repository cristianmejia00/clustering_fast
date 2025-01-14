# 20210813 Last checked

# 20160120
# This code outputs a .csv file of the words with their relevance values
# This code depends on 06_citation_to_topic_model_converter.R

print("###################### reports/07_prob_exclu_keywords.R")

# INPUTS
K <- length(unique(myDataCorrect$X_C)) #K
term.frequency <- term.frequency
phi <- phi
my_vocab <- my_vocab
term.frequency

#########################################
#########################################
print("-- Parameters to compute Relevance")
#########################################
# Values inherited from the topic model codes.
# The following parameters are used by the next auxiliary function.
R <-  100
topic_seq <- rep(seq_len(K), each = R)
category <- paste0("Topic", topic_seq)
term.proportion <- term.frequency/sum(term.frequency)
phi <- t(phi)
lift <- phi / term.proportion
vocab <- my_vocab

# Auxiliary function.
find_relevance <- function(i) {
  relevance <- i*log(phi) + (1 - i)*log(lift)
  idx <- apply(relevance, 2, 
               function(x) order(x, decreasing = TRUE)[seq_len(R)])
  # for matrices, we pick out elements by their row/column index
  indices <- cbind(c(idx), topic_seq)
  data.frame(Term = vocab[idx], Category = category,
             logprob = round(log(phi[indices]), 4),
             loglift = round(log(lift[indices]), 4),
             relev = round(i*log(phi[indices]) + (1 - i)*log(lift[indices]), 4), #I added this line
             stringsAsFactors = FALSE)
}

#########################################
#########################################
print("-- Compute relevance: most probable")
#########################################
relevance_vector <- find_relevance(1)

topic_names <- unique(relevance_vector$Category)

#Vector of terms in topics
ALL_Topics <- lapply(topic_names, function(x) {
  topic_index <- which(relevance_vector$Category == x)
  term <- unlist(relevance_vector$Term[topic_index])
  relevance <- unlist(relevance_vector$relev[topic_index])
  relevance <- relevance + abs(min(relevance)) + 0.01 #Linear scaling, to have positive weigths always
  return (x = data.frame(term, relevance))})

names(ALL_Topics) <- sapply(1:length(ALL_Topics), function(x) paste("T", as.character(x)))

#write file
#write.csv(ALL_Topics, file=PROJECTtopwords, row.names = F)


#########################################
#########################################
print("-- Compute relevance: most exclusive")
#########################################
relevance_vector <- find_relevance(0.03)

topic_names_exclu <- unique(relevance_vector$Category)

#Vector of terms in topics
ALL_Topics_exclu <- lapply(topic_names_exclu, function(x) {
  topic_index <- which(relevance_vector$Category == x)
  term <- unlist(relevance_vector$Term[topic_index])
  relevance <- unlist(relevance_vector$relev[topic_index])
  relevance <- relevance + abs(min(relevance)) + 0.01 #Linear scaling, to have positive weigths always
  return (x = data.frame(term, relevance))})

names(ALL_Topics_exclu) <- sapply(1:length(ALL_Topics_exclu), function(x) paste("T", as.character(x)))
test <- ALL_Topics_exclu[[1]]

#write file
#write.csv(ALL_Topics, file=PROJECTtopwordsexclusive, row.names = F)

