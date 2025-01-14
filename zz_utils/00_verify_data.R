# 20190828
# Check requirements for the file dataset from where will we compute the reports

#################################################################################
# Force to data frame object
dataset <- as.data.frame(dataset)


################################################################################
# Available columns at this point
available_columns <- colnames(dataset)

#################################################################################
# Append necessary columns when missing
# Critical
if (!("TI" %in% available_columns)) {
  print("ERROR: NO TITLE")
}
if (!("AB" %in% available_columns)) {
  print("ERROR: NO ABSTRACT")
}
if (!("X_C" %in% available_columns)) {
  print("ERROR: NO CLUSTER")
}

#################################################################################
# Solvable
if (!("X_E" %in% available_columns)) {
  if ("Z9" %in% available_columns) {
    dataset$X_E <- dataset$Z9
  }
}
if (!("PY" %in% available_columns)) {
  dataset$PY <- settings$rp$most_recent_year
}
if (!("DT" %in% available_columns)) {
  dataset$DT <- "Article"
}
if (!("Z9" %in% available_columns)) {
  dataset$Z9 <- 1
}


get_keywords_split <- function(a_column) {
  text<- a_column %>%
    tolower() %>%  # Convert text to lowercase
    removeNumbers() %>%  # Remove numbers
    removePunctuation() %>%  # Remove punctuation
    removeWords(stopwords("english")) %>%  # Remove English stopwords
    stripWhitespace() %>% # Remove extra whitespace
    str_replace_all("\\s+", "; ") 
  return(text)
}

if (!("DE" %in% available_columns)) {
  dataset$DE <- get_keywords_split(dataset$TI)
}
if (!("ID" %in% available_columns)) {
  dataset$ID <- get_keywords_split(dataset$TI)
}

#################################################################################
# Optional
if (!("WC" %in% available_columns)) {
  print("warning: no WC")
}
if (!("AU" %in% available_columns)) {
  print("warning: no AU")
}
if (!("DI" %in% available_columns)) {
  print("warning: no DI")
}
if (!("SO" %in% available_columns)) {
  print("warning: no SO")
}
# if (!("C1" %in% available_columns)) {
#   print("warning: no C1")
# }

#################################################################################
# Fix future years
# Near the end of the year we have papers having a PY on the next year. 
# Because they are already accepted and available online, but for printing and official date
# They will be published next year.

# From bibliometrics standpoint, these papers are already there. Hence we treat papers with a future
# year as if they were published this year. 
this_year <- format(Sys.Date(), "%Y") %>% as.numeric()
dataset$PY[is.na(dataset$PY)] <- this_year
future_year_papers <- sum(dataset$PY > this_year)
if (future_year_papers > 0) {
  print(glue('we found {future_year_papers} that will be published next year, and we treat them as published this year.'))
  dataset$PY[dataset$PY > this_year] <- this_year
}

##########################################################################
# Format classes
dataset$X_N <- as.numeric(as.character(dataset$X_N))
dataset$X_C <- as.numeric(as.character(dataset$X_C))
dataset$X_E <- as.numeric(dataset$X_E)
dataset$Z9 <- as.numeric(dataset$Z9)
dataset$PY <- as.numeric(as.character(dataset$PY))




