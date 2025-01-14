# 20221107

# Transform the RTF files obtained from FACTIVA with the code in R-codesFACTIVA
# Run 00, 01, and 02.
# This will create a `data_environ.rdata` file
# Load it here.

# Get the dataset object
dataset <- newsVOS

# Save it
save(dataset, file="data.rdata")

# Copy `data.rdata` file and paste it in its query folder in the bibliometrics folder.
# Usually, you'll need to create the query folder because is the first time we need it. 



# Remove citation network setting for news, in case we accidentally left it here.
# If we do not remove it it will cause problem creating the heatmap keywords
if (settings$params$type_of_dataset == "news" & exists("cno")) {
  rm(cno)
}


# Preparation for news
if (settings$params$type_of_dataset == "news") {
  if (all(dataset$UT == myDataCorrect$UT)) {
    print("append cols to dataset")
    if (all((!c("X_C", "cluster_code", "X_E", "related_topics") %in% colnames(dataset))) &
        all(c("X_C", "cluster_code", "X_E", "related_topics") %in% colnames(myDataCorrect))) {
      dataset <- cbind(dataset, myDataCorrect[, c("X_C", "cluster_code", "X_E", "related_topics")])
      dataset$level0 <- dataset$X_C
    } else {
      print("colnames of dataset seems to be OK.")
      dataset$level0 <- dataset$X_C
    }
  } else {
    print("column mismatch between dataset and myDataCorrect")
  }
}



# Faceted news datasets are a special case where documents do not have any sorting metric
# And thus we add 1 to X_E
if (settings$params$type_of_dataset == "news") {
  if (!exists("myDataCorrect")) {
    myDataCorrect <- dataset
    setnames(myDataCorrect, c('SCORE','Score'), c('score','score'), skip_absent = TRUE)
  }
  if (!settings$params$unit_of_analysis %in% c("topics", "topic", "clusters", "cluster")) {
    myDataCorrect$cluster_code <- myDataCorrect$X_C
    myDataCorrect$related_topics <- "" # This can be added with the neighbors of the network
    if ('score' %in% colnames(myDataCorrect)) {
      if (!'Z9' %in% colnames(myDataCorrect)) {
        myDataCorrect$Z9 <- myDataCorrect$score
      } else {
        myDataCorrect$Z9 <- 1
      }
      if (!'X_E' %in% colnames(myDataCorrect)) {
        myDataCorrect$X_E <- myDataCorrect$score
      } else {
        myDataCorrect$X_E <- 1
      }
    } 
  }
}



# Get countries column for news (In Factiva this is the RE regions column)
if (settings$params$type_of_dataset == "news") {
  if ("C1" %in% available_columns) {
    dataset$Countries <- dataset$C1
  }
}


# Add institutions column
if (!("Institutions") %in% available_columns) {
  if (settings$params$type_of_dataset == "news") {
    if ("ID" %in% available_columns) {
      dataset$Institutions <- as.character(getInstitutions(dataset$ID))
      dataset$Institutions <- gsub("NA", "", dataset$Institutions)
    }
  }
  if (settings$params$type_of_dataset == "papers") {
    if ("C1" %in% available_columns) {
      dataset$Institutions <- as.character(getInstitutions(dataset$C1))
    }
  }
}
