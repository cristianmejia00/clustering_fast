# 20190828
# Check requirements for the file myDataCorrect from where will we compute the reports

# Any file, passing through this code will have the following columns in the following format

# # INPUTS
# myDataCorrect
# settings$rp$top_documents
# settings$rp$column_labels
# rn$PROJECTarticlereport


print("###################### reports/01_document_report_with_abstract.R")

# Helper function to get urls from DOI
convert_doi_to_url <- function(a_list_of_DOI) {
  a_list_of_DOI[is.na(a_list_of_DOI)] <- ""
  sapply(a_list_of_DOI, function(x) {
    if (nchar(x) > 0) {
      paste("https://doi.org/", x, sep = "")
    } else {
      x
    }
  })
}

# Find which colnames exist.
# This define which columns and in which order they will appear in the article report
potential_columns <- c(
  "X_C", "cluster_code",
  "topic", "related_topics", "TD",
  "AU", "PY", "DI", "TI", "AB", "Z9", "X_E", "DE", "SO", "WC", "Countries", "sentiment", "sentiment_factor", "UT", "uuid",
  "global_degree", "global_in_degree", "global_page_rank"
)

if (level_report == 0) {
  potential_columns <- c(potential_columns, "level0_page_rank",  "level0_degree", "level0_in_degree" )
}

if (level_report == 1) {
  potential_columns <- c(potential_columns, "level1_page_rank",  "level1_degree", "level1_in_degree" )
}

# Retain what's available
columns_in_myDataCorrect <- intersect(
  potential_columns,
  colnames(myDataCorrect)
)

# Create the file
article_report <- myDataCorrect %>% select(all_of(columns_in_myDataCorrect))

## Format columns
# Format DOI
if ("DI" %in% colnames(article_report)) {
  article_report$DI <- convert_doi_to_url(article_report$DI)
}

# Replace NA by empty character
article_report[is.na(article_report)] <- ""

# Filter to the top_documents of each cluster
if (settings$rp$top_documents != 0) {
  article_report <- article_report %>%
    group_by(X_C) %>%
    top_n(settings$rp$top_documents, X_E)
}

# Clean the columns
article_report$Z9 <- as.integer(article_report$Z9)
article_report$Z9[is.na(article_report$Z9)] <- 0

article_report$PY <- as.integer(article_report$PY)
article_report$PY[is.na(article_report$PY)] <- median(article_report$PY, na.rm = TRUE)

# Order the report 
article_report <- article_report[order(article_report$X_C, 
                                       -article_report$X_E, 
                                       -article_report$Z9, 
                                       -article_report$PY),]

# Change colnames to natural names
setnames(article_report, 
         names(settings$rp$column_labels), 
         unname(settings$rp$column_labels) %>% unlist(), 
         skip_absent = TRUE)


# Write the article report
write.csv(article_report, 
          file = rn$PROJECTarticlereport, 
          row.names = FALSE)

# Cleaning up
rm('columns_in_myDataCorrect')

# Filter to the top_documents of each cluster
if (settings$rp$top_documents == 0) {
  article_report_20 <- article_report %>%
    group_by(`Cluster Code`) %>%
    top_n(20, Degree)
}

# Write the article report
write.csv(article_report_20,
          file = gsub('_report', '_report_20', rn$PROJECTarticlereport),
          row.names = FALSE)
