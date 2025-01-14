# Create a direct citation network `ncol` file from WOS datasets


# Add a custom identifier to those articles without UT
blank_ut_index <- which(dataset$UT == "")
if (length(blank_ut_index) > 0 ){
  custom_id <- paste("wos:ccc", c(1:length(blank_ut_index)), sep = "")
  dataset$UT[blank_ut_index] <- custom_id
}

# Transform each article in the dataset to the "record" format:
# Author, Year, Publication name, vol, page, DOI
# AU, PY, J9, VL, BP, DI
first_author <- gsub("; .*$", "", dataset$AU) %>% gsub(",", "", .)
volume <- paste("V", dataset$VL, sep = "")
page <- paste("P", dataset$BP, sep = "")
article_records <- paste(first_author, dataset$PY, dataset$J9, volume, page, sep = ", ") 
# Cleansing
article_records <- gsub(", VNA|, P$", ",", article_records)
article_records <- gsub(", ,|,,", ",", article_records)
article_records <- gsub(", ,|,,", ",", article_records) #Yes, we need it twice.
article_records <- gsub(",$|, $", "", article_records)
article_records <- tolower(article_records)


# Valid articles to use: 
# If we would like to have a threshold then put it here
# e.g. Those mentioned at least by 2 articles in the dataset
valid_article_records <-  article_records # No thresholds

# Get and format the dois from the articles in the dataset.
article_dois <- dataset$DI %>% unlist %>% tolower
valid_article_dois <-  article_dois[article_dois!=""]

# Get the cited references
# For each article in the dataset, we get a list of the cited references from the CR column.
references <- strsplit(dataset$CR, "; ")


# Count the number of references per article
number_of_references <- sapply(references, length)

# Unlisted references. This unnest the reference object.
references_unlisted <- unlist(references) %>% tolower()
rm(references)

# Create a vector of article indexes (To be appended to the cited reference list)
# The index of a given article is repeated n times, where n is the number of references found in that article
# If the article does not have references, then a placeholder "numeric(0)" is returned. 
# The placeholders are dropped once unlisted, so they don't affect the computation.
indices <- sapply(c(1:length(article_records)), function(x){
  rep(x, number_of_references[x])
}) %>% unlist


# Create the association table.
# This is the table to be used to match the cited references and the articles in our dataset.
# First, we verify that indices and the references are of same length.
# Then, we transform both as a single data.frame
# Hence, the "article_index_ column tells us the article to which the "reference_records" belongs to.
if (length(indices) == length(references_unlisted)) {
  references_df <- data.frame("article_index" = indices, 
                              "reference_records" = references_unlisted, 
                              stringsAsFactors = FALSE)
}
rm(references_unlisted)

# DOIs are the last element of the reference. Not all articles have the DOI. 
# Here we separate the DOI and put it as a different column.
# It will help to match the cited references to the articles in our dataset by comparing
# either the formated citation or the DOI.
separated <- str_split_fixed(references_df$reference_records, ", doi ", 2)
references_df$reference_records <- separated[, 1]
references_df$reference_dois <- separated[, 2]
rm(separated)
format(object.size(references_df), units = "Gb")

# Let only valid references
# The list of cited references also contains references to paper outside our dataset (or even outside of WOS)
# The citation network approach we use in our lab, stablish connection to among the papers within our dataset only.
# Therefore, we must remove any other unncesery reference. As naturally it won't create a match.
references_df$is_valid_reference_record <- references_df$reference_records %in% valid_article_records
table(references_df$is_valid_reference_record)
references_df$is_valid_reference_doi <- references_df$reference_dois %in% valid_article_dois
table(references_df$is_valid_reference_doi)
references_df <- references_df[references_df$is_valid_reference_record|references_df$is_valid_reference_doi,]

###########
# Matching: Create the association between the papers in the dataset and their cited references.
# The objects to compare to are "article_records" and "article_dois" which must have the same size.
###########
# Find those references where only DOI is available
references_df$only_doi <- (!references_df$is_valid_reference_record) & references_df$is_valid_reference_doi
# Create a link from the reference to the article by using the "record" format
references_df$links_to_article_index[references_df$is_valid_reference_record] <- match(references_df$reference_records[references_df$is_valid_reference_record], article_records)
# Create a link from the reference to the article by using the DOI, for those articles where just DOI is available
references_df$links_to_article_index[references_df$only_doi] <- match(references_df$reference_dois[references_df$only_doi], article_dois)

# Compute the network file
ncol_file <- cbind(as.character(references_df$article_index), 
                   as.character(references_df$links_to_article_index)) %>% as.matrix 

# As data frame
network <- ncol_file %>% as.data.frame()



