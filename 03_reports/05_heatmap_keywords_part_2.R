# 20210812 Last updated
print("###################### reports/05_heatmap_keywords_part_2.R")
zz_env$x02 <- ls()
#########################################
#########################################
print("-- SAMPLING")
#########################################
# When the dataset is too large., instead of computing this vector based on all text, we use a sample.
# The sample is the top x% most cited articles. (We are NOT using a random sample!)


# The main (first) sampling must be based on the finest granularity clustering we selected
# This is to avoid sampling articles which vocabulary is not reflected in the top articles of the parent cluster
if (settings$params$type_of_analysis == "citation_network") {
  if (settings$params$recursive_level == 0) {
    myDataCorrect$sampling_column <- myDataCorrect$"level0"
  }
  if (settings$params$recursive_level == 1) {
    myDataCorrect$sampling_column <- myDataCorrect$"subcluster_label1"
  }
  if (settings$params$recursive_level == 2) {
    myDataCorrect$sampling_column <- myDataCorrect$"subcluster_label2"
  }
  if (settings$params$recursive_level == 3) {
    myDataCorrect$sampling_column <- myDataCorrect$"subcluster_label3"
  }
}

if (settings$params$type_of_analysis == "topic_model") {
  myDataCorrect$sampling_column <- myDataCorrect$"level0"
}

sample_columns <- intersect(c('TI', 'AB', 'DE', 'ID','X_E','X_C','sampling_column',
                              'subcluster_label1','subcluster_label2','subcluster_label3'),
                            colnames(myDataCorrect))


# Sampling sizes vary depending on level
my_thres <- c(100, 50, 10, 10)

if (nrow(myDataCorrect) > 10000) {
  myDataCorrect_SAMPLE <- myDataCorrect[,sample_columns] %>%
    group_by(sampling_column) %>%
    top_n(my_thres[level_report + 1], X_E) %>%
    ungroup() # optionally use fractions %>% top_frac_ceiling(0.05, X_E)
} else {
  myDataCorrect_SAMPLE <- myDataCorrect[,sample_columns]
}


myDataCorrect$sampling_column <- NULL

# The fastest implementation is achieved with this:
# myDataCorrect_SAMPLE2 <- myDataCorrect %>% group_by("subcluster_label3") %>% top_frac_ceiling(0.1, X_E)
# But for this to work we need all cluster to be larger than 10 so that our tiny fraction is always 1 or more.
# Because this rounds to zero, some clusters are skipped. So we cannot use it here.

#########################################
#########################################
print("-- Merge keywords and get conversion vectors")
#########################################

# We are using title, abstract, DE, and ID keywords only.
# To use Abstract keywords we need to merge them with the title.
# In the case of using abstracs, is advised to remove copyright statements

# text preparation of each column
title_text <- tolower(iconv(myDataCorrect_SAMPLE$TI, "UTF-8", "UTF-8", sub = ""))
de_keywords <- myDataCorrect_SAMPLE$DE %>% tolower()
id_keywords <- myDataCorrect_SAMPLE$ID %>% tolower()

# Concatenate the strings
if (nrow(myDataCorrect_SAMPLE) > 100000) {
  # Use only the title for large datasets
  tiab_keywords_raw <- title_text
} else {
  # Otherwise use the title and abstract
  ab_text <- tolower(iconv(myDataCorrect_SAMPLE$AB, "UTF-8", "UTF-8", sub = ""))
  tiab_keywords_raw <- paste(title_text, ab_text, sep = ". ") %>% tolower()
}

tiab_keywords <- get_keywords_by_stopword_method(tiab_keywords_raw, 
                                                 myStopWords = settings$stopwords$myStopWords, 
                                                 useStemming = FALSE) %>% corpusToText()
tiab_keywords <- gsub("-", " ", tiab_keywords)
tiab_keywords <- gsub("(; )+", "; ", tiab_keywords)
all_keywords <- paste(tiab_keywords, de_keywords, id_keywords, sep = "; ")

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

# Get the unique raw keywords (raw = as they appear in the text)
all_unique_raw_keywords <- gsub(";$| ;$|^; |^ |^a | s | d |\\\\\\b", "", all_keywords) %>%
  strsplit(split = ";") %>%
  unlist() %>%
  trimws() %>%
  trimws() %>%
  table()
all_unique_raw_keywords <- all_unique_raw_keywords[!names(all_unique_raw_keywords) %in% c("", "NA", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")]
all_unique_raw_keywords <- data.frame(
  keywords = names(all_unique_raw_keywords),
  counts = as.numeric(all_unique_raw_keywords),
  stringsAsFactors = FALSE
)

# Get stems
from_raw_to_stem <- tidyText(all_unique_raw_keywords$keyword, 
                             useStemming = TRUE, 
                             myStopWords = settings$stopwords$myStopWords) %>% corpusToText()
names(from_raw_to_stem) <- all_unique_raw_keywords$keyword

# Conversion vector
from_stem_to_raw <- all_unique_raw_keywords$keyword[order(all_unique_raw_keywords$counts, decreasing = TRUE)]
from_stem_to_raw <- from_stem_to_raw[!duplicated(from_stem_to_raw)]
names(from_stem_to_raw) <- from_raw_to_stem[from_stem_to_raw]

#########################################
#########################################
print("-- Create clean and zqzq vectors")
#########################################
# THIS PART IS SLOW!!!
# Transform to stems
# Here, we need to consider on case-by-case basis if we want the full count of terms per document
# Or every term, once per document (Because it will be aggregated at the cluster level)
clean_keywords <- strsplit(all_keywords, "; ")
clean_keywords <- lapply(clean_keywords, function(x) {
  temp <- c(x)
  # temp <- unique(temp)
  temp <- from_raw_to_stem[temp]
  # Custom correction can also be done here
  # temp <- mesh_conversion_table$mesh_root_stem[match(temp, mesh_conversion_table$mesh_syn_stem)]
  temp <- temp[!is.na(temp)]
  temp <- paste(temp, collapse = "; ")
  return(temp)
}) %>% unlist()


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

# Attach to sample
myDataCorrect_SAMPLE$papersText <- papersText

# Save the objects
save(myDataCorrect_SAMPLE, 
     papersText, 
     from_raw_to_stem,
     from_stem_to_raw,
     file = file.path(output_folder_reports, 'papersText.rdata'))
