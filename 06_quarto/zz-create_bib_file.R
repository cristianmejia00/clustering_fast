print("###################### zz-create_bib_file.R")

# 20230717

# Create bibliography.
# This is a custom version of "zz_utils/04a-wos_to_bibtext.R"

# transforms from a dataset to .
library(RefManageR)
library(tools)
library(stringr)

# Inputs
# dataset <- dataset[!duplicated(dataset$UT),]
dataset <- dataset
file_name <- file.path(output_folder_level, "index_files", "bibliography.bib")
file_name <- file.path(, "index_files", "bibliography.bib")
file.choose()
# Check columns
if (!all(c("TI", "AU", "PY", "SO", "VL", "IS", "BP", "DI") %in% colnames(dataset))) {
  stop("Necessary column for .bib is missing.")
}

# Utils
#' @description
#' Save all the articles in a dataset as a reference file `.bib`.
#' .bib files can be used in any reference manager like Zotero, and are needed for Latex and Markdown articles.
#' @param a_data_frame DATAFRAME. The dataset. (needed columns: TI, AU, PY, SO, VL, IS, BP, EP, DI, citation_key)
#' @param file_name STRING. The name of the output file including the extension `.bib`
write_refs_from_df <- function(a_data_frame, file_name = "references.bib") {
  bib_docs <- lapply(c(1:nrow(a_data_frame)), function(x) {
    roww <- a_data_frame[x, ]
    bib <- c(
      bibtype = "article",
      key = roww$citation_key,
      volume = roww$VL %>% gsub("V", "", .),
      number = roww$IS,
      pages = roww$BP %>% gsub("P|U", "", .),
      title = str_to_sentence(roww$TI),
      author = toTitleCase(roww$AU) %>% gsub(";", ". and", .) %>% paste(., ".", sep = ""),
      journal = roww$SO %>% tolower() %>% toTitleCase(),
      year = if (is.na(roww$PY)) {
        1800
      } else {
        roww$PY
      },
      doi = roww$DI
    )
    return(bib)
  }) %>% as.BibEntry()
  WriteBib(bib_docs, file = file_name)
}

###################################
###################################
# Generate bibliography
###################################

# Create the keys for citation analysis
citation_keys <- sapply(1:nrow(dataset), \(x) {
  if (dataset$AU[x] != "") {
    first_au <- gsub(",|;", "", dataset$AU[x]) %>%
      strsplit(" ") %>%
      unlist() %>%
      tolower() %>%
      .[[1]]
  } else {
    first_au <- "anon"
  }
  first_kwd <- tolower(dataset$TI[x]) %>%
    gsub("^the |^a |^an ", "", .) %>%
    gsub(" of | the | a | an | from | to | in | on ", " ", .) %>%
    strsplit(" ") %>%
    unlist() %>%
    .[c(1:2)] %>%
    gsub("[[:punct:]]", "", .) %>%
    paste(collapse = "-")
  tmp <- paste(first_au, as.character(dataset$PY[x]), first_kwd, sep = "-")
  tmp <- gsub('%|:|,|"|;', "", tmp)
  if (length(tmp) > 1) {
    print(x)
  }
  return(tmp)
})

citation_keys[duplicated(citation_keys)] <- paste(citation_keys[duplicated(citation_keys)], c(1:length(citation_keys[duplicated(citation_keys)])), sep = "")
dataset$citation_key <- citation_keys
# file_name = "/Users/cristian/Library/CloudStorage/OneDrive-Personal/Documentos/03-bibliometrics/Q302 human augmentation (core)/001/level0/index_files/bibliography.bib"
# We get the files that have a summary.
# Because those are the only ones we reference in the generated article
dataset_bibliography <- subset(dataset, summary != "")
write_refs_from_df(dataset_bibliography, file_name = file_name)
