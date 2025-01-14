# 20230717

# Create bibliography.
# This is a custom version of "zz_utils/04a-wos_to_bibtext.R"

# transforms from a dataset to .
library(RefManageR)
library(tools)
library(stringr)

gsub("\\(.*\\)", "", news$HD[227]) %>% trimws()

# Inputs
# dataset <- dataset[!duplicated(dataset$UT),]
# Add the dates. We need the full date for newspapers
dataset <- merge(dataset,
  news[, c("AN", "month", "day")],
  by.x = "UT",
  by.y = "AN",
  all.x = TRUE,
  all.y = FALSE
)

# This allows names of orgs be displayed properly
dataset$BY_full2[dataset$BY == ""] <- paste(", ", dataset$BY_full, sep = "")[dataset$BY == ""]

# Path to .bib
file_name <- file.path(output_folder_level, "index_files", "bibliography.bib")

# Check columns
if (!all(c("TI", "SO", "date", "BY", "BY_full") %in% colnames(dataset))) {
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
      author = toTitleCase(roww$BY_full2),
      title = gsub("\\(.*\\)", "", roww$TI) %>% trimws(),
      journal = roww$SO %>% tolower() %>% toTitleCase(),
      year = if (is.na(roww$date)) {
        "1800"
      } else {
        as.character(roww$date)
      },
      # {paste(as.character(roww$PY), roww$month, as.character(roww$day), sep = '-')},
      # month = roww$month,
      # day = roww$day,
      type = "Newspaper Article"
    )
    return(bib)
  }) %>% as.BibEntry()
  WriteBib(bib_docs, file = file_name)
}

###################################
###################################
# Generate bibliography
###################################
dataset$AU_backup <- dataset$AU
dataset$AU <- dataset$BY
class(dataset$day)
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
dataset$citation_key

# We get the files that have a summary.
# Because those are the only ones we reference in the generated article
dataset_bibliography <- subset(dataset, summary != "")
write_refs_from_df(dataset_bibliography, file_name = file_name)
