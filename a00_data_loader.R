print("***************`a00_data_loader.R`***************")

###############################################################################

# Open a window to select the directory with the files to merge
myfolder <- file.path(
  settings$metadata$input_folder_path,
  settings$metadata$input_project_folder
)
paths_to_files <- list.files(
  path = myfolder,
  full.names = TRUE,
  pattern = "*.txt",
  recursive = TRUE
)

###########################################################################################
## Path to the Bibliometrics folder

# Read each file and store them in a vector
# fread sometimes fails when reading the header, what to do?
list_of_all_files <- lapply(paths_to_files, function(a_path) {
  data1 <- fread(a_path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8", quote = "")
  # data1 <- read_from_wos(a_path) # NOTE: DO NOT USE read_from_wos() from package OPNER5, it cut off the lines before finish it, hence it does not read PY and UT for all rows.
  # data1 <- read.table(a_path, sep = '\t', fill = TRUE, stringsAsFactors = FALSE, header = TRUE, check.names = FALSE, quote = "", comment.char="", encoding = "UTF-16")
  # data1 <- read.csv(a_path, stringsAsFactors = FALSE, check.names = FALSE)
  # data1 <- read.delim(a_path, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-16", sep = "\t")
  return(data1)
})

# Verify than the files have the expected number of rows: 500. Except for a few that were the tails.
plot(unlist(sapply(list_of_all_files, nrow))) # The number of rows in each file, mostly 500.

# Create the merged dataset
dataset <- rbind.fill(list_of_all_files)
dataset <- as.data.frame(dataset)
if (colnames(dataset)[1] == "V1") {
  colnames(dataset) <- c("PT", colnames(dataset)[3:length(colnames(dataset))], "END")
  dataset["END"] <- NULL
}

# check for possible errors
# Verify correct reading by inspecting the publication year.
# If several non numeric values are present, it means there, there was a problem reading the files.
names(dataset)[1:20]

############################################################################
############################################################################
# Filtering ----- ROWS
############################################################################
############################################################################

# Remove duplicated records
filter_label <- names(settings$filtering)

# Remove duplicates
if (settings$filtering[[filter_label]]$rows_filter$removed_duplicated_UT) {
  dataset <- dataset %>% filter(!duplicated(UT))
}

# A record without PY, EA, or CY can be NA or "" empty string. We normalize anything to NA
dataset$PY[dataset$PY == ""] <- NA
dataset$EA[dataset$EA == ""] <- NA
dataset$CY[dataset$CY == ""] <- NA

# Correct records without PY
table(dataset$PY)
table(is.na(dataset$PY))

# Apply most recent year to NAs
dataset$PY[is.na(dataset$PY)] <- settings$filtering[[filter_label]]$rows_filter$most_recent_year %>% as.numeric()

test <- sapply(dataset$PY, function(x) {
  return(nchar(as.character(x)))
}) %>% unname()

dataset <- dataset %>% filter(test == 4)

# Correct records without PY
table(dataset$PY)
table(is.na(dataset$PY))
dataset$TI[is.na(dataset$PY)]

dataset$PY <- as.character(dataset$PY)
for (i in c(1:ncol(dataset))) {
  dataset[, i] <- as.character(dataset[, i]) %>% enc2utf8()
}


############################################################################
############################################################################
# Appending Columns
############################################################################
############################################################################
# IDs
dataset <- dataset %>%
  mutate(
    X_N = c(1:n()),
    uuid = UUIDgenerate(n = n())
  )

# Countries and Institutions

# Load utils
source("zz_utils/zz_auxiliary_functions.R")


# Add Countries column
if ("C1" %in% colnames(dataset)) {
  dataset$Countries <- getCountries(dataset$C1)
  dataset$IsoCountries <- as.character(getIsoCountries(dataset$Countries))
  dataset$IsoCountries <- gsub("NA; |; NA$", "", dataset$IsoCountries)
  dataset$IsoCountries <- gsub("; NA", "", dataset$IsoCountries)
  print("Countries column has been added")
}

# Add institutions column
if ("C1" %in% colnames(dataset)) {
  dataset$Institutions <- as.character(getInstitutions(dataset$C1))
  print("Institutions column has been added")
}


############################################################################
############################################################################
# Edits
############################################################################
############################################################################
# Clean abstract
dataset$AB <- remove_copyright_statements(dataset$AB)
dataset$AB <- remove_word_counts_line(dataset$AB)


############################################################################
############################################################################
# Filtering ----- COLUMNS
############################################################################
############################################################################
available_columns <- intersect(colnames(dataset), c(unlist(settings$filtering[[filter_label]]$columns_filter$columns_selected), "Countries", "IsoCountries", "Institutions"))
dataset <- dataset %>%
  select(all_of(c("X_N", "uuid", available_columns)))


##########################
# Save the file
results_folder_path <- file.path(
  settings$metadata$output_folder_path,
  settings$metadata$project_folder,
  filter_label
)

dir.create(file.path(results_folder_path), showWarnings = FALSE)

write.csv(dataset,
  file = file.path(results_folder_path, "dataset_raw_cleaned.csv"),
  row.names = FALSE
)

# Write the filtering settings in the same location
writeLines(
  RJSONIO::toJSON(settings$filtering,
    pretty = TRUE,
    auto_unbox = TRUE
  ),
  file.path(results_folder_path, "filtering_settings.json")
)


############################################################################
# Run this only if there are many missing PY
#
# # Conference papers may have the year in CY
# # Early access paper may have the year in EA
# # So, we are going to complete from there
# idx <- is.na(dataset$PY) & !is.na(dataset$CY)
# dataset$PY[idx] <- sapply(dataset$CY[idx], function(x) {substr(x,nchar(x)-4, nchar(x))}) %>% as.numeric()
#
# idx <- is.na(dataset$PY) & !is.na(dataset$EA)
# dataset$PY[idx] <- sapply(dataset$EA[idx], function(x) {substr(x,nchar(x)-4, nchar(x))}) %>% as.numeric()
#
# table(is.na(dataset$PY))
# # A way to infer the PY of an article is by looking at the cited references. An article will cite articles
# # that were published on the same year of publication at most. i.e. by extracting the max year in the CR
# # We know that the paper was published in that or a posterior year. Hence, we infer here that it was published
# # On the same year of the last reference.
#
# ## An extra way to know the PY is by looking at the copyright year in the Abstract. (But I have not yet implemented this)
# idx <- is.na(dataset$PY) & dataset$CR != ""
# CR_years_available <- str_extract_all(dataset$CR[idx], "[[:digit:]]{4},")
# CR_max_year <- sapply(CR_years_available, function(x) {
#   tmp <- gsub(",", "", x)
#   tmp <- as.numeric(tmp)
#   if (length(tmp) > 0) {
#     tmp <- tmp[tmp > 0]
#     tmp <- tmp[tmp <= 2021]
#     return(max(tmp))
#   } else {
#     return (NA)
#   }
# })
# dataset$PY[idx] <- CR_max_year
#
# table(is.na(dataset$PY))

# Check NA UTs
# test <- dataset[!grepl("^WOS", dataset$UT),]

#
# # Correct records without UT
# table(grepl("^WOS", dataset$UT))
#
# idx <- !grepl("^WOS", dataset$UT)
# #dataset$UT[idx] <- paste(as.character(dataset$PY[idx]), gsub("[[:punct:]]| ", "", tolower(dataset$TI[idx])), sep = "") %>% sapply(., function(x) {substr(x,1,50)})
# dataset$UT
# # Remove files without UT
# #dataset <- dataset[!nchar(dataset$UT) != 19,]


#
# # Remove columns that are not used anywhere (i.e. let only those that are used or can be used)
# # Usable columns as of 20181201
# usable_columns <- c("PT", "AU", "TI", "SO", "LA", "DT", "DE", "ID", "AB", "C1", "OI", "AF",
#                     "RP", "FU", "FX", "CR", "NR", "TC", "Z9", "U1", "U2", "PU", "SN", "J9",
#                     "JI", "PY", "VL", "IS", "BP", "EP", "AR", "DI", "PG", "WC", "SC","UT")
