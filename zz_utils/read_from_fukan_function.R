# Last updated 20210806
read_from_wos_2 <- function (path_to_file) {
  #my_data <- read.table(con, header = T, sep = "\t", fill = T, quote = "", row.names = NULL, stringsAsFactors = F, check.names = F) #Note. This causes missing information, due to truncation halfway through the abstract. It truncates it silently thus gives the appeareance of working fine.
  my_data <- fread(path_to_file, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
  my_data <- as.data.frame(my_data)
  colnames(my_data) <- c("PT", colnames(my_data)[3:length(colnames(my_data))], 
                         "END")
  my_data["END"] <- NULL
  return(my_data)
}


# Last updated 20210803
# Working fine.
read_from_fukan_2 <- function(data_folder, what = "facets") {
  # Function to read data from Fukan
  
  # Read the dataset
  if (what == "facets") {
    dataset <- read.table(paste(data_folder, "\\mission.facet.all.tsv", sep = ""), sep = '\t',fill = TRUE, stringsAsFactors = FALSE, header = FALSE, check.names = FALSE, quote = "")
  } else {
    dataset <- read.table(paste(data_folder, "\\mission.facet.orphan.tsv", sep = ""), sep = '\t',fill = TRUE, stringsAsFactors = FALSE, header = FALSE, check.names = FALSE, quote = "", comment.char="")
  }
  
  colnames(dataset) <- as.character(dataset[1,])
  dataset <- dataset[-1,]
  
  # Force as data.frame type
  dataset <- as.data.frame(dataset)
  
  # Adjust header names
  setnames(dataset, c('_N', '_C', '_D', '_E'), c('X_N', 'X_C', 'X_D', 'X_E'), skip_absent = TRUE)
  
  # Keep potentially usable columns
  usable_columns <- c('X_N', 'X_C', 'X_D', 'X_E',
                      "AU", "PY", "EA", "CY", "TI", "SO", "DE", "ID", "AB", "C1", "DT",
                      "CR",  "Z9", "VL", "IS", "BP", "DI",  "WC", "UT")
  # Optional columns: "PT", "LA", "OI", "AF", "RP", "FU", "FX","NR", "TC","U1", "U2", "PU","SN", "J9","JI","CY", "EP","PG","SC",
  
  available_usable_columns <- intersect(usable_columns, colnames(dataset))
  dataset <- dataset[,available_usable_columns]
  
  
  # Format classes
  numeric_columns <- intersect(c('X_N', 'X_C', 'X_D', 'X_E', "TC", "Z9", "PY"), available_usable_columns)
  for (i in numeric_columns) {
    dataset[,i] <- as.numeric(dataset[,i])
    print(i)
    print(class(dataset[,i]))
  }
  
  
  # A record without PY, EA, or CY can be NA or "" empty string. We normalize anything to NA
  dataset$PY[dataset$PY == "" | is.na(dataset$PY)] <- NA

  # Conference papers may have the year in CY 
  # Early access paper may have the year in EA
  # So, we are going to complete from there
  if ("EA" %in% colnames(dataset)) {
    dataset$EA[dataset$EA == "" | is.na(dataset$EA)] <- NA
    idx <- is.na(dataset$PY) & !is.na(dataset$EA)
    dataset$PY[idx] <- sapply(dataset$EA[idx], function(x) {substr(x,nchar(x)-4, nchar(x))}) %>% as.numeric()
  }
  
  if ("CY" %in% colnames(dataset)) {
    dataset$CY[dataset$CY == "" | is.na(dataset$CY)] <- NA
    idx <- is.na(dataset$PY) & !is.na(dataset$CY)
    dataset$PY[idx] <- sapply(dataset$CY[idx], function(x) {substr(x,nchar(x)-4, nchar(x))}) %>% as.numeric()
  }

  # All format to lower case
  # text_columns <- intersect(c("AU", "TI", "SO", "LA", "DT", "DE", "ID", "AB", "C1", "AF", "RP", "PU", "FU", "FX", "J9", "JI"), available_usable_columns)
  # for (i in text_columns) {
  #   dataset[,i] <- tolower(enc2utf8(dataset[,i]))
  # }
  
  # Fill-in missing UT fields. We fill them with their row number (Can be anything, as is non repeated)
  if ("UT" %in% available_usable_columns) {
    blank_UT <- which(dataset$UT == "")
    dataset$UT[blank_UT] <- paste(dataset$AU[blank_UT], dataset$PY[blank_UT], sep = "-")
  }
  
  # Remove rows not numbered by Fukan System
  if ("X_N" %in% available_usable_columns) {
    dataset <- dataset[!duplicated(dataset$X_N),]
    dataset <- dataset[!is.na(dataset$X_N),]
  }
  
  # return dataset
  return(dataset)
}
