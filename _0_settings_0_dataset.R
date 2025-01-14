# Settings file
# Update the settings and run the file.
# This creates directory in the Output folder and JSON file inside it
# With the directive for creating the dataset.

# Call necessary libraries
source("zz_utils/02_libraries.R")
source("zz_utils/00_system_paths.R")

# Initialize settings
settings <- list()

## Dataset Metadata
settings$metadata <- list(
  input_project_folder = "Q321_TI_robot", # This is the folder where you saved the Web of Science files
  
  type_of_dataset = "papers", # don't change
  dataset_source = "wos", # don't change

  # Analysis ID (the date + number is the label of this analysis)
  date_id = "2025-01-11", # format(Sys.Date(), "%Y-%m-%d"),

  # Query and data
  query = "TI=robot*",

  # project
  dataset_name = "ti_robot",
  dataset_file_name_suffix = "ti_robot", # suffix is used for file names
  dataset_description = "",
  created_by = "cristianmejia00@gmail.com",
  notes = "Part of the Human Augmentation project for Mitsubishi"
)

## Dataset General Parameters
settings$filtering <- list(
  "f01" = list(
    rows_filter = list(
      removed_duplicated_UT = TRUE,
      most_recent_year = format(Sys.Date(), "%Y")
    ),
    columns_filter = list(
      columns_selected = c(
        "PT", "AU", "TI", "SO", "LA", "DT", "DE", "ID", "AB", "C1", "OI", "AF", "OA",
        "RP", "FU", "FX", "CR", "NR", "TC", "Z9", "U1", "U2", "PU", "SN", "J9",
        "JI", "PY", "VL", "IS", "BP", "EP", "AR", "DI", "PG", "WC", "SC", "UT"
      )
    )
  )
)

# Embed Parameters
settings$embeds <- list(
  # If copute embeds
  get_embeds = TRUE,

  # The filtered label
  from_filtered_dataset = "f01",

  # Embeds parameters
  "e01" = list(
    # The text columns to combine to form the corpus
    text_columns = c("TI", "AB"),

    # Text preparation
    to_lowercase = FALSE,
    remove_stopwords = FALSE,
    remove_numbers = FALSE,
    remove_symbols = FALSE,
    stemming = FALSE,
    lemmatization = FALSE,

    # Column to use as the ID of the embeds. It can be a concatenation of multiple columns
    id_column = c("UT"),

    # The huggingface ID of the embed model
    transformer_model = "all-MiniLM-L6-v2",

    # Comments
    notes = ""
  )
)


settings$network <- list(
  # If compute network
  get_network = TRUE,

  # The filtered label
  from_filtered_dataset = "f01",

  # Type of network
  network_type = "direct_citation",

  # notes
  notes = ""
)

source("_1_entry_0_dataset.R")