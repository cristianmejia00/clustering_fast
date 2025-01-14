print("***************`_1_entry_0_dataset.R`***************")

###############################################################################
# Add the input and output paths as part of the metadata
settings$metadata$input_folder_path <- input_folder_path
settings$metadata$output_folder_path <- output_folder_path
settings$metadata$project_folder <- settings$metadata$input_project_folder

###############################################################################
project_folder_path <- file.path(
  settings$metadata$output_folder_path,
  settings$metadata$project_folder
)
dir.create(project_folder_path, showWarnings = FALSE)

settings_file_path <- file.path(
  settings$metadata$output_folder_path,
  settings$metadata$project_folder,
  paste("settings_dataset_directive",
        #format(Sys.time(), "%Y-%m-%d-%H-%M"),
        ".json",
        sep = ""
  )
)

# Save readable settings
writeLines(
  RJSONIO::toJSON(settings, pretty = TRUE, auto_unbox = TRUE),
  settings_file_path
)

# Print to console
settings_file_path

###############################################################################
source("a00_data_loader.R")
source("a01_network.R")