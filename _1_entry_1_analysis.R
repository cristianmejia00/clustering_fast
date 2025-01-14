print("***************`_1_entry_1_analysis.R`***************")

###############################################################################
# In the case of the analysis settings we must create the directory first.
analysis_results_folder_path <- file.path(
  settings$metadata$bibliometrics_folder,
  settings$metadata$project_folder,
  settings$metadata$analysis_id
)
dir.create(analysis_results_folder_path, showWarnings = FALSE)

# Save the analysis directive
settings_file_path <- file.path(
  analysis_results_folder_path,
  paste("settings_analysis_directive",
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
print(settings_file_path)

###############################################################################
source("a02_components.R")
source("a03_clustering.R")
source("a04_thresholding.R")
source("a05_reports.R")