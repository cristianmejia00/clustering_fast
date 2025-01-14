print("***************`a05_reports.R`***************")

# 20180323 -> 20220526
# Framework for Citation Network Analysis with Recursive Clustering, or Topic Models.

###############################################################################
# Load raw dataset

# Citation network assets
if (settings$params$type_of_analysis %in% c("citation_network")) {
  dataset <- readr::read_csv(file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$analysis_id,
    "dataset_comp.csv"
  ))

  # Load the clustering solution once thresholded
  dataset_minimal <- readr::read_csv(file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$analysis_id,
    settings$cno$clustering$algorithm,
    settings$cno$thresholding$threshold %>% as.character(),
    "dataset_minimal.csv"
  ))
}

# Topic Model
if (settings$params$type_of_analysis %in% c("topic_model", "both")) {
  dataset <- readr::read_csv(file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$filtered_folder,
    "dataset_raw_cleaned.csv"
  ))

  # Load the clustering solution once thresholded
  dataset_minimal <- readr::read_csv(file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$analysis_id,
    "dataset_minimal.csv"
  ))
}


# Ensure we have all the papers in the network
dataset_minimal$uuid <- dataset$uuid[match(dataset_minimal$UT, dataset$UT)]
stopifnot(all(dataset_minimal$uuid %in% dataset$uuid))

# Merge them
dataset <- merge(
  dataset_minimal %>%
    select(all_of(c(
      "uuid",
      setdiff(
        colnames(dataset_minimal),
        colnames(dataset)
      )
    ))),
  dataset,
  by = "uuid",
  all.x = TRUE,
  all.y = TRUE
)

# Verify the data is correctly formatted for reports
source(file.path(getwd(), "zz_utils", "00_verify_data.R"))
zz_env <- list("x01" = ls())


###############################################################################
###############################################################################
###############################################################################
# Reporting clusters
source(file.path(
  getwd(),
  "02_citation_network",
  "01_execute_and_reports.R"
))

###############################################################################
###############################################################################
###############################################################################
# Save code snapshot
files_to_save <- list.files(getwd(), full.names = TRUE, recursive = TRUE, pattern = "*\\.R$|*\\.r$")
files_to_omit <- list.files(file.path(getwd(), "renv", "library"), full.names = TRUE, recursive = TRUE, pattern = "*\\.R$|*\\.r$")
files_to_save <- setdiff(files_to_save, files_to_omit)

# Not to zip Rdata environments as they are heavy and saved separately
files_to_save <- files_to_save[!grepl("rdata$", tolower(files_to_save))]
# Zip them. This needs Rtools to work
zip(
  zipfile = file.path(output_folder_level, "source_code"),
  files = files_to_save
)


# Save package list
session_info <- sessionInfo()
save(session_info, file = file.path(output_folder_level, "sessionInfo.rdata"))
writeLines(capture.output(sessionInfo()), file.path(output_folder_level, "sessionInfo.txt"))

# # Save Global environment
# save.image(file.path(output_folder_level, "environ_zz_reports.rdata"))
#
# # Save cluster IDS
# if ('fukan_original_cluster_id' %in% colnames(dataset)) {
#   print('Saving cluster id comparison for subclusters')
#   cluster_comparison <- dataset[c('X_C', 'fukan_X_C', 'fukan_original_cluster_id', 'fukan_subcluster_label')]
#   cluster_comparison <- cluster_comparison[!duplicated(cluster_comparison$fukan_subcluster_label),]
#   cluster_comparison <- cluster_comparison[order(cluster_comparison$fukan_X_C),]
#   write.csv(cluster_comparison, file = file.path(output_folder_level, "cluster_id_comparison.csv"), row.names = FALSE)
# }

# ###############################################
# # LLM
# # Using OpenAI and Claude in R.
# source("05_llm/zz-llm_v2_0_prompts.R")
# source("05_llm/zz-llm_v2_1_functions.R")
# source("05_llm/zz-llm_v2_2_execution.R")
#
# source("06_quarto/zz-create_bib_file.R")
#
# source("06_quarto/zz-generate_quarto_document.R")
# #source("06_quarto/zz-generate_quarto_word.R")
#
# ###############################################
# # Send to display
# source("zzz-send_to_display_repo.R")
#
# # Save Global environment
# save.image(file.path(output_folder_level, "environ_zzz_llm.rdata"))
