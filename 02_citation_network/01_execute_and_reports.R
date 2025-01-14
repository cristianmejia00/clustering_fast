# 20220526 Last updated

####################################################
# CITATION NETWORK
####################################################
if (settings$params$type_of_analysis == "citation_network") {
  output_folder_reports <- file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$analysis_id,
    settings$cno$clustering$algorithm,
    settings$cno$thresholding$threshold %>% as.character()
  )
}

settings$params$type_of_analysis <- "topic_model"
if (settings$params$type_of_analysis == "topic_model") {
  output_folder_reports <- file.path(
    settings$metadata$bibliometrics_folder,
    settings$metadata$project_folder,
    settings$metadata$analysis_id
  )
}

####################################################
# Reports
####################################################
# Keep track of time
time_01_started <- Sys.time()

# Data preparation
for (level_report_iteration in c(0:settings$params$recursive_level)) {
  level_report <<- level_report_iteration
  print(paste("...Starting reports for level",
    as.character(level_report),
    sep = " "
  ))

  # Get the level path and create the directory
  output_folder_level <- file.path(
    output_folder_reports,
    paste("level",
      as.character(level_report),
      sep = ""
    )
  )
  dir.create(output_folder_level)

  # Get the right dataset for the corresponding level
  if (level_report_iteration == 0) {
    source("02_citation_network/04_data_scope_level0.R")
  }
  if (level_report_iteration == 1) {
    source("02_citation_network/04_data_scope_level1.R")
  }
  if (level_report_iteration == 2) {
    source("02_citation_network/04_data_scope_level2.R")
  }
  if (level_report_iteration == 3) {
    source("02_citation_network/04_data_scope_level3.R")
  }

  # update report names
  # PROJECT <- paste(PROJECTNAME, "_", as.character(i), sep = "")
  # Docker container Ubuntu (Comment it out in Windows):
  # PROJECT <- paste("/var/container/", PROJECT, sep = "")
  rn <- list(
    PROJECTarticlereport = file.path(output_folder_level, "article_report.csv"),
    PROJECTrcs = file.path(output_folder_level, "rcs.csv"),
    PROJECTrcs2 = file.path(output_folder_level, "rcs2.csv"),
    PROJECTrcsviz = file.path(output_folder_level, "rcs_viz.html"),
    PROJECTKeywords = file.path(output_folder_level, "ALL_Cluster_keywords.rdata"),
    PROJECTKeywords_report = file.path(output_folder_level, "report_keyword.csv"),
    PROJECTenviron = file.path(output_folder_level, "environ.rdata")
  )


  # Number of clusters
  K <- length(unique(myDataCorrect$X_C))

  # Document report
  print("Article report")
  source("03_reports/01_document_report_with_abstract.R")
  zz_env$x02 <- ls()

  # Get detail reports reports by feature by cluster
  print("Clusters reports")
  source("03_reports/04_cluster_reports.R")
  zz_env$x03 <- ls()

  # RCS
  print("Computing RCS")
  source("03_reports/02_rcs.R")
  zz_env$x04 <- ls()
  #
  # # # Keywords for heatmap # Now, do it for all.
  # # print("Heatmap keywords")
  # # if (file.exists(file.path(output_folder_reports, "papersText.rdata"))) {
  # #   load(file.path(output_folder_reports, "papersText.rdata"))
  # # }
  # # source("03_reports/05_heatmap_keywords_part_1.R")
  # # if (!exists("papersText")) {
  # #   source("03_reports/05_heatmap_keywords_part_2.R")
  # # }
  # # source("03_reports/05_heatmap_keywords_part_3.R")
  #
  # # # Keywords explorer
  # # # This takes a lot of time when there are too many clusters like in level 2 and 3.
  # # if (level_report <= 4) {
  # #   print("Citations to topic model visual")
  # #   use_tfidf <- TRUE
  # #   source("zz_utils/zz_createJSON_cnet.R")
  # #   source("03_reports/06_citation_to_topic_model_converter.R")
  # #   source("03_reports/07_prob_exclu_keywords.R")
  # # }
  # #
  # # # Create the Keywords report
  # # if (level_report <= 4) {
  # #   print("Keywords report")
  # #   source("03_reports/08_all_keywords_report.R")
  # #   # Optional views
  # #   # source("03_reports/09_keywords_per_clusters.R")
  # #   source("03_reports/10_rcs_keywords.R")
  # # }
  # # zz_env$x05 <- c(zz_env$x04, "papersText", "myDataCorrect_SAMPLE", "unified_keywords", "PHI", "from_stem_to_raw")

  # Overlays (Only for WOS data)
  if (settings$params$dataset_source == "wos" & "WC" %in% colnames(myDataCorrect)) {
    source(file.path(getwd(), "03_reports", "13_WC_overlays.R"))
  }

  ############################################################################

  # Dataset merged RCS
  source(file.path(getwd(), "03_reports", "15_rcs_merged.R"))

  # figures
  # Save PNG figures. Normal raster figures for easy navigation in PC.
  print("###################### PNG CHARTS")
  extension <- "png"
  subfolder_dataset <- "charts_dataset"
  subfolder_clusters <- "charts_clusters"
  source(file.path(getwd(), "04_charts", "zz-charts_dataset.R"))
  source(file.path(getwd(), "04_charts", "zz-charts_clusters_scatterplots.R"))
  source(file.path(getwd(), "04_charts", "zz-charts_clusters_stats1_bp.R"))
  source(file.path(getwd(), "04_charts", "zz-charts_clusters_stats2_bars.R"))
  source(file.path(getwd(), "04_charts", "zz-charts_clusters_stats3_lda.R"))
  # source(file.path(getwd(), "04_charts", "zz-charts_clusters_stats4_heatmap.R"))
  source(file.path(getwd(), "04_charts", "zz-charts_trends_and_clustered_bars.R"))


  # Save PNG figures. Needed for notebook.
  print("###################### SVG CHARTS")
  extension <- "svg"
  subfolder_dataset <- "index_files/charts"
  subfolder_clusters <- "index_files/charts"
  source(file.path(getwd(), "04_charts", "zz-charts_dataset.R"))
  source(file.path(getwd(), "04_charts", "zz-charts_clusters_scatterplots.R"))
  source(file.path(getwd(), "04_charts", "zz-charts_clusters_stats1_bp.R"))
  source(file.path(getwd(), "04_charts", "zz-charts_clusters_stats2_bars.R"))
  source(file.path(getwd(), "04_charts", "zz-charts_clusters_stats3_lda.R"))
  # source(file.path(getwd(), "04_charts", "zz-charts_clusters_stats4_heatmap.R"))
  source(file.path(getwd(), "04_charts", "zz-charts_trends_and_clustered_bars.R"))

  ############################################################################
  # Save complete environment by level. First remove unnecessary variables
  print("Saving image")
  # rm(list = setdiff(ls(), zz_env$x05))
  save.image(rn$PROJECTenviron)

  # Time per level
  time_02_finished <- Sys.time()
  time_03_taken <- time_02_finished - time_01_started
  print(time_03_taken)
}
