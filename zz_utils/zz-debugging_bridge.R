# 20230725
# When I need to run the codes, one-by-one manually, I need to run this part
# instead of `01_execute_and_reports.R`
# Meaning that besides settings, these are the necessary objects for to run the rest of codes

level_report <<- 0
output_folder_level <- file.path(output_folder_reports, 
                                 paste("level", as.character(level_report), sep = ""))
rn <- list(
  PROJECTarticlereport = file.path(output_folder_level, "article_report.csv"),
  PROJECTrcs = file.path(output_folder_level, "rcs.csv"),
  PROJECTrcs2 = file.path(output_folder_level, "rcs2.csv"),
  PROJECTrcsviz = file.path(output_folder_level, "rcs_viz.html"),
  PROJECTKeywords = file.path(output_folder_level, "ALL_Cluster_keywords.rdata"),
  PROJECTKeywords_report = file.path(output_folder_level, "report_keyword.csv"),
  PROJECTenviron = file.path(output_folder_level, "environ.rdata")
)
