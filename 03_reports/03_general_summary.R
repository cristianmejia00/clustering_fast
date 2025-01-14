# 20181206
print("###################### reports/03_general_summary.R")

##  Create the orphans dataset if does not exists
if (!exists("orphans")) {
  orphans <- data.frame()
}

## Add common values
general_summary <- c(
  "total_articles" = nrow(orphans) + nrow(dataset),
  "nodes" = nrow(dataset),
  "orphans" = nrow(orphans),
  "orphans_percent" = round(nrow(orphans) / (nrow(orphans) + nrow(dataset)), 2)
)

## Add values depending the type of analysis
if (settings$params$type_of_analysis %in% c("citation_network", "both")) {
  if (settings$params$recursive_level > 0) {
    general_summary <- c(
      general_summary,
      "edges" = ecount(g1),
      "clusters_level0" = nrow(edges_level1),
      "clusters_level1" = if (settings$params$recursive_level >= 1) {
        nrow(edges_level2)
      } else {
        0
      },
      "clusters_level2" = if (settings$params$recursive_level >= 2) {
        nrow(edges_level3)
      } else {
        0
      }
    )
  }
} else {
  general_summary <- c(
    general_summary,
    "clusters" = tmo$K
  )
}

## Save the report
write.csv(as.data.frame.list(general_summary) %>% t(),
  file = file.path(network_folder_path, "general_summary.csv")
)
