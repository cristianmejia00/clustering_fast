# 20230714
# Bring all RCS-related summaries as a single RCS

print("###################### reports/15-RCS_merged.R")

# Input
output_folder_level <- output_folder_level

#############################################################################

# Load rcs files
rcs_core <- read.csv(file.path(output_folder_level, "rcs.csv"))
#rcs_keywords <- read.csv(file.path(output_folder_level, "rcs_keywords.csv"))

# Load numeric reports
rcs_numeric_reports <- list()
for (i in settings$rp$numerical_reports) {
  if (file.exists(file.path(output_folder_level, glue("report_{i}.csv")))) {
    tmp <- read.csv(file.path(output_folder_level, glue("report_{i}.csv")))
    colnames(tmp) <- paste(i, colnames(tmp), sep = "_")
    rcs_numeric_reports[[i]] <- tmp
  }
}

# Initialize the merged RCS
rcs_merged <- rcs_core[, c("cluster", "cluster_code")]
rcs_merged$main_cluster <- sapply(rcs_merged$cluster_code, function(x) {
  x <- strsplit(as.character(x), "-") %>% unlist()
  x <- x[[1]] %>% as.numeric()
  return(x)
})
rcs_merged$cluster_name <- ""
rcs_merged$documents <- rcs_core$cluster_size[match(rcs_merged$cluster, rcs_core$cluster)]
rcs_merged$documents_percent <- round(rcs_merged$documents * 100 / sum(rcs_merged$documents, na.rm = TRUE), 2)
rcs_merged$documents_cummulative <- cumsum(rcs_merged$documents_percent)

# Add numeric values
for (i in c(1:length(rcs_numeric_reports))) {
  rcs_merged <- merge(rcs_merged,
    rcs_numeric_reports[[i]],
    by.x = "cluster",
    by.y = paste(names(rcs_numeric_reports)[i], "cluster", sep = "_"),
    all.x = TRUE
  )
}

# Other numeric info
rcs_merged$participation <- rcs_core$participation[match(rcs_merged$cluster, rcs_core$cluster)]
rcs_merged$growth_rate <- rcs_core$growth_rate[match(rcs_merged$cluster, rcs_core$cluster)]
#rcs_merged$edges <- rcs_keywords$Edges[match(rcs_merged$cluster, rcs_keywords$Cluster)]
#rcs_merged$edges_nodes <- rcs_keywords$E.N[match(rcs_merged$cluster, rcs_keywords$Cluster)]

# RCS labels
rcs_merged$rcs_label <- rcs_core$label[match(rcs_merged$cluster, rcs_core$cluster)]

# Hub info
rcs_merged$hub_title <- rcs_core$hub_title[match(rcs_merged$cluster, rcs_core$cluster)]
rcs_merged$hub_year <- rcs_core$hub_year[match(rcs_merged$cluster, rcs_core$cluster)]
rcs_merged$hub_degree <- rcs_core$dmax[match(rcs_merged$cluster, rcs_core$cluster)]
rcs_merged$hub_id <- rcs_core$hub_ID[match(rcs_merged$cluster, rcs_core$cluster)]
rcs_merged$hub_type1 <- rcs_core$hub_type1[match(rcs_merged$cluster, rcs_core$cluster)]
rcs_merged$hub_type2 <- rcs_core$hub_type2[match(rcs_merged$cluster, rcs_core$cluster)]

# Keywords
#rcs_merged$unique_keywords <- rcs_keywords$Unique.Keywords[match(rcs_merged$cluster, rcs_keywords$Cluster)]
#rcs_merged$frequent_keywords <- rcs_keywords$Frequent.Keywords[match(rcs_merged$cluster, rcs_keywords$Cluster)]

# Rest of columns
for (i in settings$rp$categorical_long_reports) {
  if (i %in% colnames(rcs_core)) {
    rcs_merged[[i]] <- rcs_core[match(rcs_merged$cluster, rcs_core$cluster), i]
  }
}

# Save
write.csv(rcs_merged, 
          file.path(output_folder_level, "rcs_merged.csv"), 
          row.names = FALSE)
