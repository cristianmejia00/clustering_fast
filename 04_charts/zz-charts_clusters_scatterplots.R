print("###################### zz-charts_clusters_scatterplots.R")

# 20220915

# We plot these scatterplots
# - years x citations (or score)
# - years x size
# - citations x size
# - years x sentiment
# - citations x sentiment
# - size x sentiment

# This is an scatterplot and thus is preferred to have the actual names of the clusters if possible
# When using clusters or topic models we write the topic names at a later stage.
# We can add the names here, so the reports and charts show those names.

# There are 3 options:
# If cluster/topic analysis and we have not named the clusters. Use the cluster number
# If cluster/topic analysis and we named the clusters directly in `rcs_merged.csv`
# If facet analysis use the facet names. In this case we expect the `dataset` to have a X_C_name column since the beginning


# Global inputs
dataset <- dataset
rcs_merged <- rcs_merged
unit_of_analysis <- settings$params$unit_of_analysis
column_labels <- settings$rp$column_labels
output_folder_level <- output_folder_level
subfolder_clusters <- subfolder_clusters
extension <- extension


##############################################################################

# Load libraries
library("plotly")
library("reshape2")
library("ggplot2")
library("ggrepel")
if (exists("topic_names")) {
  rm(topic_names)
}

#################################################################
# In case of sub-clusters we may want to have the main cluster in the RCS as well
main_cluster <- gsub("---|-0", "", rcs$cluster_code)
main_cluster <- strsplit(main_cluster, "-")
main_cluster <- sapply(main_cluster, function(x) {x[[1]]})
rcs_merged$main_cluster <- factor(main_cluster, levels = as.numeric(main_cluster) %>% unique() %>% sort() %>% as.character())

# Add palette
default_palette <- c("#E69F00", "#56B4E9", "#009E73", "#8B0000", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#9b5de5", "#2270e7", "#e5e510", "#f00f15", "#3524ae", "#26cc3a", "#ec058e", "#9cb8c2", "#fffdd0", "#b40e68", "#AFA100", "#F67963")
default_palette[length(unique(rcs_merged$main_cluster))] <- "#d3d3d3"

# Load files
# load(file.path(settings$analysis_metadata$bibliometrics_folder, 
#                settings$analysis_metadata$project_folder, 
#                settings$analysis_metadata$analysis_folder,
#                "dataset.rdata"))
# rcs_merged <- read.csv(file.path(output_folder_level, "rcs_merged.csv"))

# Create directory
dir.create(file.path(output_folder_level, subfolder_clusters))


# Get the topic names based on 3 options:
# Use cluster numbers
if (unit_of_analysis %in% c("topic", "topics", "cluster", "clusters") &
  all(rcs_merged$cluster_name == "")) {
  print("Unnamed clusters. Attaching topic/cluster based on cluster number")
  dataset$X_C_name <- as.character(dataset$X_C)
  rcs_merged$X_C_name <- as.character(rcs$cluster_code)
}

# Use cluster names if all clusters have been named in rcs_merged.csv
if (unit_of_analysis %in% c("topic", "topics", "cluster", "clusters") &
  !all(rcs_merged$cluster_name == "")) {
  print("Attaching topic/cluster names from file")
  dataset$X_C_name <- rcs_merged$cluster_name[match(dataset$X_C, rcs_merged$X_C)]
  rcs_merged$cluster_code2 <- gsub('---', '', rcs_merged$cluster_code)
  rcs_merged$X_C_name <- paste(rcs_merged$cluster_code2, '. ', rcs_merged$cluster_name, sep = '') %>% substr(start = 1, stop = 27)
}

# Get the cluster name from the dataset. This only applies to facet datasets
if (!(unit_of_analysis %in% c("topic", "topics", "cluster", "clusters"))) {
  print("Attaching facet names")
  topic_names <- dataset[!duplicated(dataset$X_C), c("X_C", "X_C_name")]
  rcs_merged$X_C_name <- topic_names$X_C_name[match(rcs_merged$cluster, topic_names$X_C)]
}


#################################################################
# Backups
dataset_tmp <- dataset
rcs_tmp <- rcs_merged

# Cluster selection (a.k.a. Facet subsetting)
# When doing facet analysis, the list of facets is expected to be long. e.g. firms
# Hence, I plot facets mentioned in at least 10 news
if (!(unit_of_analysis %in% c("topic", "topics", "cluster", "clusters"))) {
  selected_clusters <- rcs_merged$X_C_name[rcs_merged$documents >= 10]
  dataset_tmp <- dataset[dataset$X_C_name %in% selected_clusters, ]
  rcs_tmp <- rcs[rcs$X_C_name %in% selected_clusters, ]
}

# Remove the -99 subclusters and 99
rcs_tmp <- rcs_tmp %>% 
  filter(!grepl("-99", cluster_code)) %>% 
  filter(!grepl("99", cluster_code))

##################################################################
# SCATTERPLOT: Ave. Year x Cites
##################################################################
plot_scatter <- function(rcs_data,
                         point_labels,
                         x_column,
                         y_column,
                         color_hex_column,
                         color_labels,
                         size_column,
                         x_column_label = x_column,
                         y_column_label = y_column) {
  # format the df
  df <- rcs_data[, c(point_labels, x_column, y_column, color_hex_column, color_labels, size_column)]
  colnames(df) <- c("point_labels", "x", "y", "color_hex", "color_label", "size")
  df$labels <- as.character(df$point_labels)
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(color = color_hex, 
                   size = size)) +
    scale_color_identity(df$color_hex) +
    xlab(x_column_label) +
    ylab(y_column_label)
  p <- p + geom_text_repel(aes(label = gsub("---|-0", "", labels)))
  p <- p + theme_bw() + theme(legend.position = "none")
  p
}

# Note: consider dynamic color changing. Currecntly we can assign color as follows:
# Categoricals
# - All same color:
# - Color based on the `main_cluster` <- THIS IS USED
# - Color based on the `sentiment_label`
# - Color based on the `rcs_label`
# - Color based on custom partition (is this possible)

# Gradients:
# - PY, Z9, growth, participation

# assign hex colors based on main_cluster
rcs_tmp$color_hex <- default_palette[as.integer(rcs_tmp$main_cluster)]
rcs_tmp$color_hex[is.na(rcs_tmp$color_hex)] <- "#d3d3d3"

# - years x citations (or score)
# - years x size
# - size x citations
plot_scatter(rcs_tmp, "X_C_name", "PY_Mean", "Z9_Mean", "color_hex", "main_cluster", "documents", "Ave. Publication Year", "Ave. Citations")
ggsave(file.path(output_folder_level, subfolder_clusters, glue("fig_scatter_clusters_PY_x_Z9.{extension}")))

plot_scatter(rcs_tmp, "X_C_name", "PY_Mean", "documents", "color_hex", "main_cluster", "Z9_Mean", "Ave. Publication Year", "Documents")
ggsave(file.path(output_folder_level, subfolder_clusters, glue("fig_scatter_clusters_PY_x_size.{extension}")))

plot_scatter(rcs_tmp, "X_C_name", "documents", "Z9_Mean", "color_hex", "main_cluster", "PY_Mean", "Documents", "Ave. Citations")
ggsave(file.path(output_folder_level, subfolder_clusters, glue("fig_scatter_clusters_size_x_Z9.{extension}")))

getwd()
# Only for datasets with sentiment:
# - years x sentiment
# - citations x sentiment
# - size x sentiment

if ("sentiment_Mean" %in% colnames(rcs_merged)) {
  plot_scatter(rcs_tmp, "X_C_name", "PY_Mean", "sentiment_Mean", "main_cluster", "documents", settings$rp$column_labels["PY"], settings$rp$column_labels["sentiment"])
  ggsave(file.path(output_folder_level, subfolder_clusters, glue("fig_scatter_clusters_year_x_sentiment.{extension}")))

  plot_scatter(rcs_tmp, "X_C_name", "Z9_Mean", "sentiment_Mean", "main_cluster", "documents", settings$rp$column_labels["Z9"], settings$rp$column_labels["sentiment"])
  ggsave(file.path(output_folder_level, subfolder_clusters, glue("fig_scatter_clusters_Z9_x_sentiment.{extension}")))

  plot_scatter(rcs_tmp, "X_C_name", "documents", "sentiment_Mean", "main_cluster", "documents", "Documents", settings$rp$column_labels["sentiment"])
  ggsave(file.path(output_folder_level, subfolder_clusters, glue("fig_scatter_clusters_size_x_sentiment.{extension}")))
}
