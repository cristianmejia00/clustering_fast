# Potentially garbage code.
# This code was created the day I fixed the issue of adding colors to
# subcluster boxplots based on their main cluster.
# The function was fixed and worked fine as run by this code.
# This was tested with the Nature Capital dataset


# 20230719
# Cluster-level charts
# Bar charts for the cluster sizes
# Boxplots for rp$numerical_reports e.g. "PY", "Z9", "sentiment" "score"

# Load plotting library
library(ggplot2)
library(glue)

# INPUT
dataset <- dataset
output_folder_level <- output_folder_level
subfolder_clusters <- subfolder_clusters
extension <- extension

# Passed to the function
default_palette <- default_palette
rcs_merged <- rcs_merged

## From settings
document_label <- toTitleCase(settings$params$document_source)
column_labels <- settings$rp$column_labels
numerical_reports <- intersect(settings$rp$numerical_reports, colnames(dataset))


################################################################################
# SYSTEM
dir.create(file.path(output_folder_level, subfolder_clusters), recursive = TRUE)


################################################################################
# CLUSTER SIZE
################################################################################
stats_size <- dataset$X_C %>%
  as.numeric() %>%
  table() %>%
  data.frame() %>%
  setNames(c("Cluster", "Documents"))
if (extension != "svg") {
  write.csv(stats_size,
    row.names = FALSE,
    file = file.path(
      output_folder_level,
      subfolder_clusters,
      "data_cluster_size.csv"
    )
  )
}


ggplot(stats_size, aes(x = Cluster, y = Documents)) +
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") +
  theme_bw() +
  coord_flip() +
  scale_x_discrete(limits = rev)
ggsave(file.path(output_folder_level, subfolder_clusters, glue("fig_cluster_size_h.{extension}")))

ggplot(stats_size, aes(x = Cluster, y = Documents)) +
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") +
  theme_bw()
ggsave(file.path(output_folder_level, subfolder_clusters, glue("fig_cluster_size_v.{extension}")))


##################################################################
##################################################################
##################################################################
# BOXPLOTS   --> summarize numerical columns
##################################################################
# Boxplots sorted from lowest to highest
# Based on medians; ties are broken with the mean

# UTILS
#' param value_column: the dataset column with numerical values to summarize (e.g. PY, Z9, sentiment, score)
#' param category_column: the dataset column with single categorical items (e.g. X_C, SO, PY)
#' param value_label: the label to use in the x axis
#' param category_label: the label to use in the y axis
#' return a ggplot
plot_boxplots <- function(dataset,
                          value_column,
                          category_column,
                          value_label = value_column,
                          category_label = category_column) {
  # Get mean and median for sorting
  compound_mean <- tapply(dataset[[value_column]], dataset[[category_column]], mean, na.rm = TRUE)
  compound_median <- tapply(dataset[[value_column]], dataset[[category_column]], median, na.rm = TRUE)

  # Prepare df
  long <- dataset[, c(category_column, value_column)]
  setnames(long, c(category_column, value_column), c("category", "values"))
  long$category <- gsub("---|-0", "", as.character(long$category))
  lvl <- rcs_merged$cluster_code[order(compound_median, compound_mean)]
  lvl <- gsub("---|-0", "", lvl)
  print(lvl)
  long$category <- factor(long$category,
    levels = lvl
  )

  long$main_cluster <- strsplit(as.character(long$category), "-")
  long$main_cluster <- sapply(long$main_cluster, function(x) {
    x[[1]]
  })

  bp <- ggplot(long, aes(x = category, y = values, fill = main_cluster)) +
    geom_boxplot(
      width = 0.7
    ) +
    xlab(category_label) +
    ylab(value_label) +
    scale_fill_manual(values = default_palette) +
    theme_bw()
  # bp + coord_flip()
  K <- length(unique(dataset[[category_column]]))
  if (K > 20) {
    bp <- bp + theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1))
  }
  return(bp)
}

# Plot
for (i in numerical_reports) {
  plot_boxplots(dataset,
    value_column = i,
    category_column = "X_C",
    value_label = column_labels[i],
    category_label = "Clusters"
  )
  ggsave(file.path(output_folder_level, subfolder_clusters, glue("fig_clusters_{i}_boxplot.{extension}")))
}

plot_boxplots(dataset,
  value_column = "PY",
  category_column = "X_C",
  value_label = column_labels["PY"],
  category_label = "Clusters"
)
