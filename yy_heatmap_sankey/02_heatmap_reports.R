library(ggplot2)
library(ggrepel)
library(stats)
library(tidyr)
library(reshape2)

heatmap_analysis_id <- "H010"
settings_directive <- "heatmap_settings_H010_Human_Aug-Robotics.json"

###############################################################################
# Call necessary libraries
source("zz_utils/02_libraries.R")
source("zz_utils/00_system_paths.R")

###############################################################################
# Load the directive file
settings <- RJSONIO::fromJSON(
  file.path(
    bibliometrics_folder_path,
    heatmap_analysis_id,
    settings_directive
  ),
  simplify = FALSE
)

# Save setting inputs as a df.
inputs <- lapply(settings$inputs, function(x) {
  data.frame(x)
}) %>% rbind.fill()

###############################################################################
# Read the files
# The coordinates of all participating clusters across analysis in this heatmap
# Computed in the Heatmap colab
coords <- readr::read_csv(file.path(
  bibliometrics_folder_path,
  heatmap_analysis_id,
  "coordinates.csv"
)) %>%
  select(x, y, cluster) %>%
  rename(cluster_code = cluster)


# Read the RCS files
# For the heatmap the RCS is expected to have already
# The documents, PY_Mean, Z9_Mean, and the LLM_name (cluster_name)
rcs <- lapply(c(1:nrow(inputs)), \(x) {
  this_df <- readr::read_csv(file.path(
    bibliometrics_folder_path, # settings$metadata$input_directory,
    settings$inputs[[x]]$project_folder_name,
    settings$inputs[[x]]$analysis_folder_name,
    settings$inputs[[x]]$level_folder_name,
    "cluster_summary.csv"
  )) %>% mutate(
    # Add the dataset name to the RCS cluster code
    cluster_code = paste(inputs$display_name[[x]], cluster_code, sep = "-")
  )
}) %>%
  rbind.fill() %>%
  select(cluster_code, cluster_name, documents, PY_Mean, Z9_Mean) # Count, ave_PY, ave_Z9)

###############################################################################
###############################################################################
# Merge the datasets and create needed columns
tmp <- merge(rcs, coords, by = "cluster_code")
tmp <- tmp %>%
  separate(cluster_code,
    remove = FALSE,
    into = c("dataset", "local_cluster"),
    sep = "-",
    extra = "merge"
  )
# When using subclusters, lets remove the trailing "---"
tmp$local_cluster <- gsub("---", "", tmp$local_cluster)

tmp <- merge(tmp,
  inputs %>%
    select(display_name, color, heatmap_display_order, sankey_display_order) %>%
    rename(dataset = display_name),
  by = "dataset"
)

# The labels shown in the scatter plots
tmp$scatter_labels <- paste(tmp$local_cluster, tmp$cluster_name, sep = ":")

# Groups of clusters
km1 <- kmeans(tmp[, c("x", "y")], centers = floor(sqrt(nrow(rcs))))
tmp$group <- as.factor(km1$cluster)

###############################################################################
###############################################################################
# Get the scatter plots
# Aux plot function
plot_scatter_group <- function(rcs_data,
                               point_labels,
                               x_column,
                               y_column,
                               color_hex_column,
                               color_labels,
                               size_column,
                               x_column_label = x_column,
                               y_column_label = y_column,
                               show_tags = TRUE) {
  # format the df
  df <- rcs_data[, c(point_labels, x_column, y_column, color_hex_column, color_labels, size_column)]
  colnames(df) <- c("point_labels", "x", "y", "color_hex", "color_label", "size")

  # plot
  p <- ggplot(df, aes(x = x, y = y)) +
    stat_ellipse(
      geom = "polygon",
      aes(linetype = rcs_data$group),
      alpha = 0.07
    ) + # 0.07 OR 0.35 for colors
    geom_point(aes(
      color = color_hex,
      size = size
    )) +
    scale_color_identity() +
    xlab("") +
    ylab("")
  if (show_tags) {
    p <- p + geom_text_repel(aes(label = gsub("---", "", point_labels)), max.overlaps = 30, size = 2)
  }
  p <- p + theme_bw() + theme(legend.position = "none")
  p
}


#################################################
# Aux plot function
plot_scatter <- function(rcs_data,
                         point_labels,
                         x_column,
                         y_column,
                         color_hex_column,
                         color_labels,
                         size_column,
                         min_x,
                         max_x,
                         max_y,
                         x_column_label = x_column,
                         y_column_label = y_column,
                         show_tags = TRUE) {
  # format the df
  df <- rcs_data[, c(point_labels, x_column, y_column, color_hex_column, color_labels, size_column)]
  colnames(df) <- c("point_labels", "x", "y", "color_hex", "color_label", "size")

  # plot
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(
      colour = color_hex,
      size = size
    )) +
    scale_color_identity() +
    scale_x_continuous(limits = c(floor(min_x), ceiling(max_x))) + # = seq(2005, 2022, by = 2)) +
    # scale_x_continuous(breaks = seq(2005, 2022, by = 2)) +
    scale_y_continuous(limits = c(0, (round(max_y, 0) + 10))) +
    xlab("") +
    ylab("")
  if (show_tags) {
    p <- p + geom_text_repel(aes(label = gsub("---", "", point_labels)), max.overlaps = 15, size = 5)
  }
  p <- p + theme_bw() + theme(legend.position = "none")
  p
}

#################################################
# Aux function to map values to a desired range
# Here we use it to remap the "Value" links of the Sankeys.
map_to_range <- function(x, new_min, new_max) {
  # Handle edge case where all values are the same
  if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
    return(rep(new_min, length(x)))
  }

  # First normalize to 0-1 range, then scale to new range
  x_std <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  round(new_min + (new_max - new_min) * x_std, 0)
}

#################################################
# This prevents R generating unnecessary error from ggrepel
options(warn = 1)

# Global scatter by x and y MDS scalling
# i.e. Topic Model plot
tm_plot <- plot_scatter_group(tmp,
  point_labels = "cluster_code",
  x_column = "x",
  y_column = "y",
  color_hex_column = "color",
  color_labels = "dataset",
  size_column = "documents",
  show_tags = TRUE
)
tm_plot
ggsave(
  filename = file.path(
    bibliometrics_folder_path,
    settings$metadata$heatmap_analysis_id,
    "topic_model.png"
  ),
  plot = tm_plot,
  width = 8, # Width in inches
  height = 6, # Height in inches
  dpi = 300 # Resolution (dots per inch)
)

# Same as above without labels (Good for subcluster)
tm_plot <- plot_scatter_group(tmp,
  point_labels = "cluster_code",
  x_column = "x",
  y_column = "y",
  color_hex_column = "color",
  color_labels = "dataset",
  size_column = "documents",
  show_tags = FALSE
)
tm_plot
ggsave(
  filename = file.path(
    bibliometrics_folder_path,
    settings$metadata$heatmap_analysis_id,
    "topic_model_no_tags.png"
  ),
  plot = tm_plot,
  width = 8, # Width in inches
  height = 6, # Height in inches
  dpi = 300 # Resolution (dots per inch)
)

# PY Z9 plots
for (inst in unique(tmp$dataset)) {
  print(inst)
  p <- plot_scatter(tmp %>% filter(dataset == inst),
    point_labels = "scatter_labels", # "scatter_labels" for the full cluster name. # "local_cluster" for the cluster code only
    x_column = "PY_Mean",
    y_column = "Z9_Mean",
    color_hex_column = "color",
    color_labels = "dataset",
    size_column = "documents",
    min_x = min(tmp$PY_Mean, na.rm = TRUE) %>% floor(),
    max_x = max(tmp$PY_Mean, na.rm = TRUE) %>% ceiling(),
    max_y = max(tmp$Z9_Mean, na.rm = TRUE) %>% ceiling(),
    show_tags = TRUE
  )
  ggsave(
    filename = file.path(
      bibliometrics_folder_path,
      settings$metadata$heatmap_analysis_id,
      paste("scatter_plot_", inst, ".png", sep = "")
    ),
    plot = p,
    width = 8, # Width in inches
    height = 6, # Height in inches
    dpi = 300 # Resolution (dots per inch)
  )
}

###############################################################################
###############################################################################

# Heatmap
hm <- readr::read_csv(file.path(
  bibliometrics_folder_path,
  settings$metadata$heatmap_analysis_id,
  "heatmap_matrix.csv"
)) %>% as.data.frame()
rownames(hm) <- colnames(hm)

# Create a sorting vector
sort_vector <- sort(colnames(hm))

# Sort the heatmap
hm_sorted <- hm[sort_vector, sort_vector]

# Convert matrix to long format for ggplot
df_long <- hm_sorted %>%
  as.matrix() %>%
  melt()

# Create heatmap
tm_hm <- ggplot(df_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  coord_fixed() + # make cells square
  labs(x = "", y = "", fill = "Similarity") + # label axes
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # rotate x labels for better readability

tm_hm
ggsave(
  plot = tm_hm,
  filename = file.path(
    bibliometrics_folder_path,
    settings$metadata$heatmap_analysis_id,
    "heatmap.png"
  ),
  width = 8, # Width in inches
  height = 6, # Height in inches
  dpi = 300 # Resolution (dots per inch)
)
# # Using base R heatmap
# heatmap(as.matrix(hm_sorted),
#         Rowv = NA,  # prevent row clustering
#         Colv = NA,  # prevent column clustering
#         col = colorRampPalette(c("white", "red"))(100),
#         scale = "none")


###############################################################################
###############################################################################
# melted: The heatmap matrix melted.
# melted_filtered: Only those beyond threshold and that connect the next step in the path in the sankey
# melted_listed: Same data as `melted_filtered` but stacked. Used as intermediary processing table.
# melted_sankey: The data formatted as needed for Flourish
# melted_sankey_topics: Same as `melted_sankey` but with topic names


# Sankey
melted <- readr::read_csv(file.path(
  bibliometrics_folder_path,
  settings$metadata$heatmap_analysis_id,
  "heatmap_melted.csv"
))

melted <- melted %>%
  separate(Source,
    remove = FALSE,
    into = c("source_dataset", "source_local_cluster"),
    sep = "-"
  ) %>%
  separate(Target,
    remove = FALSE,
    into = c("target_dataset", "target_local_cluster"),
    sep = "-"
  )


melted <- merge(melted,
  inputs %>%
    select(display_name, color, heatmap_display_order, sankey_display_order) %>%
    rename(
      source_color = color,
      source_heatmap_order = heatmap_display_order,
      source_sankey_order = sankey_display_order
    ),
  by.x = "source_dataset",
  by.y = "display_name",
  all.x = TRUE,
  all.y = FALSE
)

melted <- merge(melted,
  inputs %>%
    select(display_name, color, heatmap_display_order, sankey_display_order) %>%
    rename(
      target_color = color,
      target_heatmap_order = heatmap_display_order,
      target_sankey_order = sankey_display_order
    ),
  by.x = "target_dataset",
  by.y = "display_name"
)

###############################################################################
png(filename = file.path(
  bibliometrics_folder_path,
  settings$metadata$heatmap_analysis_id,
  "similarity_boxplot.png"
))
bp <- boxplot(melted$Similarity, ylab = "Similarity")
dev.off()
bp$stats

# Remove pairs in the same sankey level (they belong to same institution)
# Remove pairs in separated for more than one step



###############################################################################
sankey_threshold <- bp$stats[3, 1] # settings$global$sankey_threshold



melted_filtered <- melted %>%
  filter(source_sankey_order != target_sankey_order) %>%
  # filter(abs(source_sankey_order - target_sankey_order) == 1) %>%
  filter(Similarity >= sankey_threshold)

sankey_steps <- unique(inputs$sankey_display_order) %>% sort()


# Stacking the datasets to reorder them
melted_filtered$pair_index <- c(1:nrow(melted_filtered)) %>% as.character()
sources_df <- lapply(sankey_steps, \(st) {
  tmp <- melted_filtered %>%
    filter(source_sankey_order == st) %>%
    select(Source, source_sankey_order, Similarity, pair_index) %>%
    rename(
      cluster = Source,
      step = source_sankey_order,
      similarity = Similarity
    )
}) %>% rbind.fill()
targets_df <- lapply(sankey_steps, \(st) {
  tmp <- melted_filtered %>%
    filter(target_sankey_order == st) %>%
    select(Target, target_sankey_order, Similarity, pair_index) %>%
    rename(
      cluster = Target,
      step = target_sankey_order,
      similarity = Similarity
    )
}) %>% rbind.fill()
melted_listed <- rbind(sources_df, targets_df)

###############################################################################
melted_sankey <- lapply(c(0, max(sankey_steps) - 1), function(st) {
  left_side <- melted_listed %>%
    filter(step == st)
  right_side <- melted_listed %>%
    filter(step > st) # filter(step == st + 1) when strictly step by step
  full_pair <- merge(
    left_side %>%
      select(cluster, step, similarity, pair_index) %>%
      rename(Source = cluster, "Step from" = step, source_similarity = similarity),
    right_side %>%
      select(cluster, step, similarity, pair_index) %>%
      rename(Dest = cluster, "Step to" = step, dest_similarity = similarity),
    by = "pair_index",
    all.x = FALSE,
    all.y = FALSE
  )
}) %>%
  rbind.fill() %>%
  select(all_of(c("Source", "Dest", "source_similarity", "Step from", "Step to"))) %>%
  rename(Similarity = source_similarity) %>%
  mutate(
    Value = 100,
    Distance = `Step to` - `Step from`
  ) %>%
  arrange(Distance, `Step from`, desc(Similarity))

###############################################################################
melted_sankey_topics <- merge(melted_sankey,
  rcs %>%
    select(cluster_code, cluster_name) %>%
    rename(source_topic = cluster_name),
  by.x = "Source",
  by.y = "cluster_code",
  all.x = TRUE,
  all.y = FALSE
)

melted_sankey_topics <- merge(melted_sankey_topics,
  rcs %>%
    select(cluster_code, cluster_name) %>%
    rename(target_topic = cluster_name),
  by.x = "Dest",
  by.y = "cluster_code",
  all.x = TRUE,
  all.y = FALSE
)

melted_sankey_topics <- melted_sankey_topics %>%
  select(all_of(c("Source", "Dest", "Value", "Step from", "Step to", "Similarity", "Distance", "source_topic", "target_topic"))) %>%
  arrange(`Step from`, Source, desc(Similarity)) %>%
  distinct(Source, Dest, .keep_all = TRUE) %>%
  mutate(
    "Value" = map_to_range(Value, 100, 10)
  )


# Special filtering


# Save files
write.csv(melted_sankey_topics,
  file = file.path(
    bibliometrics_folder_path,
    settings$metadata$heatmap_analysis_id,
    glue("sankey_df_with_deadends_{round(sankey_threshold, 2)}_selected.csv")
  ),
  row.names = FALSE
)

###############################################################################
# Write color codes for Flourish
write.csv(paste(tmp$cluster_code, tmp$color, sep = ": "),
  file = file.path(
    bibliometrics_folder_path,
    settings$metadata$heatmap_analysis_id,
    "sankey_cluster_color_for_flourish.csv"
  ),
  row.names = FALSE
)

###############################################################################
# Hardcoded!
bridge_clusters <- intersect(
  melted_sankey$Dest[melted_sankey$`Step to` == 1],
  melted_sankey$Source[melted_sankey$`Step from` == 1]
)

if (length(bridge_clusters) > 0) {
  melted_bridge <- melted_sankey %>%
    filter(`Step to` != 1 | Dest %in% bridge_clusters) %>%
    filter(`Step from` != 1 | Source %in% bridge_clusters)

  write.csv(melted_bridge,
    file = file.path(
      bibliometrics_folder_path,
      settings$metadata$heatmap_analysis_id,
      "sankey_df_without_deadends.csv"
    ),
    row.names = FALSE
  )

  # Readable
  left_side <- melted_bridge %>% filter(`Step to` == 1)
  right_side <- melted_bridge %>% filter(`Step from` == 1)
  melted_bridge_readable <- merge(
    left_side %>%
      select(Source, Dest) %>%
      rename(left = Source, bridge = Dest),
    right_side %>%
      select(Source, Dest) %>%
      rename(right = Dest, bridge = Source),
    by = "bridge",
    all.x = TRUE,
    all.y = TRUE
  )
  # Add topics
  melted_bridge_readable$left_topic <- rcs$cluster_name[match(melted_bridge_readable$left, rcs$cluster_code)]
  melted_bridge_readable$bridge_topic <- rcs$cluster_name[match(melted_bridge_readable$bridge, rcs$cluster_code)]
  melted_bridge_readable$right_topic <- rcs$cluster_name[match(melted_bridge_readable$right, rcs$cluster_code)]

  # Arrange columns
  melted_bridge_readable <- melted_bridge_readable %>% select(left, bridge, right, left_topic, bridge_topic, right_topic)

  write.csv(melted_bridge_readable,
    file = file.path(
      bibliometrics_folder_path,
      settings$metadata$heatmap_analysis_id,
      "sankey_df_without_deadends_readable.csv"
    ),
    row.names = FALSE
  )
}
