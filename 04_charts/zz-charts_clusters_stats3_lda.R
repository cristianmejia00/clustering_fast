print("###################### zz-charts_clusters_stats3_lda.R")

# 20230729
# Save the cluster plot in the style of LDAvis for Topic Models
# i.e. A Bubble charts of relative positions
# this assumes that the LDA-vis was created and is saved in the folder "keyword Explorer"
# The x and y coords are extracted from there.

subfolder_clusters <- subfolder_clusters
extension <- extension
rcs_merged <- rcs_merged

#########################################
#########################################
print("-- Creating Static LDA-like viz")
#########################################
library(jsonlite)
library(ggrepel)

#' @description
#' Plot clusters in the style of the LDA topic models
#' @param rcs DATAFRAME. the RCS with columns x, y, cluster_name (X_C_name), documents, and the column for controlling colors
#' @param color_values STRING. The column name that define the color gradient
#' @param color_label STRING. The display name of the column defining the color gradient
#' @param size_label STRING. {'Documents'} The column name defining the size of the cluster
#' @param color_gradient_limits LIST[INTEGER] A numeric vector of with the min and max values of the gradient
#' @param color_gradient LIST[STRING] {c("red", "grey", "green")} A character vector with 3 hex colors or R color names for the min, mid, high colors.
plot_clusters <- function(rcs, 
                          color_values = 'PY_Mean',
                          color_label = 'PY_Mean', 
                          size_label = 'Documents', 
                          color_gradient_limits = c(min(rcs$PY_Mean), max(rcs$PY_Mean)), 
                          color_gradient = c("red", "grey", "green")) {
  ggplot(rcs, aes(x = x, y = y, color = .data[[color_values]], size = documents), stroke=NA) +
    geom_vline(xintercept = min(rcs$x) + (max(rcs$x) - min(rcs$x)) / 2, color = 'gray') + 
    geom_hline(yintercept = min(rcs$y) + (max(rcs$y) - min(rcs$y)) / 2, color = 'gray') +
    geom_point() + 
    scale_size(name = size_label, range = c(2,20), trans = "identity") +
    scale_color_gradientn(name = color_label,
                          colors = color_gradient,
                          limits = color_gradient_limits) +
    geom_text_repel(aes(label = gsub("---|-0", "", X_C_name)), 
                    size=3, 
                    color='black') + 
    theme(panel.background = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
}

############################################################
# Get the coords

if (file.exists(file.path(output_folder_level, 'keyword_explorer', 'lda.json'))) {
  coords <- fromJSON(txt=file.path(output_folder_level, 'keyword_explorer', 'lda.json'))
  coords <- data.frame('cluster' = coords$mdsDat$topics,
                       'x' = coords$mdsDat$x,
                       'y' = coords$mdsDat$y)
  tmp <- merge(rcs_merged, coords, by='cluster')
  if ((nrow(tmp) <= 99) & (99 %in% tmp$cluster_code)) {
    tmp <- tmp[tmp$cluster_code != 99,]
  }
  
  # Plot clusters with color gradient based on years
  plot_clusters(tmp)
  ggsave(file.path(output_folder_level, subfolder_clusters, glue('clusters_lda_years.{extension}')))
  
  # Plot clusters with color gradient based on sentiment
  if ('sentiment_Mean' %in% colnames(tmp)) {
    plot_clusters(tmp, color_values = 'sentiment_Mean', color_label = 'Sentiment', color_gradient_limits = c(-1,1))
    ggsave(file.path(output_folder_level, subfolder_clusters, glue('clusters_lda_sentiment.{extension}')))
  }
}
