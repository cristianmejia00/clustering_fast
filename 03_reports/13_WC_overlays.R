print("###################### reports/13_WC_overlays.R")

# 20220821; 20171206
# Create the files to be used in VosViewer.
# It does the same as the WC15.exe from Leydesdorff page. https://www.leydesdorff.net/wc15/index.htm
# To recreate the overlay_master see at the end

# As input we need the "overlay_master" object
# And the WC_per_cluster_count object

library("tools")
library("ggrepel")
load(file.path(getwd(), "zz_assets", "overlay_master.rdata"))
WC_per_cluster_counts <- read.csv(file.path(output_folder_level, "report_WC_frequencies.csv"), check.names = FALSE)

# Add the totals to the last row
WC_per_cluster_counts[nrow(WC_per_cluster_counts) + 1,] <- as.list(unname(colSums(WC_per_cluster_counts)))

# Confirm the labels have same format
colnames(WC_per_cluster_counts) <- toTitleCase(colnames(WC_per_cluster_counts))
overlay_master$label <- toTitleCase(overlay_master$label)

# Create directory
dir.create(file.path(output_folder_level, "overlays"))


# VosViewer file creator:
# cls can be 18 or 5 (it is the cluster colors VosViwer will display)
get_overlay_data <- function(a_data_frame, cls = 18) {
  
  valid_WC <- intersect(colnames(a_data_frame), overlay_master$label)
  valid_df <- a_data_frame[,valid_WC]
  
  copy_master <- overlay_master
  index <- match(colnames(valid_df), overlay_master$label)
  
  copy_master$weight[index] <- as.integer(valid_df[1,])
  
  copy_master$label <- sapply(1:nrow(copy_master), function (x) {
    if (copy_master$weight[x] == 0) {""} else {copy_master$label[x]}
  })
  
  # Remove unnecessary categories
  copy_master <- copy_master[copy_master$label != "",]
  
  # Order from the largest
  copy_master <- copy_master[order(copy_master$weight, decreasing = TRUE),]
  
  # Only show labels for top 15
  if (nrow(copy_master) > 15) {
    copy_master$label[c(15: nrow(copy_master))] <- ""
  }
  
  # Clusters of categories must be characters
  copy_master$cluster18 <- as.character(copy_master$cluster18)
  copy_master$cluster5 <- as.character(copy_master$cluster5)
  
  if (cls == 18) {copy_master$cluster5 <- NULL} else {copy_master$cluster18 <- NULL}
  setnames(copy_master, colnames(copy_master)[ncol(copy_master)], "cluster")
  return(copy_master)
}


# Select a cluster and return the vosviewer file
# a_cluster is character string of the cluster ID
get_overlay_image <- function(a_cluster, write_file = FALSE) {
  temp_df <- WC_per_cluster_counts[which(row.names(WC_per_cluster_counts) == as.character(a_cluster)), ]
  overlay <- get_overlay_data(temp_df, cls = 5)
  
  # Image
  p <- ggplot(overlay, aes(x=x, y=y, colour=cluster, size=weight)) + 
    geom_point() +
    xlim(min(overlay_master$x), max(overlay_master$x)) +
    ylim(min(overlay_master$y), max(overlay_master$y)) +
    theme(axis.text=element_text(size=14), 
          panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          axis.title = element_blank(),
          axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(), 
          axis.text.y=element_blank(),  
          axis.ticks.y=element_blank(),  
          legend.position = "none"
    )
  p + geom_text_repel(aes(label=label, colour=cluster), max.overlaps = 20) + 
    scale_color_brewer(palette = "Set1")
  
  # By convention I'm calling 0 to the "all cluster" results across report.
  # Here, I convert the last cluster in the file as cluster 0.
  if (a_cluster == nrow(WC_per_cluster_counts)) {
    a_cluster <- 0
  }
  
  ggsave(file.path(output_folder_level, "overlays", paste(as.character(a_cluster),".jpg", sep = "")))
  
  if (write_file) {
    file_name <- file.path(output_folder_level, "overlays", paste(as.character(a_cluster),".csv", sep = ""))
    write.csv(overlay, row.names = FALSE, file = file_name)   
  }
}


# Create an overlay map per cluster
for (i in c(1:nrow(WC_per_cluster_counts))) {
  get_overlay_image(i)
}
