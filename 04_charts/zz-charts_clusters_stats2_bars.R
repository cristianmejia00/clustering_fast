print("###################### zz-charts_clusters_stats2_bars.R")

# 20230719
# Cluster-level charts
# Bar charts for the cluster sizes
# All bar charts per cluster for rp$categorical_long_reports:
# "AU", "WC", "SO", "Countries", "Institutions", "DE", "sentiment_factor",
# "PY.

# INPUTS
categorical_long_reports <- settings$rp$categorical_long_reports
column_labels <- settings$rp$column_labels
output_folder_level <- output_folder_level
subfolder_clusters <- subfolder_clusters
extension <- extension


################################################################################
# Create output folder
dir.create(file.path(output_folder_level, subfolder_clusters, 'by_clusters'), recursive = TRUE)
dir.create(file.path(output_folder_level, subfolder_clusters, 'by_columns'), recursive = TRUE)


# Libraries
library(glue)
library(data.table)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(stringr)


# Load datasets in settings$rp$categorical_long_reports
charts_datasets <- list()
for (i in categorical_long_reports) {
  report_name <- glue('report_{i}.csv')
  report_path <- file.path(output_folder_level, report_name)
  if (file.exists(report_path)) {
    charts_datasets[[i]] <- read.csv(report_path)
  }
}

# Load keywords
report_path <- file.path(output_folder_level, "report_keyword.csv")
if (file.exists(report_path)) {
  tmp <- read.csv(report_path)
  tmp <- subset(tmp, type == 'probable') #tf, tfidf, probable, exclusive
  setnames(tmp, c("term", "freq", "cluster", "type", "normalized"), c("Term", "Freq", "Cluster", "Type", "Normalized"))

  charts_datasets[['Keywords']] <- tmp
}

# Utils
#' @description
#' Compute the number of digits of integer.
#' This is code taken from StackOverflow and is faster than the nchar(as.character(x))
#' @param x INTEGER. a positive integer
get_digits <- function(x){
  floor(log10(x)) + 1
}


#' @description
#' Generate a ggplot bar plot for cluster's categorical columns
#' @param plot_data DATAFRAME. a long report for a given column
#' @param cluster_number INTEGER. the cluster number
#' @param column_data_position INTEGER. {2} The position of the column with data values
#' @param item_label STRING. {"ITEM"} the column label
#' @param document_label STRING. {"Documents"} what is being counted
#' @returns a bar plot ggplot
plot_cluster_data <- function(plot_data, cluster_number, column_data_position = 2, item_label = 'Item', document_label = 'Documents') {
  plot_data <- plot_data[plot_data$Cluster != 0,]
  cluster_data <- plot_data[plot_data$Cluster == cluster_number,]
  # Remove potential white spaces
  cluster_data <- cluster_data[cluster_data[[1]] != '',]
  cluster_data <- cluster_data[cluster_data[[1]] != ' ',]
  cluster_data <- cluster_data[cluster_data[[1]] != 'NA',]
  cluster_data <- cluster_data[!is.na(cluster_data[[1]]),]
  # Adjust label size
  cluster_data[[1]] <- tolower(cluster_data[[1]]) %>% 
                       substr(start = 0, stop = 20)
  cluster_data[[1]][duplicated(cluster_data[[1]])] <- paste(cluster_data[[1]][duplicated(cluster_data[[1]])], ' ', sep = '')
  cluster_levels <- cluster_data[[1]][order(cluster_data[[column_data_position]])]
  cluster_levels <- cluster_levels[!is.na(cluster_levels)]
  cluster_labels <- sapply(cluster_levels, 
                           function(x) {
                             if (nchar(x) >= 20) {
                               paste(x, '...', sep = '')
                              } else {
                                x
                              }})
  cluster_data[[1]] <- factor(cluster_data[[1]], 
                              levels = cluster_levels,
                              labels = cluster_labels)
  p <- ggplot(cluster_data[c(1:min(5, nrow(cluster_data))),], 
              aes(x=.data[[names(cluster_data)[1]]], 
                  y=.data[[names(cluster_data)[column_data_position]]],
                  na.rm = TRUE)) + 
        geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + 
        scale_y_continuous(name=document_label, limits=c(0, max(plot_data[[column_data_position]]))) + 
        scale_x_discrete(name=item_label) +
        coord_flip() +
        theme_bw()

  return(p)
}

# Plot and save
available_charts <- names(charts_datasets)
for (i in available_charts) {
  tmp <- charts_datasets[[i]]
  clusters_n <- unique(tmp$Cluster) %>% sort()
  clusters_n <- clusters_n[clusters_n != 0]
  char_size <- get_digits(max(clusters_n))
  for (j in clusters_n) {
    print(i)
    print(j)
    if (tolower(i) %in% c('keyword', 'keywords')) {
      plot_cluster_data(tmp, j, column_data_position = 5, item_label = column_labels[i], document_label ='TFIDF')
    } else {
      plot_cluster_data(tmp, j, item_label = column_labels[i])
    }

    
    # by columns
    if (extension != 'svg') {
      xxfile <- file.path(output_folder_level,
                subfolder_clusters,
                'by_columns',
                glue('fig_{i}_{str_pad(j, char_size, "left", "0")}_.{extension}'))
      print(xxfile)
      ggsave(filename = xxfile, 
             width = 1000, 
             height = 1000, 
             units = 'px')
    }

    # by cluster
    xxfile <- file.path(output_folder_level,
                        subfolder_clusters,
                        'by_clusters',
                        glue('fig_{str_pad(j, char_size, "left", "0")}_{i}_.{extension}'))
    print(xxfile)
    ggsave(filename = xxfile, 
           width = 1000, 
           height = 1000, 
           units = 'px')
  }
}

