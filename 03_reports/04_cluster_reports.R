# 20181206 Cluster summaries
print("###################### reports/04_cluster_reports.R")

# Load utils
source("zz_utils/zz_auxiliary_functions.R")

# INPUT
list_of_clusters <- myDataCorrect$X_C %>%
  unique() %>%
  sort()
available_columns <- colnames(myDataCorrect)

## Check the columns that are actually available
categorical_long_reports <- settings$rp$categorical_long_reports %>%
  unlist() %>%
  .[. %in% available_columns]
categorical_simple_wide_reports <- settings$rp$categorical_simple_wide_reports %>%
  unlist() %>%
  .[. %in% available_columns]
categorical_multi_wide_reports <- settings$rp$categorical_multi_wide_reports %>%
  unlist() %>%
  .[. %in% available_columns]
numerical_reports <- settings$rp$numerical_reports %>%
  unlist() %>%
  .[. %in% available_columns]


## DEFINITIONS

## A long table means that there are 3 columns:
## X_C
## The comparative column which is a factor (e.g. PY,Journals,Countries,etc.)
## The value

## A wide table means it is a matrix:
## X_C is the rows
## The values of the comparative column which is a factor (e.g. PY,Journals,Countries,etc.) represents
## as many columns needed
## The intersection is the value (e.g. of cluster 1 (row) vs Country US(column))

## A numerical report
## Takes a numeric column and computes
## min, 1q, mean, media, 3q, max, NAs

##################################################################
## Utils:
#' @description
#' Creates summary df properly formatted by removing NAs and adjusting for the items found
#' @param a_table TABLE. Usually the output of `TopSomething()`
#' @param a_cluster INTEGER. The cluster being formatted.
#' @param top INTEGER. the number of results to include in the report
#' @returns DATAFRAME. A long shaped data frame
format_long_table <- function(a_table, a_cluster, top) {
  ## Update the table to remove possible NAs
  a_table <- a_table[!is.na(a_table)]
  if (length(a_table) >= 1) {
    ## Update "top" in case the table contains fewer values.
    top1 <- min(top, length(a_table))
    data.frame(toupper(names(a_table)), unname(a_table), rep(a_cluster, top1))
  }
}

#' @description
#' Creates a long report based on selected column.
#' @param df DATAFRAME. Usually `myDataCorrect`.
#' @param a_column STRING. the name of the column to summarize
#' @param clusters LIST[INTEGERS]. a list of clusters to include in the summary
#' @param top INTEGER. the number of results to include in the report
#' @param with_all BOOL. if the summary including all data should be include as `Cluster 0`
#' @returns Nothing. --> It writes a .csv with the report.
generate_long_report <- function(df, a_column, clusters, top, with_all = TRUE) {
  ## Get clusters' results
  result_list <- lapply(clusters, function(c) {
    cluster_data <- subset(df, df$"X_C" == c)
    cluster_tops <- TopSomething(cluster_data, coll = a_column, top = top)
    return(format_long_table(cluster_tops, a_cluster = c, top = top))
  }) %>% rbind.fill()
  if (ncol(result_list) > 4) {
    result_list <- result_list[, c(1:4)]
  }

  ## Insert `cluster 0` summary
  if (with_all & a_column != "AU") {
    cluster_zero <- TopSomething(df, coll = a_column, top = top) %>%
      format_long_table(a_cluster = 0, top = top)
    result_list <- rbind(cluster_zero, result_list)
  }

  ## Romat and write
  if (ncol(result_list) == 3) {
    colnames(result_list) <- c(a_column, "Freq", "Cluster")
  }
  if (ncol(result_list) == 4) {
    colnames(result_list) <- c(a_column, "X", "Freq", "Cluster")
  }
  result_list$X <- NULL
  result_list <- result_list[!is.na(result_list[, c(a_column)]), ]
  write.csv(result_list, file = file.path(output_folder_level, paste("report_", a_column, ".csv", sep = "")), row.names = FALSE)
}

## Write reports
for (cc in categorical_long_reports) {
  generate_long_report(df = myDataCorrect, a_column = cc, clusters = list_of_clusters, top = settings$rp$top_items)
}


generate_categorical_simple_wide_reports <- function(df, a_column) {
  # Creates a `clusters x a_column` reports for frequencies and proportions based on selected column.
  # This column has a single value per record e.g. PY
  # df = a data frame. Usually `myDataCorrect` or any other with X_C column
  # a_column = the column to summarize
  # clusters = a list of clusters to include in the summary
  cluster_frequencies <- table(df$X_C, df[, a_column]) %>% as.matrix()
  cluster_proportions <- prop.table(cluster_frequencies, 2)

  cluster_frequencies <- cbind("cluster" = row.names(cluster_frequencies), cluster_frequencies)
  cluster_proportions <- cbind("cluster" = row.names(cluster_proportions), cluster_proportions)


  write.csv(cluster_frequencies,
    file = file.path(output_folder_level, paste("report_", a_column, "_frequencies.csv", sep = "")),
    row.names = FALSE
  )
  write.csv(cluster_proportions,
    file = file.path(output_folder_level, paste("report_", a_column, "_proportions.csv", sep = "")),
    row.names = FALSE
  )
}

generate_categorical_multi_wide_reports <- function(df, a_column, clusters) {
  # Creates a `clusters x a_column` reports for frequencies and proportions based on selected column.
  # This column has a multiple values per record e.g. WC because a paper can be "Engineering; Finances"
  # df = a data frame. Usually `myDataCorrect` or any other with X_C column
  # a_column = the column to summarize
  # clusters = a list of clusters to include in the summary
  column_summary <- df[, a_column] %>%
    strsplit(., split = "; ") %>%
    unlist() %>%
    table() %>%
    sort(., decreasing = TRUE)

  # Define the max number of categories to analyze
  max_cols <- if (length(names(column_summary)) > 100) {
    100
  } else {
    length(names(column_summary))
  }
  column_summary <- column_summary[1:max_cols]

  # WC frequencies by cluster
  cluster_frequencies <- lapply(clusters, function(x) {
    temp <- df[, a_column][which(df$X_C == x)]
    column_summary <- temp %>%
      strsplit(., split = "; ") %>%
      unlist() %>%
      table() %>%
      as.matrix() %>%
      t() %>%
      data.frame(., stringsAsFactors = FALSE, check.names = FALSE)
    if (ncol(column_summary) == 0) {
      column_summary <- data.frame("no_data" = 1)
    }
    return(column_summary)
  }) %>%
    rbindlist(., fill = TRUE) %>%
    as.data.frame() %>%
    .[, names(column_summary)[1:max_cols]]
  cluster_frequencies[is.na(cluster_frequencies)] <- 0
  cluster_proportions <- t(t(as.matrix(cluster_frequencies)) / as.integer(column_summary))

  cluster_frequencies <- cbind("cluster" = clusters, cluster_frequencies)
  cluster_proportions <- cbind("cluster" = clusters, cluster_proportions)

  write.csv(cluster_frequencies,
    file = file.path(output_folder_level, paste("report_", a_column, "_frequencies.csv", sep = "")),
    row.names = FALSE
  )

  write.csv(cluster_proportions,
    file = file.path(output_folder_level, paste("report_", a_column, "_proportions.csv", sep = "")),
    row.names = FALSE
  )
}

generate_numerical_report <- function(df, a_column, clusters, with_all = TRUE) {
  # Creates a  numeric summary (min, mean, meadian, max, sd) report based on selected column.
  # df = a data frame. Usually `myDataCorrect` or any other with X_C column
  # a_column = the column to summarize
  # clusters = a list of clusters to include in the summary
  # with_all = if the summary including all data should be include as `Cluster 0`

  ## Get clusters' results
  result_list <- lapply(clusters, function(c) {
    cluster_data <- subset(df, df$"X_C" == c)
    tmp <- summary(cluster_data[, a_column]) %>%
      as.matrix() %>%
      t() %>%
      data.frame()
    tmp$sd <- sd(cluster_data[, a_column], na.rm = TRUE) %>% round(3)
    tmp$cluster <- c
    return(tmp)
  }) %>% rbind.fill()

  ## Insert `cluster 0` summary
  if (with_all) {
    cluster_zero <- summary(df[, a_column]) %>%
      as.matrix() %>%
      t() %>%
      data.frame()
    cluster_zero$sd <- sd(df[, a_column]) %>% round(3)
    cluster_zero$cluster <- 0
    result_list <- rbind.fill(cluster_zero, result_list)
  }

  ## Format and write
  write.csv(result_list,
    file = file.path(output_folder_level, paste("report_", a_column, ".csv", sep = "")),
    row.names = FALSE
  )
}

##################################################################

## Write reports
for (cc in categorical_long_reports) {
  generate_long_report(df = myDataCorrect, a_column = cc, clusters = list_of_clusters, top = settings$rp$top_items)
}

for (cc in categorical_simple_wide_reports) {
  generate_categorical_simple_wide_reports(df = myDataCorrect, a_column = cc)
}

for (cc in categorical_multi_wide_reports) {
  generate_categorical_multi_wide_reports(df = myDataCorrect, a_column = cc, clusters = list_of_clusters)
}

for (cc in numerical_reports) {
  generate_numerical_report(df = myDataCorrect, a_column = cc, clusters = list_of_clusters)
}

# Cleaning up
rm(
  "cc", "list_of_clusters", "available_columns",
  "categorical_long_reports", "categorical_simple_wide_reports",
  "categorical_multi_wide_reports", "numerical_reports"
)
