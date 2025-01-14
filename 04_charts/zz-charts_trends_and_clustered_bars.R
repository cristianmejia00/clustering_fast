print("###################### zz-trend_and_clustered_bars.R")

# 2023-07-06 Exploratory Data analysis of a dataset

# We expect a dataset with WOS format:
# i.e. multi-factor columns like WC are separated by semicolon.

#INPUTS
dataset <- dataset
categorical_long_reports <- settings$rp$categorical_long_reports
column_labels <- settings$rp$column_labels
output_folder_level <- output_folder_level
subfolder_dataset <- subfolder_dataset
subfolder_clusters <- subfolder_clusters
extension <- extension



###############################################################################
library(glue)
library(scales)


# UTILS
## Generic
# Works for "NS"("AU") categories "CO"("ID") companies "IN"("DE") industries "RE"("C1") regions. Factiva("WOS")
#' @description
#' Top values from columns in a dataset
#' @param df DATAFRAME. the dataset in WOS format
#' @param column STRING. default = 'ID' name of column to summarize
#' @param top INTEGER. default = 5 the number of top items to extract
#' @param decreasing BOOL. {TRUE} whether the items should be sorted by its value. When FALSE they are sorted alphabetically.
#' @param include LIST[STRINGS] items to include
#' @param exclude LIST[STRINGS] items to exclude
#' @returns TABLE[NAMED INTEGERS] the top items and their values as an R table.
get_top_items <- function(df, column = "ID" , top = 5, decreasing = TRUE, include = list(), exclude = list()){
  a_column <- df[[column]] |> as.character()
  categories <- 
    strsplit(a_column, split ="; ") %>% 
    unlist %>% 
    table
  if (decreasing) {
    categories <- categories |> sort(decreasing = TRUE)
  }
  if (length(include) > 0) {
    categories <- categories[names(categories) %in% include]
  }
  if (length(exclude) > 0) {
    categories <- categories[! names(categories) %in% exclude]
  }
  if (top > 0) {
    top1 <- min(top, length(categories))
    categories <-  categories[1:top1]
  }
  return(categories)
}


#' @description
#' Creates a data frame properly formatted by removing NAs and 
#' adjusting for the items found. Tables created here serve as input to ggplots.
#' The input comes from `get_top_items()`.
#' This is a internal function used by `summarize_two_tables()`. 
#' @param summary_table TABLE[NAMED INTEGERS]. the summary table
#' @param category_name STRING. the category being summarized
#' @returns DATAFRAME. the long form dataset needed by ggplot.
format_long_table <- function(summary_table, category_name) {
  # Creates summary tables properly formatted
  # By removing NAs and adjusting for the items found
  
  ## Update the table to remove possible NAs
  summary_table <- summary_table[!is.na(summary_table)] 
  if(length(summary_table) >= 1) {
    data.frame('category' = rep(category_name, length(summary_table)),
               'sub_category' = summary_table |> names() |> as.character() |> tolower(), 
               'value' = summary_table %>% unname %>% as.integer,
               'value_percent_category' = summary_table |> prop.table() |> unname() |> as.numeric() |> round(4)
    )
  }
}



#' @description
#' Summarize column 'a' (categories) in terms of column b (sub-categories).
#' @param df DATAFRAME. the dataset in WOS format
#' @param a_column STRING. name of column defining the categories
#' @param b_column STRING. name of column defining the sub-categories
#' @param a_top INTEGER. {20} the top items to consider from `a_column`. 0 means all.
#' @param a_decreasing BOOL. {TRUE} If items of `a_column` need to be sorted from the largest
#' @param a_include LIST[STRING] items to include in `a_column`
#' @param a_exclude LIST[STRING] items to exclude from `a_column` 
#' @param b_top INTEGER. {20} the top items to consider from `a_column` 0 means all.
#' @param b_decreasing BOOL. {TRUE} If items of `b_column` need to be sorted from the largest
#' @param b_include LIST[STRING] items to include in `b_column`
#' @param b_exclude LIST[STRING] items to exclude from `b_column` 
#' @returns DATAFRAME. with 5 columns: category, sub_category, value, value_percent_category, value_percent_sub
summarize_two_columns <- function(df, 
                                  a_column, 
                                  b_column,
                                  a_top = 20, # 0 means all
                                  a_decreasing = TRUE,
                                  a_include = list(),
                                  a_exclude = list(),
                                  b_top = 20, # 0 means all
                                  b_decreasing = TRUE,
                                  b_include = list(),
                                  b_exclude = list()) {
  
  # Summarize the full columns
  print('Phase 1')
  a_column_values <- get_top_items(df, column = a_column, decreasing = a_decreasing, include = a_include, exclude = a_exclude, top = a_top)
  b_column_values <- get_top_items(df, column = b_column, decreasing = b_decreasing, include = b_include, exclude = b_exclude, top = b_top)
  
  # Get summary
  print('Phase 2')
  tmp_column <- df[[a_column]] %>% as.character() %>% strsplit("; ")
  result_list <- lapply(names(a_column_values), function(x) {
    category_data <- subset(df, sapply(tmp_column, function(y) {as.character(x) %in% y}))
    category_top_subcategories <- get_top_items(category_data, column = b_column, decreasing = b_decreasing, include = b_include, exclude = b_exclude, top = 0)
    return(format_long_table(category_top_subcategories, x))
  }) %>% rbind.fill()
  
  # Add the % based on subcategory
  print('Phase 3')
  result_list$value_percent_sub <- sapply(c(1:nrow(result_list)), function(x) {
    return((result_list$value[x] / b_column_values[names(b_column_values) == result_list$sub_category[x]]) %>% round(4))
  })
  
  return(result_list)
}


#' @description
#' Assign custom colors to labels
#' @param summary_df DATAFRAME. a data frame created with `summarize_two_columns()`.
#' @param legend_palette  LIST[STRING] where the strings are the hex values of the color
#' @returns LIST[STRINGS] where the strings are the hex values of colors for each item.
create_palette <- function(a_summary_df, legend_palette) {
  needed_colors <- length(levels(factor(a_summary_df$category)))
  chart_palette <- hue_pal()(needed_colors)
  names(chart_palette) <- levels(factor(a_summary_df$category))
  
  # Return default if user did not provide palette
  if (length(legend_palette) == 0) {
    print('user did no provided palette')
    return(chart_palette)
  }
  
  # Create the user palette
  # Assign colors to items when user provides named palette
  if (!all(is.na(names(legend_palette)))) {
    tmp_palette <- legend_palette[names(legend_palette) %in% names(chart_palette)]
    if (length(legend_palette) > 0) {
      chart_palette[names(tmp_palette)] <- tmp_palette 
    }
  } else {
    # If not named, simply add the palette to the front of the default
    if (length(legend_palette) > length(chart_palette)) {
      legend_palette <- legend_palette[c(1:length(chart_palette))]
    }
    chart_palette[c(1:length(legend_palette))] <- legend_palette
  }
  return(chart_palette)
}


#' @description
#' Plot a ggplot line plot of normalized values
#' @param a_summary_df DATAFRAME. a dataframe generated with `summarize_two_columns()`
#' @param a_column_label STRING. Label of column defining categories. i.e. X-axis label
#' @param b_column_label  STRING. Label of column defining sub-categories. i.e. legend title
#' @param zero_one BOOL. {TRUE} If the axis extends up to 1.
#' @param a_decreasing BOOL. {TRUE}
#' @param b_decreasing BOOL. {TRUE}
#' @param legend_palette LIST[STRINGS] a list of hex values to color the legend items
#' @returns GGPLOT. a line plot
plot_percent_line <- function(a_summary_df, 
                              a_column_label, 
                              b_column_label, 
                              zero_to_one = TRUE, 
                              a_decreasing = TRUE, 
                              b_decreasing = TRUE, 
                              legend_palette = list()) {
  if (b_decreasing) {
    sub_cat_order <- tapply(a_summary_df$value, a_summary_df$sub_category, sum, na.rm=TRUE) %>% sort(decreasing = TRUE) %>% names
    a_summary_df$sub_category <- factor(a_summary_df$sub_category, levels = sub_cat_order)
  }
  if (a_decreasing) {
    cat_order <- tapply(a_summary_df$value, a_summary_df$category, sum, na.rm=TRUE) %>% sort(decreasing = TRUE) %>% names
    a_summary_df$category <- factor(a_summary_df$category, levels = cat_order)
  }
  n_categories <- length(unique(a_summary_df$category))
  p <- ggplot(a_summary_df, aes(x = sub_category, 
                                y = value_percent_sub, 
                                group = category, 
                                color = category,
                                shape = category)) +
    geom_line(linewidth=0.7) +
    geom_point() +
    scale_shape_manual(values=seq(0,n_categories)) +
    labs(x = b_column_label, 
         y = "% of documents", 
         color = a_column_label,
         shape = a_column_label,
         title = glue("Percentage of {a_column_label} by {b_column_label}")) + 
    theme_bw()
  if (zero_to_one) {
    p <- p + scale_y_continuous(limits = c(0, 1))
  }
  
  # Add palette
  chart_palette = create_palette(a_summary_df, legend_palette)
  print('---')
  p <- p + scale_color_manual(values=chart_palette)
  
  return(p)
}


#' @description
#' Plot a ggplot of clustered bars.
#' @param a_summary_df DATAFRAME. a dataframe generated with `summarize_two_columns()`
#' @param a_column_label STRING. Label of column defining categories. i.e. X-axis label
#' @param b_column_label  STRING. Label of column defining sub-categories. i.e. legend title
#' @param a_decreasing BOOL. {TRUE}
#' @param b_decreasing BOOL. {TRUE}
#' @param legend_palette LIST[STRINGS] a list of hex values to color the legend items
#' @returns GGPLOT. a clustered bar plot
plot_clustered_bars <- function(a_summary_df, 
                                a_column_label, 
                                b_column_label, 
                                a_decreasing = TRUE, 
                                b_decreasing = TRUE, 
                                legend_palette = list()) {
  
  if (b_decreasing) {
    sub_cat_order <- tapply(a_summary_df$value, a_summary_df$sub_category, sum, na.rm=TRUE) %>% sort(decreasing = TRUE) %>% names
    a_summary_df$sub_category <- factor(a_summary_df$sub_category, levels = sub_cat_order)
  }
  if (a_decreasing) {
    cat_order <- tapply(a_summary_df$value, a_summary_df$category, sum, na.rm=TRUE) %>% sort(decreasing = TRUE) %>% names
    a_summary_df$category <- factor(a_summary_df$category, levels = cat_order)
  }
  p <- ggplot(a_summary_df, aes(x = sub_category, y = value, fill = category)) +
    geom_bar(width=.6, stat='identity', position=position_dodge(.7)) +
    geom_text(stat='identity', 
              position=position_dodge(.7), 
              vjust=1.5,
              size = 2,
              aes(label=paste(format(round(value_percent_sub * 100, 1), nsmall = 0), "%"))) +
    labs(x = b_column_label, 
         y = "Documents", 
         fill = a_column_label,
         title = glue("Distribution of {a_column_label} by {b_column_label}")) + 
    theme_bw()
  
  # Add palette
  chart_palette = create_palette(a_summary_df, legend_palette)
  p + scale_fill_manual(values=chart_Palette)
  
  return(p)
}


########################################################
# Custom palettes
default_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#9b5de5", "#2270e7", "#e5e510", "#f00f15", "#3524ae", "#26cc3a", "#ec058e", "#9cb8c2", "#fffdd0", "#b40e68", "#AFA100", "#F67963")
color_blind_Palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
sentiment_palette <- c('positive' = "#00BFC4", 'neutral'= "#999999", 'negative'="#CC79A7")
issue_palette <- c('economic'= "#999999", 'environmental'= "#009E73", 'health'= "#56B4E9", 'social'= "#E69F00")

########################################################
########################################################
########################################################
dataset$DE <- tolower(dataset$DE)
PY_max <- min(max(dataset$PY, na.rm = TRUE), settings$rp$most_recent_year)
PY_min <- PY_max - 10 + 1

# Compute yearly trends of clusters
cluster_year <- summarize_two_columns(df = dataset, 
                                      a_column = 'X_C',
                                      a_exclude = c(99),
                                      b_column = 'PY',
                                      b_include = c(PY_min:PY_max),
                                      b_decreasing = FALSE,
                                      b_top = 0)
plot_percent_line(cluster_year, 
                  a_column_label = 'Cluster', 
                  b_column_label = 'Year',
                  b_decreasing = FALSE,
                  zero_to_one = FALSE,
                  legend_palette = color_blind_Palette)

ggsave(file.path(output_folder_level, 
                 subfolder_dataset, 
                 glue("fig_yearly_trends_clusters.{extension}")),
       dpi = 300)

# Compute yearly trends at the dataset level for different columns
trend_reports <- list()
available_charts <- intersect(settings$rp$categorical_long_reports, colnames(dataset))
for (i in available_charts) {
  tmp <- summarize_two_columns(df = dataset, 
                                a_column = i,
                                b_column = 'PY',
                                b_include = c(PY_min:PY_max),
                                b_decreasing = FALSE,
                                b_top = 0)
  plot_percent_line(tmp, 
                    a_column_label = settings$rp$column_labels[i], 
                    b_column_label = 'Year',
                    b_decreasing = FALSE,
                    zero_to_one = FALSE,
                    legend_palette = color_blind_Palette)
  
  ggsave(file.path(output_folder_level, 
                   subfolder_dataset, 
                   glue("fig_yearly_trends_{i}.{extension}")))
}

##############################################################################
# Use from here for manual edits. 
# 
# journal_year <- summarize_two_columns(df = dataset, 
#                                       a_column = 'SO',
#                                       b_column = 'PY',
#                                       b_include = c(PY_min:PY_max),
#                                       b_decreasing = FALSE,
#                                       b_top = 0)
# 
# 
# country_year <- summarize_two_columns(df = dataset, 
#                                       a_column = 'Countries',
#                                       b_column = 'PY',
#                                       b_include = c(PY_min:PY_max),
#                                       b_decreasing = FALSE,
#                                       b_top = 0)
# wc_year <- summarize_two_columns(df = dataset, 
#                                  a_column = 'Institutions',
#                                  b_column = 'PY',
#                                  b_include = c(PY_min:PY_max),
#                                  b_decreasing = FALSE,
#                                  b_top = 0)
# wc_year <- summarize_two_columns(df = dataset, 
#                                       a_column = 'WC',
#                                       b_column = 'PY',
#                                       b_include = c(PY_min:PY_max),
#                                       b_decreasing = FALSE,
#                                       b_top = 0)
# keywords_year <- summarize_two_columns(df = dataset, 
#                                        a_column = 'DE',
#                                        b_column = 'PY',
#                                        b_include = c(PY_min:PY_max),
#                                        b_decreasing = FALSE,
#                                        b_top = 0)
# 
# 
# 
# #############################################################################
# #############################################################################
# 
# 
# plot_percent_line(cluster_year, 
#                   a_column_label = 'Cluster', 
#                   b_column_label = 'Year',
#                   b_decreasing = FALSE,
#                   zero_to_one = FALSE,
#                   legend_palette = color_blind_Palette)
# 
# plot_percent_line(journal_year, 
#                   a_column_label = 'Countries', 
#                   b_column_label = 'Year',
#                   b_decreasing = FALSE,
#                   zero_to_one = FALSE,
#                   legend_palette = color_blind_Palette)
# plot_percent_line(country_year, 
#                   a_column_label = 'Countries', 
#                   b_column_label = 'Year',
#                   b_decreasing = FALSE,
#                   zero_to_one = FALSE,
#                   legend_palette = color_blind_Palette)
# plot_percent_line(keywords_year, 
#                   a_column_label = 'Keywords', 
#                   b_column_label = 'Year',
#                   b_decreasing = FALSE,
#                   zero_to_one = FALSE,
#                   legend_palette = color_blind_Palette)
# 
# #############################################################################
# #############################################################################
# plot_clustered_bars(sentiment_year,
#                     a_column_label = 'Sentiment',
#                     b_column_label = 'Year',
#                     a_decreasing = FALSE,
#                     b_decreasing = FALSE)
# plot_clustered_bars(policy_cluster,
#                     a_column_label = 'Policy-related news',
#                     b_column_label = 'Cluster')
# plot_clustered_bars(sentiment_cluster,
#                     a_column_label = 'Sentiment',
#                     a_decreasing = FALSE,
#                     b_column_label = 'Cluster')
# plot_clustered_bars(issue_cluster,
#                     a_column_label = 'Issue',
#                     b_column_label = 'Cluster')
# plot_clustered_bars(issue_sentiment,
#                     a_column_label = 'Sentiment',
#                     a_decreasing = FALSE,
#                     b_column_label = 'Issue')
