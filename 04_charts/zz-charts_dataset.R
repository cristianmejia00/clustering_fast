print("###################### zz-charts_dataset.R")

# 2023/07/13
# Dataset-level stats
# All bar charts in this dataset for rp$categorical_long_reports:
# "AU", "WC", "SO", "Countries", "Institutions", "DE", "sentiment_factor",
# "PY.

# Load plotting library
library(ggplot2)
library(glue)

# INPUT
dataset <- dataset
document_label <- toTitleCase(settings$params$dataset_source)
output_folder_level <- output_folder_level
subfolder_dataset <- subfolder_dataset #"charts_dataset"
extension <- extension
available_columns <- colnames(dataset)
column_labels <- settings$rp$column_labels
categorical_long_reports <- settings$rp$categorical_long_reports

# Create directory to store charts
dir.create(file.path(output_folder_level, subfolder_dataset), recursive = TRUE)


##################################################################
##################################################################
##################################################################
# BAR CHARTS   --> summarize categorical columns
##################################################################
# UTILS
#' @description
#' Takes a STRING or FACTOR column and saves the report with all values and a bar chart of top_items.
#' @param column_data LIST. a DF column in the form of `dataset$something`
#' @param column_data STRING. the column header of this column
#' @param item_label STRING. {'ITEM'} the label of the category axis
#' @param document_label STRING. {'DOCUMENT'} 'Document' or 'Paper', 'News, 'Patent', etc.
#' @param top_items INTEGER. {20} the max number of bars to show. The report will contain everything anyways.
#' @param sorted_bars BOOLEAN. {TRUE}, sort from the highest
#' @param horizontal BOOLEAN. {TRUE}, for horizontal bars
#' @returns nothing. But it saves the plot and report
create_report_and_barchart <- function(column_data,
                                       column_name,
                                       item_label = "Item",
                                       document_label = "Documents",
                                       top_items = 20,
                                       sorted_bars = TRUE,
                                       horizontal = TRUE) {
  stats_column <- column_data %>%
    as.character() %>%
    strsplit("; ") %>%
    unlist() %>%
    tolower() %>%
    toTitleCase() %>% 
    gsub('Ieee', 'IEEE', .) %>%
    gsub('International', 'Int.', .) %>% 
    gsub('Usa', 'USA', .) %>%
    gsub('Peoples r China', 'China', .) %>% 
    substr(1,45) %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    data.frame() %>%
    setNames(c("Item", "Documents"))
  if (extension != 'svg') {
    write.csv(stats_column,
              file = file.path(output_folder_level, 
                               subfolder_dataset, 
                               glue("dataset_{column_name}.csv")),
              row.names = FALSE)
  }

  ggplot(stats_column[c(1:min(nrow(stats_column), top_items)), ], aes(x = Item, y = Documents)) +
    geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") +
    theme(axis.text=element_text(size=14)) +
    coord_flip() +
    scale_x_discrete(name = item_label, limits = rev) +
    scale_y_continuous(name = document_label) +
    theme_bw()
  ggsave(file.path(output_folder_level, 
                   subfolder_dataset, 
                   glue("fig_{gsub(' ', '_', column_name)}.{extension}")), 
         width = 1000, 
         height = 1000, 
         units = 'px')
}

################################################################################
# TEXT COLUMNS TRENDS
################################################################################
for (i in categorical_long_reports) {
  if (i %in% available_columns) {
    print(i)
    create_report_and_barchart(dataset[[i]], 
                               column_name = i,
                               item_label = column_labels[i])
  }
}

################################################################################
# YEARLY TRENDS
################################################################################
yearly_trends <- dataset$PY %>%
  as.numeric() %>%
  table() %>%
  data.frame() %>%
  setNames(c("Year", "Articles"))
yearly_trends
yearly_trends <- yearly_trends[order(yearly_trends$Year, decreasing = TRUE), ]
if (extension != 'svg') {
  write.csv(yearly_trends, file = file.path(output_folder_level, subfolder_dataset, "data_yearly_trends.csv"), row.names = FALSE)
}

ggplot(yearly_trends[1:10, ], aes(x = Year, y = Articles)) +
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") +
  theme_bw()
ggsave(file.path(output_folder_level, subfolder_dataset, glue("fig_yearly_trends.{extension}")))
