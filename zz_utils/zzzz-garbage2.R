# Potentially garbage code.
# This code was created the day I fixed the issue of adding colors to 
# subcluster boxplots based on their main cluster.
# The function was fixed and worked fine as run by this code. 
# This was tested with the Nature Capital dataset

value_column <- "PY"
category_column <- "X_C"
value_label <- value_column
category_label <- category_column

# Get mean and median for sorting
compound_mean <- tapply(dataset[[value_column]], dataset[[category_column]], mean, na.rm = TRUE)
compound_median <- tapply(dataset[[value_column]], dataset[[category_column]], median, na.rm = TRUE)

# Match to the sort in RCS_merged
compound_mean <- compound_mean[rcs_merged$cluster_code]
compound_median <- compound_median[rcs_merged$cluster_code]


# Prepare df
long <- dataset[, c(category_column, value_column)]
setnames(long, c(category_column, value_column), c("category", "values"))
long$category <-  gsub("---|-0", "", as.character(long$category))
lvl <- rcs_merged$cluster_code[order(compound_median, compound_mean)]
lvl <- gsub("---|-0", "", lvl)
long$category <- factor(long$category, 
                        levels = lvl)

long$main_cluster <- strsplit(as.character(long$category), "-")
long$main_cluster <- sapply(long$main_cluster, function(x) {x[[1]]})
long$main_cluster <- factor(long$main_cluster,
                            levels = long$main_cluster %>% 
                              unique() %>% 
                              as.numeric() %>% 
                              sort())

bp <- ggplot(long, aes(x = category, 
                       y = values, 
                       fill = main_cluster)) +
  geom_boxplot(
    width = 0.7
  ) +
  xlab(category_label) +
  ylab(value_label) +
  scale_fill_manual(values = default_palette) +
  theme_bw()

K <- length(unique(dataset[[category_column]]))
if (K > 20) {
  bp <- bp + theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1))
}

bp
