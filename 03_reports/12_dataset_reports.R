# Before the cluster level stats we need the dataset level stats
print("###################### reports/12_dataset_reports.R")

# Load plotting library
library(ggplot2)

################################################################################
# YEARLY TRENDS
################################################################################
yearly_trends <- dataset$PY %>%
  as.numeric() %>%
  table() %>%
  data.frame() %>%
  setNames(c("Year", "Articles"))
yearly_trends <- yearly_trends[order(yearly_trends$Year, decreasing = TRUE), ]
write.csv(yearly_trends, file = file.path(output_folder_reports, "stats", "yearly_trends.csv"), row.names = FALSE)

ggplot(yearly_trends[1:15, ], aes(x = Year, y = Articles)) +
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") #+ #, color = "black", fill = "white") +
# theme(axis.text=element_text(size=14))
# coord_flip() +
# scale_x_discrete(limits=rev)
ggsave(file.path(output_folder_reports, "stats", "fig_yearly_trends.jpg"))

################################################################################
# CLUSTER SIZE
################################################################################
stats_size <- dataset$level0 %>%
  table() %>%
  data.frame() %>%
  setNames(c("Cluster", "Articles"))
write.csv(stats_size, file = file.path(output_folder_reports, "stats", "cluster_size.csv"), row.names = FALSE)

ggplot(stats_size, aes(x = Cluster, y = Articles)) +
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + # , color = "black", fill = "white") +
  # theme(axis.text=element_text(size=14)) +
  coord_flip() +
  scale_x_discrete(limits = rev)
ggsave(file.path(output_folder_reports, "stats", "fig_cluster_size_h.jpg"))

ggplot(stats_size, aes(x = Cluster, y = Articles)) +
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") #+ #, color = "black", fill = "white") +
# theme(axis.text=element_text(size=14))
# coord_flip() +
# scale_x_discrete(limits=rev)
ggsave(file.path(output_folder_reports, "stats", "fig_cluster_size_v.jpg"))

################################################################################
# JOURNALS
################################################################################
stats_journals <- dataset$SO %>%
  as.character() %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  data.frame() %>%
  setNames(c("Journals", "Articles"))
write.csv(stats_journals, file = file.path(output_folder_reports, "stats", "journals.csv"), row.names = FALSE)


ggplot(stats_journals[c(1:settings$rp$top_items), ], aes(x = Journals, y = Articles)) +
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + # , color = "black", fill = "white") +
  theme(axis.text = element_text(size = 14)) +
  coord_flip() +
  scale_x_discrete(limits = rev)
ggsave(file.path(output_folder_reports, "stats", "fig_journals.jpg"))

################################################################################
# COUNTRIES
################################################################################
stats_regions <- dataset$Countries %>%
  strsplit("; ") %>%
  unlist() %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  data.frame() %>%
  setNames(c("Regions", "News"))
write.csv(stats_regions, file = file.path(output_folder_reports, "stats", "regions.csv"), row.names = FALSE)

ggplot(stats_regions[c(1:settings$rp$top_items), ], aes(x = Regions, y = News)) +
  geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + # , color = "black", fill = "white") +
  theme(axis.text = element_text(size = 14)) +
  coord_flip() +
  scale_x_discrete(limits = rev)
ggsave(file.path(output_folder_reports, "stats", "fig_regions.jpg"))

################################################################################
# CATEGORIES (or Patent IPC)
################################################################################
if ("WC" %in% colnames(dataset)) {
  stats_WC <- dataset$WC %>%
    toupper() %>%
    strsplit("; ") %>%
    unlist() %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    data.frame() %>%
    setNames(c("Categories", "Articles"))
  # if (settings$params$type_of_dataset == "news") { setnames(stats_DE, c("Keywords", "Articles"), c("Topic", "News"))}
  write.csv(stats_WC, file = file.path(output_folder_reports, "stats", "categories.csv"), row.names = FALSE)

  ggplot(stats_WC[c(1:settings$rp$top_items), ], aes(x = Categories, y = Articles)) +
    geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + # , color = "black", fill = "white") +
    theme(axis.text = element_text(size = 14)) +
    coord_flip() +
    scale_x_discrete(limits = rev)
  ggsave(file.path(output_folder_reports, "stats", "fig_categories.jpg"))
}

################################################################################
# KEYWORDS (or Factiva topics)
################################################################################
if ("DE" %in% colnames(dataset)) {
  stats_DE <- dataset$DE %>%
    tolower() %>%
    strsplit("; ") %>%
    unlist() %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    data.frame() %>%
    setNames(c("Keywords", "Articles"))
  # if (settings$params$type_of_dataset == "news") { setnames(stats_DE, c("Keywords", "Articles"), c("Topic", "News"))}
  write.csv(stats_DE, file = file.path(output_folder_reports, "stats", "keywords.csv"), row.names = FALSE)

  ggplot(stats_DE[c(1:settings$rp$top_items), ], aes(x = Keywords, y = Articles)) +
    geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + # , color = "black", fill = "white") +
    theme(axis.text = element_text(size = 14)) +
    coord_flip() +
    scale_x_discrete(limits = rev)
  ggsave(file.path(output_folder_reports, "stats", "fig_keywords.jpg"))
}


################################################################################
# Authors (or Factiva types, or Patents inventors)
################################################################################
if ("AU" %in% colnames(dataset)) {
  stats_AU <- dataset$AU %>%
    toupper() %>%
    strsplit("; ") %>%
    unlist() %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    data.frame() %>%
    setNames(c("Authors", "Articles"))
  # if (settings$params$type_of_dataset == "news") { setnames(stats_DE, c("Authors", "Articles"), c("Types", "News"))}
  # if (settings$params$type_of_dataset == "patents") { setnames(stats_DE, c("Authors", "Articles"), c("Inventors", "Patents"))}
  write.csv(stats_AU, file = file.path(output_folder_reports, "stats", "authors.csv"), row.names = FALSE)

  ggplot(stats_AU[c(1:settings$rp$top_items), ], aes(x = Authors, y = Articles)) +
    geom_bar(stat = "identity", width = 0.7, fill = "deepskyblue3") + # , color = "black", fill = "white") +
    theme(axis.text = element_text(size = 14)) +
    coord_flip() +
    scale_x_discrete(limits = rev)
  ggsave(file.path(output_folder_reports, "stats", "fig_authors.jpg"))
}
