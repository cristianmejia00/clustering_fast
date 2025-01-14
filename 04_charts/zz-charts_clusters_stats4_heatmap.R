print("###################### zz-charts_clusters_stats4_heatmap.R")

# 20230729
# Save the self-heatmap
# i.e. A squared heatmap with diagonals = 1 based on the cosine similarity of tfidf
# this assumes that the LDA-vis was created and is saved in the folder "keyword Explorer"
# Because that's the same code that creates the tf matrix PHI.

# INPUT:
PHI <- PHI
output_folder_level <- output_folder_level
subfolder_clusters <- subfolder_clusters
extension <- extension
rcs_merged <- rcs_merged

# Compute tfidf
idf <- (PHI > 0) %>% colSums()
idf <- log((1 + nrow(PHI)) / (1 + idf)) + 0.01
idf <- matrix(rep(idf, nrow(PHI)), nrow = nrow(PHI), ncol = length(idf), byrow = TRUE)
tfidf <- PHI * idf

if ((nrow(rcs_merged) <= 99) & (99 %in% rcs_merged$cluster_code)) {
  print("Remove cluster 99 from heatmap")
  tfidf <- tfidf[1:(nrow(tfidf) - 1), ]
}

# Cosine similarity
cos_matrix <- tfidf %>%
  as.matrix() %>%
  t() %>%
  lsa::cosine()


# Long form needed by ggplot
melted <- reshape2::melt(cos_matrix)

# Plot as static image
p <- ggplot(melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_x_continuous(labels = as.character(melted$Var1), breaks = melted$Var1, position = "top") +
  scale_y_reverse(labels = as.character(melted$Var1), breaks = melted$Var1) +
  coord_cartesian(
    xlim = c(min(melted$Var1), max(melted$Var1)),
    ylim = c(max(melted$Var2), min(melted$Var2))
  ) +
  scale_fill_gradientn(
    name = "Similarity",
    colors = c("white", "red"),
    limits = c(0, 1)
  ) +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Save static
ggsave(p, filename = file.path(
  output_folder_level,
  subfolder_clusters,
  glue("heatmap.{extension}")
))

# Save interactive
fig <- ggplotly(p, tooltip = "value")
htmlwidgets::saveWidget(
  widget = fig,
  file = file.path(
    output_folder_level,
    subfolder_clusters,
    glue("heatmap.html")
  ),
  selfcontained = TRUE
)
