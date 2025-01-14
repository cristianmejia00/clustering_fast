cluster_names <- dataset[,c('X_C', 'X_C_label')] %>% 
  filter(!duplicated(X_C)) %>% 
  arrange(X_C)
rcs_merged$cluster_name <- rcs_merged$cluster_code

default_palette

# Coords from LDAviz (keyword explorer)
coords <- fromJSON(txt=file.path(output_folder_level, 'keyword_explorer', 'lda.json'))
coords <- data.frame('cluster' = coords$mdsDat$topics,
                     'x' = coords$mdsDat$x,
                     'y' = coords$mdsDat$y)



# Coords from notebook Bertopic
coords <- read_csv("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/kubota/reduced_data_kubota_2024-06-14.csv")
coords <- coords %>% rename(c('cluster_code' = 'label'))

# Attach x and y
tmp <- merge(rcs_merged, 
             coords, 
             by='cluster_code')

table(dataset$AU)
table(dataset_initial$AU)

firm_names <- dataset$SO %>% strsplit("; ") %>% unlist() %>% table() %>% sort(decreasing = TRUE) 
firm_names[1:20]

# Add color categories
tmp$color[grepl("KUBOTA", tmp$cluster_name)] <- "#FF0000"
tmp$color[grepl("DEERE", tmp$cluster_name)] <- '#00FFFF'
tmp$color[grepl("CNH", tmp$cluster_name)] <- '#FFD700'

tmp$label[grepl("KUBOTA", tmp$cluster_name)] <- "KUBOTA"
tmp$label[grepl("DEERE", tmp$cluster_name)] <- 'DEERE'
tmp$label[grepl("CNH", tmp$cluster_name)] <- 'CNH'

plot_scatter(tmp,
             point_labels = "cluster_name",
             x_column = "x",
             y_column = "y",
             color_column = "color",
             size_column = "documents")

colnames(tmp2)

ggplot(data = tmp, 
       aes(x = x, 
           y = y, 
           color = label, 
           size = documents)) +
  geom_point() +
  scale_color_manual(values = unique(tmp$color)) +
  labs(title = "Scatter Plot",
       x = "X-axis",
       y = "Y-axis",
       color = "Legend Title",
       size = "Point Size") +
  geom_text(aes(label = cluster_code), vjust = -1, hjust = 0.5, size = 3) +
  theme_minimal()


plot_scatter <- function(rcs_data,
                         point_labels,
                         x_column,
                         y_column,
                         color_hex_column,
                         color_labels,
                         size_column,
                         x_column_label = x_column,
                         y_column_label = y_column) {
  # format the df
  df <- rcs_data[, c(point_labels, x_column, y_column, color_hex_column, color_labels, size_column)]
  colnames(df) <- c("point_labels", "x", "y", "color_hex", "color_label", "size")
  df$labels <- as.character(df$point_labels)
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(colour = color_label, 
                   size = size)) +
    scale_color_manual(values = unique(df$color_hex)) +
    xlab(x_column_label) +
    ylab(y_column_label)
  p <- p + geom_text_repel(aes(label = gsub("---|-0", "", labels)))
  p <- p + theme_bw() + theme(legend.position = "none")
  p
}

plot_scatter(tmp,
             point_labels = "cluster_name",
             x_column = "x",
             y_column = "y",
             color_hex_column = "color",
             color_labels = "label",
             size_column = "documents")




dataset$text <- paste(dataset$TI, dataset$AB, sep=" ")
dataset$text <- tolower(dataset)
dataset$rice <- grepl(' rice', dataset$text)
table(dataset$rice)

table(dataset$rice, dataset$AU)

cluster_rice <- table(dataset$X_C_label[dataset$rice]) %>% sort(decreasing = TRUE) %>% as.data.frame.AsIs()
write.csv(cluster_rice, file="cluster_rice.csv")


tmp2 <- tmp[tmp$cluster_code %in% rownames(cluster_rice),]
plot_scatter(tmp2, point_labels = "cluster_name",
             x_column = "x",
             y_column = "y",
             color_column = "color",
             size_column = "documents")

write.csv(table(dataset$Topic, dataset$AU), file='topic_firms.csv')


library(tidyr)
ttt <- dataset[1:50,]
ttt$WC2 <- strsplit(ttt$WC, '; ')
ttt <- ttt %>% 
  select(UT, PY, WC2, TI) %>% 
  unnest(WC2)
