# Load required libraries
library(dplyr)
library(ggplot2)
library(ggrepel)


# This function was used!
# Create the scatter plot
ggplot(rcs_merged2 %>% filter(cluster <= 9), 
       aes(x = PY_Mean, 
           y = Z9_Mean, 
           size = documents, 
           label = cluster_code)) +
  geom_point(color = "#4682B4", alpha = 0.7) +  # Steel Blue color
  scale_size_continuous(range = c(5, 30)) +  # Adjust the size range as needed
  scale_x_continuous(breaks = c(2017:2022)) +  # Set x-axis markers for each year from 2018 to 2021
  scale_y_continuous(breaks = seq(10,120, by=20)) +  # Set y-axis range from 15 to 130
  geom_text(
    aes(size = NULL),  # This removes size mapping for labels
    # hjust = -0.1,
    # vjust = -0.1,
    size = 4  # Set a fixed size for all labels
  ) +
  labs(
    x = "Average Publication Year",
    y = "Average Citations",
    size = "Number of Documents"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "none"
  )
# 
# 
# # Use this snippet instead of `geom_text` above
# # to display labels with ggrepel
# geom_text_repel(
#   aes(size = NULL),
#   box.padding = 0.5,
#   point.padding = 0.5,
#   force = 4,
#   segment.color = "grey50",
#   size = 4
# ) +
