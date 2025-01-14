# 2024-11-18
# Code to plot a Sankey directly in Rstudo.
# This codes depends on `04_Analyze_after_colab_TM_v3.R`...
# Meaning that we must run it after it. 

# This code:
# Plots the Sanky in RStudio
# Saves the plot as an HTML widget
# Colors the edges by the target cluster color (Gradients are not possible yet)
# Displays a max of 50 paths.

# We need two inputs from that environment:
# - `melted_sankey_topics`: the final data frame with the paths
# - `inputs`: the dataframe with the display names and colors as assigned in settings

# Load required libraries
library(dplyr)
library(networkD3)
library(htmlwidgets)
library(RColorBrewer)
library(readr)

# Read the data
data <- melted_sankey_topics #

# Retain upto 50
data <- data %>% top_n(100, wt=Similarity)

# Remove 'others'
data<- data %>% 
  filter(!grepl("99---|99$", Source)) %>%
  filter(!grepl("99---|99$", Dest))

# Remove dashes
data$Source <- gsub("---", ": ", data$Source)
data$Dest <- gsub("---", ": ", data$Dest)

# Prepare names
data$Source <- paste(data$Source, data$source_topic)
data$Dest <- paste(data$Dest, data$target_topic)

# Create nodes dataframe
# Get unique nodes from both source and destination
nodes <- unique(c(data$Source, data$Dest))
nodes_group <- sapply(nodes, \(x) {unlist(strsplit(x, "-"))[[1]]})
nodes_dn <- paste0("'", inputs$display_name, "'", collapse = ", ")
nodes_cl <- paste0("'", inputs$color, "'", collapse = ", ")

nodes_df <- data.frame(
  name = nodes,
  node_group = factor(nodes_group)
)

# Create links dataframe
links_df <- data.frame(
  source = match(data$Source, nodes_df$name) - 1,  # 0-based indexing
  target = match(data$Dest, nodes_df$name) - 1,
  value = data$Value
)
links_df$link_group <- nodes_df[links_df$target + 1, 'node_group']

# Create custom color scale
my_color_scale <- JS(sprintf(
  glue("d3.scaleOrdinal()
    .domain([{nodes_dn}])
    .range([{nodes_cl}])")
))

# Create Sankey diagram
sankey <- sankeyNetwork(
  Links = links_df,
  Nodes = nodes_df,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "node_group",
  LinkGroup = "link_group",
  colourScale = my_color_scale,
  fontSize = 11,
  nodeWidth = 25,
  nodePadding = 10,
  height = 600,
  width = 1100,
  sinksRight = FALSE
)

# Display the Sankey in R
sankey

# Save the widget
saveWidget(sankey, 
           file=file.path(bibliometrics_folder_path,  
                          settings$metadata$heatmap_analysis_id,
                          "sankey_diagram.html"), 
           selfcontained = TRUE)


# # Tests for gradient edges
# sankey <- htmlwidgets::onRender(
#   sankey,
#   '
#   function(el, x) {
#     // Color scales for each group
#     const nodeColors = {
#       group1: "#E67E22",  // Orange for PIK
#       group2: "#82E0AA",  // Green for Plant
#       group3: "#5DADE2"   // Blue for RIKEN
#     };
#     
#     // Create gradient definitions
#     const svg = d3.select(el).select("svg");
#     const defs = svg.append("defs");
#     
#     // Create gradient for each link
#     d3.select(el).selectAll(".link").each(function(d) {
#       const gradientID = `gradient-${d.source.index}-${d.target.index}`;
#       
#       // Determine source and target colors based on node groups
#       const sourceColor = nodeColors[d.source.node_group];
#       const targetColor = nodeColors[d.target.node_group];
#       
#       // Create gradient definition
#       const gradient = defs.append("linearGradient")
#         .attr("id", gradientID)
#         .attr("gradientUnits", "userSpaceOnUse")
#         .attr("x1", d.source.x1)
#         .attr("x2", d.target.x0);
#       
#       // Add gradient stops
#       gradient.append("stop")
#         .attr("offset", "0%")
#         .attr("stop-color", sourceColor);
#         
#       gradient.append("stop")
#         .attr("offset", "100%")
#         .attr("stop-color", targetColor);
#       
#       // Apply gradient to link
#       d3.select(this)
#         .style("stroke", `url(#${gradientID})`)
#         .style("opacity", 0.7);
#     });
#     
#     // Customize node labels
#     d3.select(el)
#       .selectAll(".node text")
#       .style("font-weight", "bold")
#       .style("fill", "black")
#       .attr("x", function(d) { 
#         return d.x0 < width / 2 ? 6 + sankey.nodeWidth() : -6;
#       })
#       .attr("text-anchor", function(d) {
#         return d.x0 < width / 2 ? "start" : "end";
#       });
#   }
#   '
# )

