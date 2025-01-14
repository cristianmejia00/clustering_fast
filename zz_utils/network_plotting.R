# 20200918

# This code has 2 functions:
# 1-
# Prepare a .ncol file to be used in the server version of the LGL algorithm
# See Notion @Install and use LGL for details.
# 2-
# Plot networks image like in the Fukan System using the LGL layout 
# from the LGL server or from Igraph

library(igraph)
library(Opener5)

########################################################################
## Produce a .ncol file for the LGL algorithm in the server

# Read the network file mission.pairs.tsv 
my_network <- read.csv(file.choose(), sep = "\t", header = FALSE, stringsAsFactors = FALSE)
g1 <- graph_from_data_frame(ncol_file, directed = FALSE)
write_graph(g1, "file.ncol", format = "ncol")
getwd()
# Use the .ncol file in a server running the LGL algorithm.

########################################################################
## Import files

# Read the network file mission.pairs.tsv 
my_network <- read.csv(file.choose(), sep = "\t", header = FALSE, stringsAsFactors = FALSE)
g1 <- graph_from_data_frame(my_network, directed = FALSE)
g1 <- simplify(g1)

# Read the dataset mission.facet.all.tsv
# dataset <- read_from_fukan(file.choose())

# Read the layout file
# Using original LGL layout from server
# If we dont have a layout file, no worries as we can compute it with Igraph later in this code.
coords <- read.csv("/var/container/final.coords", sep = " ", header = FALSE, stringsAsFactors = FALSE)


########################################################################
## Plotting preparation

# List of clusters
# id_com <- sort(unique(dataset$`_C`))
id_com <- sort(unique(dataset_minimal$level0))
id_com[[16]] <- 16

# Create correspondance table for nodes and clusters
# node_cluster <- dataset[,c("_N", "_C")]
node_cluster <- dataset_minimal[,c("X_N", "level0")]
setnames(node_cluster, c("X_N", "level0"), c("_N", "_C"))
node_cluster$`_C`[node_cluster$`_C` == 99] <- 16

# Sort it from last to first cluster
node_cluster <- node_cluster[order(node_cluster$`_C`, decreasing = TRUE),]#, decreasing = TRUE


# Sort the graph vertices in same order as node_cluster
# There are differences in the count of nodes. After inspecting them I reached these conclusions:
# As long as the dataset has the lowest number seems to be no problem
# Need to check how I built the ncol_file. As I did not filter the network only for the nodes in the dataset. It has other nodes too.
nodes_database <- node_cluster$`_N`
nodes_g1 <- names(V(g1)) %>% as.numeric()
nodes_coords <- coords$V1

g1 <- induced_subgraph(g1, which(nodes_g1 %in% nodes_database))
vv <- V(g1) %>% names %>% as.numeric #Order of nodes in the graph
idx <- match(vv, node_cluster$`_N`) #Index to reorder graph
g1 <- permute(g1, idx)

# Update the coords to contain nodes in our dataset
coords <- coords[coords$V1 %in% nodes_database, ]

# Prepare the coords
if (exists("coords")) {
  # Using original LGL layout from server
  coords[,4] <- NULL
  vv <- V(g1) %>% names %>% as.numeric
  idx <- match(vv, coords$V1)
  coords <- coords[idx,]
  coords <- as.matrix(coords[,c(2,3)])
} else {
  # Compute it with Igraph
  st <- Sys.time()
  coords <- layout_with_lgl(g1)
  print("Time lapsed for layout:")
  print(as.character(Sys.time() - st))
}

# Verify the assignation is correct. It must be TRUE
all(as.numeric(names(V(g1))) == node_cluster$`_N`)
vcount(g1)
vertex_attr_names(g1)
########################################################################
# Create color palette
fukan_colors <- c("#f00f15","#2270e7","#e5e510","#ff8103","#4f3dd1","#26cc3a","#ec058e","#9cb8c2","#fffdd0","#b40e68")
fukan_colors_extended <- c(fukan_colors, tolower(c("#5AFB5A", "#BEAED4", "#FDC086", "#99FDFF", "#C430FF", "#E4DBE0", "#BF5B17", "#666666")))#RColorBrewer::brewer.pal(8, "Accent"))
color_palette <- rep_len(fukan_colors_extended, length(id_com))

########################################################################
# Plot all clusters

# Get the pairs of X_N composing each edge
edge_list <- as.data.frame(ends(g1, E(g1)), stringsAsFactors = FALSE)
# From node name to cluster
node_to_cluster <- node_cluster$`_C`
names(node_to_cluster) <- as.character(as.integer(node_cluster$`_N`))
edge_list[,3] <- node_to_cluster[(edge_list[,1])]
edge_list[,4] <- node_to_cluster[(edge_list[,2])]
# Intracluster links
edge_list[,5] <- edge_list[,3] == edge_list[,4]
table(edge_list[,5])
# Edge color based on largest-cluster incident node
# edge_list[,6] <- sapply(c(1:nrow(edge_list)), function(x) {max(edge_list[x,3], edge_list[x,4])})
edge_list[,6] <- sapply(c(1:nrow(edge_list)), function(x) {min(edge_list[x,3], edge_list[x,4])})
# From cluster to color
# edge_list[,7] <- adjustcolor(color_palette[edge_list[,6]], alpha.f = 0.4)
edge_list[,7] <- paste(color_palette[edge_list[,6]], "66", sep = "")
# edge_list[,7] <- color_palette[edge_list[,6]]

# Custom modifications
# Last cluster "99" transparent
# Intercluster edges transparent
edge_list[,7][!edge_list[,5]] <- "#00000000"
edge_list[,7][edge_list[,6] == max(id_com)] <- "#00000000"

# Assign color to edges
E(g1)$color <- edge_list[,7]

# Save image
png(file="ALL_Clusters.png", width=1280, height=800)
par(bg = "white")
plot(g1, layout = coords_igraph_drl, vertex.size = 0, vertex.color = NA, vertex.frame.color = NA, vertex.label = NA)
dev.off()
getwd()

########################################################################
# Plot clusters
# Visualize each community separately
for (cc in c(1:min(length(id_com),15))) { 
  temp_color_vector <- edge_list[,c(6,7)]
  temp_color_vector$V7[(temp_color_vector$V6 != cc) & (temp_color_vector$V7 != "#00000000")] <- adjustcolor("grey40", alpha.f = 0.3)
  E(g1)$color <- temp_color_vector$V7
  png(file=paste("/var/container/Cluster_", cc,".png", sep = ""), width=1280, height=800)
  par(bg = "black")
  plot(g1, layout = coords, vertex.size = 0, vertex.color = NA, vertex.frame.color = NA, vertex.label = NA, main=paste("Cluster ", cc))
  dev.off()
}
