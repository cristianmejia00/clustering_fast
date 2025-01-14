
#network <- read.csv("file.ncol", sep = " ", header = FALSE, stringsAsFactors = FALSE)
g1 <- graph_from_data_frame(network, directed = FALSE)

########################################################################
## Plotting preparation

# List of clusters
id_com <- sort(unique(dataset$level0))
# Change last cluster, 99, to natural number 
id_com[[length(id_com)]] <- length(id_com)

# Create correspondence table for nodes and clusters
node_cluster <- myDataCorrect[,c("X_N", "level0")]
setnames(node_cluster, c("level0"), c("X_C"))
node_cluster$X_C[node_cluster$X_C == 99] <- length(id_com)

# Sort it from last to first cluster
node_cluster <- node_cluster[order(node_cluster$X_C, decreasing = TRUE),]


# Sort the graph vertices in same order as node_cluster
# There are differences in the count of nodes. After inspecting them I reached these conclusions:
# As long as the dataset has the lowest number seems to be no problem
# Need to check how I built the ncol_file. As I did not filter the network only for the nodes in the dataset. It has other nodes too.
nodes_database <- node_cluster$X_N
nodes_g1 <- names(V(g1)) %>% as.numeric()

g1 <- induced_subgraph(g1, which(nodes_g1 %in% nodes_database))
vv <- V(g1) %>% names %>% as.numeric #Order of nodes in the graph
idx <- match(vv, node_cluster$X_N) #Index to reorder graph
g1 <- permute(g1, idx)






# Verify the assignation is correct. It must be TRUE
all(as.numeric(names(V(g1))) == node_cluster$X_N)

########################################################################
# Create color palette
fukan_colors <- c("#f00f15","#2270e7","#e5e510","#ff8103","#4f3dd1","#26cc3a","#ec058e","#9cb8c2","#fffdd0","#b40e68")
fukan_colors_extended <- c(fukan_colors, tolower(c("#5AFB5A", "#BEAED4", "#FDC086", "#99FDFF", "#C430FF", "#E4DBE0", "#BF5B17", "#666666")))#RColorBrewer::brewer.pal(8, "Accent"))
color_palette <- rep_len(fukan_colors_extended, length(id_com))

########################################################################
# Give colors to the edges

# Get the pairs of X_N composing each edge
edge_list <- as.data.frame(ends(g1, E(g1)), stringsAsFactors = FALSE)
# From node name to cluster
node_to_cluster <- node_cluster$X_C
names(node_to_cluster) <- as.character(as.integer(node_cluster$X_N))
edge_list[,3] <- node_to_cluster[(edge_list[,1])]
edge_list[,4] <- node_to_cluster[(edge_list[,2])]
# Intracluster links
edge_list[,5] <- edge_list[,3] == edge_list[,4]
table(edge_list[,5])

# Color the edge. Either strategy will color intracluster edges the same.
# Edge color based on largest-cluster incident node
# edge_list[,6] <- sapply(c(1:nrow(edge_list)), function(x) {max(edge_list[x,3], edge_list[x,4])})
# Edge color based on smallest-cluster incident node <- This is preferred because we can 
edge_list[,6] <- sapply(c(1:nrow(edge_list)), function(x) {min(edge_list[x,3], edge_list[x,4], na.rm = TRUE)})

# From cluster to color
# edge_list[,7] <- adjustcolor(color_palette[edge_list[,6]], alpha.f = 0.4)
edge_list[,7] <- paste(color_palette[edge_list[,6]], "66", sep = "")
# edge_list[,7] <- color_palette[edge_list[,6]]


# Custom modifications
# Last cluster "99" transparent
# Intercluster edges transparent
#edge_list[,7][!edge_list[,5]] <- "#00000000"
edge_list[,7][edge_list[,6] == max(id_com)] <- "#00000000"

# Assign color to edges
E(g1)$color <- edge_list[,7]

########################################################################
library(png)


# plot with my default params
myplot <- function(network, ...) {
  plot(network, 
       rescale = FALSE,
       xlim = xlim,
       ylim = ylim,
       layout = matrix(c(V(network)$x, V(network)$y), ncol = 2), 
       vertex.size = 0, 
       vertex.color = NA, 
       vertex.frame.color = NA, 
       vertex.label = NA, 
       ...)
}
###########################################################################
# Set the GLOBAL layout
# We choose to compute the centroids of clusters based on DRL
# Given that is the one with more distributed effects. 

#coords <- layout_with_sugiya(g1)$layout # For layered network. Can't be used

#coords <- layout_with_drl(g1)
#coords <- layout_with_lgl(g1, maxiter = 1000)
#coords <- layout_with_fr(g1, niter = 1000)
#coords <- layout_with_gem(g1)

coords <- layout_with_mds(g1)
coords <- layout_with_dh(g1)

coords <- layout_with_kk(g1) # OK best
coords <- layout_with_graphopt(g1) # OK





V(g1)$x <- coords[,1]
V(g1)$y <- coords[,2]

xlim <- c(min(V(g1)$x), max(V(g1)$x))
ylim <- c(min(V(g1)$y), max(V(g1)$y))

mygroups <- list()
for (i in id_com) {
  mygroups[[i]] <- as.character(node_cluster$X_N[node_cluster$X_C == i])
}
mygroups


# plot with my default params
myplot_framed <- function(network, ...) {
  plot(network, 
       rescale = FALSE,
       #curved = 0.5, # Not working
       #margin = list(0), # Not working 
       #frame = TRUE, # Not working
       xlim = xlim,
       ylim = ylim,
       layout = matrix(c(V(network)$x, V(network)$y), ncol = 2), 
       vertex.size = 0, 
       vertex.color = NA, 
       vertex.frame.color = NA, 
       vertex.label = NA, 
       mark.groups = mygroups[1:9],
       mark.shape = 1,
       mark.col = NA,
       mark.border = color_palette[1:9]
       )
}

# plot with my default params
myplot <- function(network, ...) {
  plot(network, 
       rescale = FALSE,
       #curved = 0.5, # Not working
       #margin = list(0), # Not working 
       #frame = TRUE, # Not working
       xlim = xlim,
       ylim = ylim,
       layout = matrix(c(V(network)$x, V(network)$y), ncol = 2), 
       vertex.size = 0, 
       vertex.color = NA, 
       vertex.frame.color = NA, 
       vertex.label = NA,
       ...
  )
}
par(bg = "#222222")
myplot(g1)

?plot.igraph

igraph::plo