# 20231216

# TODO
# - intead of normalize the DRL with scale, do a maxmin between -5 to 5, for example.
# -  Or use a heatmap to infer better placements
# - give intracolor edges their correspondent color.
# -  Or check why the edges look disconnected.

# Generating a citation network like in the Fukan System
# We tried to use LGL with the original implementation from Adai et al, using their code,
# And also, using the LGL implementation of igraph. 
# Both have not work as we wanted. Because they tend to create the spheric network with all nodes
# tied at the center. We can see some cluster trend, but so far, not as clear as in the Fukan System.

# Therefore, I'm creating this adhoc method.
# Here, we first get a layout for all nodes using the DRL algorithm which is fast
# for large networks, but clump the cluster as small knots, but the distrubution is OK
# meaning that is not a spheric distribution, but a layout that spreads the cluster in the space

# We use this layout just to get the centroids of the clusters. 

# Then, we go cluster by cluster computing LGL per cluster, as this is a spheric algorithm
# And is relatively fast. 
# We normalize the centroids from DRL, and nodes per cluster from LGL, and then
# We 'push' each node coordinate according to the centroid values. 
# So that, the final image has the cluster overlaped but still better distributed in the space. 

# INPUTS
# the network object. Either we load g1 or compute it with an ncol.
# the myDataCorrect. This is to get the clusters. 

# OUTPUTS
# the global network image
# the cluster by cluster image. 

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

###########################################################################
# Set the GLOBAL layout
# We choose to compute the centroids of clusters based on DRL
# Given that is the one with more distributed effects. 
#coords_igraph_drl <- layout_with_drl(g1)
#coords_igraph_lgl <- layout_with_lgl(g1, maxiter = 1000)
#coords_igraph_mds <- layout_with_mds(g1)
coords_igraph_kk <- layout_with_kk(g1)


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

max(id_com)
########################################################################
# Special Layout
coords_all_df <- data.frame('node' = V(g1)$name %>% as.numeric(),
                            'x'= coords_igraph_kk[,1],
                            'y'= coords_igraph_kk[,2])

coords_all_df <- merge(coords_all_df, 
                       myDataCorrect[,c('X_N', 'X_C')], 
                       by.x = 'node', 
                       by.y = 'X_N', 
                       all.x = TRUE, 
                       all.y = FALSE)

coords_all_centers <- coords_all_df %>% 
  group_by(X_C) %>% 
  summarize(mean_x = mean(x, na.rm=TRUE),
            mean_y = mean(y, na.rm=TRUE))

coords_all_centers$X_C[coords_all_centers$X_C == 99] <- nrow(coords_all_centers)
coords_all_centers$mean_x <- scale(coords_all_centers$mean_x)
coords_all_centers$mean_y <- scale(coords_all_centers$mean_y)


# Correction of outliers
bpx <- boxplot(coords_all_centers$mean_x)
bpy <- boxplot(coords_all_centers$mean_y)

# The correction is to substract the delta between the whisker and the outliers
# Correct x axis
coords_all_centers$mean_x[coords_all_centers$mean_x > bpx$stats[5,1]] <- coords_all_centers$mean_x[coords_all_centers$mean_x > bpx$stats[5,1]] - abs(coords_all_centers$mean_x[coords_all_centers$mean_x > bpx$stats[5,1]] - bpx$stats[5,1])
coords_all_centers$mean_x[coords_all_centers$mean_x < bpx$stats[1,1]] <- coords_all_centers$mean_x[coords_all_centers$mean_x < bpx$stats[1,1]] + abs(coords_all_centers$mean_x[coords_all_centers$mean_x < bpx$stats[1,1]] - bpx$stats[1,1])

# Correct y axis
coords_all_centers$mean_y[coords_all_centers$mean_y > bpy$stats[5,1]] <- coords_all_centers$mean_y[coords_all_centers$mean_y > bpy$stats[5,1]] - abs(coords_all_centers$mean_y[coords_all_centers$mean_y > bpy$stats[5,1]] - bpy$stats[5,1])
coords_all_centers$mean_y[coords_all_centers$mean_y < bpy$stats[1,1]] <- coords_all_centers$mean_y[coords_all_centers$mean_y < bpy$stats[1,1]] + abs(coords_all_centers$mean_y[coords_all_centers$mean_y < bpy$stats[1,1]] - bpy$stats[1,1])

# Verify: The boxplot without outliers
bpx <- boxplot(coords_all_centers$mean_x)
bpy <- boxplot(coords_all_centers$mean_y)

########################################################################
# Each cluster is plot with LGL
coords_special <- lapply(c(id_com), function(i) {
  print(i)
  this_cluster_ids <- myDataCorrect$X_N[myDataCorrect$X_C == i] %>% as.character()
  gtmp <- induced_subgraph(g1, this_cluster_ids)
  gtmp_coords <- layout_with_kk(gtmp) %>% as.data.frame()
  colnames(gtmp_coords) <- c('x','y')
  
  # Treat outliers
  bpx <- boxplot(gtmp_coords$x, plot = FALSE)
  gtmp_coords$x[gtmp_coords$x > bpx$stats[5,1]] <- bpx$stats[5,1]
  gtmp_coords$x[gtmp_coords$x < bpx$stats[1,1]] <- bpx$stats[1,1]
  bpy <- boxplot(gtmp_coords$y, plot = FALSE)
  gtmp_coords$y[gtmp_coords$y > bpy$stats[5,1]] <- bpy$stats[5,1]
  gtmp_coords$y[gtmp_coords$y < bpy$stats[1,1]] <- bpy$stats[1,1]
  
  gtmp_coords_df <- data.frame('node' = V(gtmp)$name,
                               'x' = scale(gtmp_coords[,1]) + 
                                     coords_all_centers$mean_x[coords_all_centers$X_C == i] * 1.5,
                               'y' = scale(gtmp_coords[,2]) + 
                                     coords_all_centers$mean_y[coords_all_centers$X_C == i] * 1.5)
}) %>% rbind.fill()

coords_special2 <- coords_special[match(V(g1)$name, coords_special$node),]
all(coords_special2$node == V(g1)$name)
coords_special2 <- coords_special2[,c('x','y')]
rownames(coords_special2) <- NULL
coords_special2 <- as.matrix(coords_special2)

########################################################################
library(png)

V(g1)$x <- coords_special2[,1]
V(g1)$y <- coords_special2[,2]
V(g1)$x <- coords_igraph_kk[,1]
V(g1)$y <- coords_igraph_kk[,2]
xlim <- c(min(V(g1)$x), max(V(g1)$x))
ylim <- c(min(V(g1)$y), max(V(g1)$y))


# Create the colored clusters
# Hex transparency codes: https://gist.github.com/lopspower/03fb1cc0ac9f32ef38f4
# 1A = 10%, 33=20%, FF=100%; 100% = Solid, 0% full transparent
g_by_cluster <- lapply(id_com, function(i) {
  this_cluster_ids <- myDataCorrect$X_N[myDataCorrect$X_C == i] %>% as.character()
  gtmp <- induced_subgraph(g1, this_cluster_ids)
  E(gtmp)$color <- paste(color_palette[i], "FF", sep = "")
  if (i == length(id_com)) {
    print("last cluster transparent")
    E(gtmp)$color <- "#00000000"
  }
  return(gtmp)
})

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

########################################################################
# Print the colored full network
png(file="networkQ302_9.png", width=1280, height=800)
par(bg = "black")
myplot(g_by_cluster[[length(g_by_cluster)]])
for (i in rev(id_com[1:length(id_com) - 1])) {
  print(i)
  gtmp <- g_by_cluster[[i]]
  myplot(gtmp, add = TRUE)
}
dev.off()

# Print by cluster over gray.
# This solution is very inneficient, because we print everything per each cluster
# We need to find a way to have the plots stored and just overlay the colored cluster.
st <- Sys.time()
for (cc in c(1:min(length(id_com),15))) { 
  print("==============")
  print(cc)
  png(file=paste("Cluster_gbk_", cc,".png", sep = ""), width=1280, height=800)
  par(bg = "#222222")
  # plot the inital being the last one
  myplot(g_by_cluster[[length(g_by_cluster)]])
  # Print in grey every cluster that is not cc
  for (i in rev(id_com)) {
    if ((i != cc) && (i != length(id_com))) {
      print(i)
      gtmp <- g_by_cluster[[i]]
      myplot(gtmp, add = TRUE, edge.color = adjustcolor("grey40", alpha.f = 0.8))
    }
  }
  # print cc
  gtmp <- g_by_cluster[[cc]]
  myplot(gtmp, add = TRUE)
  dev.off()
}
Sys.time() - st


########################################################################
# Print by cluster over black background
for (cc in c(1:min(length(id_com),15))) {
  print(cc)
  png(file=paste("Cluster_", cc,".png", sep = ""), width=1280, height=800)
  par(bg = "black")
  myplot(g_by_cluster[[cc]], main=paste("Cluster ", cc))
  dev.off()
}

# Print the grey full network (This will serve as the silhoutte background)
png(file="grey.png", width=1280, height=800)
par(bg = "black")
myplot(g_by_cluster[[length(g_by_cluster)]], edge.color = adjustcolor("grey40", alpha.f = 0.3))
for (i in rev(id_com[1:length(id_com) - 1])) {
  print(i)
  gtmp <- g_by_cluster[[i]]
  myplot(gtmp, add = TRUE, edge.color = adjustcolor("grey40", alpha.f = 0.3))
}
dev.off()

# Print by cluster with the silhoutte background
gb <- readPNG("grey.png")
for (cc in c(1:min(length(id_com),15))) {
  print(cc)
  png(file=paste("Clusterg_", cc,".png", sep = ""), width=1280, height=800)
  myplot(g_by_cluster[[cc]])
  lim <- par()
  rasterImage(gb, lim$usr[1] - 2, lim$usr[3], lim$usr[2], lim$usr[4])#xleft = xlim[1], xright = xlim[2], ybottom = ylim[1], ytop = ylim[2])
  myplot(g_by_cluster[[cc]], add = TRUE)
  dev.off()
}



