# 20220924

# Network of clusters.
# Works for direct networks

# INPUT:
# Clustering environment. Or at least
# - g1 the graph
# - myDataCorrect

#==================================
# Create a network of clusters 
#==================================

# Assign weigth of 1 to vertex and edges. 1 because we are aggregating them
V(g1)$size = 1
E(g1)$count = 1

# The community vector in the order of g1 vertices
repl <- myDataCorrect$X_C[match(V(g1)$name, as.character(myDataCorrect$X_N))]

# The contracted network 
merged.graph = contract.vertices(g1, repl, vertex.attr.comb=list(size="sum", "ignore"))
merged.graph = simplify(merged.graph, remove.loops=T, edge.attr.comb=list(count="sum", "ignore"))

# We add colors to each cluster just in case
merged.graph$color = sample(rainbow(length(id_com)))
V(merged.graph)$label.cex = (log(V(merged.graph)$size))/5

#Choose one layout (Just run one of the following three lines)
#layout3=layout.fruchterman.reingold(merged.graph)
#layout3=layout_with_kk(merged.graph)
layout3=layout.lgl(merged.graph)
#layout3=layout.circle(merged.graph)

#Plot an interactive network of clusters
# tkplot(merged.graph,
#        layout= layout3, 
#        vertex.size= log(V(merged.graph)$size), 
#        vertex.label=id_com, 
#        vertex.label.cex = V(merged.graph)$label.cex,
#        vertex.color= merged.graph$color, 
#        vertex.frame.color=NA, 
#        edge.width=log(E(merged.graph)$count)
# )

# plot(merged.graph, layout= layout3, vertex.label=NA, 
#      vertex.size=log(V(merged.graph)$size), vertex.frame.color=NA, 
#      edge.width =log(E(merged.graph)$count), edge.curved = FALSE, main="ALL Clusters")

# Create the adjacency matrix
merged.graph.adj = as_adjacency_matrix(merged.graph, attr="count", type = "both", sparse = FALSE)
write.csv(merged.graph.adj, file.path(output_folder_level, "inter_edges_matrix.csv"), row.names = FALSE)


# Create the melted matrix to plot the heatmap
pairs <- melt(merged.graph.adj)

# Visualize the matrix
heatmap_self <- ggplot(pairs, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="red", name="Edges") +
  scale_x_continuous(
    breaks= c( seq(from=1, to=length(id_com), by=1)),
    labels= c( seq(from=1, to=length(id_com), by=1))) +
  scale_y_continuous(
    breaks= c( seq(from=1, to=length(id_com), by=1)),
    labels= c( seq(from=1, to=length(id_com), by=1))) +
  ggtitle("Interedges count") +
  theme ( axis.title.x = element_blank(),
          axis.title.y = element_blank())
# Save image
ggsave(file.path(output_folder_level, "interedges_heatmap.jpg"))
