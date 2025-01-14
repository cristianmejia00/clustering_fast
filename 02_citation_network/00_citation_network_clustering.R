# Citation Network Clustering
# This code define the initial column X_C.
# Depends on `00_clustering.R`

#####################################################
# If any of these is TRUE, then we need a network object.
if (!settings$cno$using_initial_column_C_from_fukan) {
  print("Clustering will be performed using the provided network")
  #####################################################
  # open network file.
  if (!settings$cno$using_mission_pairs_from_fukan) {
    print("Loading computed network")
    g1 <- graph_from_data_frame(network, directed = TRUE)
  } else {
    # The network objects is based on "mission.pairs.tsv" from Fukan's results
    print("Graph from mission pairs")
    g1 <- read.graph(network, format = "ncol")
  }

  #####################################################
  # Verify some properties of the graph
  network_description <- list(
    is_directed = is_directed(g1), # Confirms that the network is directed
    is_weighted = is_weighted(g1), # Confirms that the network is weighted
    edges = ecount(g1), # Return the number of edges
    nodes = vcount(g1), # Return the number of vertices
    edge_density = edge_density(g1),
    ave_degree = mean(degree(g1))
  )
  
  print(network_description)


  # Choose initial clustering solution.
  # We either create the initial column _C or use that from Fukan.
  if (settings$cno$clustering$algorithm == "louvain" | settings$cno$clustering$algorithm == "infomap") {
    if (!settings$cno$using_initial_column_C_from_fukan) {
      source("02_citation_network/02_louvain_Infomap.R")
    }
  }
} else {
  print("No clustering or recursive clustering needed. We use the provided column X_C")
}
