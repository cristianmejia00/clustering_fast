# Create a network object, and simplify it.
# We need to simplify it because the Fukan pairs have repeated edges
g1 <- graph_from_data_frame(network, directed = FALSE)
g1 <- simplify(g1)

# Write the file
write_graph(g1, "file.ncol", format = "ncol")