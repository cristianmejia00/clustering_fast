# Depends on `00_citation_network_clustering.R`
# Objects needed:
# - g1
# - dataset
# - network_description

#####################################################################################
print("Executing: 02_citation_network/02_louvain_infomap.R")

if (settings$cno$clustering$algorithm == "louvain") {
  com <- cluster_louvain(as.undirected(g1))
}
if (settings$cno$clustering$algorithm == "infomap") {
  com <- cluster_infomap(as.undirected(g1))
}

m_com <- membership(com)
network_description$modularity <- modularity(g1, m_com)

id_com <- sort(unique(m_com))
ordered <- as.numeric(names(sort(table(m_com), decreasing = TRUE)))
repl <- sapply(m_com, function(x) {
  which(ordered == x)
})
names(repl) <- names(m_com)
m_com <- repl

# Verify the clusters are ordered from the largest
plot(table(m_com))

# plot(table(dataset$"_C")) #Compare with original Newman Solution
table(m_com)

# Order vector of communities as they appear in the dataset
vertex <- as.numeric(names(V(g1)))
ids <- dataset$"X_N"
dataset$X_C <- m_com[order(match(vertex, ids))]

# dataset_minimal
dataset_minimal <- dataset %>% select(X_N, UT, uuid, X_C)
