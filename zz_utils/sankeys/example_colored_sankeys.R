# Examples of colored Sankeys 
# 2024-11-16. 

# Option 1
# sankeyNetwork with the links colored, either as the target or the source. 
# NO GRADIATION available
# THI IS WHAT WE USE!!!


URL <-  paste0('https://cdn.rawgit.com/christophergandrud/networkD3/',
               'master/JSONdata/energy.json')
energy <- jsonlite::fromJSON(URL)

# Plot
sankeyNetwork(Links = energy$links, Nodes = energy$nodes, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'name',
              units = 'TWh', fontSize = 12, nodeWidth = 30)

# Colour links
energy$links$energy_type <- sub(' .*', '',
                                energy$nodes[energy$links$source + 1, 'name'])
energy$links$energy_type
sankeyNetwork(Links = energy$links, Nodes = energy$nodes, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'name',
              LinkGroup = 'energy_type', NodeGroup = NULL)

energy$links$energy_type

############
# Option 2
# Only works for 2 column sankeys
# https://ssp3nc3r.github.io/post/2020-03-30-r-package-to-create-a-ggplot-based-sankey-graphic-that-encodes-flows-with-a-color-gradient/
devtools::install_github("ssp3nc3r/ggSankeyGrad", ref = "master")

library(ggSankeyGrad)

c1     <- c("A", "A", "B", "B")
c2     <- c("C", "D", "C", "D")
values <- c(2L, 5L, 8L, 3L)
col1   <- c("red", "red", "green", "green")
col2   <- c("blue", "orange", "blue", "orange")

ggSankeyGrad(c1, c2, col1, col2, values, label = TRUE)
