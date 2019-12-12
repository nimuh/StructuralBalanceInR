
library(igraph)
library(Rcpp)
sourceCpp('superNodes.cpp')

super_node_memberships <- function(g) {
  V(g)$color <- 1
  V(g)$super <- -1
  V(g)$name <- V(g)$id
  create_super_nodes(get.data.frame(g, what='vertices'), get.data.frame(g, what='edges'))
}



