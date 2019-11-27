
library(igraph)
library(queue)
library(Rcpp)
sourceCpp('superNodes.cpp')


# TEST GRAPHS
test <- function() {
  sourceCpp('superNodes.cpp')
  g1 <- graph_from_literal(1-2, 1-3, 2-3)
  V(g1)$color <- 1
  V(g1)$super <- -1
  E(g1)$sign <- c(1, 1, 1)
  g1_v <- get.data.frame(g1, what='vertices')
  g1_e <- get.data.frame(g1, what='edges')
  
  g2 <- graph_from_literal(1-2, 1-3, 2-3)
  V(g2)$color <- 1
  V(g2)$super <- -1
  E(g2)$sign <- c(1, 1, -1)
  g2_v <- get.data.frame(g2, what='vertices')
  g2_e <- get.data.frame(g2, what='edges')
  
  g3 <- graph_from_literal(1-2, 1-3, 2-3, 3-4)
  V(g3)$color <- 1
  V(g3)$super <- -1
  E(g3)$sign <- c(1, 1, -1, 1)
  g3_v <- get.data.frame(g3, what='vertices')
  g3_e <- get.data.frame(g3, what='edges')
  
  create_super_nodes(g3_v, g3_e)
  
}





