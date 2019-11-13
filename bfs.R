
library(igraph)
library(queue)

get_edge_sign_ht <- function(g) {
  df <- get.data.frame(g, what="edges")
  ht <- new.env(hash=TRUE)
  for (i in 1:nrow(df)) {
    from <- df[i, "from"]
    to <- df[i, "to"]
    sign <- df[i, "sign"]
    ename1 <- paste("edge", from, to, sep="")
    ename2 <- paste("edge", to, from, sep="")
    ht[[ename1]] <- sign
    ht[[ename2]] <- sign
  }
  ht
}

# grab adjacents of v in graph g
get_adj <- function(queue, v, g, nodes) {
  n <- neighbors(g, v)
  for (i in 1:length(n)) {
    e <- c(as.numeric(v),as.numeric(n[i]))
    enqueue(queue, e)
  }
}



positive_vibes_only <- function(g) {
  
  edge_signs <- get_edge_sign_ht(g)
  df_edges <- get.data.frame(g, what='edges')
  df_nodes <- get.data.frame(g, what='vertices')
  df_nodes$visited <- FALSE
  df_nodes$super <- 0
  q <- create_queue("edges")
  
  # set super node number to 1
  super_nu <- 1
  
  # still need to iterate on each node but check if it is visited
  # after each time the queue is emptied out the super_nu value is incremented
  # for the next super node
  for (i in 1:nrow(df_nodes)) {
    
    if (df_nodes[i, 'visited'] == FALSE) {
      
      curr <- df_edges[i, 'from']
      df_nodes[i, "visited"] <- TRUE
      df_nodes[i, "super"] <- super_nu
      get_adj(q, curr, g, df_nodes)
      
      while (!is_empty(q)) {
        e <- dequeue(q)
        ename <- paste("edge", e[1], e[2], sep="")

        if (!is.null(edge_signs[[ename]])) {
          
          # if neighbor positive and have not visited yet
          if (df_nodes[e[2], "visited"] == FALSE & edge_signs[[ename]] == 1) {
            
            # assign current super node number
            df_nodes[e[2], "super"] <- super_nu
            df_nodes[e[2], "visited"] <- TRUE
            get_adj(q, e[2], g, df_nodes)
            
          }
          # if neighbor negative and in the same cluster
          else if (edge_signs[[ename]] == 0 & df_nodes[e[2], "visited"] == TRUE) {
            
            if (df_nodes[e[2], 'super'] == df_nodes[e[1], 'super']) {
              # remove from cluster
              df_nodes[e[2], 'super'] <- 0
              df_nodes[e[2], 'visited'] <- FALSE
            }
          }
          
        }
        
      }
      
      # increment after reaching all nodes from starting point
      super_nu <- super_nu + 1
      
    }
  }
  
  delete_queue(q)
  df_nodes
  
}

# TEST GRAPHS

# 1 supernode
g1 <- graph_from_literal(1-2, 1-3, 2-3)
E(g1)$sign <- c(1, 1, 1)
positive_vibes_only(g1)

# 2 super nodes
g2 <- graph_from_literal(1-2, 1-3, 2-3)
E(g2)$sign <- c(1, 1, 0)
positive_vibes_only(g2)

# 2 super nodes
g3 <- graph_from_literal(1-2, 1-3, 2-3, 3-4)
E(g3)$sign <- c(1, 1, 0, 1)
positive_vibes_only(g3)

# bipartite graph (this is weird, I don't know why it gives these supers)
g4 <- graph_from_literal(1-4, 1-5, 1-6, 2-4, 2-5, 2-6, 3-4, 3-5, 3-6)
E(g4)$sign <- c(1, 0, 1, 1, 0, 1, 1, 0, 1)
get.data.frame(g4)
positive_vibes_only(g4)


