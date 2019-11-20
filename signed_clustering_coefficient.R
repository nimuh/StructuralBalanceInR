# Helper function which takes in a signed graph g
# returning a HashTable mapping edges to their sign values
# Keys are stored as edge<from><to>
# Example: The sign of edge 1 --> 2 is stored at key "edge12"
get_edge_sign_ht <- function(g) {
  df <- get.data.frame(g, what="edges")
  ht <- new.env(hash=TRUE)
  for (i in 1:nrow(df)) {
    from <- df[i, "from"]
    to <- df[i, "to"]
    sign <- df[i, "sign"]
    ename <- paste("edge", from, to, sep="")
    ht[[ename]] <- sign
  }
  ht
}

# gets the product of the signs of the edges associated
# with the given triangle. Signs are assumed to be stored
# in a hash_table that is passed in to the function as a helper param.
get_edge_product <- function(tri, ht, directed=TRUE) {
  product <- 1
  if (directed) {
    for (i in 1:length(tri)) {
      for (j in 1:length(tri)) {
        if (i != j) {
          edge_name <- paste("edge", tri[i], tri[j], sep="")
          sign <- ht[[edge_name]]
          if (!is.null(sign)) {
            product <- product * sign
          }
        }
      }
    }
  } else {
    for (i in 1:length(tri)) {
      for (j in i:length(tri)) {
        if (i != j) {
          edge_name <- paste("edge", tri[i], tri[j], sep="")
          sign <- ht[[edge_name]]
          if (!is.null(sign)) {
            product <- product * sign
          }
        }
      }
    }
  }
  product
}

# This function takes in a graph and a hash table
# (R hashed environment) mapping edges to their sign values
# returns a closure that takes in a triangle and returns the
# product of the sign of its associated edges via get_edge_product
get_edge_product_closure <- function(g, ht) {
  function(tri) {
    get_edge_product(tri, ht, is.directed(g))
  }
}

# A CPP implementation of counting good and bad triangles for optimized looping
# For/while loops are far too slow in R, so this function is needed
cppFunction('NumericVector countTriangles(NumericVector tris, Function get_edge_prod) {
    int good = 0;
    int bad = 0;
    for (int i = 0; i < tris.size(); i+=3) {
        NumericVector tri = NumericVector(3);
        tri[0] = tris[i];
        tri[1] = tris[i+1];
        tri[2] = tris[i+2];
        NumericVector product = NumericVector(get_edge_prod(tri));
        int iprod = product[0];
        if (iprod > 0) {
          good++;
        } else {
          bad++;
        }
    }
    NumericVector result = NumericVector(2);
    result[0] = good;
    result[1] = bad;
    return result;
}')

# This function is an improvement on a function written
# by Gabor Csardi Thu May 24 16:43:40 CEST 2007 and later
# modified by Professor Daniel Suthers
#' The improved structural balance function that makes use
#' of the edge_product_closure and optimized C++ counting function
#' @param g the graph to be evaluated
#' @return the counts of the good and bad triangles in the graph

signed_clustering_coefficient <- function(g) {
  get_prod <- get_edge_product_closure(g, ht)
  tris <- triangles(g)
  ht <- get_edge_sign_ht(g)
  counts <- countTriangles(tris, get_prod)
  relative_signed_cc <- (counts[1] - counts[2]) / (counts[1] + counts[2])
  signed_cc <- relative_signed_cc * transitivity(NS, "global")
  list(
    good=counts[1], bad=counts[2], 
    relative_signed_cc=relative_signed_cc,
    signed_cc=signed_cc
  ) # DS added the field names.
}
