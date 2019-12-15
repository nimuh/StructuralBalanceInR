cppFunction('Environment get_edge_sign_ht_helper(DataFrame df) {
    Environment ht = new_env(df.nrows()*2);
    NumericVector sources = df["from"];
    NumericVector targets = df["to"];
    NumericVector signs = df["sign"];
    for (int i = 0; i < sources.size(); i++) {
      int source = sources[i];
      int target = targets[i];
      int sign = signs[i];
      ht.assign("edge"+std::to_string(source)+std::to_string(target), sign);
      ht.assign("edge"+std::to_string(target)+std::to_string(source), sign);
    }
    return ht;
}')

# Helper function which takes in a signed graph g
# returning a HashTable mapping edges to their sign values
# Keys are stored as edge<from><to>
# Example: The sign of edge 1 --> 2 is stored at key "edge12"
get_edge_sign_ht <- function(g) {
  df <- get.data.frame(g, what="edges")
  get_edge_sign_ht_helper(df)
}

# A CPP implementation of counting good and bad triangles for optimized looping
# For/while loops are far too slow in R, so this function is needed
cppFunction('NumericVector countTriangles(NumericVector tris, Environment ht) {
    int good = 0;
    int bad = 0;
    for (int i = 0; i < tris.size(); i+=3) {
        int v1 = tris[i];
        int v2 = tris[i+1];
        int v3 = tris[i+2];
        NumericVector s1 = ht.get("edge"+std::to_string(v1)+std::to_string(v2));
        NumericVector s2 = ht.get("edge"+std::to_string(v1)+std::to_string(v3));
        NumericVector s3 = ht.get("edge"+std::to_string(v2)+std::to_string(v3));
        int si1 = s1[0];
        int si2 = s2[0];
        int si3 = s3[0];
        int product = si1 * si2 * si3;
        if (product > 0) {
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

#' Signed clustering coefficient
#'
#' This function calculates the relative signed clustering
#' coefficient by counting the number of balance and unbalanced
#' triangles in the the graph.  It returns a list with the counts
#' of the good and bad triangles in the graph, the relative signed
#' clustering coeffecient, and the signed clustering coeffecient.
#'
#' This function is an improvement on a function written
#' by Gabor Csardi Thu May 24 16:43:40 CEST 2007 and later
#' modified by Professor Daniel Suthers
#' The improved structural balance function that makes use
#' of the edge_product_closure and optimized C++ counting function
#'
#' @param g the graph to be evaluated
#' @return a list with the counts of the good and bad triangles in the graph, the relative signed clustering coeffecient, and the signed clustering coeffecient
#'
signed_clustering_coefficient <- function(g) {
  tris <- triangles(g)
  ht <- get_edge_sign_ht(g)
  counts <- countTriangles(tris, ht)
  relative_signed_cc <- (counts[1] - counts[2]) / (counts[1] + counts[2])
  signed_cc <- relative_signed_cc * transitivity(g, "global")
  list(
    good=counts[1], bad=counts[2],
    relative_signed_cc=relative_signed_cc,
    signed_cc=signed_cc
  ) # DS added the field names.
}
