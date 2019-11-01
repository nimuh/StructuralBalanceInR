
# This function is an improvement on a function written
# by Gabor Csardi Thu May 24 16:43:40 CEST 2007 
# but it still runs slow on large signed networks e.g. Bitcoin
# We will look at improving the performance by writing
# the while loop logic in C++
structural.balance.improved <- function(g) {
  tris <- triangles(g) #BTW, KT, NA, no need to enumerate all triples
  i <- 0
  good <- bad <- 0
  # loop over all triangles and count bad & good
  while (i < length(tris)) {
    tri <- c(tris[i], tris[i+1], tris[i+2])
    i <- i + 3
    edges <- E(g) [ tri %--% tri ]
    if (prod(E(g)[edges]$sign) > 0) {
       good <- good +1
    } else {
       bad <- bad +1
    }
  }
  c(good=good, bad=bad) # DS added the field names.
}