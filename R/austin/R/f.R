mkGraph <- function(adjacency, as.directed=FALSE) {
  if ( is.character(adjacency) && ( length(adjacency) == 1 ) ) {
    x <- scan(textConnection(adjacency),quiet=TRUE)
    n <- sqrt(length(x))
    if ( n %% 1 != 0 ) stop("The number of elements is not a perfect square.")
    adjacency <- matrix(x,nrow=n,ncol=n)
  }
  if ( ( ! is.matrix(adjacency) ) || ( nrow(adjacency) != ncol(adjacency) ) ) stop("'adjacency' should be a square matrix.")
  storage.mode(adjacency) <- "logical"
  if ( as.directed ) s$.DirectedGraph$apply(adjacency)
  s$.UndirectedGraph$apply(adjacency)
}


