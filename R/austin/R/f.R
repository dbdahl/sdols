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

probLink <- function(w1,w2) 1 - exp(-2*w1*w2)

generalizedGammaProcess <- function(etaTilde, mu) {
  p <- function(etaTilde,alpha,kappa,gamma) {
    if ( alpha <= 0 ) stop("alpha must be greater than 0.")
    if ( kappa < 0 ) stop("kappa must be greater than or equal to 0.")
    if ( ( gamma < 0 ) || ( gamma >= 1 ) ) stop("gamma must be greater than or equal to 0 and less than 1.")
    exp(-kappa*etaTilde) / ( etaTilde^(1+gamma) * gamma(1-gamma) )
  }
}




