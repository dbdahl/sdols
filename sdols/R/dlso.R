#' Perform Draws-Based Latent Structure Optimization
#'
#' Among the supplied latent structures, this function picks the structure that minimizes
#' one of various loss functions.
#'
#' @param x A
#' collection of clusterings as a
#' \code{B}-by-\code{n} matrix, each of the \code{B} rows represents a
#' clustering of \code{n} items using cluster labels.  For clustering
#' \code{b}, items \code{i} and \code{j} are in the same cluster if
#' \code{x[b,i] == x[b,j]}.
#' @param loss One of \code{"squaredError"}, \code{"absoluteError"}, \code{"binder"}, or
#' \code{"lowerBoundVariationOfInformation"} to indicate the optimization should seeks to
#' minimize expectation of the squared error loss, absolute error loss, Binder loss (Binder 1978), or the lower
#' bound of the variation of information loss (Wade & Ghahramani 2017), respectively.
#' The first three are equivalent.
#' @param multicore Logical indicating whether computations should take advantage of
#' multiple CPU cores.
#' @param expectedPairwiseAllocationMatrix A \code{n}-by-\code{n} symmetric matrix
#' whose \code{(i,j)} elements gives the estimated expected number of times that items
#' \code{i} and \code{j} are in the same subset (i.e., cluster).  If \code{NULL},
#' it is computed from \code{x}.
#'
#' @return A list A clustering (as a vector of cluster labels).
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' suppressWarnings({  # For testing purposes, suppress deprecation warning.
#'
#' dlso(iris.clusterings)
#' 
#' })
#'
#' @seealso \code{\link{expectedPairwiseAllocationMatrix}}, \code{\link{salso}}
#'
#' @references Wade, S. and Ghahramani, Z. (2017). Bayesian cluster analysis: Point estimation and credible balls. Bayesian analysis.
#' @references Binder, D. (1978). Bayesian Cluster Analysis. Biometrika, 65: 31–38.
#'
#' @export
#' @importFrom salso dlso
#' 
dlso <- function(x, loss=c("squaredError","absoluteError","binder","lowerBoundVariationOfInformation")[1],
                 multicore=TRUE, expectedPairwiseAllocationMatrix=NULL) {
  .Deprecated("dlso","salso","This function is deprecated.  Please use the 'dlso' function in the 'salso' package.")
  if ( ! is.matrix(x) ) stop("'x' is misspecified.")
  loss <- as.character(loss[1])
  if ( ! loss %in% c("squaredError","absoluteError","binder","lowerBoundVariationOfInformation") ) {
    stop("'loss' should be 'squaredError', 'absoluteError', 'binder', or 'lowerBoundVariationOfInformation'.")
  }
  loss <- if ( loss == "lowerBoundVariationOfInformation" ) "VI.lb" else "binder"
  multicore <- as.logical(multicore[1])
  if ( is.null(expectedPairwiseAllocationMatrix) ) expectedPairwiseAllocationMatrix <- psm(x, multicore)
  salso::dlso(expectedPairwiseAllocationMatrix, loss, x, parallel = multicore)$estimate
}
