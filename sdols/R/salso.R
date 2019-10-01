#' Perform Sequentially-Allocated Latent Structure Optimization
#'
#' This function implements the sequentially-allocated latent structure optimization (SALSO)
#' to find a clustering that minimizes various loss functions.
#' The SALSO method was presented at the workshop "Bayesian Nonparametric Inference: Dependence
#' Structures and their Applications" in Oaxaca, Mexico on December 6, 2017.
#'
#' @param expectedPairwiseAllocationMatrix A \code{n}-by-\code{n} symmetric matrix
#' whose \code{(i,j)} elements gives the estimated expected number of times that items
#' \code{i} and \code{j} are in the same subset (i.e., cluster).
#' @param loss One of \code{"squaredError"}, \code{"absoluteError"}, \code{"binder"}, or
#' \code{"lowerBoundVariationOfInformation"} to indicate the optimization should seeks to
#' minimize squared error loss, absolute error loss, Binder loss (Binder 1978), or the lower
#' bound of the variation of information loss (Wade & Ghahramani 2017), respectively.
#' The first three are equivalent.
#' @param nCandidates The (maximum) number of candidates to consider.  Fewer than
#' \code{nCandidates} may be considered if the time in \code{budgetInSeconds} is exceeded.
#' The computational cost is linear in the number of candidates and there are rapidly
#' diminishing returns to more candidates.
#' @param budgetInSeconds The (maximum) number of seconds to devote to the optimization.
#' When this time is exceeded, no more candidates are considered.
#' @param maxSize Either zero or a positive integer.  If a positive integer, the
#' optimization is constrained to produce solutions whose number of clusters
#' is no more than the supplied value.  If zero, the size is not constrained.
#' @param maxScans The maximum number of reallocation scans after the intial allocation.
#' The actual number of scans may be less than \code{maxScans} since the algorithm stops
#' if the result does not change between scans.
#' @param multicore Logical indicating whether computations should take advantage of
#' multiple CPU cores.
#'
#' @return A clustering (as a vector of cluster labels).
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' suppressWarnings({  # For testing purposes, suppress deprecation warning.
#'
#' probabilities <- expectedPairwiseAllocationMatrix(iris.clusterings)
#' salso(probabilities)
#'
#' })
#'
#' @seealso \code{\link{expectedPairwiseAllocationMatrix}}, \code{\link{dlso}}
#'
#' @references Wade, S. and Ghahramani, Z. (2017). Bayesian cluster analysis: Point estimation and credible balls. Bayesian analysis.
#' @references Binder, D. (1978). Bayesian Cluster Analysis. Biometrika, 65: 31–38.
#'
#' @export
#' @importFrom salso salso
#
salso <- function(expectedPairwiseAllocationMatrix,
                  loss=c("squaredError","absoluteError","binder","lowerBoundVariationOfInformation")[1],
                  nCandidates=100, budgetInSeconds=10, maxSize=0, maxScans=10, multicore=TRUE) {
  .Deprecated("salso","salso","This function is deprecated.  Please use the 'salso' function in the 'salso' package.")
  loss <- as.character(loss[1])
  if ( ! loss %in% c("squaredError","absoluteError","binder","lowerBoundVariationOfInformation") ) {
    stop("'loss' should be 'squaredError', 'absoluteError', 'binder', or 'lowerBoundVariationOfInformation'.")
  }
  loss <- if ( loss == "lowerBoundVariationOfInformation" ) "VI.lb" else "binder"
  epam <- expectedPairwiseAllocationMatrix
  nCandidates <- as.integer(nCandidates[1])
  budgetInSeconds <- as.double(budgetInSeconds[1])
  maxSize <- as.integer(maxSize[1])
  maxScans <- as.integer(maxScans[1])
  multicore <- as.logical(multicore[1])
  out <- salso::salso(expectedPairwiseAllocationMatrix, loss, maxSize, maxScans, nCandidates, budgetInSeconds, multicore)
  result <- out$estimate
  attr(result,"nScans") <- out$nScans
  attr(result,"nCandidates") <- out$nPermutations
  result
}

