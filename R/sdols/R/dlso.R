#' Perform Draws-Based Latent Structure Optimization
#'
#' Among the supplied latent structures, this function picks the structure that minimizes
#' one of various loss functions.
#'
#' @param x A collection of clusterings or feature allocations.  If \code{x} is a
#' \code{B}-by-\code{n} matrix, each of the \code{B} rows represents a clustering of
#' \code{n} items using cluster labels.  For clustering \code{b}, items \code{i} and
#' \code{j} are in the same cluster if and only if \code{x[b,i] == x[b,j]}.  If \code{x}
#' is a list of length \code{B}, each element of list represents a feature allocation using
#' a binary matrix of \code{n} rows and an arbitrary number of columns.  For feature
#' allocation \code{b}, items \code{i} and \code{j} share \code{m} features if, for \code{k}
#' = 1, 2, ..., the expression \code{x[[b]][i,k] == x[[b]][j,k] == 1} is true exactly
#' \code{m} times.
#' @param loss One of \code{"squaredError"}, \code{"absoluteError"}, \code{"binder"}, or
#' \code{"lowerBoundVariationOfInformation"} to indicate the optimization should seeks to
#' minimize squared error loss, absolute error loss, Binder loss (Binder 1978), or the lower
#' bound of the variation of information loss (Wade & Ghahramani 2017), respectively.  For
#' clustering, the first three are equivalent.  For feature allocation, only the first two
#' are valid.
#' @param maxSize Either zero or a positive integer.  If a positive integer, the
#' optimization is constrained to produce solutions whose number of clusters or number of
#' features is no more than the supplied value.  If zero, the size is not constrained.
#' @param multicore Logical indicating whether computations should take advantage of
#' multiple CPU cores.
#' @param expectedPairwiseAllocationMatrix A \code{n}-by-\code{n} symmetric matrix
#' whose \code{(i,j)} elements gives the estimated expected number of times that items
#' \code{i} and \code{j} are in the same subset (i.e., cluster or feature).  If \code{NULL},
#' it is computed from \code{x}.
#'
#' @return A clustering (as a vector of cluster labels) or a feature allocation (as a binary
#' matrix of feature indicators).
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' \donttest{
#' dlso(iris.clusterings)
#' dlso(USArrests.featureAllocations)
#' }
#'
#' @seealso \code{\link{expectedPairwiseAllocationMatrix}}, \code{\link{salso}}
#'
#' @references Wade, S. and Ghahramani, Z. (2017). Bayesian cluster analysis: Point estimation and credible balls. Bayesian analysis.
#' @references Binder, D. (1978). Bayesian Cluster Analysis. Biometrika, 65: 31–38.
#'
#' @export
#' @import rscala

dlso <- function(x, loss=c("squaredError","absoluteError","binder","lowerBoundVariationOfInformation")[1],
                 maxSize=0,multicore=TRUE,expectedPairwiseAllocationMatrix=NULL) {
  doClustering <- is.matrix(x)
  loss <- as.character(loss[1])
  if ( doClustering ) {
    if ( ! loss %in% c("squaredError","absoluteError","binder","lowerBoundVariationOfInformation") )
      stop("'loss' should be 'squaredError', 'absoluteError', 'binder', or 'lowerBoundVariationOfInformation'.")
  } else {
    if ( ! loss %in% c("squaredError","absoluteError") )
      stop("'loss' should be 'squaredError' or 'absoluteError' when 'x' contains feature allocations.")
  }
  maxSize <- as.integer(maxSize[1])
  multicore <- as.logical(multicore[1])
  epam <- if ( is.null(expectedPairwiseAllocationMatrix) ) s$.None
  else s$.Some$apply(as.matrix(expectedPairwiseAllocationMatrix))
  if ( doClustering ) {
    x <- cleanUpClusteringMatrix(x)
    ref <- s$.ClusteringSummary$minAmongDraws(x,maxSize,multicore,loss,epam)
    ref$toLabels()+1L
  } else {
    x <- scalaConvert.featureAllocation(x)
    ref <- s$.FeatureAllocationSummary$minAmongDraws(x,maxSize,multicore,loss,epam)
    result <- scalaConvert.featureAllocation(ref,withParameters=FALSE)
    attr(result,"scalaReference") <- NULL
    result
  }
}

