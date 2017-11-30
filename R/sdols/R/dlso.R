#' Perform Draws-Based Latent Structure Optimization
#'
#' This function picks the latent structure among the supplied latent structures minimizes
#' one of various loss functions.
#'
#' @param x A collection of clusterings or feature allocations.  If \code{x} is a
#' \code{B}-by-\code{n} matrix, each of the \code{B} rows represents a clustering of
#' \code{n} items using cluster labels.  For clustering \code{b}, items \code{i} and
#' \code{j} are in the same cluster if \code{x[b,i] == x[b,j]}.  If \code{x}
#' is a list of length \code{B}, each element of list represents a feature allocation using
#' a binary matrix of \code{n} rows and an arbitrary number of columns.  For feature
#' allocation \code{b}, items \code{i} and \code{j} share \code{m} features if, for \code{k}
#' = 1, 2, ..., the expression \code{x[[b]][i,k] == x[[b]][j,k] == 1} is true exactly
#' \code{m} times.
#' @param structure Either \code{"clustering"} or \code{"featureAllocation"} to indicate
#' the optimization seeks to produce a clustering or a feature allocation.
#' @param loss One of \code{"squaredError"}, \code{"absoluteError"}, \code{"binder"}, or
#' \code{"lowerBoundVariationOfInformation"} to indicate the optimization should seeks to
#' minimize squared error loss, absolute error loss, Binder loss (Binder 1978), or the lower
#' bound of the variation of information loss (Wade & Ghahramani 2017), respectively.  When
#' \code{structure="clustering"}, the first three are equivalent.  When
#' \code{structure="featureAllocation"}, only the first two are valid.
#' @param maxSize Either zero or a positive integer.  If a positive integer, the
#' optimization is constrained to produce solutions whose number of clusters or number of
#' features is no more than the supplied value.  If zero, the size is not constrained.
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' probabilities <- expectedPairwiseAllocationMatrix(iris.clusterings)
#' dlso(probabilities)
#'
#' expectedCounts <- expectedPairwiseAllocationMatrix(USArrests.featureAllocations)
#' dlso(expectedCounts,"featureAllocation")
#'
#' @seealso \code{\link{expectedPairwiseAllocationMatrix}} \code{\link{salso}}
#'
#' @export
#' @import rscala

dlso <- function(x, structure=c("clustering","featureAllocation")[1],
                 loss=c("squaredError","absoluteError","binder","lowerBoundVariationOfInformation")[1],
                 maxSize=0) {
  if ( identical(structure,"clustering") ) doClustering <- TRUE
  else if ( identical(structure,"featureAllocation") ) doClustering <- FALSE
  else stop("'structure' must be either 'clustering' or 'featureAllocation'.")
  loss <- as.character(loss[1])
  if ( doClustering ) {
    if ( ! loss %in% c("squaredError","absoluteError","binder","lowerBoundVariationOfInformation") )
      stop("'loss' should be 'squaredError', 'absoluteError', 'binder', or 'lowerBoundVariationOfInformation'.")
  } else {
    if ( ! loss %in% c("squaredError","absoluteError") )
      stop("'loss' should be 'squaredError' or 'absoluteError' when 'structure' is 'featureAllocation'.")
  }
  maxSize <- as.integer(maxSize[1])
  if ( doClustering ) {
    x <- cleanUpClusteringMatrix(x)
    ref <- s$.ClusteringSummary$minAmongDraws(x,maxSize,loss,s$.None)
    ref$toLabels()+1L
  } else {
    x <- scalaConvert.featureAllocation(x)
    ref <- s$.FeatureAllocationSummary$minAmongDraws(x,maxSize,loss,s$.None)
    result <- scalaConvert.featureAllocation(ref,withParameters=FALSE)
    attr(result,"scalaReference") <- NULL
    result
  }
}

