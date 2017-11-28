#' Compute Expected Pairwise Allocation Matrix
#'
#' This function computes the \code{n}-by-\code{n} matrix whose \code{(i,j)} element gives
#' the estimated expected number of times that \code{i} and \code{j} are in the same subset.
#' For clusterings, this is the estimated probability that items are clustered together.
#' For feature allocations, this is the estimated expectation of the number of shared
#' features.  These estimates are based on the frequencies from the supplied,
#' randomly-sampled clusterings or feature allocations.
#'
#' @param x a collection of clusterings or feature allocations.  If \code{x} is a
#' \code{B}-by-\code{n} matrix, each of the \code{B} rows represents a clustering of
#' \code{n} items using cluster labels.  For clustering \code{b}, items \code{i} and
#' \code{j} are in the same subset (i.e., cluster) if \code{x[b,i] == x[b,j]}.  If \code{x}
#' is a list of length \code{B}, each element of list represents a feature allocation using
#' a binary matrix of \code{n} rows and an arbitrary number of columns.  For feature
#' allocation \code{b}, items \code{i} and \code{j} share \code{m} features if, for \code{k}
#' = 1, 2, ..., the expression \code{x[[b]][i,k] == x[[b]][j,k] == 1} is true exactly
#' \code{m} times.
#'
#' @return a \code{n}-by-\code{n} symmetric matrix whose \code{(i,j)} elements gives the
#' estimated expected number of times that items \code{i} and \code{j} are in the same
#' subset based on the frequencies from the supplied clusterings or feature allocations.
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' probabilities <- expectedPairwiseAllocationMatrix(iris.clusterings)
#' probabilities
#'
#' expectedCounts <- expectedPairwiseAllocationMatrix(USArrests.featureAllocations)
#' expectedCounts
#'
#' @export
#' @import rscala

expectedPairwiseAllocationMatrix <- function(x) {
  if ( is.matrix(x) ) {
    if ( nrow(x) == 0 ) stop("The matrix 'x' must have a least one clustering (i.e, row).")
    if ( ncol(x) == 0 ) stop("The matrix 'x' must have a least one item (i.e, column).")
    if ( is.character(x) ) {
      x <- t(apply(x,1,function(p) {
        as.integer(as.factor(p))
      }))
    } else storage.mode(x) <- "integer"
    r <- s$.PartitionSummary$expectedPairwiseAllocationMatrix(x)
    names <- colnames(x)
  } else if ( is.list(x) ) {
    reference <- scalaConvert.featureAllocation(x)
    r <- s$.FeatureAllocationSummary$expectedPairwiseAllocationMatrix(reference)
    names <- rownames(x[[1]])
  }
  dimnames(r) <- list(names,names)
  r
}

