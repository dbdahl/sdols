#' Compute Expected Pairwise Allocation Matrix
#'
#' This function computes the \code{n}-by-\code{n} matrix whose \code{(i,j)}
#' element gives the (estimated) expected number of times that \code{i} and
#' \code{j} are in the same subset (i.e, cluster).
#' This is the (estimated) probability that items are clustered together.
#' These estimates are based on the frequencies from the supplied,
#' randomly-sampled clusterings.
#'
#' @param x A
#' collection of clusterings as a
#' \code{B}-by-\code{n} matrix, each of the \code{B} rows represents a
#' clustering of \code{n} items using cluster labels.  For clustering
#' \code{b}, items \code{i} and \code{j} are in the same cluster if
#' \code{x[b,i] == x[b,j]}.
#'
#' @return A \code{n}-by-\code{n} symmetric matrix whose \code{(i,j)} elements
#' gives the estimated expected number of times that items \code{i} and
#' \code{j} are in the same subset (i.e, cluster) based on the
#' frequencies from the supplied clusterings.
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' suppressWarnings({  # For testing purposes, suppress deprecation warning.
#'
#' probabilities <- expectedPairwiseAllocationMatrix(iris.clusterings)
#' 
#' })
#' 
#' @seealso \code{\link{dlso}}, \code{\link{salso}}
#'
#' @export
#' @importFrom salso psm
#' 
expectedPairwiseAllocationMatrix <- function(x) {
  .Deprecated("psm","salso","This function is deprecated.  Please use the 'psm' function in the 'salso' package.")
  psm(x, FALSE)
}
