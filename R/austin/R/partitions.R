#' Compute Pairwise Probability Matrix
#'
#' This function computes the \code{n}-by-\code{n} pairwise probability matrix
#' (a.k.a., pairwise similarity matrix) whose \code{(i,j)} element gives the
#' proportion of times that items \code{i} and \code{j} are in the same subset
#' (i.e., cluster) among the supplied partitions (i.e., clusterings).
#'
#' @param clusterings a \code{B}-by-\code{n} matrix, where each row represents
#' a partition (i.e., clustering) of \code{n} items using cluster labels.  For
#' partition \code{k}, items \code{i} and \code{j} are in the same subset (i.e.,
#' cluster) if \code{clusterings[k,i] == clusterings[k,j]}.
#'
#' @return a \code{n}-by-\code{n} pairwise probability matrix (a.k.a., pairwise
#' similarity matrix) whose \code{(i,j)} element gives the proportion of times
#' that items \code{i} and \code{j} are in the same subset (i.e., cluster) among
#' the supplied partitions (i.e., clusterings).
#'
#' @examples
#' clusterings <- matrix(c(1,1,1,  1,2,2,  1,1,2,  1,1,1,  1,2,2), ncol=3, byrow=TRUE)
#' pairwiseProbabilityMatrix(clusterings)
#'
#' @export
#' @import rscala

pairwiseProbabilityMatrix <- function(clusterings) {
  if ( ! is.matrix(clusterings) ) stop("'clusterings' must be a matrix.")
  if ( any(dim(clusterings)==0) ) stop("'clusterings' matrix must have a least one row and one column.")
  if ( is.character(clusterings) ) {
    clusterings <- t(apply(clusterings,1,function(p) {
      as.integer(as.factor(p))
    }))
  } else storage.mode(clusterings) <- "integer"
  s$.partition.PairwiseProbabilityMatrix$apply(clusterings)
}

