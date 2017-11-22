#' Compute Expected Pairwise Clustering Matrix
#'
#' This function computes the \code{n}-by-\code{n} matrix whose \code{(i,j)} elements gives
#' the estimated expected probability that items \code{i} and \code{j} are in the same subset
#' (i.e., cluster) based on the frequencies from the supplied partitions (i.e., clusterings).
#'
#' @param clusterings a \code{B}-by-\code{n} matrix, where each row represents
#' a partition (i.e., clustering) of \code{n} items using cluster labels.  For
#' partition \code{k}, items \code{i} and \code{j} are in the same subset (i.e.,
#' cluster) if \code{clusterings[k,i] == clusterings[k,j]}.
#'
#' @return a \code{n}-by-\code{n} matrix whose \code{(i,j)} elements gives the estimated
#' expected probability that items \code{i} and \code{j} are in the same subset
#' (i.e., cluster) based on the frequencies from the supplied partitions (i.e., clusterings).
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' clusterings <- matrix(c(1,1,1,  1,2,2,  1,1,2,  1,1,1,  1,2,2), ncol=3, byrow=TRUE)
#' epcm <- expectedPairwiseClusteringMatrix(clusterings)
#' epcm
#'
#' @export
#' @import rscala

expectedPairwiseClusteringMatrix <- function(clusterings) {
  if ( ! is.matrix(clusterings) ) stop("'clusterings' must be a matrix.")
  if ( any(dim(clusterings)==0) ) stop("'clusterings' matrix must have a least one row and one column.")
  if ( is.character(clusterings) ) {
    clusterings <- t(apply(clusterings,1,function(p) {
      as.integer(as.factor(p))
    }))
  } else storage.mode(clusterings) <- "integer"
  r <- s$.partition$expectedPairwiseClusteringMatrix(clusterings)
  dimnames(r) <- list(colnames(clusterings),colnames(clusterings))
  r
}

