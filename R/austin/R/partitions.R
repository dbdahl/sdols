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
  r <- s$.partition.PairwiseProbabilityMatrix$apply(clusterings)
  dimnames(r) <- list(colnames(clusterings),colnames(clusterings))
  r
}

#' @export
#' @import rscala

confidence <- function(pairwise.probabilities, partition) {
  tmpObj <- pairwise.probabilities$ref$confidenceComputations(.labels2partition(partition,.nullModel()))
  partition <- tmpObj$"_1"() + 1
  names(partition) <- pairwise.probabilities$names
  confidence <- tmpObj$"_2"()
  names(confidence) <- names(partition)
  confidence.matrix <- tmpObj$"_3"()
  dimnames(confidence.matrix) <- list(1:nrow(confidence.matrix),1:ncol(confidence.matrix))
  order <- tmpObj$"_4"() + 1L
  names(order) <- names(partition)
  exemplar <- tmpObj$"_5"() + 1L
  names(exemplar) <- 1:length(exemplar)
  result <- list(partition=partition,confidence=confidence,confidence.matrix=confidence.matrix,exemplar=exemplar,order=order,pairwise.probabilities=pairwise.probabilities)
  class(result) <- "shallot.confidence"
  result
}



