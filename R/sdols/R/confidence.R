#' Compute Clustering Confidence
#'
#' This function computes the confidence values for \code{n} observations
#' based on the supplied clustering and the expected pairwise clustering
#' matrix.
#'
#' @param clustering A vector of length \code{n}, where \code{i} and \code{j}
#' are in the same subset (i.e., cluster) if \code{clustering[i] ==
#' clustering[j]}.
#' @param expectedPairwiseAllocationMatrix A \code{n}-by-\code{n} symmetric
#' matrix whose \code{(i,j)} elements gives the estimated expected probability
#' that items \code{i} and \code{j} are in the same cluster based on the
#' frequencies from the supplied clusterings.
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' epam <- expectedPairwiseAllocationMatrix(iris.clusterings)
#' clustering <- rep(1:3,each=50)
#' conf <- confidence(clustering,epam)
#' conf
#'
#' @export
#' @import rscala

confidence <- function(clustering, expectedPairwiseAllocationMatrix) {
  clustering <- as.clustering(clustering)
  expectedPairwiseAllocationMatrix <- as.expectedPairwiseAllocationMatrix(expectedPairwiseAllocationMatrix)
  tmpObj <- s$.PartitionSummary$confidenceComputations(clustering,expectedPairwiseAllocationMatrix)
  partition <- tmpObj$"_1"() + 1
  names(partition) <- colnames(expectedPairwiseAllocationMatrix)
  confidence <- tmpObj$"_2"()
  names(confidence) <- names(partition)
  confidenceMatrix <- tmpObj$"_3"()
  dimnames(confidenceMatrix) <- list(1:nrow(confidenceMatrix),1:ncol(confidenceMatrix))
  order <- tmpObj$"_4"() + 1L
  names(order) <- names(partition)
  exemplar <- tmpObj$"_5"() + 1L
  names(exemplar) <- 1:length(exemplar)
  result <- list(clustering=partition,confidence=confidence,confidenceMatrix=confidenceMatrix,exemplar=exemplar,order=order,expectedPairwiseAllocationMatrix=expectedPairwiseAllocationMatrix)
  class(result) <- "sdols.confidence"
  result
}

as.clustering <- function(clustering) {
  if ( ! is.atomic(clustering) ) stop("'clustering' must be a vector.")
  as.integer(clustering)
}

as.expectedPairwiseAllocationMatrix <- function(expectedPairwiseAllocationMatrix, isProbability=TRUE) {
  if ( ! is.matrix(expectedPairwiseAllocationMatrix) ) stop("'expectedPairwiseAllocationMatrix' must be a matrix.")
  if ( ! isSymmetric(expectedPairwiseAllocationMatrix) ) stop("'expectedPairwiseAllocationMatrix' must be symmetric.")
  storage.mode(expectedPairwiseAllocationMatrix) <- "double"
  if ( min(expectedPairwiseAllocationMatrix) < 0.0 ) stop("Elements of 'expectedPairwiseAllocationMatrix' are less than 0.")
  if ( isProbability && ( max(expectedPairwiseAllocationMatrix) > 1.0 ) ) stop("Elements of 'expectedPairwiseAllocationMatrix' are greater than 0.")
  expectedPairwiseAllocationMatrix
}

