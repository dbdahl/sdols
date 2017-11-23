#' Compute Clustering Confidence
#'
#' This function computes the confidence values for \code{n} observations
#' based on the supplied clustering and the expected pairwise clustering
#' matrix.
#'
#' @param clustering a vector of length \code{n}, where \code{i} and \code{j}
#' are in the same subset (i.e., cluster) if \code{clustering[i] ==
#' clustering[j]}.
#' @param expectedPairwiseClusteringMatrix a \code{n}-by-\code{n} matrix whose
#' \code{(i,j)} elements gives the estimated expected probability that items
#' \code{i} and \code{j} are in the same subset (i.e., cluster) based on the
#' frequencies from the supplied partitions (i.e., clusterings).
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' epcm <- expectedPairwiseClusteringMatrix(iris.clusterings)
#' clustering <- rep(1:3,each=50)
#' conf <- confidence(clustering,epcm)
#' conf
#'
#' @export
#' @import rscala

confidence <- function(clustering, expectedPairwiseClusteringMatrix) {
  clustering <- as.clustering(clustering)
  expectedPairwiseClusteringMatrix <- as.expectedPairwiseClusteringMatrix(expectedPairwiseClusteringMatrix)
  tmpObj <- s$.PartitionSummary$confidenceComputations(clustering,expectedPairwiseClusteringMatrix)
  partition <- tmpObj$"_1"() + 1
  names(partition) <- colnames(expectedPairwiseClusteringMatrix)
  confidence <- tmpObj$"_2"()
  names(confidence) <- names(partition)
  confidenceMatrix <- tmpObj$"_3"()
  dimnames(confidenceMatrix) <- list(1:nrow(confidenceMatrix),1:ncol(confidenceMatrix))
  order <- tmpObj$"_4"() + 1L
  names(order) <- names(partition)
  exemplar <- tmpObj$"_5"() + 1L
  names(exemplar) <- 1:length(exemplar)
  result <- list(clustering=partition,confidence=confidence,confidenceMatrix=confidenceMatrix,exemplar=exemplar,order=order,expectedPairwiseClusteringMatrix=expectedPairwiseClusteringMatrix)
  class(result) <- "sdols.confidence"
  result
}

as.clustering <- function(clustering) {
  if ( ! is.vector(clustering) ) stop("'clustering' must be a vector.")
  as.integer(clustering)
}

as.expectedPairwiseClusteringMatrix <- function(expectedPairwiseClusteringMatrix) {
  if ( ! is.matrix(expectedPairwiseClusteringMatrix) ) stop("'expectedPairwiseClusteringMatrix' must be a matrix.")
  if ( ! isSymmetric(expectedPairwiseClusteringMatrix) ) stop("'expectedPairwiseClusteringMatrix' must be symmetric.")
  storage.mode(expectedPairwiseClusteringMatrix) <- "double"
  if ( min(expectedPairwiseClusteringMatrix) < 0.0 ) stop("Elements of 'expectedPairwiseClusteringMatrix' are less than 0.")
  if ( max(expectedPairwiseClusteringMatrix) > 1.0 ) stop("Elements of 'expectedPairwiseClusteringMatrix' are less than 0.")
  expectedPairwiseClusteringMatrix
}

