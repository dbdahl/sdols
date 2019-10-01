#' Compute Clustering Confidence
#'
#' This function computes the confidence values for \code{n} observations
#' based on a clustering estimate and the expected pairwise allocation
#' matrix.
#'
#' @param estimate A vector of length \code{n}, where \code{i} and \code{j}
#' are in the same cluster if and only if \code{clustering[i] == clustering[j]}.
#' @param expectedPairwiseAllocationMatrix A \code{n}-by-\code{n} symmetric
#' matrix whose \code{(i,j)} elements gives the estimated expected probability
#' that items \code{i} and \code{j} are in the same cluster.
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' suppressWarnings({  # For testing purposes, suppress deprecation warning.
#' 
#' probabilities <- expectedPairwiseAllocationMatrix(iris.clusterings)
#' clustering <- salso(probabilities)
#' conf <- confidence(clustering, probabilities)
#' conf
#' 
#' })
#'
#' @seealso \code{\link{expectedPairwiseAllocationMatrix}}, \code{\link{dlso}}, \code{\link{salso}}
#'
#' @export
#' @importFrom salso confidence
#'
confidence <- function(estimate, expectedPairwiseAllocationMatrix) {
  .Deprecated("confidence","salso","This function is deprecated.  Please use the 'confidence' function in the 'salso' package.")
  salso::confidence(estimate, expectedPairwiseAllocationMatrix)
}
