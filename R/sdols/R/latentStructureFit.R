#' Compute Fit Summaries for a Latent Structure Estimate
#'
#' This function computes various summaries of the fit of a clustering or feature allocation based on the expected
#' pairwise allocation matrix.
#'
#' @param estimate A clustering or feature allocation.  If \code{estimate} is a length \code{n}
#' vector, it is taken to be a clustering where items \code{i} and
#' \code{j} are in the same cluster if \code{estimate[i] == estimate[j]}.  If \code{estimate}
#' is a binary matrix of \code{n} rows and an arbitrary number of columns, it is taken to be a feature allocation
#' where items \code{i} and \code{j} share \code{m} features if, for \code{k}
#' = 1, 2, ..., the expression \code{estimate[i,k] == estimate[j,k] == 1} is true exactly
#' \code{m} times.
#' @param expectedPairwiseAllocationMatrix A \code{n}-by-\code{n} symmetric matrix
#' whose \code{(i,j)} elements gives the estimated expected number of times that items
#' \code{i} and \code{j} are in the same subset (i.e., cluster or feature).  This can be computed by the
#' \code{\link{expectedPairwiseAllocationMatrix}} function.
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' probabilities <- expectedPairwiseAllocationMatrix(iris.clusterings)
#' estimate <- salso(probabilities)
#' latentStructureFit(estimate, probabilities)
#'
#' expectedCounts <- expectedPairwiseAllocationMatrix(USArrests.featureAllocations)
#' estimate <- salso(expectedCounts,"featureAllocation")
#' latentStructureFit(estimate, expectedCounts)
#'
#' @seealso \code{\link{expectedPairwiseAllocationMatrix}} \code{\link{salso}}
#'
#' @export
#' @import rscala

latentStructureFit <- function(estimate, expectedPairwiseAllocationMatrix) {
  if ( ! is.atomic(estimate) ) stop("'estimate' should be a vector of labels giving a clustering or a binary matrix giving a feature allocation.")
  doClustering <- ! is.matrix(estimate)
  epam <- as.expectedPairwiseAllocationMatrix(expectedPairwiseAllocationMatrix,doClustering)
  if ( doClustering ) {
    ref <- s$.Clustering$apply(as.integer(estimate))
    ss <- s$.ClusteringSummary$binderSumOfSquares(ref,epam)
    sa <- s$.ClusteringSummary$binderSumOfAbsolutes(ref,epam)
    binder <- s$.ClusteringSummary$binderSumOfAbsolutes(ref,epam) / 2
    vi <- s$.ClusteringSummary$lowerBoundVariationOfInformation(ref,epam)
    list("squaredError"=ss,"absoluteError"=sa,"binder"=binder,"lowerBoundVariationOfInformation"=vi)
  } else {
    ref <- scalaConvert.featureAllocation(estimate,withParameters=FALSE)
    ss <- s$.FeatureAllocationSummary$binderSumOfSquares(ref,epam)
    sa <- s$.FeatureAllocationSummary$binderSumOfAbsolutes(ref,epam)
    list("squaredError"=ss,"absoluteError"=sa)
  }
}

