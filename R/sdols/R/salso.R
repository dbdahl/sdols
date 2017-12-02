#' Perform Sequentially-Allocated Latent Structure Optimization
#'
#' This function implements the sequentially-allocated latent structure optimization (SALSO)
#' to find a clustering or feature allocation that minimizes various loss functions.
#' The SALSO method was presented at the workshop "Bayesian Nonparametric Inference: Dependence
#' Structures and their Applications" in Oaxaca, Mexico on December 6, 2017.
#'
#' @param expectedPairwiseAllocationMatrix A \code{n}-by-\code{n} symmetric matrix
#' whose \code{(i,j)} elements gives the estimated expected number of times that items
#' \code{i} and \code{j} are in the same subset (i.e., cluster or feature).
#' @param structure Either \code{"clustering"} or \code{"featureAllocation"} to indicate
#' the optimization seeks to produce a clustering or a feature allocation.
#' @param loss One of \code{"squaredError"}, \code{"absoluteError"}, \code{"binder"}, or
#' \code{"lowerBoundVariationOfInformation"} to indicate the optimization should seeks to
#' minimize squared error loss, absolute error loss, Binder loss (Binder 1978), or the lower
#' bound of the variation of information loss (Wade & Ghahramani 2017), respectively.  When
#' \code{structure="clustering"}, the first three are equivalent.  When
#' \code{structure="featureAllocation"}, only the first two are valid.
#' @param nCandidates The (maximum) number of candidates to consider.  Fewer than
#' \code{nCandidates} may be considered if the time in \code{budgetInSeconds} is exceeded.
#' The computational cost is linear in the number of candidates and there are rapidly
#' diminishing returns to more candidates.
#' @param budgetInSeconds The (maximum) number of seconds to devote to the optimization.
#' When this time is exceeded, no more candidates are considered.
#' @param maxSize Either zero or a positive integer.  If a positive integer, the
#' optimization is constrained to produce solutions whose number of clusters or number of
#' features is no more than the supplied value.  If zero, the size is not constrained.
#' To avoid overfitting in feature allocation estimation, it is recommended that
#' \code{"maxSize"} be close the mean number of features (i.e., columns) in the
#' feature allocations that generated the \code{expectedPairwiseAllocationMatrix}.
#'
#' @return A clustering (as a vector of cluster labels) or a feature allocation (as a binary
#' matrix of feature indicators).
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' \donttest{
#' probabilities <- expectedPairwiseAllocationMatrix(iris.clusterings)
#' salso(probabilities)
#'
#' expectedCounts <- expectedPairwiseAllocationMatrix(USArrests.featureAllocations)
#' salso(expectedCounts,"featureAllocation")
#' }
#'
#' @seealso \code{\link{expectedPairwiseAllocationMatrix}}, \code{\link{dlso}}
#'
#' @references Wade, S. and Ghahramani, Z. (2017). Bayesian cluster analysis: Point estimation and credible balls. Bayesian analysis.
#' @references Binder, D. (1978). Bayesian Cluster Analysis. Biometrika, 65: 31–38.
#'
#' @export
#' @import rscala

salso <- function(expectedPairwiseAllocationMatrix, structure=c("clustering","featureAllocation")[1],
                  loss=c("squaredError","absoluteError","binder","lowerBoundVariationOfInformation")[1],
                  nCandidates=100, budgetInSeconds=10, maxSize=0, newMethod=FALSE) {
  if ( identical(structure,"clustering") ) doClustering <- TRUE
  else if ( identical(structure,"featureAllocation") ) doClustering <- FALSE
  else stop("'structure' must be either 'clustering' or 'featureAllocation'.")
  epam <- as.expectedPairwiseAllocationMatrix(expectedPairwiseAllocationMatrix,doClustering)
  loss <- as.character(loss[1])
  if ( doClustering ) {
    if ( ! loss %in% c("squaredError","absoluteError","binder","lowerBoundVariationOfInformation") )
      stop("'loss' should be 'squaredError', 'absoluteError', 'binder', or 'lowerBoundVariationOfInformation'.")
  } else {
    if ( ! loss %in% c("squaredError","absoluteError") )
      stop("'loss' should be 'squaredError' or 'absoluteError' when 'structure' is 'featureAllocation'.")
  }
  nCandidates <- as.integer(nCandidates[1])
  budgetInSeconds <- as.integer(budgetInSeconds[1])
  maxSize <- as.integer(maxSize[1])
  newMethod <- as.logical(newMethod)
  result <- if ( doClustering ) {
    ref <- s$.ClusteringSummary$sequentiallyAllocatedLatentStructureOptimization(nCandidates,budgetInSeconds,epam,maxSize,loss,newMethod)
    ref$"_1"()$toLabels()+1L
  } else {
    ref <- s$.FeatureAllocationSummary$sequentiallyAllocatedLatentStructureOptimization(nCandidates,budgetInSeconds,epam,maxSize,loss)
    result <- scalaConvert.featureAllocation(ref$"_1"(),withParameters=FALSE)
    attr(result,"scalaReference") <- NULL
    result
  }
  attr(result,"nCandidates") <- ref$"_2"()
  result
}

