#' Perform Sequentially-Allocated Latent Structure Optimization
#'
#' This function implements the sequentially-allocated latent structure optimization (SALSO)
#' to find a clustering or feature allocation that minimizes the Binder (1978) loss function
#' or the lower bound of the variation of information loss function.  The SALSO method is
#' introduced in Dahl and Müller (2018).
#'
#' @param expectedPairwiseAllocationMatrix A \code{n}-by-\code{n} symmetric matrix
#' whose \code{(i,j)} elements gives the estimated expected number of times that items
#' \code{i} and \code{j} are in the same subset based on the frequencies from the supplied
#' clusterings or feature allocations.  This can be computed by the
#' \code{\link{expectedPairwiseAllocationMatrix}} function.
#' @param structure Either \code{"clustering"} or \code{"featureAllocation"} to indicate
#' the optimization seeks to produce a clustering or a feature allocation.
#' @param loss One of \code{"squaredError"}, \code{"absoluteError"}, \code{"binder"}, or
#' \code{"vi"} to indicate the optimization should seeks to minimize squared error loss,
#' absolute error loss, Binder loss (Binder 1978), or variation of information loss (Wade
#' & Ghahramani 2017), respectively.  When \code{structure="clustering"}, the first three
#' are equivalent.  When \code{structure="featureAllocation"}, only the first two are valid.
#' @param nCandidates The number of candidates to consider.  The computational cost is linear
#' in the number of candidates.
#' @param maxSize Either zero or a positive integer.  If a positive integer, the
#' optimization is constrained to produce solutions whose number of clusters or number of
#' features is no more than the supplied value.  If zero, there is no constraint on the
#' size.
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' probabilities <- expectedPairwiseAllocationMatrix(iris.clusterings)
#' salso(probabilities)
#'
#' expectedCounts <- expectedPairwiseAllocationMatrix(USArrests.featureAllocations)
#' salso(expectedCounts,"featureAllocation")
#'
#' @seealso \code{\link{expectedPairwiseAllocationMatrix}}
#'
#' @export
#' @import rscala

salso <- function(expectedPairwiseAllocationMatrix, structure=c("clustering","featureAllocation")[1],
                  loss=c("squaredError","absoluteError","binder","vi")[1], nCandidates=100, maxSize=0) {
  if ( identical(structure,"clustering") ) seekClustering <- TRUE
  else if ( identical(structure,"featureAllocation") ) seekClustering <- FALSE
  else stop("'structure' must be either 'clustering' or 'featureAllocation'.")
  epam <- as.expectedPairwiseAllocationMatrix(expectedPairwiseAllocationMatrix,seekClustering)
  loss <- as.character(loss[1])
  if ( seekClustering ) {
    if ( ! loss %in% c("squaredError","absoluteError","binder","vi") )
      stop("'loss' should be 'squaredError', 'absoluteError', 'binder', or 'vi'.")
  } else {
    if ( ! loss %in% c("squaredError","absoluteError") )
      stop("'loss' should be 'squaredError' or 'absoluteError' when 'structure' is 'featureAllocation'.")
  }
  nCandidates <- as.integer(nCandidates[1])
  maxSize <- as.integer(maxSize[1])
  if ( seekClustering ) {
    ref <- s$.PartitionSummary$sequentiallyAllocatedLatentStructureOptimization(nCandidates,epam,maxSize,loss)
    result <- ref$toLabels()+1L
    attr(result,loss) <- if ( loss == "squaredError" ) s$.PartitionSummary$binderSumOfSquares(ref,epam)
    else if ( loss %in% "absoluteError" ) s$.PartitionSummary$binderSumOfAbsolutes(ref,epam)
    else if ( loss == "binder" ) s$.PartitionSummary$binderSumOfAbsolutes(ref,epam) / 2
    else if ( loss == "vi" ) s$.PartitionSummary$lowerBoundVariationOfInformation(ref,epam)
    else stop("match error")
  } else {
    ref <- s$.FeatureAllocationSummary$sequentiallyAllocatedLatentStructureOptimization(nCandidates,epam,maxSize,loss)
    result <- scalaConvert.featureAllocation(ref,withParameters=FALSE)
    attr(result,"scalaReference") <- NULL
    attr(result,loss) <- if ( loss == "squaredError" ) s$.FeatureAllocationSummary$binderSumOfSquares(ref,epam)
    else if ( loss == "absoluteError" )  s$.FeatureAllocationSummary$binderSumOfAbsolutes(ref,epam)
    else stop("match error")
  }
  result
}

