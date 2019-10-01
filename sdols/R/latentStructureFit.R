#' Compute Fit Summaries for a Latent Structure Estimate
#'
#' This function computes various summaries of the fit of a clustering based on the expected
#' pairwise allocation matrix.
#'
#' @param estimate A clustering.  If \code{estimate} is a length \code{n}
#' vector, it is taken to be a clustering where items \code{i} and
#' \code{j} are in the same cluster if and only if \code{estimate[i] == estimate[j]}.
#' @param expectedPairwiseAllocationMatrix A \code{n}-by-\code{n} symmetric matrix
#' whose \code{(i,j)} elements gives the estimated expected number of times that items
#' \code{i} and \code{j} are in the same subset (i.e., cluster).
#' 
#' @return A list of the following elements: \describe{
#'   \item{absoluteError}{The expectation of the absolute error loss.}
#'   \item{binder}{The expectation of the binder loss.}
#'   \item{lowerBoundVariationOfInformation}{The lower bound of the expectation of the variation of information loss.} }
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' suppressWarnings({  # For testing purposes, suppress deprecation warning.
#' 
#' probabilities <- expectedPairwiseAllocationMatrix(iris.clusterings)
#' estimate <- salso(probabilities)
#' latentStructureFit(estimate, probabilities)
#'
#' })
#'
#' @seealso \code{\link{expectedPairwiseAllocationMatrix}}, \code{\link{salso}}
#'
#' @export
#' @importFrom salso binder VI.lb
#'
latentStructureFit <- function(estimate, expectedPairwiseAllocationMatrix) {
  if ( ! is.atomic(estimate) ) stop("'estimate' should be a vector of labels giving a clustering.")
  binder <- salso::binder(estimate, expectedPairwiseAllocationMatrix)
  sa <- 2*binder
  vi <- salso::VI.lb(estimate, expectedPairwiseAllocationMatrix)
  list("absoluteError"=sa,"binder"=binder,"lowerBoundVariationOfInformation"=vi)
}
