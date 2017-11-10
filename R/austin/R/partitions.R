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
  r <- s$.Partition$pairwiseProbabilityMatrix(clusterings)
  dimnames(r) <- list(colnames(clusterings),colnames(clusterings))
  r
}

#' @export
#' @import rscala

confidence <- function(clustering, pairwiseProbabilityMatrix) {
  tmpObj <- s$.Partition$confidenceComputations(clustering,pairwiseProbabilityMatrix)
  partition <- tmpObj$"_1"() + 1
  names(partition) <- colnames(pairwiseProbabilityMatrix)
  confidence <- tmpObj$"_2"()
  names(confidence) <- names(partition)
  confidenceMatrix <- tmpObj$"_3"()
  dimnames(confidenceMatrix) <- list(1:nrow(confidenceMatrix),1:ncol(confidenceMatrix))
  order <- tmpObj$"_4"() + 1L
  names(order) <- names(partition)
  exemplar <- tmpObj$"_5"() + 1L
  names(exemplar) <- 1:length(exemplar)
  result <- list(clustering=partition,confidence=confidence,confidenceMatrix=confidenceMatrix,exemplar=exemplar,order=order,pairwiseProbabilityMatrix=pairwiseProbabilityMatrix)
  class(result) <- "shallot.confidence"
  result
}

# Confidence or pairs plot

.rotateForConfidencePlot <- function(pairwiseProbabilityMatrix, order) s %!% '
  val nItems = pairwiseProbabilityMatrix.length
  val xx = Array.ofDim[Double](nItems, nItems)
  for (i <- 0 until nItems) {
    for (j <- 0 until nItems) {
      xx(i)(nItems - j - 1) = pairwiseProbabilityMatrix(order(i) - 1)(order(j) - 1)
    }
  }
  xx
'

#' @export
#' @import rscala

plot.shallot.confidence <- function(x, clustering=NULL, data=NULL, show.labels=length(x$clustering)<=50, ...) {
  if ( ! is.null(data) ) {
    if ( ! is.null(clustering) ) stop("'clustering' must be 'NULL' for pairs plot.")
    i <- x$exemplar[x$clustering]
    c <- rainbow(length(x$exemplar))[x$clustering]
    panelFnc <- function(x0,y0,...) {
      points(x0,y0,col=c,pch=19,...)
      segments(x0,y0,x0[i],y0[i],col=c,...)
      points(x0[x$exemplar],y0[x$exemplar],pch=22,bg="white",cex=2,...)
    }
    pairs(data,panel=panelFnc)
    return(invisible())
  }
  if ( is.null(clustering) ) {
    clustering <- x$clustering
    o <- x$order
  } else {
    o <- order(clustering)
  }
  pm <- .rotateForConfidencePlot(x$pairwiseProbabilityMatrix,o)
  n <- nrow(pm)
  sizes <- rle(clustering[o])$lengths
  cuts <- cumsum(sizes)
  centers <- ( c(0,cuts[-length(cuts)]) + cuts ) / 2
  cuts <- cuts[-length(cuts)]
  labels <- rle(clustering[o])$values
  if ( show.labels ) {
    mymai <- c(1.5,0.5,0.5,1.5)
    cexscale <- 0.85 * 50 / length(clustering)
  } else {
    mymai <- c(0,0,0,0)
    cexscale <- 1 * 50 / length(clustering)
  }
  opar <- par(pty="s",mai=mymai)
  colors <- topo.colors(200)
  colors <- rev(heat.colors(200))
  image(x=1:n,y=1:n,z=pm,axes=FALSE,xlab="",ylab="",col=colors)
  box()
  abline(v=cuts+0.5,lwd=3)
  abline(h=n-cuts+0.5,lwd=3)
  text(centers+0.5,n-centers+0.5,labels,cex=0.8*cexscale*sizes)
  if ( show.labels ) {
    axisLabels <- if ( is.null(names(clustering)) ) o
    else names(clustering[o])
    axis(4,1:length(clustering),rev(axisLabels),las=2,cex.axis=0.8*cexscale)
    axis(1,1:length(clustering),axisLabels,las=2,cex.axis=0.8*cexscale)
    nn <- length(colors)
    bx <- par("usr")
    bx.cx <- c(bx[1] - 1.6 * (bx[2] - bx[1]) / 50, bx[1] - 0.3 * (bx[2] - bx[1]) / 50)
    bx.cy <- c(bx[3], bx[3])
    bx.sy <- (bx[4] - bx[3]) / nn
    xx <- rep(bx.cx, each=2)
    for ( i in 1:nn ) {
      yy <- c(bx.cy[1] + (bx.sy * (i - 1)),
              bx.cy[1] + (bx.sy * (i)),
              bx.cy[1] + (bx.sy * (i)),
              bx.cy[1] + (bx.sy * (i - 1)))
      polygon(xx,yy,col=colors[i],border=colors[i],xpd=TRUE)
    }
  }
  par(opar)
  invisible()
}


