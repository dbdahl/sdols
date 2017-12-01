#' Confidence and Exemplar Plotting
#'
#' Functions to produce confidence plots (e.g., heatmaps of pairwise clustering probabilities) and exemplar plots.
#'
#' @param x An object of class \code{shallot.confidence}.
#' @param clustering A vector of cluster labels, or \code{NULL}.
#' @param data The data from which the distances were computed.
#' @param show.labels Show the items names be shown in the plot?
#' @param ... Currently ignored.
#'
#' @author David B. Dahl \email{dahl@stat.byu.edu}
#'
#' @examples
#' \donttest{
#' probabilities <- expectedPairwiseAllocationMatrix(iris.clusterings)
#' clustering <- salso(probabilities)
#' conf <- confidence(clustering,probabilities)
#' plot(conf)
#' plot(conf,data=iris)
#' }
#'
#' @seealso \code{\link{expectedPairwiseAllocationMatrix}}, \code{\link{dlso}}, \code{\link{salso}}
#'
#' @importFrom grDevices heat.colors rainbow topo.colors
#' @importFrom graphics abline axis box image pairs par points polygon segments text
#' @import rscala
#' @export

plot.sdols.confidence <- function(x, clustering=NULL, data=NULL, show.labels=length(x$clustering)<=50, ...) {
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
  pm <- .rotateForConfidencePlot(x$expectedPairwiseAllocationMatrix,o)
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

.rotateForConfidencePlot <- function(expectedPairwiseAllocationMatrix, order) s %!% '
  val nItems = expectedPairwiseAllocationMatrix.length
  val xx = Array.ofDim[Double](nItems, nItems)
  for (i <- 0 until nItems) {
    for (j <- 0 until nItems) {
      xx(i)(nItems - j - 1) = expectedPairwiseAllocationMatrix(order(i) - 1)(order(j) - 1)
    }
  }
  xx
'

