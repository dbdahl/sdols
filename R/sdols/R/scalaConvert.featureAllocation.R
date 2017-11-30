#' \strong{(Developers Only:)} Convert Between R and Scala Representations of Feature Allocations
#'
#' \strong{This function is not intended for end users}, but is exported for the benefit of
#' developers whose wish to write other packages that depend on this package.
#'
#' @param x Either: i. a binary matrix or list of binary matrices encoding feature allocation(s).
#'          ii. a Scala reference to a feature allocation.
#' @param names A character vector giving the row names when converting from Scala to R.
#' @param withParameters A logical indicating whether model parameters should also be converted.
#'
#' @import rscala
#' @export

scalaConvert.featureAllocation <- function(x, names=NULL, withParameters=TRUE) {
  if ( is.scalaReference(x) ) {
    singleton <- ! grepl("^Array\\[",x$type)
    if ( singleton ) {
      xSingleton <- x
      x <- s$.Array$apply(xSingleton)
    }
    dataTuple <- if ( withParameters ) {
      tmp <- s$.FeatureAllocation$serializeWithParameters(x)
      list(tmp$"_1"(),tmp$"_2"())
    }
    else list(s$.FeatureAllocation$serialize(x),double())
    xx <- dataTuple[[1]]
    yy <- dataTuple[[2]]
    if ( length(xx) == 0 ) return(NULL)
    nItems <- xx[1]
    N <- xx[2]
    i <- 3
    doW <- ( ! is.null(yy) ) && ( length(yy) != 0 )
    M <- if ( doW ) {
      ii <- 2
      yy[1]
    } else 0
    Zs <- vector("list",N)
    for ( a in seq_len(N) ) {
      K <- xx[i]
      i <- i + 1
      Z <- matrix(0L,nrow=nItems,ncol=K)
      for ( k in seq_len(K) ) {
        size <- xx[i]
        i <- i + 1
        Z[xx[i:(i+size-1)]+1,k] <- 1L
        i <- i + size
      }
      if ( doW ) {
        values <- if ( K*M > 0 ) yy[ii:(ii+K*M-1)] else 0
        attr(Z,"parameters") <- matrix(values,nrow=K,ncol=M,byrow=TRUE)
        ii <- ii + K*M
      }
      rownames(Z) <- names
      Zs[[a]] <- Z
    }
    if ( singleton ) {
      Z <- Zs[[1]]
      attr(Z,"scalaReference")  <- xSingleton
      Z
    } else {
      attr(Zs,"scalaReference") <- x
      Zs
    }
  } else {
    if ( ! is.null(attr(x,"scalaReference")) ) {
      xx <- attr(x,"scalaReference")
      if ( xx$true ) return(xx)
    }
    singleton <- is.matrix(x)
    if ( singleton ) x <- list(x)
    if ( ! is.list(x) ) stop("'x' should be a list of feature allocations.")
    if ( length(x) == 0 ) stop("'x' should not be empty.")
    for ( i in 1:length(x) ) {
      Z <- x[[i]]
      if ( ! is.matrix(Z) ) stop(paste0("Element ",i," of 'x' is not a matrix."))
      if ( ! all(unique(as.vector(Z)) %in% c(0,1)) ) stop(paste0("Element ",i," of 'x' is not a binary feature matrix."))
    }
    data <- integer(0)
    data2 <- double(0)
    sizeDeclared <- FALSE
    size <- -1L
    data[1] <- nrow(x[[1]])
    data[2] <- length(x)
    i <- 3
    ii <- 1
    for ( fa in x ) {
      if ( withParameters ) {
        W <- attr(fa,"parameters")
        if ( ! is.null(W) ) {
          if ( ! sizeDeclared ) {
            sizeDeclared <- TRUE
            data2[ii] <- ncol(W)
            ii <- ii + 1
          }
          data2[ii:(ii+length(W)-1)] <- t(W)
          ii <- ii + length(W)
        }
      }
      data[i] <- ncol(fa)
      i <- i + 1
      for ( k in 1:ncol(fa) ) {
        what <- which(fa[,k]==1)-1L
        data[i] <- length(what)
        i <- i + 1
        data[i:(i+length(what)-1)] <- what
        i <- i + length(what)
      }
    }
    result <- if ( length(data2) > 0 ) {
      s$.FeatureAllocation$deserializeWithParameters(data,data2)
    } else {
      s$.FeatureAllocation$deserialize(data)
    }
    if ( singleton ) result$head()
    else result
  }
}

