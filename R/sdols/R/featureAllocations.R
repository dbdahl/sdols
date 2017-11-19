#' @importFrom rscala II
#' @export II
#' @export

scalaConvert.featureAllocation <- function(x, withParameters=TRUE) {
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
    if ( ! is.null(attr(x,"scalaReference")) ) return(attr(x,"scalaReference"))
    singleton <- is.matrix(x)
    if ( singleton ) x <- list(x)
    dataTuple <- if ( length(x) == 0 ) list(integer(0),double(0))
    else {
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
      list(data,data2)
    }
    result <- if ( length(dataTuple[[2]]) > 0 ) {
      s$.FeatureAllocation$deserializeWithParameters(dataTuple[[1]],dataTuple[[2]])
    } else {
      s$.FeatureAllocation$deserialize(dataTuple[[1]])
    }
    if ( singleton ) result$head()
    else result
  }
}

#' @export

leastSquaresFA <- function(x, sfm=sharedFeaturesMatrix(x)) {
  if ( ! inherits(x,"aibdFeatureAllocationVector") ) stop("'x' is not of the right type.")
  obj <- s$.LeastSquaresFeatureAllocation$apply(x$obj,s$.Some$apply(sfm))
  type <- x$type
  structure(list(obj=obj, type=type), class="aibdFeatureAllocation")
}

#' @export

sumOfSquaresFA <- function(fa, sfm) {
  s$.LeastSquaresFeatureAllocation$sumOfSquares(fa$obj,sfm)
}

