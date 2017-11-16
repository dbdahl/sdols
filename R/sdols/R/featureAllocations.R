#' @export

feature <- function(parameter, items) {
  if ( ! is.vector(items) ) stop("Items should be a vector of integers.")
  if ( is.logical(items) ) items <- which(items)
  items <- sort(unique(items[items>=1L]))
  storage.mode(items) <- "integer"
  structure(list(parameter=parameter,items=items), class="sdolsFeature")
}

## x <- feature(3,c(2,3,4,5))

#' @importFrom rscala II
#' @export II
#' @export

scalaConvert.feature <- function(x) {
  if ( is.scalaReference(x) ) {
    feature(x$parameter(),sort(x$set()$toArray()+1L))
  } else if ( inherits(x,"sdolsFeature") ) {
    items <- I(x$items-1L)
    parameter <- x$parameter
    if ( is.null(parameter) ) {
      s['items',drop='x'] %.!% 'Feature(null: Null, items: _*)'
    } else if ( is.scalaReference(parameter) ) {
      s['parameter','items',drop='x'] %.!% 'Feature(parameter, items: _*)'
    } else if ( inherits(parameter,"ScalaAsIs") ) {
      s['parameter','items',drop='x'] %.!% 'Feature(R.getReference(parameter), items: _*)'
    } else {
      s['parameter','items',drop='x'] %.!% 'Feature(parameter, items: _*)'
    }
  } else stop("Argument is not a feature.")
}

#' @export

featureAllocation <- function(nItems, ...) {
  nItems <- as.integer(nItems)
  features <- list(...)
  for ( f in features) if ( ! inherits(f,"sdolsFeature") ) stop("Argument is not a feature.")
  if ( length(features) > 0 ) features <- features[sapply(features,function(f) length(f$items)>0)]
  structure(list(nItems=nItems, features=features), class="sdolsFeatureAllocation")
}

#' @export

scalaConvert.featureAllocation <- function(x,typeIfEmpty="Null") {
  if ( is.scalaReference(x) ) {
    nItems <- x$nItems()
    nFeatures <- x$nFeatures()
    if ( nFeatures == 0 ) return(featureAllocation(nItems))
    fs <- x$toVector()
    features <- vector("list",nFeatures)
    for ( i in 1:nFeatures ) {
      features[[i]] <- scalaConvert.feature(fs$apply(i-1L))
    }
    do.call("featureAllocation",c(nItems,features))
  } else if ( inherits(x,"sdolsFeatureAllocation") ) {
    nItems <- x$nItems
    features <- x$features
    if ( length(features) == 0 ) {
      fa <- s$.FeatureAllocation$do(paste0("empty[",typeIfEmpty,"]"))(nItems)
    } else {
      fa <- s$.FeatureAllocation$apply(nItems,scalaConvert.feature(features[[1]]))
      if ( length(features) > 1 ) for ( f in features[2:length(features)] ) fa <- fa$add(scalaConvert.feature(f))
    }
    fa
  } else stop("Argument is not a feature.")
}

#' @export

toString.aibdFeatureAllocation <- function(x, ...) {
  x$obj$toString(FALSE)
}

#' @export

print.aibdFeatureAllocation <- function(x, ...) {
  cat(x$obj$toString(FALSE),"\n",sep="")
  invisible(x)
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

# Private

deserializeFeatureAllocations <- function(x,y=NULL) {
  if ( length(x) == 0 ) return(list())
  nItems <- x[1]
  N <- x[2]
  i <- 3
  doW <- ( ! is.null(y) ) && ( length(y) != 0 )
  M <- if ( doW ) {
    ii <- 2
    y[1]
  } else 0
  Zs <- vector("list",N)
  for ( a in seq_len(N) ) {
    K <- x[i]
    i <- i + 1
    Z <- matrix(0L,nrow=nItems,ncol=K)
    for ( k in seq_len(K) ) {
      size <- x[i]
      i <- i + 1
      Z[x[i:(i+size-1)]+1,k] <- 1L
      i <- i + size
    }
    if ( doW ) {
      values <- if ( K*M > 0 ) y[ii:(ii+K*M-1)] else 0
      attr(Z,"W") <- matrix(values,nrow=K,ncol=M,byrow=TRUE)
      ii <- ii + K*M
    }
    Zs[[a]] <- Z
  }
  Zs
}

# Private

serializeFeatureAllocations <- function(featureAllocation, withParameters=TRUE) {
  if ( length(featureAllocation) == 0 ) return(list(integer(0),double(0)))
  data <- integer(0)
  data2 <- double(0)
  sizeDeclared <- FALSE
  size <- -1L 
  data[1] <- nrow(featureAllocation[[1]])
  data[2] <- length(featureAllocation)
  i <- 3
  ii <- 1
  for ( fa in featureAllocation ) {
    if ( withParameters ) {
      W <- attr(fa,"W")
      if ( ! sizeDeclared ) { 
        sizeDeclared <- TRUE
        data2[ii] <- ncol(W)
        ii <- ii + 1 
      }   
      data2[ii:(ii+length(W)-1)] <- t(W)
      ii <- ii + length(W)
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

