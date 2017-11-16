#' @export

feature <- function(parameter, items) {
  if ( ! is.vector(items) ) stop("Items should be a vector of integers.")
  if ( is.logical(items) ) items <- which(items)
  if ( length(items) < 1 ) stop("Items may not be empty.")
  storage.mode(items) <- "integer"
  structure(list(parameter=parameter,items=sort(items)), class="sdolsFeatureR")
}

## x <- feature(3,c(2,3,4,5))

#' @importFrom rscala II
#' @export II
#' @export

scalaConvert.feature <- function(x) {
  if ( inherits(x,"sdolsFeatureR") ) {
    items <- I(x$items-1L)
    parameter <- x$parameter
    if ( is.null(parameter) ) {
      s['items',drop='x'] %.!% 'Feature(null: Null, items: _*)'
    } else if ( is.scalaReference(parameter) ) {
      s['parameter','items',drop='x'] %.!% 'Feature(parameter, items: _*)'
    } else if ( ! inherits(parameter,"ScalaAsIs") ) {
      s['parameter','items',drop='x'] %.!% 'Feature(parameter, items: _*)'
    } else {
      s['parameter','items',drop='x'] %.!% 'Feature(R.getReference(parameter), items: _*)'
    }
  } else if ( is.scalaReference(x) ) {
    feature(x$parameter(),sort(x$set()$toArray()+1L))
  } else stop("Argument is not a feature.")
}

#' @export

featureAllocation <- function(nItems, ...) {
  nItems <- as.integer(nItems)
  features <- list(...)
  fs <- lapply(features, function(f) {
    if ( is.scalaReference(f) ) f
    else if ( inherits(f,"sdolsFeatureR") ) scalaConvert.feature(f)
    else stop("Cannot interprete argument as a feature.")
  })
  type <- unique(sapply(fs, function(x) x$type))
  if ( length(type) != 1 ) stop("'features' are of different types.")
  features <- I(sapply(fs, function(x) get("identifier",envir=x$obj)))
  obj <- s["features"] %.!% paste0('
    val fs = features.map(R.cached(_).asInstanceOf[Feature[',type,']])
    FeatureAllocation(nItems, fs: _*)
  ')
  structure(list(obj=obj, type=type), class="sdolsFeatureAllocation")
}


#' @export

featureOff <- function(parameter, items) {
  if ( missing(items) && is.list(parameter) && ( all(names(parameter) %in% c("parameter","items")) ) ) {
    return(feature(parameter$parameter, parameter$items))
  }
  if ( ! is.vector(items) || length(items) == 0 ) stop("'items' is misspecified.")
  if ( is.logical(items) ) items <- which(items)
  if ( ! is.numeric(items) ) stop("'items' is misspecified.")
  items <- I(as.integer(items-1))
  obj <- if ( is.null(parameter) ) {
    type <- "Null"
    s %.!% 'Feature(null: Null, items: _*)'
  } else if ( inherits(parameter,"ScalaInterpreterReference") || inherits(parameter,"ScalaCachedReference") ) {
    type <- parameter$type
    s %.!% 'Feature(parameter, items: _*)'
  } else {
    type <- "PersistentReference"
    parameter <- II(parameter)
    s %.!% 'Feature(R.getReference(parameter), items: _*)'
  }
  structure(list(obj=obj,type=type), class="sdolsFeature")
}

#' @export

featureAllocationOff <- function(nItems, ...) {
  nItems <- as.integer(nItems)
  features <- list(...)
  fs <- lapply(features, function(f) {
    if ( inherits(f,"sdolsFeature") ) f else feature(f)
  })
  type <- unique(sapply(fs, function(x) x$type))
  if ( length(type) != 1 ) stop("'features' are of different types.")
  features <- I(sapply(fs, function(x) get("identifier",envir=x$obj)))
  obj <- s["features"] %.!% paste0('
    val fs = features.map(R.cached(_).asInstanceOf[Feature[',type,']])
    FeatureAllocation(nItems, fs: _*)
  ')
  structure(list(obj=obj, type=type), class="sdolsFeatureAllocation")
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

