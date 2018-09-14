#' @import rscala
scalaSerialize.featureAllocation <- function(x, bridge=scalaFindBridge(), verbose=FALSE, withParameters=TRUE, ...) {
  if ( verbose ) cat("scalaSerialize.featureAllocation: Trying...\n")
  singleton <- is.matrix(x)
  if ( singleton ) x <- list(x)
  if ( ! is.list(x) ) {
    if ( verbose ) cat("scalaSerialize.featureAllocation: Object is not a list.\n")
    return(NULL)
  }
  if ( length(x) == 0 ) {
    if ( verbose ) cat("scalaSerialize.featureAllocation: Object should not be empty.\n")
    return(NULL)
  }
  nRow <- nrow(x[[1]])
  for ( i in 1:length(x) ) {
    Z <- x[[i]]
    if ( ! is.matrix(Z) ) {
      if ( verbose ) cat(paste0("Element ",i," of 'x' is not a matrix.\n"))
      return(NULL)
    }
    if ( nrow(Z) != nRow ) {
      if ( verbose ) cat(paste0("Number of rows must be consistent.\n"))
      return(NULL)      
    }
    if ( ! all(unique(as.vector(Z)) %in% c(0,1)) ) {
      if ( verbose ) cat(paste0("Element ",i," of 'x' is not a binary feature matrix.\n"))
      return(NULL)
    }
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
        if ( length(W) > 0 ) {
          data2[ii:(ii+length(W)-1)] <- t(W)
          ii <- ii + length(W)
        }
      }
    }
    data[i] <- ncol(fa)
    i <- i + 1
    for ( k in seq_len(ncol(fa)) ) {
      what <- which(fa[,k]==1)-1L
      data[i] <- length(what)
      i <- i + 1
      data[i:(i+length(what)-1)] <- what
      i <- i + length(what)
    }
  }
  result <- if ( length(data2) > 0 ) {
    if ( length(data) == 0 ) s ^ 'Array[FeatureAllocation[Vector[Double]]]()'
    else s(data=I(data),data2=I(data2)) ^ '
      val iter = data.iterator
      val iter2 = data2.iterator
      var sizeDeclared = false
      var size = -1
      val nItems = iter.next()
      val N = iter.next()
      val fas = Array.ofDim[FeatureAllocation[Vector[Double]]](N)
      for ( i <- 0 until N ) {
        var fa = FeatureAllocation.empty[Vector[Double]](nItems)
        val K = iter.next()
        for ( k <- 0 until K ) {
          if ( ! sizeDeclared ) {
            sizeDeclared = true
            size = iter2.next().toInt
          }
          val parameter = Vector.fill(size) { iter2.next() }
          fa = fa.add(Feature(parameter,Seq.fill(iter.next()) { iter.next() }:_*))
        }
        fas(i) = fa
      }
      fas
    '
  } else {
    if ( length(data) == 0 ) s ^ 'Array[FeatureAllocation[Null]]()'
    else s(data=I(data)) ^ '
      val iter = data.iterator
      val nItems = iter.next()
      val N = iter.next()
      val fas = Array.ofDim[FeatureAllocation[Null]](N)
      for ( i <- 0 until N ) {
        var fa = FeatureAllocation.empty[Null](nItems)
        val K = iter.next()
        for ( k <- 0 until K ) {
          fa = fa.add(Feature(Seq.fill(iter.next()) { iter.next() }:_*))
        }
        fas(i) = fa
      }
      fas
    '
  }
  if ( singleton ) result$head() else result
}

#' @import rscala
scalaUnserialize.featureAllocation <- function(reference, type=scalaType(reference), bridge=scalaFindBridge(reference), verbose=FALSE, names=NULL, withParameters=TRUE, ...) {
  if ( verbose ) cat("scalaUnserialize.featureAllocation: Trying...\n")
  singleton <- FALSE 
  if ( reference$"isInstanceOf[org.ddahl.sdols.featureallocation.FeatureAllocation[_]]"() ) {
    reference <- s(x=reference) ^ 'List(x)'
    type <- scalaType(reference)
    singleton <- TRUE
  }
  if ( ! reference$"isInstanceOf[List[org.ddahl.sdols.featureallocation.FeatureAllocation[_]]]"() ) return(NULL)
  withParameters <- withParameters && ( substr(type,nchar(type)-16,nchar(type)) == "[Vector[Double]]]" )
  dataTuple <- if ( withParameters ) { 
    tmp <- if ( reference$isEmpty() ) s ^ '(Array[Int](),Array[Double]())'
    else s(featureAllocations=reference) ^ '
      import scala.collection.mutable.ArrayBuffer
      val fas = featureAllocations.map(_.leftOrderedForm)
      val data = ArrayBuffer[Int]()
      val data2 = ArrayBuffer[Double]()
      var sizeDeclared = false
      data += fas.head.nItems
      data += fas.length
      fas.foreach { fa =>
        data += fa.size
        fa.features.foreach { f =>
          data += f.size
          data ++= f.set.toArray
          if ( ! sizeDeclared ) {
            sizeDeclared = true
            data2 += f.parameter.length
          }
          data2 ++= f.parameter
        }
      }
      (data.toArray, data2.toArray)
    '
    list(tmp$"_1"(),tmp$"_2"())
  }   
  else {
    tmp <- if ( reference$isEmpty() ) s * 'Array[Int]()'
    else s(featureAllocations=reference) * '
      import scala.collection.mutable.ArrayBuffer
      val fas = featureAllocations.map(_.dropParameters.leftOrderedForm)
      val data = ArrayBuffer[Int]()
      data += fas.head.nItems
      data += fas.size
      fas.foreach { fa =>
        data += fa.size
        fa.features.foreach { f =>
          data += f.size
          data ++= f.set.toArray
        }
      }
      data.toArray
    '
    list(tmp,double())
  }
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
  if ( singleton ) Zs[[1]] else Zs
}
