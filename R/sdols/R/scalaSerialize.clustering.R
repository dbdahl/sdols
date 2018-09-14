#' @import rscala
scalaSerialize.clustering <- function(x, bridge=scalaFindBridge(), verbose=FALSE, withParameters=TRUE, ...) {
  if ( verbose ) cat("scalaSerialize.clustering: Trying...\n")
  singleton <- is.vector(x)
  if ( singleton ) x <- matrix(x,nrow=1)
  if ( ! is.matrix(x) ) {
    if ( verbose ) cat("scalaSerialize.clustering: Object is not a matrix.\n")
    return(NULL)
  }
  if ( nrow(x) == 0 ) {
    if ( verbose ) cat("scalaSerialize.clustering: Object should not be empty.\n")
    return(NULL)
  }
  result <- s(x=x) ^ 'x.map(Clustering.apply).toList'
  if ( singleton ) result$head() else result
}

#' @import rscala
scalaUnserialize.clustering <- function(reference, type=scalaType(reference), bridge=scalaFindBridge(reference), verbose=FALSE, names=NULL, withParameters=TRUE, ...) {
  if ( verbose ) cat("scalaUnserialize.clusterings: Trying...\n")
  if ( ! reference$"isInstanceOf[List[org.ddahl.sdols.clustering.Clustering[_]]]"() ) return(NULL)
  if ( verbose ) cat("scalaUnserialize.clusterings: Found match...\n")
  if ( withParameters && ( substr(type,nchar(type)-26,nchar(type)) == "[org.ddahl.rscala.RObject]]" ) ) {
    if ( verbose ) cat("scalaUnserialize.clusterings: ... with parameters.\n")
    r <- s(ref=reference) ^ '
      val list = ref.map { partition =>
        (partition.toLabelsWithParameters, partition.nClusters)
      }
      val labels = list.map(_._1._1).toArray
      val parameters = list.map(_._1._2).flatten
      val sizes = list.map(_._2).toArray
      (labels, parameters, sizes)
    '
    labels <- r$"_1"()
    parameters <- scalaUnserialize.generic(r$"_2"(),bridge=bridge,verbose=verbose)
    sizes <- r$"_3"()
    p <- vector(length(sizes), mode="list")
    j <- 1
    for ( i in seq_along(sizes) ) {
      p[[i]] <- parameters[j:(j+sizes[i]-1)]
      j <- j + sizes[i]
    }
    parameters <- p
  } else {
    if ( verbose ) cat("scalaUnserialize.clusterings: ... without parameters.\n")
    labels <- s(ref=reference) * 'ref.map(_.toLabels).toArray'
    parameters <- NULL
  }
  labels <- labels + 1L
  colnames(labels) <- names
  if ( verbose ) cat("scalaUnserialize.clusterings: Success.\n")
  list(labels=labels, parameters=parameters)
}
