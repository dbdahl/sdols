# \strong{(Developers Only:)} Convert Between R and Scala Representations of Clusterings
#
# \strong{This function is not intended for end users}, but is exported for the benefit of
# developers whose wish to write other packages that depend on this package.
#
# @param x Either: i. a list containing a elements 'labels' and 'parameters' which are,
#          respectively, a matrix of cluster labels and list whose number of elements
#          is the same as the number of rows of 'labels' and whose elements are are lists
#          whose length is the number of clusters for the corresponding cluster.
#          ii. a Scala reference to a clustering or a list of Scala references to
#          clusterings.
# @param names A character vector giving the item labels when converting from Scala to R.
# @param withParameters A logical indicating whether model parameters should also be converted.
#
# @import rscala
scalaUnserialize.clusterings <- function(reference, type=scalaType(reference), bridge=scalaFindBridge(reference), verbose=FALSE, names=NULL, withParameters=TRUE) {
  if ( verbose ) cat("scalaUnserialize.clusterings: Trying...\n")
  if ( grepl("^List\\[org\\.ddahl\\.shallot\\.parameter\\.partition\\.Partition\\[.*\\]$",type) ) {
    if ( verbose ) cat("scalaUnserialize.clusterings: Found match...\n")
    if ( withParameters && ( type == "List[org.ddahl.shallot.parameter.partition.Partition[org.ddahl.rscala.RObject]]" ) ) {
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
  } else stop("Unsupported type.")
}

#' \strong{(Developers Only:)} Convert Between R and Scala Representations of Clusterings
#'
#' \strong{This function is not intended for end users}, but is exported for the benefit of
#' developers whose wish to write other packages that depend on this package.
#'
#' @param x Either: i. a list containing a elements 'labels' and 'parameters' which are,
#'          respectively, a matrix of cluster labels and list whose number of elements
#'          is the same as the number of rows of 'labels' and whose elements are are lists
#'          whose length is the number of clusters for the corresponding cluster.
#'          ii. a Scala reference to a clustering or a list of Scala references to
#'          clusterings.
#' @param names A character vector giving the item labels when converting from Scala to R.
#' @param withParameters A logical indicating whether model parameters should also be converted.
#'
#' @import rscala
scalaConvert.clustering_OFF <- function(x, names=NULL, withParameters=TRUE) {
  if ( inherits(x,"rscalaReference") ) {
    if ( grepl("^List\\[org\\.ddahl\\.shallot\\.parameter\\.partition\\.Partition\\[.*\\]$",scalaType(x)) ) {
      if ( withParameters && ( scalaType(x) == "List[org.ddahl.shallot.parameter.partition.Partition[org.ddahl.rscala.RObject]]" ) ) {
        r <- s(ref=x) ^ '
          val list = ref.map { partition =>
            (partition.toLabelsWithParameters, partition.nClusters)
          }
          val labels = list.map(_._1._1).toArray
          val parameters = list.map(_._1._2).flatten
          val sizes = list.map(_._2).toArray
          (labels, parameters, sizes)
        '
        labels <- r$"_1"()
        parameters <- -(r$"_2"())
        sizes <- r$"_3"()
        p <- vector(length(sizes), mode="list")
        j <- 1
        for ( i in seq_along(sizes) ) {
          p[[i]] <- parameters[j:(j+sizes[i]-1)]
          j <- j + sizes[i]
        }
        parameters <- p
      } else {
        labels <- s(ref=x) * 'ref.map(_.toLabels).toArray'
        parameters <- NULL
      }
      labels <- labels + 1L
      colnames(labels) <- names
      result <- list(labels=labels, parameters=parameters)
      attr(result,"scalaReference")  <- x
      result
    } else stop("Unsupported type.")
  } else stop("Unsupported type.")
}

