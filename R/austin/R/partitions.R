psm <- function(clusterings=matrix(0L), parallel=TRUE) {
  if ( ! is.matrix(clusterings) ) stop("'clusterings' must be a matrix.")
  if ( is.character(clusterings) ) {
    clusterings <- t(apply(clusterings,1,function(p) {
      as.integer(as.factor(p))
    }))
  }
  s %!% '
    val x = clusterings.map(p => Partition(NullSamplingModel,p)).toList
    PairwiseProbability(x,parallel).toArray
  '
}

