load("iris-partitions.Rbin")

library(rscala)
options(rscala.heap.maximum="48g")
s <- scala(classpath.packages="shallot")

s %@% '
  import org.ddahl.shallot.parameter.NullSamplingModel
  import org.ddahl.shallot.parameter.partition.Partition
  import org.ddahl.shallot.parameter.partition.PairwiseProbability
'

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

x1 <- psm(iris10.5)

library(mcclust)
x2 <- comp.psm(iris10.5)

library(microbenchmark)
microbenchmark(
  psm(iris10.5,TRUE),
  psm(iris10.5,FALSE),
  comp.psm(iris10.5),
  times=1)

