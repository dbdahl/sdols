load("iris")

library(rscala)
options(rscala.heap.maximum="48g")
s <- scala(classpath.packages="shallot")

s %@% '
  import org.ddahl.shallot.parameter.NullSamplingModel
  import org.ddahl.shallot.parameter.partition.Partition
  import org.ddahl.shallot.parameter.partition.PairwiseProbability
'

library(shallot)
example(shallot)

raw <- sample.partitions(distribution, 50000, parallel=TRUE)
samples <- process.samples(raw,as.matrix=TRUE)

y <- samples$partitions$labels

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

x1 <- psm(samples$partitions$labels)

library(mcclust)
x2 <- comp.psm(samples$partitions$labels)

library(microbenchmark)
microbenchmark(
  psm(samples$partitions$labels,TRUE),
  psm(samples$partitions$labels,FALSE),
  comp.psm(samples$partitions$labels),
  times=5)

