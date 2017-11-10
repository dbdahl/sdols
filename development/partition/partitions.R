load("iris-partitions.Rbin")

options(rscala.heap.maximum="4g")
library(austin)

library(microbenchmark)

microbenchmark(pairwiseProbabilityMatrix(iris10.3), times=50)


microbenchmark(pairwiseProbabilityMatrix(iris10.3), pairwiseProbabilityMatrix(iris10.3), times=500)



library(austin)
system.time(example(pairwiseProbabilityMatrix))
q()

