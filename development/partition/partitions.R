load("iris-partitions.Rbin")

options(rscala.heap.maximum="4g")
library(austin)

library(microbenchmark)

microbenchmark(pairwiseProbabilityMatrixn(iris10.3), pairwiseProbabilityMatrixnn(iris10.3), times=50)


microbenchmark(pairwiseProbabilityMatrixnn(iris10.3), pairwiseProbabilityMatrixn(iris10.3), times=500)



library(austin)
system.time(example(pairwiseProbabilityMatrix))
q()

