options(rscala.heap.maximum="4g")
library(austin)

load("iris-partitions.Rbin")

clusterings <- iris10.3
est <- iris10.3[1,]

ppm <- pairwiseProbabilityMatrix(clusterings)
conf <- confidence(est,ppm)

plot(conf)
plot(conf,data=iris)

