# Uncomment to provide more memory if computations are really slow or crashes occcur.
# options(rscala.heap.maximum="4g")

library(sdols)

load("iris-partitions.Rbin")
ls()

clusterings <- iris10.3
ppm <- pairwiseProbabilityMatrix(clusterings)
est.ls <- minbinder(ppm,clusterings,method="draws")$cl   # Do least-squares clustering.

conf <- confidence(est.ls,ppm)

plot(conf)
plot(conf,data=iris)

