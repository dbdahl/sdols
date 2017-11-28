library(sdols)

ppm <- expectedPairwiseAllocationMatrix(iris.clusterings)

library(rscala)
s <- sdols:::s

b <- s$.PartitionSummary$sequentiallyAllocatedLatentStructureOptimization(1000L,ppm,0L)
s$.PartitionSummary$sumOfSquares(b,ppm)
b

library(mcclust.ext)
system.time(b.wade <- minbinder.ext(ppm,method="greedy"))
bb.wade <- s$.Partition$apply(as.integer(b.wade$cl))
s$.PartitionSummary$sumOfSquares(bb.wade,ppm)

system.time(b.wade <- minbinder.ext(ppm,method="avg"))
bb.wade <- s$.Partition$apply(as.integer(b.wade$cl))
s$.PartitionSummary$sumOfSquares(bb.wade,ppm)


source("/home/dahl/docs/devel/student-carterj4/partitions/minbinder/minbinder.R")
c1 <- minbinder(ppm,method="core")
c1a <- s$.Partition$apply(as.integer(c1$cl))
s$.PartitionSummary$sumOfSquares(c1a,ppm)

