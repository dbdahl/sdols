library(sdols)

ppm <- expectedPairwiseAllocationMatrix(iris.clusterings)

library(rscala)
s <- sdols:::s

a <- s$.PartitionSummary$minBinderAmongDraws(iris.clusterings)
sum((a$pairwiseAllocationMatrix() - ppm)^2)/2
s$.PartitionSummary$binderSumOfSquares(a,ppm)
s$.PartitionSummary$binderSumOfSquaresSlow(a,ppm)
s$.PartitionSummary$binderSumOfAbsolutes(a,ppm)
s$.PartitionSummary$binderSumOfAbsolutesSlow(a,ppm)
s$.PartitionSummary$lowerBoundVariationOfInformation(a,ppm)
a



b <- s$.PartitionSummary$sequentiallyAllocatedLatentStructureOptimization(1000L,ppm,0L)
s$.PartitionSummary$sumOfSquares(b,ppm)
s$.PartitionSummary$sumOfAbsolutes(b,ppm)
b

library(mcclust.ext)
system.time(b.wade <- minbinder.ext(ppm,method="greedy"))
bb.wade <- s$.Partition$apply(as.integer(b.wade$cl))
s$.PartitionSummary$sumOfSquares(bb.wade,ppm)
s$.PartitionSummary$sumOfAbsolutes(bb.wade,ppm)

minVI(ppm,method="draws",cls.draw=iris.clusterings)
minVI(ppm,method="avg")
minVI(ppm,method="comp")


library(mcclust)
system.time(b.all <- minbinder(ppm,cls.draw=iris.clusterings,method="all",include.lg=FALSE))


bb.wade <- s$.Partition$apply(as.integer(b.wade$cl))
s$.PartitionSummary$sumOfSquares(bb.wade,ppm)

source("/home/dahl/docs/devel/student-carterj4/partitions/minbinder/minbinder.R")
c1 <- minbinder(ppm,method="core")
c1a <- s$.Partition$apply(as.integer(c1$cl))
s$.PartitionSummary$sumOfSquares(c1a,ppm)

