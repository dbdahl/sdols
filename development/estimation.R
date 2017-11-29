options(rscala.heap.maximum="12G")
library(sdols)

ppm <- expectedPairwiseAllocationMatrix(iris.clusterings)
a <- salso(ppm,loss="squaredError")
a <- salso(ppm,loss="absoluteError",maxSize=3)
a <- salso(ppm,loss="binder",maxSize=4)
a <- salso(ppm,loss="lowerBoundVariationOfInformation")
a
table(a)

conf <- confidence(rep(1:3,each=50),ppm)
plot(conf)
plot(conf,data=iris)


latentStructureFit(a,ppm)



library(mcclust.ext)

binder(matrix(a,nrow=1),ppm)
latentStructureFit(a,ppm)$binder

VI.lb(matrix(a,nrow=1),ppm)
latentStructureFit(a,ppm)$lowerBoundVariationOfInformation



pcm <- expectedPairwiseAllocationMatrix(USArrests.featureAllocations)
a <- salso(pcm,structure="featureAllocation",loss="squaredError",nCandidates=1000)
b <- salso(pcm,structure="featureAllocation",loss="absoluteError",nCandidates=3000)
all(a==b)

latentStructureFit(a,pcm)



library(rscala)
s <- sdols:::s

a <- s$.PartitionSummary$minBinderAmongDraws(iris.clusterings)
sum((a$pairwiseAllocationMatrix() - ppm)^2)
s$.PartitionSummary$binderSumOfSquares(a,ppm)
s$.PartitionSummary$binderSumOfSquaresSlow(a,ppm)
sum(abs(a$pairwiseAllocationMatrix() - ppm))
s$.PartitionSummary$binderSumOfAbsolutes(a,ppm)
s$.PartitionSummary$binderSumOfAbsolutesSlow(a,ppm)
s$.PartitionSummary$lowerBoundVariationOfInformation(a,ppm)
a



b <- s$.PartitionSummary$sequentiallyAllocatedLatentStructureOptimization(1000L,ppm,0L,"binder")
s$.PartitionSummary$binderSumOfSquares(b,ppm)
s$.PartitionSummary$binderSumOfAbsolutes(b,ppm)
s$.PartitionSummary$lowerBoundVariationOfInformation(b,ppm)
b

library(mcclust.ext)
system.time(b.wadeBinder <- minbinder.ext(ppm,method="greedy",start.cl.greedy=a))
all(b.wadeBinder$cl==a)



bb.wadeBinder <- s$.Partition$apply(as.integer(b.wadeBinder$cl))
s$.PartitionSummary$sumOfSquares(bb.wadeBinder,ppm)
s$.PartitionSummary$sumOfAbsolutes(bb.wadeBinder,ppm)


system.time(b <- s$.PartitionSummary$sequentiallyAllocatedLatentStructureOptimization(1000L,ppm,0L,"vi"))
s$.PartitionSummary$lowerBoundVariationOfInformation(b,ppm)
b$toLabels()

system.time(b.wadeVI <- minVI(ppm,cls.draw=iris.clusterings,method="all",include.greedy=TRUE))
b.wadeVI$value
identical(b$toLabels()+1L,b.wadeVI$cl["greedy",])

library(mcclust)
system.time(b.all <- minbinder(ppm,cls.draw=iris.clusterings,method="draws"))


library(mcclust)
data(cls.draw1.5)
ppm <- expectedPairwiseAllocationMatrix(cls.draw1.5)
b <- s$.PartitionSummary$sequentiallyAllocatedLatentStructureOptimization(10L,ppm,0L,"vi")
s$.PartitionSummary$lowerBoundVariationOfInformation(b,ppm)
b$toLabels()

system.time(b.wadeVI <- minVI(ppm,cls.draw=cls.draw1.5,method="all",include.greedy=TRUE))
b.wadeVI$value
identical(b$toLabels()+1L,b.wadeVI$cl["greedy",])



bb.wade <- s$.Partition$apply(as.integer(b.wade$cl))
s$.PartitionSummary$sumOfSquares(bb.wade,ppm)

source("/home/dahl/docs/devel/student-carterj4/partitions/minbinder/minbinder.R")
c1 <- minbinder(ppm,method="core")
c1a <- s$.Partition$apply(as.integer(c1$cl))
s$.PartitionSummary$sumOfSquares(c1a,ppm)




library(sdols)

pam <- expectedPairwiseAllocationMatrix(USArrests.featureAllocations)

library(rscala)
s <- sdols:::s

fas <- scalaConvert.featureAllocation(USArrests.featureAllocations)

b <- s$.FeatureAllocationSummary$sequentiallyAllocatedLatentStructureOptimization(100L,pam,0L,"squaredError")
c <- s$.FeatureAllocationSummary$minBinderAmongDraws(fas)

s$.FeatureAllocationSummary$binderSumOfSquares(c,pam)
s$.FeatureAllocationSummary$binderSumOfSquares(b,pam)


sum((b$pairwiseAllocationMatrix() - pam)^2)



