clusterings <- matrix(scan("from-page/BivariateGaussian_sPPM.txt"), nrow=1000, byrow=TRUE)

library(sdols)
pp <- expectedPairwiseAllocationMatrix(clusterings)

pp <- expectedPairwiseAllocationMatrix(iris.clusterings)

library(mcclust.ext)
est.avg <- minbinder(pp,method="avg")$cl

system.time(est.salso <- salso(pp,nCandidates=100000,budgetInSeconds=0,newMethod=TRUE))
attr(est.salso,"nCandidates")

unlist(latentStructureFit(est.salso,pp))
unlist(latentStructureFit(est.avg,pp))

library(mcclust)
est.avg <- minbinder(pp,method="avg")$cl
latentStructureFit(est.avg,pp)

library(mcclust.ext)
est.greedy <- minbinder.ext(pp,method="greedy")$cl
latentStructureFit(est.greedy,pp)


est.comp <- minbinder(pp,method="comp")$cl
latentStructureFit(est.comp,pp)

est.lg <- minbinder(pp,method="laugreen")$cl
latentStructureFit(est.lg,pp)

library(mcclust.ext)
est.greedy <- minbinder.ext(pp,method="greedy")$cl
latentStructureFit(est.greedy,pp)


