clusterings <- matrix(scan("from-page/BivariateGaussian_sPPM.txt"), nrow=1000, byrow=TRUE)

library(sdols)
pp <- expectedPairwiseAllocationMatrix(clusterings)

system.time(est.salso <- salso(pp,nCandidates=100,budgetInSeconds=1))
attr(est.salso,"nCandidates")
latentStructureFit(est.salso,pp)

library(mcclust)
est.avg <- minbinder(pp,method="avg")$cl
latentStructureFit(est.avg,pp)

est.comp <- minbinder(pp,method="comp")$cl
latentStructureFit(est.comp,pp)

est.lg <- minbinder(pp,method="laugreen")$cl
latentStructureFit(est.lg,pp)

library(mcclust.ext)
est.greedy <- minbinder.ext(pp,method="greedy")$cl
latentStructureFit(est.greedy,pp)


