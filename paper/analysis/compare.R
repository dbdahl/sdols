clusterings <- matrix(scan("from-page/BivariateGaussian_sPPM.txt"), nrow=1000, byrow=TRUE)

library(sdols)
pp <- expectedPairwiseAllocationMatrix(clusterings)

pp <- expectedPairwiseAllocationMatrix(iris.clusterings)

library(mcclust.ext)
est.avg <- minbinder(pp,method="avg")$cl
est.comp <- minbinder(pp,method="comp")$cl

system.time(est.salso <- salso(pp,nCandidates=1000,budgetInSeconds=0))
system.time(est.salso <- salso(pp))
attr(est.salso,"nScans")
attr(est.salso,"nCandidates")

unlist(latentStructureFit(est.salso,pp))
unlist(latentStructureFit(est.avg,pp))
unlist(latentStructureFit(est.comp,pp))

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


