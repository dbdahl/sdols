library(sdols)
library(mcclust.ext)

epam <- expectedPairwiseAllocationMatrix(iris.clusterings)

engine <- function(nCandidates, estimator) {
  pairing <- t(sapply(1:nCandidates, function(i) {
    est <- estimator(i)
    c(VI(est,iris.clusterings), VI.lb(est,epam), length(unique(est)))
  }))
  print(cor(pairing))
  plot(pairing[,1],pairing[,2],col=rainbow(max(pairing[,3]))[pairing[,3]],asp=1)
  pairing
}

p.observed <- engine(nrow(iris.clusterings), function(i) iris.clusterings[i,])
p.salso <- engine(1000, function(i) as.vector(salso(epam,multicore=FALSE,nCandidates=1)))
save(p.observed,p.salso,file="p.RData")

