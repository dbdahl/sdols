load("clusterings.RData")

library(sdols)

rscala::scalaMemory(sdols:::s)

system.time(psm <- expectedPairwiseAllocationMatrix(clusterings))
system.time(psm <- rpm::psm(clusterings))

rscala::scalaMemory(sdols:::s)

system.time(salso(psm, loss = "lowerBoundVariationOfInformation", nCandidates = 20, budgetInSeconds = 10))

rscala::scalaMemory(sdols:::s)

