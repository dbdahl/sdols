load("clusterings.RData")

# library(sdols)
# rscala::scalaMemory(sdols:::s)
# system.time(psm <- expectedPairwiseAllocationMatrix(clusterings))
# rscala::scalaMemory(sdols:::s)
# system.time(e1 <- salso(psm, loss = "lowerBoundVariationOfInformation", nCandidates = 20, budgetInSeconds = 10))
# rscala::scalaMemory(sdols:::s)

library(rpm)
system.time(psm <- psm(clusterings))
system.time(e2 <- salso(psm, loss="experimental", nCandidates=1))
e2$lossValue


