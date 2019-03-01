rscala::scalaMemory("1G")
library(sdols)
EPAM=readRDS("EPAM_test.rds")
for(i in 1:2000){
  print(i)
  SALSO = salso(EPAM,budgetInSeconds=7500,multicore=FALSE,nCandidates=100,maxSize=4)
  rm(SALSO)
}

