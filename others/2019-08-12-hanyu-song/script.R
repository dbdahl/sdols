load("clusterings.RData")

library(salso)
psm <- psm(clusterings)
fm <- salso(psm, loss="VI.lb")
table(fm$estimate)

