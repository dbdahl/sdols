library(sdols)

f1 <- feature(NULL,c(2,3,4,1))
f2 <- feature(3,c(2,3,4,1))
f3 <- feature(I(3),c(2,3,4,1))
f4 <- feature(II(3),c(2,3,4,1))

nf1 <- scalaConvert.feature(f1)
nf2 <- scalaConvert.feature(f2)
nf3 <- scalaConvert.feature(f3)
nf4 <- scalaConvert.feature(f3)

scalaConvert.feature(nf1)
scalaConvert.feature(nf2)
scalaConvert.feature(nf3)
scalaConvert.feature(nf4)

identical(f1,scalaConvert.feature(scalaConvert.feature(f1)))
identical(f2,scalaConvert.feature(scalaConvert.feature(f2)))
identical(f3,scalaConvert.feature(scalaConvert.feature(f3)))
identical(f4,scalaConvert.feature(scalaConvert.feature(f4)))


scalaConvert.feature(feature(NULL,c(2,3,4,1)))
scalaConvert.feature(feature(3,c(2,3,4,1)))
scalaConvert.feature(feature(I(3),c(2,3,4,1)))
scalaConvert.feature(feature(II(3),c(2,3,4,1)))

