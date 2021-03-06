library(sdols)

Z <- matrix(as.integer(scan()),nrow=5,ncol=2,byrow=TRUE)
    1 0
    1 1
    0 1
    0 1
    1 0

attr(Z,"parameters") <- matrix(c(2,3),nrow=2,byrow=TRUE)

ZZ <- Z
e <- scalaConvert.featureAllocation(ZZ)
f <- scalaConvert.featureAllocation(e)
identical(ZZ,f)





f1 <- feature(NULL,c(2,3,4,1))

f2 <- feature(c(2,1,4),integer())
f2 <- feature(II(list()),integer())

fa1 <- featureAllocation(10,f1,f2,f1,f2,f1,f1,f1)
fa1


scalaConvert.featureAllocation(
  featureAllocation(10,feature(NULL,c(2,3)))
)

scalaConvert.featureAllocation(
  featureAllocation(10,feature(1L,3)),type="Double"
)


scalaConvert.featureAllocation(
  featureAllocation(10,
    feature(NULL,c(1,2))
  )
)


fa <- scalaConvert.featureAllocation(
  featureAllocation(10,
    feature(19L,c(2,3))
    ,
    feature(3L,c(1,2))
  )
)

scalaConvert.featureAllocation(fa)


scalaConvert.featureAllocation(featureAllocation(10,feature(34,c(2,3))))


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

