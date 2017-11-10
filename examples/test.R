library(sdols)

adjacency <- '
  0 1 0 0
  1 0 1 1
  0 1 0 1
  0 1 1 0
'

a <- mkGraph(adjacency)
a$toAdjacencyMatrix()

r <- matrix(0,nrow=100,ncol=2)
for ( i in seq.int(nrow(r))) {
  s <- sampleGGP(5,1,0,rnorm)
  r[i,1] <- max(s$weights)
  s <- sampleGGP2(5,1,0,rnorm)
  r[i,2] <- max(s$weights)
}
t.test(r[,1],r[,2])

a <- sampleGraph(10,1,0.8)
dim(a$toAdjacencyMatrix())
a$writeEdgeList("edges.cvs")



library(rscala)
s <- sdols:::s

ggp <- s$.org.ddahl.sdols.network.GeneralizedGammaProcess$apply(100.0,1.0,0.5)
ggp$intensityIntervalArea(1,2)
ggp$intensityIntervalAreaSlow(1,2,100L)


rg <- s$.org.apache.commons.math3.random.MersenneTwister$new()
ggp$sampleWeights(0.01,5L,rg)


wseq <- seq(0.1,1,length=1000)
y <- Vectorize(function(x) ggp$intensity(x))(wseq)
plot(wseq,y,type="l")

rd <- s$.org.ddahl.sdols.network.RampDistribution$apply(5,10,25,100)
rd$distributionFunction(9.9)
rd$quantileFunction(rd$distributionFunction(5.9))

rg <- s$.org.apache.commons.math3.random.MersenneTwister$new()
rd$sample(rg)


f <- function(t,m,x,kappa,gamma) {
  exp(-kappa*x) - m*exp(-kappa*t*x)/t^(1+gamma)
}

