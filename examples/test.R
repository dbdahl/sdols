library(austin)

adjacency <- '
  0 1 0 0
  1 0 1 1
  0 1 0 1
  0 1 1 0
'

a <- mkGraph(adjacency)
a$toAdjacencyMatrix()


library(rscala)
s <- austin:::s

ggp <- s$.org.ddahl.austin.GeneralizedGammaProcess$apply(1.0,1.0,0.0)

ggp$intensityIntervalArea(10.0,10000.0)
ggp$intensityTailArea(0.5)

ggp$intensityTailArea(0.000000001)
ggp$intensityTailArea(0.000000001^(1/2))
ggp$intensityTailArea(0.000000001^(1/4))
ggp$intensityTailArea(0.000000001^(1/8))
ggp$intensityTailArea(0.000000001^(1/16))
ggp$intensityTailArea(0.000000001^(1/32))

ggp$intensityTailArea(0.0000001)
ggp$intensityTailArea(0.0000001^(1/2))
ggp$intensity(0.0000001)
ggp$intensity(0.0000001^(1/2))
ggp$intensity(0.0000001^(1/3))





ggp$steps(20,0.0001, 0.01)

f <- function(t,m,x,kappa,gamma) {
  exp(-kappa*x) - m*exp(-kappa*t*x)/t^(1+gamma)
}

m <- 10
x <- 0.00001
while ( x < 10 ) {
  cat("---\n")
  cat("# ",x,"\n")
  print(ggp$intensity(x))
  u <- uniroot(function(t) f(t,m,x,2,0.5),c(1,20))
  print(u$root)
  print(ggp$intensityIntervalArea(x,x*u$root))
  x <- x*u$root
  print(m*ggp$intensity(x))
}




ggp$levyIntensityIntervalSlow(10.0,10000.0,100000000L)
ggp$levyIntensityTail(10.0)

rpois(1,ggp$levyIntensity(500.0,1000.0))

dpois(0,ggp$levyIntensity(500.0,1000.0))
dpois(0,ggp$levyIntensity(100.0,500.0))
dpois(0,ggp$levyIntensity(10.0,100.0))
dpois(0,ggp$levyIntensity(3.0,10.0))

setmaxEval <- function(x=ggp,value=100L) s %!% '
  x.maxEval = value
'

setmaxEval(ggp,100000)


kappa <- 1
gamma <- 0
p <- function(x) exp(-kappa*x) * x^(-1-gamma)
curve(p,0,2)

p(0.01)
p(0.02)
p(0.1)
p(0.2)

