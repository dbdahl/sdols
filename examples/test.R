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

ggp$levyIntensity(500.0,1000.0)
ggp$levyIntensity(100.0,500.0)
ggp$levyIntensity(10.0,100.0)
ggp$levyIntensity(10.0,100.0)

setmaxEval <- function(x=ggp,value=100L) s %!% '
  x.maxEval = value
'

setmaxEval(ggp,100000)

