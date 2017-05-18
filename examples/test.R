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
ggp <- s$.org.ddahl.austin.GeneralizedGammaProcess$apply(I(1.0),I(1.0),I(0.0))

ggp$levyIntensity(I(3.0))

