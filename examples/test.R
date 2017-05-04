library(austin)

adjacency <- '
  0 1 0 0
  1 0 1 1
  0 1 0 1
  0 1 1 0
'

a <- mkGraph(adjacency)
a$toAdjacencyMatrix()


