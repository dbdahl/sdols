options(rscala.heap.maximum="6g")
library(shallot)

data <- iris[,-ncol(iris)]
truth <- as.integer(iris[,ncol(iris)])
distance <- as.dist(as.matrix(dist(scale(data))+0.001))
decay <- decay.exponential(temperature(9.0, fixed=TRUE), distance)
permutation <- permutation(n.items=nrow(data), fixed=FALSE)
attraction <- attraction(permutation, decay)
mass <- mass(1.0, fixed=TRUE)
discount <- discount(0.2, fixed=TRUE)
distribution <- ewens.pitman.attraction(mass, discount, attraction)

for ( i in c(3,4,5) ) {
  raw <- sample.partitions(distribution, 10^i, parallel=TRUE)
  samples <- process.samples(raw,as.matrix=TRUE)
  assign(paste0("iris10.",i),samples$partition$labels)
}

save(iris10.3, iris10.4, iris10.5, file="iris-partitions.Rbin")

