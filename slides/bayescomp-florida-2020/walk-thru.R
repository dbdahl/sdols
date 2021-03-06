library(salso)
probs <- psm(iris.clusterings)
subset <- c(1, 20, 60, 110, 130)
p <- probs[subset, subset]
p
salso(p)

f <- function(e) {
  l <- VI.lb(e,p[1:length(e),1:length(e)])
  cat(sprintf("$$f(\\hat{c}) = f((\\texttt{%s})) = %4.2f$$\n",paste0(e,collapse=","),l))
}

f(c(1,1)) #
f(c(1,2))

f(c(1,1,1))
f(c(1,1,2)) #

f(c(1,1,2,1))
f(c(1,1,2,2)) # probExploration
f(c(1,1,2,3))

f(c(1,1,2,2,1))
f(c(1,1,2,2,2)) #
f(c(1,1,2,2,3))

f(c(1,1,2,2,2)) #
f(c(2,1,2,2,2))
f(c(3,1,2,2,2))

f(c(1,1,2,2,2)) #
f(c(1,2,2,2,2))
f(c(1,3,2,2,2))

f(c(1,1,1,2,2))
f(c(1,1,2,2,2))
f(c(1,1,3,2,2)) #

f(c(1,1,3,1,2))
f(c(1,1,3,2,2)) #
f(c(1,1,3,3,2))
f(c(1,1,3,4,2))

f(c(1,1,3,2,1))
f(c(1,1,3,2,2)) #
f(c(1,1,3,2,3))
f(c(1,1,3,2,4))

                           # Canonicalize

f(c(1,1,2,3,3)) #
f(c(2,1,2,3,3))
f(c(3,1,2,3,3))
f(c(4,1,2,3,3))

f(c(1,1,2,3,3)) #
f(c(1,2,2,3,3))
f(c(1,3,2,3,3))
f(c(1,4,2,3,3))

f(c(1,1,1,3,3))
f(c(1,1,2,3,3)) #
f(c(1,1,3,3,3))

f(c(1,1,2,1,3))
f(c(1,1,2,2,3))
f(c(1,1,2,3,3)) #
f(c(1,1,2,4,3))

f(c(1,1,2,3,1))
f(c(1,1,2,3,2))
f(c(1,1,2,3,3)) #
f(c(1,1,2,3,4))


