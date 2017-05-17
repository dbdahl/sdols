mkGraph <- function(adjacency, as.directed=FALSE) {
  if ( is.character(adjacency) && ( length(adjacency) == 1 ) ) {
    x <- scan(textConnection(adjacency),quiet=TRUE)
    n <- sqrt(length(x))
    if ( n %% 1 != 0 ) stop("The number of elements is not a perfect square.")
    adjacency <- matrix(x,nrow=n,ncol=n)
  }
  if ( ( ! is.matrix(adjacency) ) || ( nrow(adjacency) != ncol(adjacency) ) ) stop("'adjacency' should be a square matrix.")
  storage.mode(adjacency) <- "logical"
  if ( as.directed ) s$.DirectedGraph$apply(adjacency)
  s$.UndirectedGraph$apply(adjacency)
}

probLink <- function(w1,w2) 1 - exp(-2*w1*w2)
probLink(0.01,0.01)

stochasticRound <- function(x) {
  r <- x %% 1
  floor(x) + ( runif(length(x)) < r )
}

sample.NGG <- function(alpha, kappa, gamma, sample.P0, nBins=1000000, lower=0.01, upper=10, normalized=TRUE) {
  if ( alpha <= 0 ) stop("alpha must be greater than 0.")
  if ( kappa < 0 ) stop("kappa must be greater than or equal to 0.")
  if ( ( gamma < 0 ) || ( gamma >= 1 ) ) stop("gamma must be greater than or equal to 0 and less than 1.")
  intensity <- function(w) exp(-kappa*w) / ( w^(1+gamma) * gamma(1-gamma) )
  wseq <- seq(lower,upper,length=nBins)
  pseq <- intensity(wseq)
  ndraws <- stochasticRound(alpha*integrate(intensity,lower,upper)$value)
  ws <- sample(wseq,ndraws,replace=TRUE,prob=pseq)
  as <- sample.P0(ndraws)
  ws <- ws/sum(ws)
  list(weights=ws, atoms=as)
}

w <- sample.NGG(5,1,0,rnorm)
w

thresholds <- matrix(runif(length(w)^2),nrow=length(w))
probs <-      kronecker(w,t(w),probLink)
thresholds < probs


length(w)
mean(w)




poissonIntensity.dirichletProcess <- function(w, alpha) {
  # kappa <- 1
  # gamma <- 0
  p <- function(w) {
    if ( alpha <= 0 ) stop("alpha must be greater than 0.")
    exp(-w) * w^(-1)
  }
  p(w)*alpha
}

A <- integrate(function(w) poissonIntensity.generalizedGammaProcess(w,1,1,0),0.001,100)

# w.seq <- seq(0.001,100,length=100)
# prob <- poissonIntensity.generalizedGammaProcess(w.seq,1,1,0)
# sample(w.seq,ifelse(runif(1) < ( A$value %% 1),floor(A$value)+1,floor(A$value)),prob=prob,replace=TRUE)

# plot(w.seq,y,type="l")
# #abline(h=0,v=0)



# sampleDPWeight <- function(alpha,truncation=100) {
#   b <- rbeta(truncation,1,alpha)
#   w <- c(1,cumprod(1-b[-length(b)]))*b
# }

# w <- sampleDPWeight(1)
# theta <- rexp(length(w))
# edge <- list()
# for ( i in seq_along(w) ) {
#   for ( j in seq_along(w) ) {
#     if ( ( i != j ) && ( runif(1) < probLink(w[i],w[j]) ) ) edge <- c(edge,list(c(i,j)))
#   }
# }



