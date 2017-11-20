#####
#####
# Linear Gaussian sampling model with (standard) Indian buffet process and attraction Indian buffet distribution.
options(rscala.heap.maximum="8G")
library(aibd)
mass <- 1.0
data <- scale(as.matrix(USArrests))
nItems <- nrow(data)
nResponses <- ncol(data)

# Define the centering distribution G0
d <- as.matrix(dist(scale(USArrests)))
sim <- similarity(d,mkExponentialDecay(1.0))
as.matrix(sim)

parameterDistribution <- multivariateNormalParameterDistribution(rep(0,nResponses), precision=0.5*diag(nResponses))
dist <- ibp(mass,nItems,parameterDistribution)

# Obtain 100 draws, using default value for hyperparameters parameters.
system.time(result <- sampleLinearGaussianFAModel(data,dist,100000,precisionShape=10,precisionRate=0.1))

library(sdols)
pam <- expectedPairwiseAllocationMatrix(result$featureAllocation)
fa <- leastSquaresFA(result$featureAllocation,pam)
sumOfSquaresFA(fa,pam)


# Average of the shared features matrices
sfm <- result$sharedFeaturesMatrix
hist(sfm)

# Get all the feature allocation samples
Xdraws <- lapply(result2$featureAllocation, function(a) a %*% attr(a,"parameters"))

# Show shrinkage to the mean
avg <- Reduce("+",Xdraws) / length(Xdraws)
cor(as.vector(scale(data)),as.vector(avg))
plot(as.vector(scale(data)),as.vector(avg),ylim=c(-2,2),xlim=c(-2,2))
abline(lm(as.vector(avg) ~ as.vector(scale(data))))
abline(0,1,lty=2)

