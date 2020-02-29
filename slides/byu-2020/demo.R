library("DPpackage")

data(galaxy)
speeds <- galaxy$speed/1000
stem(speeds,scale=1)
stem(speeds,scale=2)

mcmc <- list(nburn=0, nsave=1, nskip=10, ndisplay=0)
prior <- list(alpha=1, m1=mean(speeds), psiinv1=20, nu1=5, k0=1/10)

# Fit the models
draws <- matrix(0L, nrow=1000, ncol=length(speeds))
sink("/dev/null")
state <- NULL
for ( i in seq_len(nrow(draws)) ) {
  state <- DPdensity(y=speeds, prior=prior, mcmc=mcmc, state=state, status=TRUE)$state
  draws[i,] <- state$ss
}
sink()

dim(draws)
head(draws)


library(sdols)
epam <- expectedPairwiseAllocationMatrix(draws)
head(epam)
salso(epam, loss="lowerBoundVariationOfInformation")
salso(epam, loss="binder")

