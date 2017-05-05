rm(list=ls())
source("GGP_R.R")
set.seed(1)

#example 1
#sparse graph
p=20
G=matrix(rbinom(p^2,1,1/p),p,p)
diag(G)=0
sum(G)
re=GGPmcmc(G,p,20000)
plot(re$sigma,type="l")


#example 2
#dense graph
set.seed(1)
p=20
G=matrix(rbinom(p^2,1,2/3),p,p)
diag(G)=0
sum(G)
re=GGPmcmc(G,p,20000)
plot(re$sigma,type="l")
