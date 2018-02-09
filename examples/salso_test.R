library(mvnfast)
library(MCMCpack)
library(reshape2)
library(ggplot2)
library(sdols)
library(label.switching)

log_vec_renorm = function(vec)
{
 
    n = length(vec);
    result =numeric(n)     
    ## take care of infinite values by ignoring them and scale on the mean
    ## scaling help numerical stability.
    indx = is.infinite(vec);
    vec2 = rep(-Inf,n);
    vec2[!indx] = vec[!indx] - mean(vec[!indx])
        for(i in 1:n)
        {
        result[i] = exp( -log(1+ sum(exp( vec2[-i] - vec2[i])))) 
        }
    return(result)
}


##########################################
##########################################
##########################################
##########################################
set.seed(1234)



## 3 dimension : 2) mixture model clustering 3) indpendent noise 



mu12 = list()
mu12[[1]] = c(-2,-2)
mu12[[2]] = c(2,2)

dati         = list()
dati$N       = 200
dati$p       = 2
dati$data    = matrix(0,dati$N,dati$p)
dati$mix_mem = integer(dati$N)



for(i in 1:dati$N){
  dati$mix_mem[i]      = sample(1:2,size =1)
  dati$data[i,1:2]     = rmvn(1,mu = mu12[[dati$mix_mem[i]]],sigma = diag(1,2))
  dati$data[i,-c(1:2)] = rnorm(dati$p -2)
}
    
##########################################
##########################################
## K-MEAN CLUSTERING
##########################################
##########################################

mod_kmean = kmeans(dati$data , 2, nstart = 20)


ggplot(data.frame(dati$data, class = factor(mod_kmean$cluster),lt = factor(dati$mix_mem )))+
geom_point(aes(X1,X2,col = class,shape = lt),size =4)


##########################################
##########################################
## gaussian mixture model --- cond-ind-kern
##########################################
##########################################

## prior
prior           = list()
prior$H         = 5 ## upperbound from the number of clusters
## normal inverse wishart
prior$mu        = 0 
prior$sigma     = 1
prior$alpha     = 2
prior$beta      = 2

## parameters

param           = list()
param$mu        = matrix(0,dati$p,prior$H)
param$sigma     = array(1, dim = c(dati$p,prior$H))
param$zi        = numeric(dati$N)
param$lambda    = rep(1/prior$H,prior$H) 





##########################################
##########################################
## initialize parameters
##########################################
##########################################
param$zi    =  sample(1:prior$H,size = dati$N,replace = TRUE)


nrep = 10000

results = list()
results$mu        = array(0,dim  = c(nrep,dati$p,prior$H))
results$Sigma     = array(0, dim = c(nrep,dati$p,prior$H))
results$zi        = array(0,dim  = c(nrep,dati$N))
results$lambda    = array(0,dim  = c(nrep,prior$H))


for(r in 1:nrep){

##########################################
##########################################
## update Kernel parameters
##########################################
##########################################


for(h in 1:prior$H){
    sel_h  = param$zi==h
    nh     = sum(sel_h ==h)
    
    if(nh >0){
      ##########################################
      ##########################################
      ## NORMAL
      ##########################################
      ##########################################

     for(j in dati$p){
     # mean 
     tmp_var  = 1/(1/prior$sigma + nh/param$sigma[j,h])
     tmp_mean = tmp_var * (prior$mu/prior$sigma +
                           sum(dati$data[sel_h,j]))
     param$mu[j,h] = rnorm(1,mean = tmp_mean,sd = sqrt(tmp_var))  

     ## variance
     param$sigma[j,h] =
     1/rgamma(1,shape = prior$alpha + nh/2,rate = prior$beta +  sum((dati$data[sel_h,j] - param$mu[j,h])^2)/2)
     }

    }
      ##########################################
      ##########################################
      ## generate from the prior for empty classes
      ##########################################
      ##########################################
      else{ 
      for(j in 1:dati$p){
       param$mu[j,h]       = rnorm(1,mean = prior$mu, sd = sqrt(prior$sigma))
       param$sigma[j,h]  = 1/rgamma(1,shape = prior$alpha, rate = prior$beta)
      }

   }
}

##########################################
##########################################
## mixture  data augmentation
##########################################
##########################################


for(i in 1:dati$N){
    
    tmp_prob = rep(0,prior$H)
    for(h in 1:prior$H){
    ##########################################
    ##########################################
    ## COMPUTE LOG-LIKELIHOOD 
    ##########################################
    ##########################################
        log_lik_h = 0
        ## continuous
        log_lik_h  = log_lik_h  +dmvn(dati$data[i,], mu = param$mu[,h], sigma = diag(param$sigma[,h]) , log = T)


        tmp_prob[h] = log_lik_h + param$lambda[h]

    }
    param$zi[i] = sample(1:prior$H,size = 1,prob = log_vec_renorm(tmp_prob))
    }



##########################################
##########################################
## Update mixture weights
##########################################
##########################################
param$lambda = c(rdirichlet(1, 1/prior$H + table(factor(param$zi,1:prior$H))))


##########################################
##########################################
## save results across iterations
##########################################
##########################################
results$mu[r,,]         = param$mu
results$Sigma[r,,]      = param$sigma 
results$zi[r,]          = param$zi
results$lambda[r,]      = param$lambda

if(r%%1000 ==0){
    cat('Iteration: ',r, '/',nrep,'\n')
    cat(paste(1:prior$H,'\t'),'\n')
    cat(paste(table(factor(param$zi,1:prior$H)),'\t'),'\n')
} 
}


##########################################
##########################################
## Define a burnin
##########################################
##########################################
burnin = 1:floor(nrep/2)


##########################################
##########################################
## using  salso
##########################################
##########################################

results$probabilities = expectedPairwiseAllocationMatrix(results$zi[-burnin,])
results$cluster       = salso(results$probabilities)

ggplot(data.frame(dati$data, class = factor(results$cluster),lt = factor(dati$mix_mem )))+
geom_point(aes(X1,X2,col = class,shape = lt),size =4)


##########################################
##########################################
## MAP assignation after label.switching correction
##########################################
##########################################

mod_ls  = label.switching(method = 'DATA-BASED',  z = results$zi[-burnin,],K = prior$H,data = dati$data)

ggplot(data.frame(dati$data[,1:2], class = factor(mod_ls$cluster),lt = factor(dati$mix_mem))) + 
geom_point(aes(X1,X2,col = class,shape = lt),size =4)

mod_ls2   = label.switching(method = 'ECR-ITERATIVE-1',  z = results$zi[-burnin,],K = prior$H,data = dati$data)


ggplot(data.frame(dati$data[,1:2], class = factor(mod_ls2$cluster),lt = factor(dati$mix_mem))) + 
geom_point(aes(X1,X2,col = class,shape = lt),size =4)


