GGPmcmc=function(G, K,niter=500,nburn=niter/2,thin=1,nadapt=nburn/2,L=5,epsilon=.1, MH_nb=2,rw_std=c(0.02,0.02),w_ini=rgamma(K,1),w_rem_ini=rgamma(1,1),alpha_ini=runif(1,0,100),sigma_ini=.5,tau_ini=runif(1,0,10)){
  #DEBUG: K=p;niter=20000;nburn=niter/2;thin=1;nadapt=nburn/2;L=5;epsilon=.1;MH_nb=2;rw_std=c(0.02,0.02);w=rgamma(K,1);w_rem=rgamma(1,1);alpha_ini=runif(1,0,100);sigma_ini=.5;tau_ini=runif(1,0,10);  
  #INPUTS
  #    - G: logical adjacency matrix  
  #    - K: number of nodes
  #    - niter: number of MCMC iterations
  #    - nburn: number of burn-in iterations
  #    - thin: thinning of the MCMC output
  #    - nadapt: number of iterations for adaptations of leapfrog stepsize
  #    - L: number of leapfrog steps
  #    - epsilon: leapfrog stepsize
  #    - MH_nb: number of MH iterations for hyperparameters
  #    - rw_std: standard deviation of the random walk for sigma and tau
  #    - w_ini: initial values of w
  #    - w_rem_ini: initial value of w_rem
  #    - alpha_ini: initial value of alpha
  #    - sigma_ini: initial value of sigma
  #    - tau_ini: initial value of tau
  #OUTPUTS
  #    - w
  #    - w_rem
  #    - alpha
  #    - logalpha
  #    - sigma
  #    - tau
  #    - rate: acceptance rate of the HMC step at each iteration
  #    - rate2: acceptance rate of the MH for the hyperparameters 

  require(copula)
  require(statmod)
  G=(G!=0)
  
  alpha = alpha_ini
  sigma = sigma_ini
  tau = tau_ini
  logalpha = log(alpha)
  w=w_ini
  w_rem=w_rem_ini
  logw = log(w)
  
  ind = which(G,arr.ind=TRUE)
  
  n = sample(10,length(ind[,1]),replace=TRUE)
  count=G
  count[G]=n#weighted adjacency matrix
  N = t(colSums(count)) + rowSums(count)#degree (m_i)
  
  epsilon = epsilon/K^(1/4); # Leapfrog stepsize
  
  n_samples = (niter-nburn)/thin;
  w_st = matrix(0,n_samples,K)
  w_rem_st = matrix(0,n_samples,1);
  alpha_st = matrix(0,n_samples, 1);
  logalpha_st = matrix(0,n_samples, 1);
  tau_st = matrix(0,n_samples, 1);
  sigma_st = matrix(0,n_samples, 1);
  
  rate = matrix(0,niter, 1)
  rate2 = matrix(0,niter, 1)
  for (i in 1:niter){
    #HMC for w
    tmp = update_w(w, logw, w_rem, N, L, epsilon, sigma, tau);
    w=tmp$w
    logw=tmp$logw
    rate[i]=tmp$rate
    #adapting leapfrog stepsize
    if (i<nadapt){
      epsilon = exp(log(epsilon) + .01*(mean(rate[1:i]) - 0.6));
    }
    
    
    #MH for hyperparameters 
    if ((i%%2)==0){
      rw_alpha = TRUE;
    }else{
      rw_alpha = FALSE;
    }
    tmp=update_hyper(w,logw,w_rem,alpha,logalpha,sigma,tau,MH_nb,rw_std,rw_alpha);
    w_rem=tmp$w_rem
    alpha=tmp$alpha
    logalpha=tmp$logalpha
    sigma=tmp$sigma 
    tau=tmp$tau
    rate2[i]=tmp$rate2 
    
    #Gibbs for count
    tmp=update_n_Gibbs(G,logw, ind[,1], ind[,2]);
    N=tmp$N
    n=tmp$n
    count=tmp$count
    if (i>nburn &&((i-nburn)%%thin)==0){
      iter = (i-nburn)/thin;
      w_st[iter, ] = w;
      w_rem_st[iter] = w_rem;
      logalpha_st[iter] = logalpha;
      alpha_st[iter] = alpha;
      tau_st[iter] = tau;        
      sigma_st[iter] = sigma;  
    }
  }
  return(list(w=w_st,w_rem=w_rem_st,alpha=alpha_st,logalpha=logalpha_st,sigma=sigma_st,tau=tau_st,rate=rate,rate2=rate2))
}


# Gradient
grad_U=function(N, w,w_rem, sigma, tau){
  return(- (N - sigma) + w*(tau+2*sum(w)+2*w_rem))
}

# update w
update_w=function (w, logw, w_rem, N, L, epsilon, sigma, tau){
  sum_w = sum(w);
  sumall_w = sum_w + w_rem;
  
  K = length(w);
  logwprop = logw;
  p = rnorm(K);
  grad1 = grad_U(N, w, w_rem, sigma, tau);
  pprop = p - epsilon * grad1/2;
  for (lp in 1:L){
    logwprop = logwprop + epsilon*pprop;
    if (lp!=L){
      pprop = pprop  - epsilon * grad_U(N, exp(logwprop), w_rem, sigma, tau);
    }
  }
  wprop = exp(logwprop);
  pprop = pprop - epsilon/2 * grad_U(N, wprop, w_rem, sigma, tau)
  sum_wprop = sum(wprop)
  sumall_wprop = sum_wprop + w_rem
  temp1 = - sumall_wprop^2 + sumall_w^2 + sum((N-sigma-1)*(logwprop - logw) ) - tau * (sum_wprop - sum_w)
  logaccept = temp1 -.5*sum(pprop^2-p^2) -sum(logw) + sum(logwprop) + sum(wprop^2) - sum(w^2)
  
  if (is.na(logaccept)){
    logaccept = -Inf
  }
  if (log(runif(1))<logaccept){
    w = wprop
    logw = logwprop
  }
  rate = min(1, exp(logaccept));
  return(list(w=w, logw=logw, rate=rate))
}

#update of the latent counts by Gibbs
update_n_Gibbs=function(G,logw, ind1, ind2){
  lograte_poi = logw[ind1] + logw[ind2]
  d = rtpois(exp(lograte_poi))
  count=G
  count[G]=d
  N = t(colSums(count)) + rowSums(count)
  return(list(N=N,d=d,count=count))
}


#update of the hyperparameters
update_hyper=function(w, logw, w_rem, alpha, logalpha, sigma,tau, nbMH, rw_std, rw_alpha){
  K = length(w);    
  for (nn in 1:nbMH){
    sum_w = sum(w);
    sumall_w = sum_w + w_rem;
    #propose
    sigmaprop = 1-exp(log(1-sigma) + rnorm(1,0,rw_std[1]));
    tauprop = exp(log(tau) + rnorm(1,0,rw_std[2]));
    if (sigmaprop>-1){
      if (!rw_alpha){
        alphaprop = rgamma(1,K,GGPpsi(2*sum_w + 2*w_rem, 1, sigmaprop, tauprop))
      }else{
        alphaprop = alpha*exp(.02*rnorm(1))
      }         
      logalphaprop = log(alphaprop)
      wprop_rem = GGPsumrnd(alphaprop, sigmaprop, tauprop + 2*sum_w + 2*w_rem)
    }else{
      if (!rw_alpha){
        alpha2prop = rgamma(1,K,GGPpsi((2*sum_w + 2*w_rem)/tauprop,1,sigmaprop,1))
        logalphaprop = log(alpha2prop) - sigmaprop*log(tauprop)
      }else{
        logalphaprop = logalpha + .02*rnorm(1)            
      }  
      alphaprop = exp(logalphaprop)
      rate_K = exp(logalphaprop-log(-sigmaprop)+sigmaprop*log(tauprop+2*sum_w+2*w_rem))
      if (rate_K>0.999*.Machine$integer.max){
        num_clust=round(rnorm(1,mean=rate_K,sd=sqrt(rate_K)))#normal approximation for large lambda
      }else{
        num_clust = rpois(1,rate_K)    
      }
      wprop_rem = rgamma(1,-sigmaprop*num_clust,tauprop+2*sum_w+2*w_rem)
    }
    
    #acceptance probability
    sum_wprop = sum(w);
    sumall_wprop = sum_wprop+wprop_rem;
    temp1=-sumall_wprop^2+sumall_w^2+(sigma - sigmaprop)*sum(logw)-(tauprop-tau-2*wprop_rem+2*w_rem)*sum_w;
    temp2=K*(lgamma(1-sigma)-lgamma(1-sigmaprop))
    logaccept = temp1 + temp2;    
    if (!rw_alpha){
      logaccept=logaccept+K*(log(GGPpsi((2*sum_wprop + 2*wprop_rem)/tau,1,sigma,1))+sigma*log(tau)-log(GGPpsi((2*sum_w+2*w_rem)/tauprop,1,sigmaprop,1))-sigmaprop*log(tauprop))
    }else{
      logaccept=logaccept-exp(logalphaprop+sigmaprop*log(tauprop))*GGPpsi((2*sum_w+2*w_rem)/tauprop,1, sigmaprop,1)+exp(logalpha+sigma*log(tau))*GGPpsi((2*sum_wprop+2*wprop_rem)/tau,1,sigma,1)+K*(logalphaprop-logalpha)
    }
    
    if (log(runif(1))<logaccept){
      w_rem = wprop_rem;
      alpha = alphaprop;
      logalpha = logalphaprop;
      sigma = sigmaprop;
      tau = tauprop;
    }
  }
  rate2 = min(1, exp(logaccept));
  return(list(w_rem=w_rem,alpha=alpha,logalpha=logalpha, sigma=sigma, tau=tau, rate2=rate2))
}

rtpois = function(lambda,tol=1e-10){
  ## Simulate from zero-truncated Poisson
  ## Initialize output
  n=length(lambda)
  x = rep(NA,n)
  ## Identify lambda values below tolerance
  low = which(lambda < tol)
  nlow = length(low)
  if(nlow > 0){
    x[low] = 1
    if(nlow < n){
      x[-low] = qpois(runif(n-nlow, dpois(0, lambda[-low]), 1), lambda[-low])
    }
  }else{
    x = qpois(runif(n-nlow, dpois(0, lambda), 1), lambda)
  }
  return(x)
}

GGPpsi=function(t, alpha, sigma, tau){
  #returns the Laplace exponent of a GGP.
  if (sigma==0){ # gamma process
    return(alpha*log(1+t/tau))
  }else{
    return(alpha/sigma*((t+tau)^sigma-tau^sigma))
  }
}


GGPsumrnd=function(alpha, sigma, tau){
  #GGPsumrnd samples from the distribution of the total mass of a GGP.
  if ((-10^-8)>sigma){ 
    # Compound Poisson case
    # S is distributed from a Poisson mixture of gamma variables
    K = rpois(1,-alpha/sigma/tau^(-sigma))
    S = rgamma(1,-sigma*K, tau)
  }else if (sigma < 10^-8){
    # Gamma process case
    # S is gamma distributed
    S = rgamma(1,alpha, tau)
  }else if (sigma==0.5 && tau==0){ 
    # Inverse Gaussian process case
    # S is distributed from an inverse Gaussian distribution
    lambda = 2*alpha^2;
    mu = alpha/sqrt(tau);
    S = rinvgauss(1,mu, lambda)
  }else{
    #General case
    #S is distributed from an exponentially tilted stable distribution
    S = retstable(sigma, alpha/sigma, tau);
  }
  return(S)
}