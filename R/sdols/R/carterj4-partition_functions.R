# package needed for class equivalence
# library(partitions)


#######################################
# Convert Paritions into Set Notation #
#######################################
set.not <- function(A){
	A <- lapply(A, function(k) paste0(k,collapse=","))
    A <- paste0("{",A,"}",collapse=",")
    paste0("{",A,"}")
}

#########################################
# Function to get a vector of cluster   #
# labels for a partition structured as  # 
# a list of vectors                     #
#########################################
list.to.labels <- function(partition) {
	labels <- lapply(1:length(partition), function(i) rep(i,each=length(partition[[i]])))
	labels <- Reduce(labels,f=c)
	order <- Reduce(partition,f=c)
	out <- numeric(length(labels))
	out[order] <- labels
	out
} 

##################################
# Converts a vector of labels to #
# a list of vectors              #
##################################
labels.to.list <- function(x) {
	n <- length(x)
	u <- unique(x)
	items <- 1:n
	lapply(u, function(i) items[x==i])
}

##########################################
# Order w/n subset - -> +, order subsets #
# w/n partition by first element - -> +  #
##########################################
part.ord <- function(A,class=c("list","equivalence")[1]){
    A <- lapply(A,sort)
    o <- order(sapply(A,"[[",1))
    A <- lapply(o,function(i) A[[i]])
    class(A) <- class
    A
}


##################################################
# Chinese Restaurant Proccess Sampling function  # 
##################################################
rCRP <- function(n,size,mass,discount=0){
  draw <- function(){
    A <- list(1)
    for (i in 2:size){
      prob <- sapply(1:length(A), function(x) (length(A[[x]])-discount)/(mass+i-1))
      prob <- c(prob, (mass+discount*length(A))/(mass+i-1))
      j <- sample(1:(length(A)+1),1,prob=prob)
      if (j > length(A)) A <- c(A,i)
      else A[[j]] <- c(A[[j]],i)
    }
    A <- lapply(A,sort)
    o <- order(sapply(A,"[[",1))
    A <- lapply(o,function(i) A[[i]])
    class(A) <- c("list","equivalence")[1]
    A
    # B <- A
    # A <- lapply(A, function(k) paste0(k,collapse=","))
    # A <- paste0("{",A,"}",collapse=",")
    # list(paste0("{",A,"}"),B)
  }
  C <- lapply(1:n, function(l) draw())
  C
  # list(sapply(C,"[[",1),lapply(C,"[[",2))
}

##########################
# EPA Sampling Function  #
##########################
rEPA <- function(n,sim,mass,discount=0,sigma){   
  size <- nrow(sim)
  draw <- function(sigma){
  	if (missing(sigma)) sigma <- sample(1:size)
    A <- list(sigma[1])
    for (i in 2:size){
      prob <- sapply(1:length(A), function(x){ 
      	((i-1-discount*length(A))/
      	(mass+i-1)) * 
      	(sum(sim[sigma[i],A[[x]]])/ 
      	sum(sim[sigma[i],sigma[1:(i-1)]]))
      	} )
      	#<
      # if(class(prob)=="list"){
      	# prob <- sapply(prob,function(x) as(x,"numeric"))
      # } 
      #>
      newsubset <- (mass + discount*length(A))/(mass+i-1)
      prob <- c(prob, newsubset)
      #<
      if(any(is.na(prob))) browser()
      #>
      j <- sample(1:(length(A)+1),1,prob=prob)
      if (j > length(A)) A <- c(A,sigma[i])
      else A[[j]] <- c(A[[j]],sigma[i])
    }
    A <- lapply(A,sort)
    o <- order(sapply(A,"[[",1))
    A <- lapply(o,function(i) A[[i]])
    # class(A) <- c("list","equivalence")
    A
    # B <- A
    # A <- lapply(A, function(k) paste0(k,collapse=","))
    # A <- paste0("{",A,"}",collapse=",")
    # list(paste0("{",A,"}"),B)
  }
  C <- lapply(1:n, function(l) draw())
  C
  # list(sapply(C,"[[",1),lapply(C,"[[",2))
}

##########################
# pdf EPA distribution   #
##########################
dEPA <- function(partition,sim,mass,discount,sigma){
	if (missing(sigma)) {
		sigma1 <- perms(nrow(sim))
		P <- sapply(1:ncol(sigma1), function(i) dEPA(partition, sim, mass, discount, sigma1[,i]))
		mean(P)
	} else {
	    p <- numeric()
	    A <- as.list(rep(NA,length(partition)))
	    for(i in 1:length(sigma)){
		    s <- sapply(partition, function(x) match(sigma[i],x, nomatch=0) > 0 )
	    	n <- (1:length(partition))[s]
		    if(anyNA(A[[n]])){
			    p <- c(p, (mass + discount*length(A[!is.na(A)]))/(mass+i-1) )
		    	A[[n]] <- sigma[i]
	    	} else {
		    	p <- c(p,((i-1-discount*length(A[!is.na(A)]))/
      	               (mass+i-1)) * 
      	               (sum(sim[sigma[i],A[[n]]])/ 
      	               sum(sim[sigma[i],sigma[1:(i-1)]])) )
      	        A[[n]] <- c(A[[n]], sigma[i])     
		    }
    	}
    	prod(p)
    }
}

###########################
# Expected No. of Subsets #
###########################
Eqn.EPA <- function(n,alpha,discount){
	w <- c(1,numeric(n-1))
	if (n >1){
		for (i in 2:n){
		    w[i] <- (alpha + discount*sum(w[1:(i-1)]))/(alpha+i-1)
        }
	}
	sum(w)
}

###########################
# ddCRP Sampling Function #
###########################
rddCRP <- function(n, sim, mass){
	diag(sim) <- mass
	nc <- apply(sim,1,sum)
	prob <- sim/nc
	draw <- function(){
		sitwith <- apply(prob,1, function(p) sample.int(nrow(sim),size=1,prob=p,replace=TRUE))
		names(sitwith) <- NULL
		sitwith
		A <- list()
		for(i in 1:length(sitwith)){
			part <- sapply(A,function(x) sitwith[i] %in% x)
			exist <- sapply(A,function(x) i %in% x)
			if(!(any(part) & any(exist))){
			    if (any(part)) A[[(1:length(A))[part]]] <- c(A[[(1:length(A))[part]]],i)
		        if (any(exist)) A[[(1:length(A))[exist]]] <- c(A[[(1:length(A))[exist]]],sitwith[i])
			    if (!any(exist) & !any(part)){
				    A <- c(A,i)
				    if(sitwith[i]!=i) A[[length(A)]] <- c(A[[length(A)]],sitwith[i])
			    }
			}
		}
		A <- lapply(A,sort)
		o <- order(sapply(A,"[[",1))
		A <- lapply(o,function(i) A[[i]])
		class(A) <- c("list","equivalence")[1]
		A
    }
    lapply(1:n, function(l) draw())
}

############################################
# pdf ddCRP (finds prob of all partitions) #
############################################
dddCRP.all <- function(sim,mass){
	elements <- 1:nrow(sim)
	diag(sim) <- mass
	nc <- apply(sim,1,sum)
	prob <- sim/nc
	lst <-  lapply(1:length(elements), function(i) elements)   
	co <- expand.grid(lst)
	p <- apply(co,1,function(y) sapply(1:length(y),function(i) prob[i,y[i]]))
	p <- apply(p,2,prod)
	f1 <- function(sitwith){
		A <- list()
		for(i in 1:length(sitwith)){
			part <- sapply(A,function(x) sitwith[i] %in% x)
			exist <- sapply(A,function(x) i %in% x)
			if(!(any(part) & any(exist))){
			    if (any(part)) A[[(1:length(A))[part]]] <- c(A[[(1:length(A))[part]]],i)
		        if (any(exist)) A[[(1:length(A))[exist]]] <- c(A[[(1:length(A))[exist]]],sitwith[i])
			    if (!any(exist) & !any(part)){
				    A <- c(A,i)
				    if(sitwith[i]!=i) A[[length(A)]] <- c(A[[length(A)]],sitwith[i])
			    }
			}
		}
		A
	}
	B <- apply(co,1,f1)
	B <- lapply(B,part.ord)
	b <- sapply(B,set.not)
	df <- data.frame(part=b,prob=p)
	as.data.frame(tapply(df$prob,df$part,sum))	
}

##########################
# pdf ddCRP distribution #
##########################
# dddCRP <- function(partition,sim,mass){
	# diag(sim) <- mass
	# nc <- apply(sim,1,sum)
	# prob <- sim/nc
	# lst <- lapply(partition, function(x) lapply(1:length(x), function(i) x))    
	# lst <- Reduce(lst,f=c)
	# co <- expand.grid(lst)
	# p <- apply(co,1,function(y) sapply(1:length(y),function(i) prob[i,y[i]]))
	# sum(apply(p,2,prod))
# }


############################
# Least Squares Clustering #
############################
assoc.matrix.old2 <- function(X){
	n <- length(Reduce(X,f=c))
	a <- Reduce(Reduce(lapply(X,function(x) expand.grid(x,x)),f=rbind),f=paste)
	b <- Reduce(expand.grid(1:n,1:n),f=paste)
	matrix(as.numeric(b %in% a),ncol=n,nrow=n)   
}

assoc.matrix.old1 <- function(X){
	n <- length(unlist(X))
	M <- matrix(0,ncol=n, nrow=n)
	M[as.matrix(Reduce(lapply(X,function(x) expand.grid(x,x)),f=rbind))] <- 1
	M  
}

assoc.matrix <- function(cl){
# Taken from cltoSim function from mcclust package
	if (is.list(cl)) {
		cl <- list.to.labels(cl)
	}
	n <- length(cl)
	cl1 <- rep(cl,n)
	cl2 <- rep(cl,each=n)
	matrix(cl1==cl2, ncol=n)*1
}

pp.matrix.est <- function(list.part){
	A <- lapply(list.part,assoc.matrix)
	Reduce(A,f="+")/length(A)
}

ls.clust <- function(list.part,pi.hat){
	A <- lapply(list.part,assoc.matrix)
	if (missing(pi.hat)) {
	     pi.hat <- Reduce(A,f="+")/length(A)
	}
	list(lsclust=list.part[[which.min(sapply(A,function(B) sum((B-pi.hat)^2)))]],pi.hat=pi.hat)
}

#######################
# Adjusted Rand Index #
#######################
adj.rand.index <- function(X,Y){
	if (is.list(X)) {
		X <- list.to.labels(X)
	}
	if (is.list(Y)) {
		Y <- list.to.labels(Y)
	}
	if (length(unique(X)) == length(X) && length(unique(Y)) == length(Y)) {
		return(1)
	}
	cont.tab <- table(X,Y)
	if (all(dim(cont.tab) == c(1,1))) {
		return(1)
	}
	index <- sum(choose(cont.tab,2))
	x.dot <- sum(choose(apply(cont.tab,1,sum),2))
	y.dot <- sum(choose(apply(cont.tab,2,sum),2))
	exp.index <- (x.dot*y.dot)/choose(sum(cont.tab),2)
	max.index <- 0.5*(x.dot+y.dot) 
	(index - exp.index)/(max.index - exp.index)	
}

#########################

#########################
# Algorithmic Functions #
#########################

#########################

###################################
# iterative comparisons           #
###################################
m1.algorithm <- function(lsclust,pi.hat){
	v <- Reduce(lsclust,f=c)
	start <- lsclust
	s <- sample(v)
	n.iter <- 0
	change <- TRUE
	while(any(change)){
		n.iter <- n.iter + 1
		change <- logical(length(v))
		for(i in s){
			base <- start
			x <- which(mapply("%in%",i,start))
			base[[x]] <- start[[x]][start[[x]]!=i]
			if( any(0==sapply(base,length)) ){
				base <- base[-which(0==sapply(base,length))]
			}
			list.part <- sapply(1:(length(base)+1),function(a) list())
			for(j in 1:length(base)){
				new <- base
				new[[j]] <- c(new[[j]],i)
				list.part[[j]] <- new
			}
			list.part[[length(list.part)]] <- c(base,i)
			A <- lapply(list.part,assoc.matrix)
			end <- list.part[[which.min(sapply(A,function(B) sum((B-pi.hat)^2)))]] 
			change[i] <- which(mapply("%in%",i,end)) != x
			start <- end
		}
	}
    list(new.clust=start,n.iter=n.iter)
}

#########################################
# creating an assoc. matrix from pi.hat #
#########################################
create.part <- function(pi.hat,cut.off=0.5) {
	best.assoc <- ifelse(pi.hat >= cut.off,1,0)
	diag(best.assoc) <- 1
	C <- unique(lapply(1:nrow(best.assoc),function(i) (1:nrow(best.assoc))[1==best.assoc[i,]]))
    Mc <- M <- sapply(C,function(c) sapply(C,function(b) length(intersect(b,c))!=0))
    diag(Mc) <- FALSE
    while(any(Mc)){
    	C <- unique(part.ord(lapply(1:nrow(M),function(m) Reduce(union,C[M[m,]])),'list'))
    	Mc <- M <- as.matrix(sapply(C,function(c) sapply(C,function(b) length(intersect(b,c))!=0)))
    	diag(Mc) <- FALSE
    }
    C
}

######################################
# Minimize binder loss function from #
# Pairwise Probability Matrix        # 
# i.e. 'core partition' algorithm    #
######################################

#' @export
#' @import rscala

## First Implementation
minbinder1 <- function(psm, cls.draw = NULL, method = c("core", "core2", "avg", "comp", "draws", "laugreen", 
	"all")[1], max.k = NULL, include.lg = FALSE, start.cl = NULL, tol = 0.001) {
	if (!(method %in% c("core", "core2"))) {
		if (!require(mcclust)) 
			stop("Package 'mcclust' not installed")
		else mcclust::minbinder(psm, cls.draw, method, max.k, include.lg, start.cl, tol)
	} else {
		best.assoc <- ifelse(psm >= 0.5, 1, 0)
		#diag(best.assoc) <- 1
		C <- lapply(1:nrow(best.assoc), function(i) (1:nrow(best.assoc))[1 == best.assoc[i, ]])
		B <- lapply(C, function(c) Reduce(intersect, C[c]))
		B <- unique(B)
		is.core <- !sapply(1:length(B), function(b) any(sapply(B[-b], function(x) all(B[[b]] %in% x))))
		A <- B[is.core]

		intersect.matrix <- function(A) {
			Mc <- sapply(A, function(a) sapply(A, function(b) length(intersect(b, a)) != 0))
			if (!is.matrix(Mc)) {
				Mc <- as.matrix(Mc)
			}
			diag(Mc) <- FALSE
			Mc
		}

		Mc <- intersect.matrix(A)
		A. <- list(0)

		if (method == "core") {
			count <- 0
			while (any(Mc)) {
				has.ints <- apply(Mc, 1, any)
				A.old <- A.
				A. <- A[has.ints]

				if (!setequal(unlist(A.), unlist(A.old))) diag(Mc) <- has.ints

				A.ints <- unique(lapply(apply(Mc[has.ints, ], 1, function(r) A[r]), function(L) Reduce(intersect, L)))
				A.ints <- A.ints[sapply(A.ints, function(a) length(a) != 0)]
				A.ints.all <- Reduce(c, A.ints)

				A.diff <- sapply(1:length(A.), function(a) setdiff(A.[[a]], Reduce(c, A.[-a])))
				A.diff <- A.diff[sapply(A.diff, function(a) length(a) != 0)]
				A.diff.all <- Reduce(c, A.diff)

				remains1 <- unique(lapply(A., function(a) setdiff(a, A.diff.all)))
				remains <- unique(lapply(remains1, function(a) setdiff(a, A.ints.all)))
				remains <- remains[sapply(remains, function(a) length(a) != 0)]

				A <- part.ord(c(A[!has.ints], A.ints, A.diff, remains))

				Mc <- intersect.matrix(A)
				count <- count + 1
				#if (count > 101) stop ("error in breaking up clusters")
			}

		} else if (method == "core2") {
			has.ints <- apply(Mc, 1, any)
			A. <- A[has.ints]
			A.ints <- lapply(A., function(a) lapply(A., function(b) {
				if (setequal(a, b)) integer(0)
				else intersect(a, b)
			}))
			A.ints.all <- unique(unlist(A.ints))
			remains <- unique(lapply(A., function(a) setdiff(a, A.ints.all)))
			A <- part.ord(c(A[!has.ints], remains, as.list(A.ints.all)))
		}

		core <- A
		merge.ss <- function(a, b) {
			if (a < b) {
				merged <- union(A[[a]], A[[b]])
				A. <- c(list(merged), A[-c(a, b)])
				sum((assoc.matrix(A.) - psm)^2)
			} else current.ss
		}

		# split.ss <- function(cc) {
		# A. <- c(sapply(A,function(a) setdiff(a,cc)),list(cc))
# sum((assoc.matrix(A.) - psm)^2)
# }

		current.ss <- sum((assoc.matrix(A) - psm)^2)
		sumsq <- sapply(1:length(A), function(a) sapply(1:length(A), function(b) merge.ss(a, b)))

		# sumsq2 <- sapply(core,split.ss)
		# sumsq2 - current.ss

		while (any((sumsq - current.ss) < 0)) {
			index <- as.vector(which(sumsq == min(sumsq), arr.ind = TRUE)[1, ])
			A <- c(A[-index], list(Reduce(union, A[index])))
			current.ss <- sum((assoc.matrix(A) - psm)^2)
			sumsq <- sapply(1:length(A), function(a) sapply(1:length(A), function(b) merge.ss(a, b)))
		}
		part.ord(A)
	}
}

## Second Implementation: Intersects, of core.clusters broken up
## to singletons instead of remaining as a cluster.

#' @export
#' @import rscala

minbinder2 <- function(psm, cls.draw = NULL, method = c("coreclust","avg", "comp", "draws", 
    "laugreen", "all")[1], max.k = NULL, include.lg = FALSE, start.cl = NULL, 
    tol = 0.001) {
    	if (method != "coreclust") {
    		if (!require(mcclust)) stop("Package 'mcclust' not installed")
    		else mcclust::minbinder(psm, cls.draw, method, max.k, include.lg, start.cl, tol)
    	} else {
    		best.assoc <- ifelse(psm >= 0.5,1,0)
		#diag(best.assoc) <- 1
		C <- lapply(1:nrow(best.assoc),function(i) (1:nrow(best.assoc))[1==best.assoc[i,]])
		B <- lapply(C, function(c) Reduce(intersect,C[c]))
		B <- unique(B)
		is.core <- !sapply(1:length(B), function(b) any(sapply(B[-b], function(x) all(B[[b]] %in% x))))
		A <- B[is.core]
		
		intersect.matrix <- function (A) {
			Mc <- sapply(A,function(a) sapply(A,function(b) length(intersect(b,a))!=0))
			if (!is.matrix(Mc)) {
				Mc <- as.matrix(Mc)
			}
	    		diag(Mc) <- FALSE
	    		Mc
		}
		
		Mc <- intersect.matrix(A)
		
		# Break up Clusters
    	has.ints <- apply(Mc,1,any)	
		A. <- A[has.ints]
		A.ints <- lapply(A., function(a) lapply(A., 
			function(b) { 
				if (setequal(a,b)) integer(0)
			    else intersect(a,b)
			}
		))
		A.ints.all <- unique(unlist(A.ints))
		remains <- unique(lapply(A.,function(a) setdiff(a,A.ints.all)))
		A <- part.ord(c(A[!has.ints], remains, as.list(A.ints.all)))
		
		core <- A  	
	    merge.ss <- function(a,b) {
		    	if (a < b) {
		    		merged <- union(A[[a]],A[[b]])
		    		A. <- c(list(merged),A[-c(a,b)])
		    		sum((assoc.matrix(A.) - psm)^2)
		    	}
		    	else {
		    		current.ss
		    	} 		
	    } 
	
	    # split.ss <- function(cc) {
	    		# A. <- c(sapply(A,function(a) setdiff(a,cc)),list(cc))
	    		# sum((assoc.matrix(A.) - psm)^2)
	    # }
	    
	    current.ss <- sum((assoc.matrix(A) - psm)^2)
	    sumsq <- sapply(1:length(A), function(a) sapply(1:length(A), function(b) merge.ss(a,b)))
	    
	    # sumsq2 <- sapply(core,split.ss)
	    # sumsq2 - current.ss
	    
	    while(any((sumsq - current.ss) < 0)) {
	    		index <- as.vector(which(sumsq==min(sumsq), arr.ind=TRUE))
			A <- c(A[-index],list(Reduce(union,A[index])))
			current.ss <- sum((assoc.matrix(A) - psm)^2)
			sumsq <- sapply(1:length(A), function(a) sapply(1:length(A), function(b) merge.ss(a,b)))
	    }
	    
		part.ord(A)
    }	
}


######################################
# Find the 'core' paritions from the # 
# 'best' assoc. matrix from pi.hat   #
######################################
# precursor to minbinder
core.partition <- function(pi.hat, cut.off=0.5) {
	best.assoc <- ifelse(pi.hat >= cut.off,1,0)
	#diag(best.assoc) <- 1
	C <- lapply(1:nrow(best.assoc),function(i) (1:nrow(best.assoc))[1==best.assoc[i,]])
	B <- lapply(C, function(c) Reduce(intersect,C[c]))
	B <- unique(B)
	core <- !sapply(1:length(B), function(b) any(sapply(B[-b], function(x) all(B[[b]] %in% x))))
	A <- B[core]
	Mc <- M <- sapply(A,function(a) sapply(A,function(b) length(intersect(b,a))!=0))
	if (!is.matrix(Mc)) {
		Mc <- M <- as.matrix(Mc)
	}
    diag(Mc) <- FALSE
    while(any(Mc)){
	    	A <- unique(part.ord(lapply(1:nrow(M),function(m) Reduce(union,A[M[m,]])),'list'))
	    	Mc <- M <- as.matrix(sapply(A,function(a) sapply(A,function(b) length(intersect(b,a))!=0)))
	    	diag(Mc) <- FALSE
    }
	A
}

#######################################
# Reduce number of clusters algorithm #
#######################################
reduce.nclust <- function(lsclust,pi.hat,reduce=TRUE,class=c("list","equivalence")) {
	s <- which.min(sapply(lsclust,length))
	new.opt <- 1:length(lsclust[-s])
	assign <- expand.grid(lapply(1:length(lsclust[[s]]),function(i) new.opt))
	a <- 0 
	if(!reduce) a <- 1
    list.part <- sapply(1:(nrow(assign)+a),function(i) list())
    if(!reduce) list.part[[a]] <- lsclust
	for(i in 1:nrow(assign)){
		base <- lsclust[-s]
		for(j in 1:ncol(assign)){
		    base[[assign[i,j]]] <- c(base[[assign[i,j]]],lsclust[[s]][j])
		}
		class(base) <- class
		list.part[[i+a]] <- base
	} 
	A <- lapply(list.part,assoc.matrix)
	list.part[[which.min(sapply(A,function(B) sum((B-pi.hat)^2)))]]
}


########################################
# Find all points of splitting/ union  #
# i.e. branch points                   #
########################################
find.branches <- function(pp.matrix, descending = TRUE) {
	# pp.matrix = pairwise probability matrix
	pp <- as.vector(pp.matrix)
	threshold <- n.clust <- numeric()
	y <- 0
	while (y != 1) {
		pp <- pp[pp != max(pp)]
		x <- max(pp) # find next candidate for threshold
		threshold <- c(threshold,x)
		y <- length(create.part(f4.pi.hat,cut.off=x)) 
		n.clust <- c(n.clust,y)
	}
	z <- max(cumsum(table(n.clust))) + 1 - cumsum(table(n.clust))
	if (descending) { z <- sort(z) }
	cbind(threshold=threshold,n.clust=n.clust)[z,]
}


#################################
# Pseudo-Association Clustering #
################################# 
psac <- function(pp.matrix) {
	N <- nrow(pp.matrix) # total number of items to be clustered
	n <- -(1:N)          # index vector for leafs (negative values) &
						 # branches replaced with positive values
	merge <- matrix(nrow=(N-1),ncol=2)
	height <- numeric(N-1)
	order <- numeric(N)
	diag(pp.matrix) <- 0	
	
	i <- 0
	while ( i < (N-1) ) {
		i <- i + 1
		not.new <- TRUE
		while ( all(not.new) ) {
			h <- max(pp.matrix)
			y <- which(h == pp.matrix,arr.ind=TRUE)
			x <- unique(t(apply(y,1,function(a) a[order(a)])))
			pp.matrix[y] <- 0
			check <- t( apply(x,1,function(a) n[a]) )
			not.new <- check[,1] == check[,2]
		}
		x <- x[!not.new,,drop=FALSE]
		for ( j in 1:nrow(x) ) {
			check2 <- n[x[j,]]
			new <- check2[1] != check2[2]
			if ( new ) {
				if ( j != 1 ) i <- i + 1
				height[i] <- h
				m  <- n[x[j,]]
				merge[i,] <- m[order(m, decreasing=TRUE)]
				n[n %in% m] <- i
			}	
		}
	}	
	out <- list()
	# code to determine order
	# recode from fortran
	order[1] <- merge[N-1,1] 
	order[2] <- merge[N-1,2]
	loc <- 2
	for ( i in seq(N-1,1) ) {
		for ( j in 1:loc ) {
			if ( order[j] == i ) {
				order[j] <- merge[i,1]
				if ( j == loc ) {
					loc <- loc + 1
					order[loc] <- merge[i,2]
				}
				else {
					loc <- loc + 1
					for ( k in seq(loc,j+2) ) {
						order[k] <- order[k-1]
					}
					order[j+1] <- merge[i,2]
				}
			}
		}
	}
	out$order <- -order
	out$height <- 1-height
	out$prob <- height
	out$merge <- merge
	class(out) <- "hclust"
	out
}

#######################################
# Create pp.matrix from hclust object #
#######################################
hclust.ppm <- function(hclust) {
	n <- length(hclust$order)
	ppm <- matrix(nrow=n,ncol=n)
	pheight <- sort(hclust$height) / ((n+1)/n * max(hclust$height))
	prob <- 1 - pheight
	
	# Function to find which values to update
	merge <- hclust$merge
	find.clust <- function(row) {
		a <- merge[row,]
		if (a[1] > 0 &&  a[2] > 0) {
			return( c(find.clust(a[1]), find.clust(a[2])) ) 
		} else if (a[1] < 0 && a[2] < 0) {
		 	return( -c(a[1], a[2]) )
		} else if (a[1] > 0 && a[2] < 0) {
			return( c(find.clust(a[1]), -a[2]) )
		} else {
			return( c(-a[1], find.clust(a[2])) )
		}
	}
	for (i in seq(n-1,1)) {
		clust <-  find.clust(i)
		update <- as.matrix(expand.grid(clust,clust))
		ppm[update] <- prob[i]
	}
	diag(ppm) <- 1
	ppm
}


#####################
# Simple comparison #
#####################
# compares the structure of the dendogram
simple.comp <- function(hclust1,hclust2) {
	hclust1$height <- sort(hclust1$height)
	hclust2$height <- sort(hclust2$height)
	list.part1 <- lapply(hclust1$height, function(h) cutree(hclust1, h=h))
	list.part2 <- lapply(hclust2$height, function(h) cutree(hclust2, h=h))
	mean(mapply(adj.rand.index,list.part1,list.part2))
}

#######################
# Integral Comparison #
#######################
dendro.comp <- function(hclust1, hclust2, bias.adj = TRUE, weight = dunif, ..., 
						subdivisions = 10000L, rel.tol = .Machine$double.eps^0.25, 
						abs.tol = rel.tol, stop.on.error = TRUE) {
	max1 <- max(hclust1$height)
	max2 <- max(hclust2$height)
	if (bias.adj){
		n <- length(hclust1$order)
		max1 <- ((n+1) / n) * max1
		max2 <- ((n+1) / n) * max2
	}
	hclust1$height <- hclust1$height / max1
	hclust2$height <- hclust2$height / max2
	func <- function(h) {
		part1 <- lapply(h,function(x) cutree(hclust1,h=x))
		part2 <- lapply(h,function(x) cutree(hclust2,h=x))
		mapply(adj.rand.index,part1,part2)*weight(h,...)
	}
	integrate(func, 0, 1, subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol, stop.on.error=stop.on.error)
} 




