norm_out_simul <-function (p=2,N=1000,nn=5,alpha=0.05,ff=2,pp=TRUE,seed = NULL){
########################################################################
#p = dimension, N = sample size, nn = # of nearest neighbours
#alpha = proportion of outliers, ff regulates the degree of difference
#between outliers and normal observations
  require(mixtools)
  require(DDoutlier)
  
  #set seed if required
  
  if(!is.null(seed))	{
    set.seed(seed)	
  }	
  
  #generate observations from the standard normal distribution	
  X <- rmvnorm(N,mu=rep(0,p),sigma=diag(p))
  
  #generate values on the sphere of radius ff and add random noise.
  Z <- t(apply(rmvnorm(floor(alpha*N),mu=rep(0,p),sigma=diag(p)),1,function(x) {ff*x/sqrt(sum(x*x))}))
  Z <- Z + rmvnorm(floor(0.05*N),mu=rep(0,p),sigma=diag(p))
  
  #randomly replace alpha*N values from the pure observations with 
  #outliers and assign labels (0 = pure and 1 = outlier	
  ind <- sample(1:N,floor(alpha*N),replace=FALSE)
  X[ind,] <- Z
  labels <- rep(0,N)
  labels[ind] <- 1
  
  #produce a plot if desired.	
  if(pp) {plot(X,col=ifelse(labels==0,"blue","red"),pch=ifelse(labels==0,1,20),xlab="",ylab="")}
  
  #compute and return lof scores and labels	
  lof_scores <- LOF(X,k=nn)
  return(list(X=X,scores = lof_scores, labels = labels))	
}
