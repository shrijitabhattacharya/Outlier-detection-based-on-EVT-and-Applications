simulate_pareto=function(n,alpha,x0){
  u=runif(n)
  X=x0/u^(1/alpha)
  return (X)
}

simulate_burr=function(n,eta,lam,tau){
  U=runif(n)
  X=(eta*(U^(-(1/lam))-1))^(-(1/tau))
  return (X)
}

simulate_rev_burr=function(n,eta,lam,tau){
  i=1
  X=rep(0,n)
  while (i<=n){
    u=runif(1)
    y=1-(eta*(u^(-(1/lam))-1))^(1/tau)
    if (y>0){
      X[i]=y
      i=i+1
    }
  }
  return(X)
}

simulate_T=function(n,df.T){
  X=rt(n,df.T)
  return (abs(X))
}

simulate_lnorm=function(n,mu,sig){
  X=rlnorm(n=n,mu,sig)
  return (X)
}

simulate_rnorm=function(n,mu,sig){
  X=rnorm(n=n,mu,sig)
  return(abs(X))
}

simulate_weibull=function(n,lam,tau){
  U=runif(n)
  X=(-log(U)/lam)^(1/tau)
  return(X)
}

simulate_beta=function(n,p,q){
  X=rbeta(n,p,q)
  return(X)
}

trim_hill_compute=function(X,k0,k){
  val=((k0+1)/(k-k0))*log(X[(k0+1)]/X[(k+1)])+(1/(k-k0))*sum(log(X[(k0+2):k]/X[(k+1)]))
  return (val)
}

trim_hill_gen_compute=function(X,k0,k,tol=1e-6){
  H=unlist(lapply((k0+1):k,function(j){return(trim_hill_compute(X,k0,j))}))
  UH=X[(k0+2):(k+1)]*H
  idx=which(UH!=0)
  UH[which(UH==0)]=UH[idx[1]]*tol
  Z1=((k0+2):k)*(log (UH[1:(k-k0-1)])-log (UH[2:(k-k0)]))
  #Z2=((k0+2):k)*(log((2:(k-k0))/(1:(k-k0-1)))-1/(1:(k-k0-1)))
  return(mean(Z1))
}

bias_trim_hill_gen_compute=function(X,k0,k,tol=1e-6){
  if (k0>0){
    return(trim_hill_gen_compute(X[-c(1:k0)],0,(k-k0)))
  }else{
    return(trim_hill_gen_compute(X,0,k))
  }
}


T_compute=function(X,k){
  E=unlist(lapply(0:(k-1), function(k0){return(trim_hill_compute(X,k0,k))}))
  count=1:(k-1)
  T.val=((k-count)/(k-count+1))*(E[(count+1)]/E[count])
  return(T.val)
}

outlier_detect_stilian=function(X,k,k0.max,level=0.05,a=1.2){
  E=unlist(lapply(0:(k-1), function(k0){return(trim_hill_compute(X,k0,k))}))
  U=rep(0,(k-1))
  C=rep(0,(k-1))
  count=1:(k-1)
  Tk0k=((k-count)/(k-count+1))*(E[(count+1)]/E[count])
  U=Tk0k^(k-count)
  C=(2*abs(U-0.5))[seq(from=(k-1),to=1,by=-1)]
  w0=a^(1:(k-1))
  w1=a^((k-1):1)
  w=(abs(log(1-level))*w0)/sum(w0)
  idx=k-intersect((k-1):(k-k0.max),which((abs(log(C))/w)<1))
  if (length(idx)==0){k0.hat=0}else{k0.hat=max(idx)}
  return (list('k0.hat'=k0.hat,'Uk0k'=abs(log(2*abs(U-0.5))),'ak0k'=abs(log((1-level)^(w1/sum(w1))))))
}


outlier_detect=function(X,k,k0.max,xi.hat,a=1.2,level=0.05){
  xi.hat.minus=min(0,xi.hat)
  Tk0k=T_compute(X,k)
  K0=0:(k-2)
  if (xi.hat.minus==0){
    Tk0k.adj=k*(1-Tk0k[(K0+1)])
  }else{
    val=(k/(K0+1))^(1-xi.hat.minus)*(1-Tk0k[(K0+1)])*(xi.hat.minus/(1-xi.hat.minus))
    Tk0k.adj=((K0+1)/xi.hat.minus)*log(1+unlist(lapply(val,function(x)max(x,-0.9999))))
  }
  #U=1-pexp(Tk0k.adj)
  U=pexp(Tk0k.adj)
  C=(2*abs(U-0.5))[seq(from=(k-1),to=1,by=-1)]
  w0=a^(1:(k-1))
  w1=a^((k-1):1)
  w=(abs(log(1-level))*w0)/sum(w0)
  idx=k-intersect((k-1):(k-k0.max),which((abs(log(C))/w)<1))
  if (length(idx)==0){k0.hat=0}else{k0.hat=max(idx)}
  return (list('k0.hat'=k0.hat,'T1'=Tk0k,'T2'=Tk0k.adj,'U'=abs(log(2*abs(U-0.5))),'thres'=abs(log((1-level)^(w1/sum(w1))))))
}

outlier_pooled_stilian=function(Y,level=0.05,alpha.min=0.05,alpha.max=0.6){
  Z=sort(Y,decreasing = TRUE)
  S=seq(from=alpha.min,by=0.01,to=alpha.max)
  n=length(Z)
  s=length(S)
  Out=rep(0,s)
  for (i in 1:s){
    k=ceiling(S[i]*n)
    Out[i]=outlier_detect_stilian(Z,k,floor(0.9*k),level=level)$k0.hat
  }
  K0.hat=as.numeric(names(which.max(table(Out))))
  return (list('K0'=K0.hat,'O'=Out))
}


outlier_pooled_truth=function(Y,alpha.max=0.6,xi){
  Z=sort(Y,decreasing = TRUE)
  S=seq(from=0.05,by=0.01,to=alpha.max)
  n=length(Z)
  s=length(S)
  Out=rep(0,s)
  for (i in 1:s){
    k=ceiling(S[i]*n)
    Out[i]=outlier_detect(Z,k,floor(0.9*k),xi)$k0.hat
  }
  K0.hat=as.numeric(names(which.max(table(Out))))
  return (list('K0'=K0.hat,'O'=Out))
}

outlier_pooled=function(Y,level=0.05,alpha.min=0.05,alpha.max=0.6,beta.max=0.2,gamma.max=0.9){
  Z=sort(Y,decreasing=TRUE)
  S1=seq(from=alpha.min,by=0.01,to=alpha.max)
  S2=seq(from=0,by=0.01,to=beta.max)
  n=length(Z)
  s1=length(S1)
  s2=length(S2)
  Out=matrix(0,nrow=s1,ncol=s2)
  Xi.hat=matrix(0,nrow=s1,ncol=s2)
  for (i in 1:s1){
    for (j in 1:s2){
      alpha=S1[i];beta=S2[j]
      k=ceiling(alpha*n)
      k0=floor(beta*k)
      Xi.hat[i,j]=bias_trim_hill_gen_compute(Z,k0,k)
      Out[i,j]=outlier_detect(Z,k,(gamma.max*k),Xi.hat[i,j],level = level)$k0.hat
    }
  }
  K0.hat=as.numeric(names(which.max(table(c(Out)))))
  return (list('K0'=K0.hat,'O'=Out,'Xi'=Xi.hat))
}


plot_trim_xi=function(Y,alpha.min=0.2,alpha.max=0.6,beta.max=0.2){
  Z=sort(Y,decreasing=TRUE);n=length(Z)
  S1=seq(from=alpha.min,by=0.01,to=alpha.max);s1=length(S1)
  S2=seq(from=0,by=0.01,to=beta.max);s2=length(S2)
  Xi.hat=matrix(0,nrow=s1,ncol=s2)
  Xi.sign=matrix(0,nrow=s1,ncol=s2)
  for (i in 1:s1){
    for (j in 1:s2){
      alpha=S1[i];beta=S2[j]
      k=ceiling(alpha*n)
      k0=floor(beta*k)
      Xi.hat[i,j]=trim_hill_gen_compute(Z,k0,k)
      Xi.sign[i,j]=sum(Xi.hat[i,j]>=0)
    }
  }
  return (list('Xi'=Xi.sign,'Xiv'=Xi.hat))
}

plot_bias_trim_xi=function(Y,alpha.min=0.2,alpha.max=0.6,beta.max=0.2){
  Z=sort(Y,decreasing=TRUE);n=length(Z)
  S1=seq(from=alpha.min,by=0.01,to=alpha.max);s1=length(S1)
  S2=seq(from=0,by=0.01,to=beta.max);s2=length(S2)
  Xi.hat=matrix(0,nrow=s1,ncol=s2)
  Xi.sign=matrix(0,nrow=s1,ncol=s2)
  for (i in 1:s1){
    for (j in 1:s2){
      alpha=S1[i];beta=S2[j]
      k=ceiling(alpha*n)
      k0=floor(beta*k)
      Xi.hat[i,j]=bias_trim_hill_gen_compute(Z,k0,k)
      Xi.sign[i,j]=sum(Xi.hat[i,j]>=0)
    }
  }
  return (list('Xi'=Xi.sign,'Xiv'=Xi.hat))
}



outlier_sign=function(Y,alpha.min=0.05,alpha.max=0.6,beta.max=0.2){
  Z=sort(Y,decreasing=TRUE)
  S1=seq(from=alpha.min,by=0.01,to=alpha.max)
  S2=seq(from=0,by=0.01,to=beta.max)
  n=length(Z)
  s1=length(S1)
  s2=length(S2)
  Xi1.sig=matrix(0,nrow=s1,ncol=s2)
  Xi2.sig=matrix(0,nrow=s1,ncol=s2)
  for (i in 1:s1){
    for (j in 1:s2){
      alpha=S1[i];beta=S2[j]
      k=ceiling(alpha*n)
      k0=floor(beta*k)
      Xi1.sig[i,j]=trim_hill_gen_compute(Z,k0,k)
      Xi2.sig[i,j]=bias_trim_hill_gen_compute(Z,k0,k)
    }
  }
  return (list('Xi1.s'=Xi1.sig,'Xi2.s'=Xi2.sig))
}


outlier_pooled_shrijita=function(Y,level=0.05,alpha.min=0.05,alpha.max=0.6,beta.max=0.2,gamma.max=0.9){
  Z=sort(Y,decreasing=TRUE)
  S1=seq(from=alpha.min,by=0.01,to=alpha.max)
  S2=seq(from=0,by=0.01,to=beta.max)
  n=length(Z)
  s1=length(S1)
  s2=length(S2)
  Out=matrix(0,nrow=s1,ncol=s2)
  Xi.hat=matrix(0,nrow=s1,ncol=s2)
  for (i in 1:s1){
    for (j in 1:s2){
      alpha=S1[i];beta=S2[j]
      k=ceiling(alpha*n)
      k0=floor(beta*k)
      Xi.hat[i,j]=sum(trim_hill_gen_compute(Z,k0,k)<0)*bias_trim_hill_gen_compute(Z,k0,k)
      Out[i,j]=outlier_detect(Z,k,(gamma.max*k),Xi.hat[i,j],level = level)$k0.hat
    }
  }
  K0.hat=as.numeric(names(which.max(table(c(Out)))))
  return (list('K0'=K0.hat,'O'=Out,'Xi'=Xi.hat))
}



contour_plot=function(X,alpha.min=0.05,alpha.max=0.6,beta.max=0.2,v=5){
  d=dim(X)
  L=table(c(X))
  L1=names(L); L2=as.numeric(L)
  L3=round(L2/sum(L2),2)
  obj=sort(L3,decreasing = T,index.return=T)
  v=min(v,length(L3))
  L4=unlist(lapply(1:v,function(i)paste0('(',L1[obj$ix[i]],',',L3[obj$ix[i]],')')))
  U=as.numeric(L1[obj$ix])
  u=length(U)
  COL=1:u
  S1=seq(from=alpha.min,by=0.01,to=alpha.max)
  S2=seq(from=0,by=0.01,to=beta.max)
  for (i in 1:d[1]){
    for (j in 1:d[2]){
      if (i+j==2){
        plot(S1[i],S2[j],col=COL[which(U==X[i,j])],cex.lab=1.4,cex.axis=1.4,cex.main=1.5,main='Majority vote plot',pch=19,xlim=c(alpha.min,alpha.max),ylim=c(0,beta.max+0.04),xlab=TeX('$\\beta$'),ylab=TeX('$\\gamma$'))
      }else{
        points(S1[i],S2[j],col=COL[which(U==X[i,j])],pch=19)
      }
    }
  }
  legend('top',legend=L4,col=COL[1:v],pch=rep(19,v),horiz=T,cex=1.2)
}

outlier_pooled_box_plot=function(Y,ut.params=NULL,lt.params=NULL,log.val=T,loc='bottomleft'){
  n=length(Y)
  Y1=sort(Y,decreasing = T)
  Y2=sort(1/Y,decreasing = T)
  if (log.val==T){Z1=log(Y1)}else{Z1=Y1}
  if (log.val==T){Z2=log(1/Y2)}else{Z2=1/Y2}
  require(grDevices)
  par(mar=c(4,5,3,2))
  if (!is.null(ut.params) & !is.null(lt.params)){
    plot(c(0,1),c(min(Z1),max(Z1)),cex.lab=1.4,cex.axis=1.4,cex.main=1.5, type = "n", xlab = "", ylab = "",main = "Tail-adjusted Boxplot",xaxt='n')
  }
  if (is.null(ut.params) & !is.null(lt.params)){
    plot(c(0,1),c(min(Z1),Z1[ceiling(0.25*n)]),cex.lab=1.4,cex.axis=1.4,cex.main=1.5, type = "n", xlab = "", ylab = "",main = "Tail-adjusted Boxplot",xaxt='n')
  }
  if (!is.null(ut.params) & is.null(lt.params)){
    plot(c(0,1),c(Z1[ceiling(0.75*n)],max(Z1)),cex.lab=1.4,cex.axis=1.4,cex.main=1.5, type = "n", xlab = "", ylab = "",main = "Tail-adjusted Boxplot",xaxt='n')
  }
  text.val=c();
  pch.val=c();
  col.val=c();
  out.val=c();
  if (!is.null(ut.params)){
    out1=ut.params
    rect(0.3,Z1[ceiling(0.75*n)], 0.7,Z1[ceiling(0.25*n)])
    segments(0.3,Z1[ceiling(0.5*n)],0.7,Z1[ceiling(0.5*n)],lwd=3)
    segments(0.5,Z1[ceiling(0.25*n)],0.5,Z1[(out1+1)],lty=2)
    segments(0.4,Z1[(out1+1)],0.6,Z1[(out1+1)])
    text.val=c(text.val,paste0('Top = ',out1))
    if (out1>0){
      points(rep(0.5,out1),Z1[1:out1],pch=4,col='red',lwd=1,cex=1.2)
      pch.val=c(pch.val,4)
      col.val=c(col.val,'red')
    }
  }
  if (!is.null(lt.params)){
    out2=lt.params
    segments(0.5,Z2[ceiling(0.25*n)],0.5,Z2[(out2+1)],lty=2)
    segments(0.4,Z2[(out2+1)],0.6,Z2[(out2+1)])
    text.val=c(text.val,paste0('Bottom  = ',out2))
    if (out2>0){
      points(rep(0.5,out2),Z2[1:out2],pch=3,col='red',lwd=1,cex=1.2)
      pch.val=c(pch.val,3)
      col.val=c(col.val,'red')
    }
  }
  if (!is.null(text.val)){
    legend(loc,cex=1.2,legend = text.val,pch=pch.val,col=col.val)
  }
}



normal_box_plot=function(X,ut.params=NULL,lt.params=NULL,log.val=T,loc='bottomleft'){
  n=length(X)
  require(grDevices)
  obj=boxplot(X,plot = F)
  out1=sum(X<obj$stats[1])
  if (log.val==T){
    Z=log(sort(X))
  }else{
    Z=sort(X)
  }
  par(mar=c(4,5,3,2))
  if (!is.null(ut.params) & !is.null(lt.params)){
    plot(c(0,1),c(min(Z),max(Z)),cex.lab=1.4,cex.axis=1.4,cex.main=1.5, type = "n", xlab = "", ylab = "",main = "Classical Boxplot",xaxt='n')
  }
  if (is.null(ut.params) & !is.null(lt.params)){
    plot(c(0,1),c(min(Z),Z[ceiling(0.75*n)]),cex.lab=1.4,cex.axis=1.4,cex.main=1.5, type = "n", xlab = "", ylab = "",main = "Classical Boxplot",xaxt='n')
  }
  if (!is.null(ut.params) & is.null(lt.params)){
    plot(c(0,1),c(Z[ceiling(0.25*n)],max(Z)),cex.lab=1.4,cex.axis=1.4,cex.main=1.5, type = "n", xlab = "", ylab = "",main = "Classical Boxplot",xaxt='n')
  }
  rect(0.3,Z[ceiling(0.25*n)], 0.7,Z[ceiling(0.75*n)])
  segments(0.3,Z[ceiling(0.5*n)],0.7,Z[ceiling(0.5*n)],lwd=3)
  text.val=c();
  pch.val=c();
  col.val=c();
  out2=sum(X>obj$stats[5])
  if(!is.null(ut.params)){
    segments(0.5,Z[ceiling(0.75*n)],0.5,Z[(n-out2)],lty=2)
    segments(0.4,Z[(n-out2)],0.6,Z[(n-out2)])
    if (out2>0){
      points(rep(0.5,out2),Z[(n-out2+1):n],pch=4,col='red',lwd=1,cex=1.2)
      pch.val=c(pch.val,4)
      col.val=c(col.val,'red')
      text.val=c(text.val,paste0('Top  = ',out2))
    }
  }
  if (!is.null(lt.params)){
    segments(0.5,Z[ceiling(0.25*n)],0.5,Z[(out1+1)],lty=2)
    segments(0.4,Z[(out1+1)],0.6,Z[(out1+1)])
    if (out1>0){
      points(rep(0.5,out1),Z[1:out1],col='red',pch=3,lwd=1,cex=1.2)
      col.val=c(col.val,'red')
      pch.val=c(pch.val,3)
      text.val=c(text.val,paste0('Bottom  = ',out1))
    }
  }
  if (!is.null(text.val)){
    legend(loc,legend = text.val,pch=pch.val,col=col.val,cex=1.2)
  }
}

pareto_quantile_plot=function(X,k0,k,reg=T){
  n=length(X)
  Y=sort(X,decreasing = TRUE)
  #Y=sort(X,decreasing = TRUE)[1:k]
  J=1:n
  #Q1=-log(J/(k+1))
  Q1=-log(J/(n+1))
  #Q2=log(Y)[1:k]
  Q2=log(Y)
  #y=Q2[(k0+1):k]-log(Y[k])
  #x=Q1[(k0+1):k]
  #beta=sum(y*x)/sum(x*x)
  #Q3=beta*Q1+log(Y[k])
  par(mar=c(4,5,3,2))
  plot(Q1,Q2,cex.lab=1.4,cex.main=1.5,cex.axis=1.4,xlab = TeX('$\\-log((j+1)/(n+1))$'),ylab=TeX('$\\log(X_{n-j,n})$'),main="Pareto QQ-plot")
  if (reg==T){
    fit=lm(Q2[(k0+1):k]~Q1[(k0+1):k])
    Q3=fit$coefficients[1]+fit$coefficients[2]*Q1
    lines(Q1,Q3,col='black')
  }
}


generalized_quantile_plot=function(X,k0,k,reg=T){
  Y=sort(X,decreasing = TRUE)
  n=length(Y)
  #J=1:k
  J=1:n
  #Q1=-log(J/(k+1))
  Q1=-log(J/(n+1))
  #H=unlist(lapply(1:k,function(j){return(trim_hill_compute(Y,0,j))}))
  H=unlist(lapply(1:n,function(j){return(trim_hill_compute(Y,0,j))}))
  #Q2=log(Y[1:k]*H)
  Q2=log(Y*H)
  par(mar=c(4,5,3,2))
  plot(Q1,Q2,cex.lab=1.4,cex.main=1.5,cex.axis=1.4,xlab = TeX('$\\-log((j+1)/(n+1))$'),ylab=TeX('$\\log(X_{n-j,n}H_{j})$'),main="Generalized QQ-plot")
  if (reg==T){
    fit=lm(Q2[(k0+1):k]~Q1[(k0+1):k])
    Q3=fit$coefficients[1]+fit$coefficients[2]*Q1
    lines(Q1,Q3,col='black')
  }
}

exponential_quantile_plot=function(X,k0,k,reg=T){
  Y=sort(X,decreasing = TRUE)[1:k]
  J=1:k
  Q1=-log(J/(k+1))
  Q2=Y[1:k]
  #y=Q2[(k0+1):k]-log(Y[k])
  #x=Q1[(k0+1):k]
  #beta=sum(y*x)/sum(x*x)
  #Q3=beta*Q1+log(Y[k])
  par(mar=c(4,5,3,2))
  plot(Q1,Q2,cex.lab=1.4,cex.main=1.5,cex.axis=1.4,xlab = TeX('$\\-log((j+1)/(n+1))$'),ylab=TeX('$X_{n-j,n}$'),main="Exponential QQ-plot")
  if (reg==T){
    fit=lm(Q2[(k0+1):k]~Q1[(k0+1):k])
    Q3=fit$coefficients[1]+fit$coefficients[2]*Q1
    lines(Q1,Q3,col='black')
  }
}




time_plot=function(X,ut.params=NULL,lt.params=NULL,lab.params,loc='topleft'){
  par(mar=c(4,5,3,2))
  n=length(X)
  Y1=sort(X,decreasing=T,index.return=T)
  pch.val=rep(1,n)
  if (!is.null(ut.params)){
    if (ut.params>0){pch.val[1:ut.params]=NA}
  }
  if (!is.null(lt.params)){
    if (lt.params>0){pch.val[n:(n-lt.params+1)]=NA}
  }
  plot(Y1$ix,Y1$x,cex.lab=1.4,pch=pch.val,cex.axis=1.4,cex.main=1.5,main=lab.params$m,xlab=lab.params$x,ylab=lab.params$y,xaxt='n')
  axis(1,at = lab.params$a,labels=lab.params$l,cex.axis=1.2)
  pch.value=c();
  if(!is.null(ut.params)){
    if (ut.params>0){
      segments(-2,Y1$x[(ut.params+1)],n+2,Y1$x[(ut.params+1)],lty=2,col='red',cex=1)
      points(Y1$ix[1:ut.params],Y1$x[1:ut.params],col='red',pch=4,cex=1)
      pch.value=c(pch.value,4)
    }
  }
  if (!is.null(lt.params)){
    if (lt.params>0){
      segments(-2,Y1$x[(n-lt.params)],n+2,Y1$x[(n-lt.params)],lty=2,col='red',cex=1)
      points(Y1$ix[(n-lt.params+1):n],Y1$x[(n-lt.params+1):n],col='red',pch=3,cex=1)
      pch.value=c(pch.value,4)
    }
  }
}
