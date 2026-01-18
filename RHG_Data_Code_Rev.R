setwd('C:/Users/shrij/Dropbox/Robust_Hill_Generalized/')
source('Code/RHG_Source_Code_Rev.R')
source('Code/multivariate_outlier_generation.R')



norm.obj=norm_out_simul(seed=NULL,nn = 500,pp = TRUE,ff=5)
D=norm.obj$scores
L=norm.obj$labels
X=D;Y=X+runif(X,-0.01,0.01);
sort.obj=sort(Y,decreasing = TRUE,index.return=TRUE)
d=D[sort.obj$ix]
l=L[sort.obj$ix]
plot(d,col=ifelse(l==0,"blue","red"),pch=ifelse(l==0,1,20))
pareto_quantile_plot(D,0,ceiling(length(D)*0.5),reg=F)

obj=outlier_pooled(sort.obj$x,alpha.min = 0.05,beta.max = 0.1);obj$K0
#contour_plot(obj$O,beta.max = 0.1,v=5)
x=norm.obj$X[sort.obj$ix,]
val=1:length(d)
plot(x,col=ifelse(l==0,"blue","black"),pch=ifelse(l==0,21,21),cex=1.5,xlab="",ylab="")
points(x,col=ifelse(is.element(val,(obj$K0+1):length(d)),NA,"red"),cex=0.5,pch=ifelse(is.element(val,(obj$K0+1):length(d)),19,19))



load('Data/burr.Rdata')
X=X0[10,]
obj=outlier_pooled(X,alpha.min = 0.05,beta.max = 0.1);obj$K0
png(paste0('Figures/Results_Version_2/Burr-0.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
contour_plot(obj$O,beta.max = 0.1,v=3)
dev.off()
X=X2[10,,5]
obj=outlier_pooled(X,alpha.min = 0.05,beta.max = 0.1);obj$K0
png(paste0('Figures/Results_Version_2/Burr-1.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
contour_plot(obj$O,beta.max = 0.1,v=3)
dev.off()

load('Data/revburr.Rdata')
X=X0[10,]
obj=outlier_pooled(X,alpha.min = 0.05,beta.max = 0.1);obj$K0
png(paste0('Figures/Results_Version_2/RBurr-0.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
contour_plot(obj$O,beta.max = 0.1,v=3)
dev.off()
X=X2[10,,5]
obj=outlier_pooled(X,alpha.min = 0.05,beta.max = 0.1);obj$K0
png(paste0('Figures/Results_Version_2/RBurr-1.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
contour_plot(obj$O,beta.max = 0.1,v=3)
dev.off()



load('Code/ClusterMax_1.0/ClusterMax/data/MaxPrecipFallFrance.RData')


## Chamoneix
D=as.numeric(MaxPrecipFallFrance$precip[,73])
png(paste0('Figures/Results_Version_2/Chamoneix-exp.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
exponential_quantile_plot(D,0,ceiling(length(D)*0.5),reg=F)
dev.off()
set.seed(21)
X=(D*10)/(10^(floor(log10(max(D)))));Y=X+runif(X,-0.01,0.01);Y=Y[Y>0]
obj=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj$K0
png(paste0('Figures/Results_Version_2/Chamoneix-maj.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
contour_plot(obj$O,alpha.min=0.05,beta.max = 0.1,v=3)
dev.off()
png(paste0('Figures/Results_Version_2/Chamoneix-adap.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
outlier_pooled_box_plot(D,ut.params = obj$K0,log.val = F,loc='topleft')
dev.off()
png(paste0('Figures/Results_Version_2/Chamoneix-norm.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
normal_box_plot(D,ut.params = 1,loc = 'topleft')
dev.off()
png(paste0('Figures/Results_Version_2/Chamoneix-time.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
time_plot(D,ut.params = obj$K0,lab.params=list('m'='Time Plot','x'='Time','y'='Precipitation','a'=seq(from=1,by=24,to=228),'l'=seq(from=1993,by=2,to=2011)))
dev.off()

## Uzein
D=as.numeric(MaxPrecipFallFrance$precip[,63])
#png(paste0('Figures/Results_Version_2/Uzein-par.png'), width =750, height = 600)
#par(mar=c(5,5,3,1),cex=1.9)
pareto_quantile_plot(D,0,ceiling(length(D)*0.5),reg=F)
#dev.off()
set.seed(21)
X=(D*10)/(10^(floor(log10(max(D)))));Y=X+runif(X,-0.01,0.01);Y=Y[Y>0]
obj=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj$K0
png(paste0('Figures/Results_Version_2/Uzein-maj.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
contour_plot(obj$O,alpha.min=0.05,beta.max = 0.1,v = 3)
dev.off()
png(paste0('Figures/Results_Version_2/Uzein-adap.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
outlier_pooled_box_plot(D,ut.params = obj$K0,log.val = F,loc = 'topleft')
dev.off()
png(paste0('Figures/Results_Version_2/Uzein-norm.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
normal_box_plot(D,ut.params = 1,loc = 'topleft')
dev.off()
png(paste0('Figures/Results_Version_2/Uzein-time.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
time_plot(D,ut.params = obj$K0,lab.params=list('m'='Time Plot','x'='Time','y'='Precipitation','a'=seq(from=1,by=24,to=228),'l'=seq(from=1993,by=2,to=2011)))
dev.off()



## Calcium
D=c(as.matrix(read.csv('Code/DATA/calcium.csv')))
png(paste0('Figures/Results_Version_2/Condroz-par-1.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
pareto_quantile_plot(D,0,ceiling(length(D)*0.5),reg=F)
dev.off()
png(paste0('Figures/Results_Version_2/Condroz-par-2.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
pareto_quantile_plot(1/D,0,ceiling(length(D)*0.5),reg=F)
dev.off()
set.seed(21)
X=(D*10)/(10^(floor(log10(max(D)))));Y=X+runif(X,-0.01,0.01);Y=Y[Y>0]
obj1=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj1$K0
png(paste0('Figures/Results_Version_2/Condroz-maj-1.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
contour_plot(obj1$O,alpha.min=0.05,beta.max = 0.1,v = 3)
dev.off()
set.seed(21)
X=((1/D)*10)/(10^(floor(log10(max((1/D))))));Y=X+runif(X,-0.01,0.01);Y=Y[Y>0]
png(paste0('Figures/Results_Version_2/Condroz-maj-2.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
obj2=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj2$K0
contour_plot(obj2$O,alpha.min=0.05,beta.max = 0.1,v=3)
dev.off()
png(paste0('Figures/Results_Version_2/Condroz-norm.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
normal_box_plot(D,ut.params = 1,lt.params = 1,log.val=F,loc='topleft')
dev.off()
png(paste0('Figures/Results_Version_2/Condroz-adap.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
outlier_pooled_box_plot(D,ut.params = obj1$K0,lt.params=obj2$K0,log.val = F,loc='topleft')
dev.off()
png(paste0('Figures/Results_Version_2/Condroz-time.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
time_plot(D,obj1$K0,obj2$K0,lab.params=list('m'='Data Plot','x'='Index','y'='Ca content','a'=seq(from=0,to=length(D),by=20)))
dev.off()



## Auto Data
D=read.table('Code/DATA/auto-mpg.data')$V5
png(paste0('Figures/Results_Version_3/Auto-gq-1.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
generalized_quantile_plot(D,0,ceiling(length(D)*0.5),reg=F)
dev.off()
png(paste0('Figures/Results_Version_3/Auto-gq-2.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
generalized_quantile_plot(1/D,0,ceiling(length(D)*0.5),reg=F)
dev.off()
set.seed(21)
X=(D*10)/(10^(floor(log10(max(D)))));Y=X+runif(X,-0.01,0.01);Y=Y[Y>0]
obj1=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj1$K0
png(paste0('Figures/Results_Version_2/Auto-maj-1.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
contour_plot(obj1$O,alpha.min=0.05,beta.max = 0.1,v = 3)
dev.off()
set.seed(21)
X=((1/D)*10)/(10^(floor(log10(max((1/D))))));Y=X+runif(X,-0.01,0.01);Y=Y[Y>0]
png(paste0('Figures/Results_Version_2/Auto-maj-2.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
obj2=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj2$K0
contour_plot(obj2$O,alpha.min=0.05,beta.max = 0.1,v=3)
dev.off()
png(paste0('Figures/Results_Version_2/Auto-norm.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
normal_box_plot(D,ut.params = 1,lt.params = 1,log.val=F,loc='topleft')
dev.off()
png(paste0('Figures/Results_Version_2/Auto-adap.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
outlier_pooled_box_plot(D,ut.params = obj1$K0,lt.params=obj2$K0,log.val = F,loc='topleft')
dev.off()
png(paste0('Figures/Results_Version_2/Auto-time.png'), width =750, height = 600)
par(mar=c(5,5,3,1),cex=1.9)
time_plot(D,obj1$K0,obj2$K0,lab.params=list('m'='Data Plot','x'='Index','y'='Car weight','a'=seq(from=0,to=length(D),by=20)))
dev.off()














## Toxicity Data

D=c(as.matrix(read.csv('Code/DATA/toxicity.csv')))
X=(D*10)/(10^(floor(log10(max(D)))))
Y=X+runif(X,-0.01,0.01)
Y=Y[Y>0]
generalized_quantile_plot(Y,0,ceiling(length(Y)*0.5))
obj1=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj1$K0
contour_plot(obj1$O,beta.max = 0.1)
X=((1/D)*10)/(10^(floor(log10(max((1/D))))))
Y=X+runif(X,-0.01,0.01)
Y=Y[Y>0]
generalized_quantile_plot(Y,0,ceiling(length(Y)*0.5))
obj2=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj2$K0
contour_plot(obj2$O,beta.max = 0.1)
normal_box_plot(D,ut.params = 1,lt.params = 1,log.val=F)
outlier_pooled_box_plot(D,ut.params = obj1$K0,lt.params=obj2$K0,log.val = F)
time_plot(D,obj1$K0,obj2$K0,lab.params=list('m'='Data Plot','x'='Index','y'='Car weight','a'=seq(from=0,to=length(D),by=20)))



obj=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.2);obj$K0
Y=Y[Y>0]
generalized_quantile_plot(Y,0,100)
obj=outlier_pooled(1/Y,alpha.min = 0.05,beta.max = 0.1);obj$K0


X=c(as.matrix(read.csv('Code/DATA/toxicity.csv')))
X=(X*10)/(10^(floor(log10(max(X)))))
Y=X+runif(X,-0.01,0.01)
Y=Y[Y>0]
generalized_quantile_plot(Y,0,100)
obj=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj$K0
X=1/c(as.matrix(read.csv('Code/DATA/toxicity.csv')))
X=(X*10)/(10^(floor(log10(max(X)))))
Y=X+runif(X,-0.01,0.01)
Y=Y[Y>0]
generalized_quantile_plot(Y,0,100)
obj=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj$K0
contour_plot(obj$O,beta.max = 0.1)





X=airquality$Wind
X=(X*10)/(10^(floor(log10(max(X)))))
Y=X+runif(X,-0.01,0.01)
Y=Y[Y>0]
generalized_quantile_plot(Y,0,100)
obj=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj$K0
X=1/airquality$Wind
X=(X*10)/(10^(floor(log10(max(X)))))
Y=X+runif(X,-0.01,0.01)
Y=Y[Y>0]
generalized_quantile_plot(Y,0,100)
obj=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj$K0


X=read.table('Code/DATA/diamond.txt')$V1
X=(X*10)/(10^(floor(log10(max(X)))))
Y=X+runif(X,-0.01,0.01)
Y=Y[Y>0]
generalized_quantile_plot(Y,0,100)
obj=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj$K0
X=1/read.table('Code/DATA/diamond.txt')$V1
X=(X*10)/(10^(floor(log10(max(X)))))
Y=X+runif(X,-0.01,0.01)
Y=Y[Y>0]
generalized_quantile_plot(Y,0,100)
obj=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj$K0





jitter_data=function(X,d=0.1){
  n=length(X)
  Xt=table(X)
  l=length(Xt)
  Xts=Xt[l:1]
  P=as.numeric(Xts)
  V=as.numeric(names(Xts))
  Xj=c();
  for (i in 1:l){
    if (P[i]>1){
      x=runif(P[i],-d*P[i],d*P[i])
      x=sort(x,decreasing = T)
      Xj=c(Xj,rep(V[i],P[i])+x)
    }
    else{
      Xj=c(Xj,V[i])
    }
  }
  return (Xj)
}


boxplot(Y)

X=read.csv('Code/DATA/winequality-red.csv')$sulphates
Y=jitter_data(X,0.01)
obj=outlier_pooled(1/Y,alpha.min = 0.05,beta.max = 0.1);obj$K0
obj=outlier_pooled(1/Y,alpha.min = 0.05,beta.max = 0.2);obj$K0



for (i in 1:100){
  load('Code/MaxPrecipFallFrance.RData')
  X=as.numeric(MaxPrecipFallFrance$precip[,i])
  Y=X+runif(length(X),-0.05,0.05)
  Y=Y[Y>0]
  generalized_quantile_plot(Y,0,0.5*length(Y))
  #Y=jitter_data(X,0.01)
  obj1=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1)
  obj2=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.2)
  print(c(i,obj1$K0,obj2$K0))
}

X=as.numeric(MaxPrecipFallFrance$precip[,63])
Y=jitter_data(X,0.01)
Y=Y[Y>0]
obj=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj$K0
obj=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.2);obj$K0
obj=outlier_pooled(1/Y,alpha.min = 0.05,beta.max = 0.1);obj$K0
obj=outlier_pooled(1/Y,alpha.min = 0.05,beta.max = 0.2);obj$K0


X=c(as.matrix(read.csv('Code/DATA/toxicity.csv')))
set.seed(5)
#Y=jitter_data(X,0.01)
Y=X+runif(length(X),-0.01,0.01)
Y=Y[Y>0]
obj=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj$K0
obj=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.2);obj$K0
obj=outlier_pooled(1/Y,alpha.min = 0.05,beta.max = 0.1);obj$K0
obj=outlier_pooled(1/Y,alpha.min = 0.05,beta.max = 0.2);obj$K0


X=c(as.matrix(read.csv('Code/DATA/calcium.csv')))
Y=X+runif(length(X),-0.01,0.01)
Y=Y[Y>0]
Y=Y[Y>0]
obj=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj$K0
obj=outlier_pooled(1/Y,alpha.min = 0.05,beta.max = 0.1);obj$K0


X=airquality$Wind
#Y=jitter_data(X,0.01)
Y=X+runif(length(X),0,0.05)
Y=Y[Y>0]
obj=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj$K0
obj=outlier_pooled(1/Y,alpha.min = 0.05,beta.max = 0.1);obj$K0


X=as.matrix(read.csv('Code/DATA/freclaim.csv',header=F))
Y=jitter_data(X,0.01)
Y=Y[Y>0]
obj=outlier_pooled(Y,alpha.min = 0.05,beta.max = 0.1);obj$K0
obj=outlier_pooled(1/Y,alpha.min = 0.05,beta.max = 0.1);obj$K0



X=c(as.matrix(read.csv('Code/DATA/calcium.csv')))
obj=outlier_pooled(Y,beta.max = 0.1)
contour_plot(obj$O)
Z=jitter_data_1(1/X)
generalized_quantile_plot(Z[Z>0],0,100)
obj=outlier_pooled(Z,beta.max = 0.1)
contour_plot(obj$O)



X=simulate_burr(n=100,eta=1,lam=0.5,tau=1/0.5)
X=simulate_beta(n=100,p=1,q=1/0.5)
X=sort(X,decreasing = TRUE)
X[1:10]=X[10]*(X[1:10]/X[10])^(0.005)
obj=outlier_pooled(X,alpha.max = 0.6,beta.max = 0.1);obj$K0

obj=outlier_pooled_stilian(X,alpha.max = 0.6);obj$K0


