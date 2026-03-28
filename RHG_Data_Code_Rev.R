setwd('C:/Users/shrij/Dropbox/Robust_Hill_Generalized/')
source('RHG_Source_Code_Rev.R')
source('multivariate_outlier_generation.R')

load('MaxPrecipFallFrance.RData')
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

## Auto Data
D=read.table('auto-mpg.data')$V5
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


## Condroz
D=c(as.matrix(read.csv('calcium.csv')))
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










