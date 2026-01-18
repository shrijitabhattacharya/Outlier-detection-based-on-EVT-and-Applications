plot_k0_xi_pos=function(dist,max.val=5){
  k0=c(3,10,20)
  for (i in 1:3){
    load(paste0('Data/Results_Version_3/',dist,'k0',0,'.Rdata'))
    K00.m=apply(K0,2,mean)
    K00.s=apply(K0,2,sd)
    load(paste0('Data/Results_Version_3/',dist,'k0',i,'.Rdata'))
    K0.m=apply(K0,c(2,3),mean)
    K0.s=apply(K0,c(2,3),sd)
    png(paste0('Figures/Results_Version_3/',dist,'L','k0',i,'.png'), width =750, height = 600)
    COL=c('black','red');l=6
    par(mar=c(5,5,3,1),cex=1.9)
    I=rbind(1:l-0.075,1:l+0.075)
#    B11=c(K0.m[1:2,2],K00.m[2],K0.m[3:5,2])
#    B12=c(K0.s[1:2,2],K00.s[2],K0.s[3:5,2])
    B11=c(K0.m[1:2,1],K00.m[1],K0.m[3:5,1])
    B12=c(K0.s[1:2,1],K00.s[1],K0.s[3:5,1])
#    B21=c(K0.m[1:2,2],K00.m[2],K0.m[3:5,2])
#    B22=c(K0.s[1:2,2],K00.s[2],K0.s[3:5,2])
    B21=c(K0.m[1:2,4],K00.m[4],K0.m[3:5,4])
    B22=c(K0.s[1:2,4],K00.s[4],K0.s[3:5,4])
    if (i==1){ylab.val=TeX('$\\widehat{k}_0^*$')}else{ylab.val=NA}
    if (i==2){xlab.val='L'}else{xlab.val=NA}    #
    #ylu=c(min((B11-B12),(B21-B22))-1,max((B11+B12),(B21+B22))+max.val)
    ylu=c(-2,24)
    plot(I[1,],B11,cex.lab=1.3,cex.axis=1.3,xlim=c(0.7,6.3),ylim =ylu,pch=21,bg=COL[1],col=COL[1],xlab=xlab.val,xaxt='n',ylab=ylab.val)
    points(I[2,],B21,pch=21,bg=COL[2],col=COL[2])
    axis(1,1:6,labels = round(c(5*1e-3,5*1e-2,1,3,10,30),3),cex.axis=1.3)
    for (j in 1:l){
      segments(I[1,j],B11[j]-B12[j],I[1,j],B11[j]+B12[j],lwd = 1.2,lty=2,col=COL[1])
      segments(I[1,j]-0.05,B11[j]-B12[j],I[1,j]+0.05,B11[j]-B12[j],lwd = 1.2,lty=1,col=COL[1])
      segments(I[1,j]-0.05,B11[j]+B12[j],I[1,j]+0.05,B11[j]+B12[j],lwd = 1.2,lty=1,col=COL[1])
      segments(I[2,j],B21[j]-B22[j],I[2,j],B21[j]+B22[j],lwd = 1.2,lty=2,col=COL[2])
      segments(I[2,j]-0.05,B21[j]-B22[j],I[2,j]+0.05,B21[j]-B22[j],lwd = 1.2,lty=1,col=COL[2])
      segments(I[2,j]-0.05,B21[j]+B22[j],I[2,j]+0.05,B21[j]+B22[j],lwd = 1.2,lty=1,col=COL[2])
    }
    if (i==1){
      legend('top',horiz = T,legend = TeX(c('EV+','EV','T')),cex=1.3,col=c(COL,'green'),lty=c(2,2,2),lwd=c(1.2,1.2,1.2),pch=c(21,21,NA),pt.bg=c(COL,NA))
    }
    segments(min(I)-1,k0[i],max(I)+1,k0[i],lty=2,col='green')
    dev.off()
    png(paste0('Figures/Results_Version_3/',dist,'C','k0',i,'.png'), width =750, height = 600)
    COL=c('black','red');l=6
    par(mar=c(5,5,3,1),cex=1.9)
    I=rbind(1:l-0.075,1:l+0.075)
    B11=c(K0.m[6:7,1],K00.m[1],K0.m[8:10,1])
    B12=c(K0.s[6:7,1],K00.s[1],K0.s[8:10,1])
#    B11=c(K0.m[6:7,2],K00.m[2],K0.m[8:10,2])
#    B12=c(K0.s[6:7,2],K00.s[2],K0.s[8:10,2])
#    B21=c(K0.m[6:7,2],K00.m[2],K0.m[8:10,2])
#    B22=c(K0.s[6:7,2],K00.s[2],K0.s[8:10,2])
    B21=c(K0.m[6:7,4],K00.m[2],K0.m[8:10,4])
    B22=c(K0.s[6:7,4],K00.s[2],K0.s[8:10,4])
    if (i==1){ylab.val=TeX('$\\widehat{k}_0^*$')}else{ylab.val=NA}
    if (i==2){xlab.val='C'}else{xlab.val=NA}
    #ylu=c(min((B11-B12),(B21-B22))-1,max((B11+B12),(B21+B22))+max.val)
    ylu=c(-2,24)
    plot(I[1,],B11,cex.lab=1.3,cex.axis=1.3,xlim=c(0.7,6.3),ylim = ylu,pch=21,bg=COL[1],col=COL[1],xlab=xlab.val,xaxt='n',ylab=ylab.val)
    points(I[2,],B21,pch=21,bg=COL[2],col=COL[2])
    axis(1,1:6,labels = round(c(1e-3,1e-2,1,10,50,200),3),cex.axis=1.3)
    for (j in 1:l){
      segments(I[1,j],B11[j]-B12[j],I[1,j],B11[j]+B12[j],lwd = 1.2,lty=2,col=COL[1])
      segments(I[1,j]-0.05,B11[j]-B12[j],I[1,j]+0.05,B11[j]-B12[j],lwd = 1.2,lty=1,col=COL[1])
      segments(I[1,j]-0.05,B11[j]+B12[j],I[1,j]+0.05,B11[j]+B12[j],lwd = 1.2,lty=1,col=COL[1])
      segments(I[2,j],B21[j]-B22[j],I[2,j],B21[j]+B22[j],lwd = 1.2,lty=2,col=COL[2])
      segments(I[2,j]-0.05,B21[j]-B22[j],I[2,j]+0.05,B21[j]-B22[j],lwd = 1.2,lty=1,col=COL[2])
      segments(I[2,j]-0.05,B21[j]+B22[j],I[2,j]+0.05,B21[j]+B22[j],lwd = 1.2,lty=1,col=COL[2])
    }
    if (i==1){
      legend('top',horiz = T,legend = TeX(c('EV+','EV','T')),cex=1.3,col=c(COL,'green'),lty=c(2,2,2),lwd=c(1.2,1.2,1.2),pch=c(21,21,NA),pt.bg=c(COL,NA))
    }
    segments(min(I)-1,k0[i],max(I)+1,k0[i],lty=2,col='green')
    dev.off()
  }
}

plot_k0_xi_pos('burr',1)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


plot_k0_xi_neg=function(dist,max.val=5){
  k0=c(3,10,20)
  for (i in 1:3){
    load(paste0('Data/Results_Version_3/',dist,'k0',0,'.Rdata'))
    K00.m=apply(K0,2,mean)
    K00.s=apply(K0,2,sd)
    load(paste0('Data/Results_Version_3/',dist,'k0',i,'.Rdata'))
    K0.m=apply(K0,c(2,3),mean)
    K0.s=apply(K0,c(2,3),sd)
    png(paste0('Figures/Results_Version_3/',dist,'L','k0',i,'.png'), width =750, height = 600)
    COL=c('black','red','blue');l=6
    par(mar=c(5,5,3,1),cex=1.9)
    I=rbind(1:l-0.15,1:l,1:l+0.15)
    B11=c(K0.m[1:2,3],K00.m[3],K0.m[3:5,3])
    B12=c(K0.s[1:2,3],K00.s[3],K0.s[3:5,3])
#    B11=c(K0.m[1:2,2],K00.m[2],K0.m[3:5,2])
#    B12=c(K0.s[1:2,2],K00.s[2],K0.s[3:5,2])
#    B21=c(K0.m[1:2,2],K00.m[2],K0.m[3:5,2])
#    B22=c(K0.s[1:2,2],K00.s[2],K0.s[3:5,2])
    B21=c(K0.m[1:2,5],K00.m[2],K0.m[3:5,5])
    B22=c(K0.s[1:2,5],K00.s[2],K0.s[3:5,5])
    B31=c(K0.m[1:2,4],K00.m[2],K0.m[3:5,4])
    B32=c(K0.s[1:2,4],K00.s[2],K0.s[3:5,4])
    if (i==1){ylab.val=TeX('$\\widehat{k}_0^*$')}else{ylab.val=NA}
    if (i==2){xlab.val='L'}else{xlab.val=NA}    #
    #ylu=c(min((B11-B12),(B21-B22),(B31-B32))-1,max((B11+B12),(B21+B22),(B31+B32))+max.val)
    ylu=c(-5,28)
    plot(I[1,],B11,cex.lab=1.3,cex.axis=1.3,xlim=c(0.7,6.3),ylim =ylu,pch=21,bg=COL[1],col=COL[1],xlab=xlab.val,xaxt='n',ylab=ylab.val)
    points(I[2,],B21,pch=21,bg=COL[2],col=COL[2])
    points(I[3,],B31,pch=21,bg=COL[3],col=COL[3])
    axis(1,1:6,labels = round(c(5*1e-3,5*1e-2,1,3,10,30),3),cex.axis=1.3)
    for (j in 1:l){
      segments(I[1,j],B11[j]-B12[j],I[1,j],B11[j]+B12[j],lwd = 1.2,lty=2,col=COL[1])
      segments(I[1,j]-0.05,B11[j]-B12[j],I[1,j]+0.05,B11[j]-B12[j],lwd = 1.2,lty=1,col=COL[1])
      segments(I[1,j]-0.05,B11[j]+B12[j],I[1,j]+0.05,B11[j]+B12[j],lwd = 1.2,lty=1,col=COL[1])
      segments(I[2,j],B21[j]-B22[j],I[2,j],B21[j]+B22[j],lwd = 1.2,lty=2,col=COL[2])
      segments(I[2,j]-0.05,B21[j]-B22[j],I[2,j]+0.05,B21[j]-B22[j],lwd = 1.2,lty=1,col=COL[2])
      segments(I[2,j]-0.05,B21[j]+B22[j],I[2,j]+0.05,B21[j]+B22[j],lwd = 1.2,lty=1,col=COL[2])
      segments(I[3,j],B31[j]-B32[j],I[3,j],B31[j]+B32[j],lwd = 1.2,lty=2,col=COL[3])
      segments(I[3,j]-0.05,B31[j]-B32[j],I[3,j]+0.05,B31[j]-B32[j],lwd = 1.2,lty=1,col=COL[3])
      segments(I[3,j]-0.05,B31[j]+B32[j],I[3,j]+0.05,B31[j]+B32[j],lwd = 1.2,lty=1,col=COL[3])
      }
    if (i==1){
      legend('top',horiz = T,legend = TeX(c('CL','EV','E0','T')),cex=1.3,col=c(COL,'green'),lty=c(2,2,2,2),lwd=c(1.2,1.2,1.2,1.2),pch=c(21,21,21,NA),pt.bg=c(COL,NA))
    }
    segments(min(I)-1,k0[i],max(I)+1,k0[i],lty=2,col='green')
    dev.off()
    png(paste0('Figures/Results_Version_3/',dist,'C','k0',i,'.png'), width =750, height = 600)
    COL=c('black','red','blue');l=6
    par(mar=c(5,5,3,1),cex=1.9)
    I=rbind(1:l-0.15,1:l,1:l+0.15)
#    B11=c(K0.m[6:7,2],K00.m[2],K0.m[8:10,2])
#    B12=c(K0.s[6:7,2],K00.s[2],K0.s[8:10,2])
     B11=c(K0.m[6:7,3],K00.m[3],K0.m[8:10,3])
     B12=c(K0.s[6:7,3],K00.s[3],K0.s[8:10,3])
#    B21=c(K0.m[6:7,2],K00.m[2],K0.m[8:10,2])
#    B22=c(K0.s[6:7,2],K00.s[2],K0.s[8:10,2])
    B21=c(K0.m[6:7,5],K00.m[5],K0.m[8:10,5])
    B22=c(K0.s[6:7,5],K00.s[5],K0.s[8:10,5])
    B31=c(K0.m[6:7,4],K00.m[2],K0.m[8:10,4])
    B32=c(K0.s[6:7,4],K00.s[2],K0.s[8:10,4])
    if (i==1){ylab.val=TeX('$\\widehat{k}_0^*$')}else{ylab.val=NA}
    if (i==2){xlab.val='C'}else{xlab.val=NA}
    ylu=c(min((B11-B12),(B21-B22))-1,max((B11+B12),(B21+B22))+max.val)
    ylu=c(-5,28)
    plot(I[1,],B11,cex.lab=1.3,cex.axis=1.3,xlim=c(0.7,6.3),ylim = ylu,pch=21,bg=COL[1],col=COL[1],xlab=xlab.val,xaxt='n',ylab=ylab.val)
    points(I[2,],B21,pch=21,bg=COL[2],col=COL[2])
    points(I[3,],B31,pch=21,bg=COL[3],col=COL[3])
    axis(1,1:6,labels = round(c(1e-3,1e-2,1,10,50,200),3),cex.axis=1.3)
    for (j in 1:l){
      segments(I[1,j],B11[j]-B12[j],I[1,j],B11[j]+B12[j],lwd = 1.2,lty=2,col=COL[1])
      segments(I[1,j]-0.05,B11[j]-B12[j],I[1,j]+0.05,B11[j]-B12[j],lwd = 1.2,lty=1,col=COL[1])
      segments(I[1,j]-0.05,B11[j]+B12[j],I[1,j]+0.05,B11[j]+B12[j],lwd = 1.2,lty=1,col=COL[1])
      segments(I[2,j],B21[j]-B22[j],I[2,j],B21[j]+B22[j],lwd = 1.2,lty=2,col=COL[2])
      segments(I[2,j]-0.05,B21[j]-B22[j],I[2,j]+0.05,B21[j]-B22[j],lwd = 1.2,lty=1,col=COL[2])
      segments(I[2,j]-0.05,B21[j]+B22[j],I[2,j]+0.05,B21[j]+B22[j],lwd = 1.2,lty=1,col=COL[2])
      segments(I[3,j],B31[j]-B32[j],I[3,j],B31[j]+B32[j],lwd = 1.2,lty=2,col=COL[3])
      segments(I[3,j]-0.05,B31[j]-B32[j],I[3,j]+0.05,B31[j]-B32[j],lwd = 1.2,lty=1,col=COL[3])
      segments(I[3,j]-0.05,B31[j]+B32[j],I[3,j]+0.05,B31[j]+B32[j],lwd = 1.2,lty=1,col=COL[3])
      }
    if (i==1){
      legend('top',horiz = T,legend = TeX(c('CL','EV','E0','T')),cex=1.3,col=c(COL,'green'),lty=c(2,2,2,2),lwd=c(1.2,1.2,1.2,1.2),pch=c(21,21,21,NA),pt.bg=c(COL,NA))
    }
    segments(min(I)-1,k0[i],max(I)+1,k0[i],lty=2,col='green')
    dev.off()
  }
}


plot_k0_xi_neg('weib',1)



plot_k0_xi_0=function(){
  k0=c(3,10,20)
  for (i in 1:3){
    load(paste0('Data/Results_Version_2/',dist,'k0',0,'.Rdata'))
    K00.m=apply(K0,2,mean)
    K00.s=apply(K0,2,sd)
    load(paste0('Data/Results_Version_2/',dist,'k0',i,'.Rdata'))
    K0.m=apply(K0,c(2,3),mean)
    K0.s=apply(K0,c(2,3),sd)
    png(paste0('Figures/Results_Version_1/',dist,'L','k0',i,'.png'), width =700, height = 600)
    COL=c('black','red','blue');l=6
    par(mar=c(5,5,3,1),cex=1.9)
    I=rbind(1:l-0.15,1:l,1:l+0.15)
    B11=c(K0.m[1:2,3],K00.m[3],K0.m[3:5,3])
    B12=c(K0.s[1:2,3],K00.s[3],K0.s[3:5,3])
    B21=c(K0.m[1:2,2],K00.m[2],K0.m[3:5,2])
    B22=c(K0.s[1:2,2],K00.s[2],K0.s[3:5,2])
    B31=c(K0.m[1:2,4],K00.m[2],K0.m[3:5,4])
    B32=c(K0.s[1:2,4],K00.s[2],K0.s[3:5,4])
    if (i==1){ylab.val=TeX('$\\widehat{k}_0$')}else{ylab.val=NA}
    if (i==2){xlab.val='L'}else{xlab.val=NA}    #
    #ylu=c(min((B11-B12),(B21-B22),(B31-B32))-1,max((B11+B12),(B21+B22),(B31+B32))+max.val)
    ylu=c(-5,30)
    plot(I[1,],B11,cex.lab=1.3,cex.axis=1.3,xlim=c(0.7,6.3),ylim =ylu,pch=21,bg=COL[1],col=COL[1],xlab=xlab.val,xaxt='n',ylab=ylab.val)
    points(I[2,],B21,pch=21,bg=COL[2],col=COL[2])
    points(I[3,],B31,pch=21,bg=COL[3],col=COL[3])
    axis(1,1:6,labels = round(c(5*1e-3,5*1e-2,1,3,10,30),3),cex.axis=1.3)
    for (j in 1:l){
      segments(I[1,j],B11[j]-B12[j],I[1,j],B11[j]+B12[j],lwd = 1.2,lty=2,col=COL[1])
      segments(I[1,j]-0.05,B11[j]-B12[j],I[1,j]+0.05,B11[j]-B12[j],lwd = 1.2,lty=1,col=COL[1])
      segments(I[1,j]-0.05,B11[j]+B12[j],I[1,j]+0.05,B11[j]+B12[j],lwd = 1.2,lty=1,col=COL[1])
      segments(I[2,j],B21[j]-B22[j],I[2,j],B21[j]+B22[j],lwd = 1.2,lty=2,col=COL[2])
      segments(I[2,j]-0.05,B21[j]-B22[j],I[2,j]+0.05,B21[j]-B22[j],lwd = 1.2,lty=1,col=COL[2])
      segments(I[2,j]-0.05,B21[j]+B22[j],I[2,j]+0.05,B21[j]+B22[j],lwd = 1.2,lty=1,col=COL[2])
      segments(I[3,j],B31[j]-B32[j],I[3,j],B31[j]+B32[j],lwd = 1.2,lty=2,col=COL[3])
      segments(I[3,j]-0.05,B31[j]-B32[j],I[3,j]+0.05,B31[j]-B32[j],lwd = 1.2,lty=1,col=COL[3])
      segments(I[3,j]-0.05,B31[j]+B32[j],I[3,j]+0.05,B31[j]+B32[j],lwd = 1.2,lty=1,col=COL[3])
    }
    if (i==1){
      legend('top',horiz = T,legend = TeX(c('CL','EV','E0','T')),cex=1.3,col=c(COL,'green'),lty=c(2,2,2,2),lwd=c(1.2,1.2,1.2,1.2),pch=c(21,21,21,NA),pt.bg=c(COL,NA))
    }
    segments(min(I)-1,k0[i],max(I)+1,k0[i],lty=2,col='green')
    dev.off()
    png(paste0('Figures/Results_Version_1/',dist,'C','k0',i,'.png'), width =700, height = 600)
    COL=c('black','red','blue');l=6
    par(mar=c(5,5,3,1),cex=1.9)
    I=rbind(1:l-0.15,1:l,1:l+0.15)
    B11=c(K0.m[6:7,3],K00.m[3],K0.m[8:10,3])
    B12=c(K0.s[6:7,3],K00.s[3],K0.s[8:10,3])
    B21=c(K0.m[6:7,2],K00.m[2],K0.m[8:10,2])
    B22=c(K0.s[6:7,2],K00.s[2],K0.s[8:10,2])
    B31=c(K0.m[6:7,4],K00.m[2],K0.m[8:10,4])
    B32=c(K0.s[6:7,4],K00.s[2],K0.s[8:10,4])
    if (i==1){ylab.val=TeX('$\\widehat{k}_0$')}else{ylab.val=NA}
    if (i==2){xlab.val='C'}else{xlab.val=NA}
    ylu=c(min((B11-B12),(B21-B22))-1,max((B11+B12),(B21+B22))+max.val)
    ylu=c(-5,30)
    plot(I[1,],B11,cex.lab=1.3,cex.axis=1.3,xlim=c(0.7,6.3),ylim = ylu,pch=21,bg=COL[1],col=COL[1],xlab=xlab.val,xaxt='n',ylab=ylab.val)
    points(I[2,],B21,pch=21,bg=COL[2],col=COL[2])
    points(I[3,],B31,pch=21,bg=COL[3],col=COL[3])
    axis(1,1:6,labels = round(c(1e-3,1e-2,1,10,50,200),3),cex.axis=1.3)
    for (j in 1:l){
      segments(I[1,j],B11[j]-B12[j],I[1,j],B11[j]+B12[j],lwd = 1.2,lty=2,col=COL[1])
      segments(I[1,j]-0.05,B11[j]-B12[j],I[1,j]+0.05,B11[j]-B12[j],lwd = 1.2,lty=1,col=COL[1])
      segments(I[1,j]-0.05,B11[j]+B12[j],I[1,j]+0.05,B11[j]+B12[j],lwd = 1.2,lty=1,col=COL[1])
      segments(I[2,j],B21[j]-B22[j],I[2,j],B21[j]+B22[j],lwd = 1.2,lty=2,col=COL[2])
      segments(I[2,j]-0.05,B21[j]-B22[j],I[2,j]+0.05,B21[j]-B22[j],lwd = 1.2,lty=1,col=COL[2])
      segments(I[2,j]-0.05,B21[j]+B22[j],I[2,j]+0.05,B21[j]+B22[j],lwd = 1.2,lty=1,col=COL[2])
      segments(I[3,j],B31[j]-B32[j],I[3,j],B31[j]+B32[j],lwd = 1.2,lty=2,col=COL[3])
      segments(I[3,j]-0.05,B31[j]-B32[j],I[3,j]+0.05,B31[j]-B32[j],lwd = 1.2,lty=1,col=COL[3])
      segments(I[3,j]-0.05,B31[j]+B32[j],I[3,j]+0.05,B31[j]+B32[j],lwd = 1.2,lty=1,col=COL[3])
    }
    if (i==1){
      legend('top',horiz = T,legend = TeX(c('CL','EV','E0','T')),cex=1.3,col=c(COL,'green'),lty=c(2,2,2,2),lwd=c(1.2,1.2,1.2,1.2),pch=c(21,21,21,NA),pt.bg=c(COL,NA))
    }
    segments(min(I)-1,k0[i],max(I)+1,k0[i],lty=2,col='green')
    dev.off()
  }
}


plot_k0_xi_neg('norm',2)



plot_k0_xi_lnorm=function(max.val=2){
  dist='lnorm'
  k0=c(3,10,20)
  for (i in 1:3){
    load(paste0('Data/',dist,'k0',0,'.Rdata'))
    K00.m=apply(K0,2,mean)
    K00.s=apply(K0,2,sd)
    K00.mse=apply(K0**2,2,mean)
    load(paste0('Data/',dist,'k0',i,'.Rdata'))
    K0.m=apply(K0,c(2,3),mean)
    K0.s=apply(K0,c(2,3),sd)
    K0.mse=apply((K0-k0[i])**2,c(2,3),mean)
    png(paste0('Figures/',dist,'L','k0',i,'.png'), width =750, height = 600)
    COL=c('black','red','blue');l=6
    par(mar=c(5,5,3,1),cex=1.9)
    I=rbind(1:l-0.15,1:l,1:l+0.15)
    B11=c(K0.m[1:2,3],K00.m[3],K0.m[3:5,3])
    B12=c(K0.s[1:2,3],K00.s[3],K0.s[3:5,3])
    B21=c(K0.m[1:2,1],K00.m[1],K0.m[3:5,1])
    B22=c(K0.s[1:2,1],K00.s[1],K0.s[3:5,1])
    B31=c(K0.m[1:2,2],K00.m[2],K0.m[3:5,2])
    B32=c(K0.s[1:2,2],K00.s[2],K0.s[3:5,2])
    if (i==1){ylab.val=TeX('$\\widehat{k}_0^*$')}else{ylab.val=NA}
    if (i==2){xlab.val='L'}else{xlab.val=NA}    #
    ylu=c(-5,28)
    plot(I[1,],B11,cex.lab=1.3,cex.axis=1.3,xlim=c(0.7,6.3),ylim =ylu,pch=21,bg=COL[1],col=COL[1],xlab=xlab.val,xaxt='n',ylab=ylab.val)
    points(I[2,],B21,pch=21,bg=COL[2],col=COL[2])
    points(I[3,],B31,pch=21,bg=COL[3],col=COL[3])
    axis(1,1:6,labels = round(c(5*1e-3,5*1e-2,1,3,10,30),3),cex.axis=1.3)
    for (j in 1:l){
      segments(I[1,j],B11[j]-B12[j],I[1,j],B11[j]+B12[j],lwd = 1.2,lty=2,col=COL[1])
      segments(I[1,j]-0.05,B11[j]-B12[j],I[1,j]+0.05,B11[j]-B12[j],lwd = 1.2,lty=1,col=COL[1])
      segments(I[1,j]-0.05,B11[j]+B12[j],I[1,j]+0.05,B11[j]+B12[j],lwd = 1.2,lty=1,col=COL[1])
      segments(I[2,j],B21[j]-B22[j],I[2,j],B21[j]+B22[j],lwd = 1.2,lty=2,col=COL[2])
      segments(I[2,j]-0.05,B21[j]-B22[j],I[2,j]+0.05,B21[j]-B22[j],lwd = 1.2,lty=1,col=COL[2])
      segments(I[2,j]-0.05,B21[j]+B22[j],I[2,j]+0.05,B21[j]+B22[j],lwd = 1.2,lty=1,col=COL[2])
      segments(I[3,j],B31[j]-B32[j],I[3,j],B31[j]+B32[j],lwd = 1.2,lty=2,col=COL[3])
      segments(I[3,j]-0.05,B31[j]-B32[j],I[3,j]+0.05,B31[j]-B32[j],lwd = 1.2,lty=1,col=COL[3])
      segments(I[3,j]-0.05,B31[j]+B32[j],I[3,j]+0.05,B31[j]+B32[j],lwd = 1.2,lty=1,col=COL[3])
    }
    if (i==1){
      legend('top',horiz = T,legend = TeX(c('CL','EV','E0','T')),cex=1.3,col=c(COL,'green'),lty=c(2,2,2,2),lwd=c(1.2,1.2,1.2,1.2),pch=c(21,21,21,NA),pt.bg=c(COL,NA))
    }
    segments(min(I)-1,k0[i],max(I)+1,k0[i],lty=2,col='green')
    dev.off()
    png(paste0('Figures/',dist,'C','k0',i,'.png'), width =750, height = 600)
    COL=c('black','red','blue');l=6
    par(mar=c(5,5,3,1),cex=1.9)
    I=rbind(1:l-0.15,1:l,1:l+0.15)
    B11=c(K0.m[6:7,3],K00.m[3],K0.m[8:10,3])
    B12=c(K0.s[6:7,3],K00.s[3],K0.s[8:10,3])
    B21=c(K0.m[6:7,1],K00.m[1],K0.m[8:10,1])
    B22=c(K0.s[6:7,1],K00.s[1],K0.s[8:10,1])
    B31=c(K0.m[6:7,2],K00.m[2],K0.m[8:10,2])
    B32=c(K0.s[6:7,2],K00.s[2],K0.s[8:10,2])
    if (i==1){ylab.val=TeX('$\\widehat{k}_0^*$')}else{ylab.val=NA}
    if (i==2){xlab.val='C'}else{xlab.val=NA}
    ylu=c(min((B11-B12),(B21-B22))-1,max((B11+B12),(B21+B22))+max.val)
    ylu=c(-5,28)
    plot(I[1,],B11,cex.lab=1.3,cex.axis=1.3,xlim=c(0.7,6.3),ylim = ylu,pch=21,bg=COL[1],col=COL[1],xlab=xlab.val,xaxt='n',ylab=ylab.val)
    points(I[2,],B21,pch=21,bg=COL[2],col=COL[2])
    points(I[3,],B31,pch=21,bg=COL[3],col=COL[3])
    axis(1,1:6,labels = round(c(1e-3,1e-2,1,10,50,200),3),cex.axis=1.3)
    for (j in 1:l){
      segments(I[1,j],B11[j]-B12[j],I[1,j],B11[j]+B12[j],lwd = 1.2,lty=2,col=COL[1])
      segments(I[1,j]-0.05,B11[j]-B12[j],I[1,j]+0.05,B11[j]-B12[j],lwd = 1.2,lty=1,col=COL[1])
      segments(I[1,j]-0.05,B11[j]+B12[j],I[1,j]+0.05,B11[j]+B12[j],lwd = 1.2,lty=1,col=COL[1])
      segments(I[2,j],B21[j]-B22[j],I[2,j],B21[j]+B22[j],lwd = 1.2,lty=2,col=COL[2])
      segments(I[2,j]-0.05,B21[j]-B22[j],I[2,j]+0.05,B21[j]-B22[j],lwd = 1.2,lty=1,col=COL[2])
      segments(I[2,j]-0.05,B21[j]+B22[j],I[2,j]+0.05,B21[j]+B22[j],lwd = 1.2,lty=1,col=COL[2])
      segments(I[3,j],B31[j]-B32[j],I[3,j],B31[j]+B32[j],lwd = 1.2,lty=2,col=COL[3])
      segments(I[3,j]-0.05,B31[j]-B32[j],I[3,j]+0.05,B31[j]-B32[j],lwd = 1.2,lty=1,col=COL[3])
      segments(I[3,j]-0.05,B31[j]+B32[j],I[3,j]+0.05,B31[j]+B32[j],lwd = 1.2,lty=1,col=COL[3])
    }
    if (i==1){
      legend('top',horiz = T,legend = TeX(c('CL','EV','E0','T')),cex=1.3,col=c(COL,'green'),lty=c(2,2,2,2),lwd=c(1.2,1.2,1.2,1.2),pch=c(21,21,21,NA),pt.bg=c(COL,NA))
    }
    segments(min(I)-1,k0[i],max(I)+1,k0[i],lty=2,col='green')
    dev.off()
  }
}


plot_k0_xi_mixture_new=function(i,k){
  val_xi=c(0.5,2,10)
  xi=val_xi[i]
  val_c=c(10,50,200)
  val_rho=c(2/100,5/100,10/100)
  rho=val_rho[k]
  K00.m=c();
  K00.s=c();
  K00.mse=c();
  for (j in 1:3){
    c=val_c[j]
    load(paste0('Data/mixture_xi_',xi,'_c_',c,'_rho_',rho,'_outlier.Rdata'))
    K00.m = rbind(K00.m,c(mean(K0_1)/10,mean(K0_2)/10,mean(K0_3)/10))
    K00.s = rbind(K00.s,c(sd(K0_1)/10,sd(K0_2)/10,sd(K0_3)/10))
    K00.mse=rbind(K00.mse,c(mean((K0_2/10-rho*100)**2),mean((K0_1/10-rho*100)**2)))
  }
  png(paste0('Figures/mixture_xi_',xi,'_rho_',rho,'_outlier.png'), width =750, height = 600)
  COL=c('red','blue','black','green');l=3
  par(mar=c(5,5,3,1),cex=1.9)
  I=rbind(1:l-0.15,1:l,1:l+0.15)
  B11=K00.m[,1]
  B12=K00.s[,1]
  B21=K00.m[,2]
  B22=K00.s[,2]
  B31=K00.m[,3]
  B32=K00.s[,3]
  if (i==2){ylab.val=TeX('$\\widehat{p}$')}else{ylab.val=NA}
  if (i==3 & k==2){xlab.val='c'}else{xlab.val=NA}    #
  ylu=c(-rho*100,4*rho*100)
  plot(I[1,],B11,cex.lab=1.3,cex.axis=1.3,xlim=c(0.2,3.8),ylim =ylu,pch=21,bg=COL[1],col=COL[1],xlab=xlab.val,xaxt='n',ylab=ylab.val)
  points(I[2,],B21,pch=21,bg=COL[2],col=COL[2])
  points(I[3,],B31,pch=21,bg=COL[3],col=COL[3])
  axis(1,1:3,labels = round(c(10,50,200),3),cex.axis=1.3)
  for (j in 1:l){
    segments(I[1,j],B11[j]-B12[j],I[1,j],B11[j]+B12[j],lwd = 1.2,lty=2,col=COL[1])
    segments(I[1,j]-0.05,B11[j]-B12[j],I[1,j]+0.05,B11[j]-B12[j],lwd = 1.2,lty=1,col=COL[1])
    segments(I[1,j]-0.05,B11[j]+B12[j],I[1,j]+0.05,B11[j]+B12[j],lwd = 1.2,lty=1,col=COL[1])
    segments(I[2,j],B21[j]-B22[j],I[2,j],B21[j]+B22[j],lwd = 1.2,lty=2,col=COL[2])
    segments(I[2,j]-0.05,B21[j]-B22[j],I[2,j]+0.05,B21[j]-B22[j],lwd = 1.2,lty=1,col=COL[2])
    segments(I[2,j]-0.05,B21[j]+B22[j],I[2,j]+0.05,B21[j]+B22[j],lwd = 1.2,lty=1,col=COL[2])
    segments(I[3,j],B31[j]-B32[j],I[3,j],B31[j]+B32[j],lwd = 1.2,lty=2,col=COL[3])
    segments(I[3,j]-0.05,B31[j]-B32[j],I[3,j]+0.05,B31[j]-B32[j],lwd = 1.2,lty=1,col=COL[3])
    segments(I[3,j]-0.05,B31[j]+B32[j],I[3,j]+0.05,B31[j]+B32[j],lwd = 1.2,lty=1,col=COL[3])
  }
  if (k==1){
    legend('top',horiz = T,legend = TeX(c('EV','E0','CL','T')),cex=1.3,col=COL,lty=c(2,2,2,2),lwd=c(1.2,1.2,1.2,1.2),pch=c(21,21,21,NA),pt.bg=c(COL[1:3],NA))
  }
  segments(min(I)-1,rho*100,max(I)+1,rho*100,lty=2,col='green')
  dev.off()
}





for (i in 1:3){
  for (k in 1:3){
    plot_k0_xi_mixture_new(i,k)
  }
}


i=1
dist='weib'
load(paste0('Data/Results_Version_3/',dist,'k0',0,'.Rdata'))
K00.mse=apply(K0**2,2,mean)
load(paste0('Data/Results_Version_3/',dist,'k0',i,'.Rdata'))
K0.mse=apply((K0-k0[i])**2,c(2,3),mean)
#png(paste0('Data/',dist,'L','k0',i,'.png'), width =700, height = 500)
B1=sqrt(c(K0.mse[1:2,3],K00.mse[3],K0.mse[3:5,3]))
B2=sqrt(c(K0.mse[1:2,2],K00.mse[2],K0.mse[3:5,2]))
plot(B1)
lines(B1,lty=2)
points(B2,col='red')
lines(B2,col='red',lty=2)
B1=sqrt(c(K0.mse[6:7,3],K00.mse[3],K0.mse[8:10,3]))
B2=sqrt(c(K0.mse[6:7,2],K00.mse[2],K0.mse[8:10,2]))
plot(B1)
lines(B1,lty=2)
points(B2,col='red')
lines(B2,col='red',lty=2)



plot_k0_xi_mixture=function(i,k){
  val_xi=c(0.5,2,10)
  xi=val_xi[i]
  val_c=c(10,50,200)
  val_rho=c(2/100,5/100,10/100)
  rho=val_rho[k]
  K00.m=c();
  K00.s=c();
  for (j in 1:3){
    c=val_c[j]
    load(paste0('Data/mixture_xi_',xi,'_c_',c,'_rho_',rho,'_outlier.Rdata'))
    K00.m = rbind(K00.m,c(mean(K0_1)/10,mean(K0_2)/10,mean(K0_3)/10,mean(K0_4)/10))
    K00.s = rbind(K00.s,c(sd(K0_1)/10,sd(K0_2)/10,sd(K0_3)/10,sd(K0_4)/10))
  }
  png(paste0('Figures/mixture_xi_',xi,'_rho_',rho,'_outlier.png'), width =750, height = 600)
  COL=c('red','blue','black','green');l=3
  par(mar=c(5,5,3,1),cex=1.9)
  I=rbind(1:l-0.3,1:l-0.1,1:l+0.1,1:l+0.3)
  B11=K00.m[,1]
  B12=K00.s[,1]
  B21=K00.m[,2]
  B22=K00.s[,2]
  B31=K00.m[,3]
  B32=K00.s[,3]
  B41=K00.m[,4]
  B42=K00.s[,4]
  if (i==2){ylab.val=TeX('$\\hat{p}$')}else{ylab.val=NA}
  if (k==2){xlab.val='C'}else{xlab.val=NA}    #
  ylu=c(-rho*100,4*rho*100)
  plot(I[1,],B11,cex.lab=1.3,cex.axis=1.3,xlim=c(0.2,3.8),ylim =ylu,pch=21,bg=COL[1],col=COL[1],xlab=xlab.val,xaxt='n',ylab=ylab.val)
  points(I[2,],B21,pch=21,bg=COL[2],col=COL[2])
  points(I[3,],B31,pch=21,bg=COL[3],col=COL[3])
  points(I[4,],B41,pch=21,bg=COL[4],col=COL[4])
  axis(1,1:3,labels = round(c(10,50,200),3),cex.axis=1.3)
  for (j in 1:l){
    segments(I[1,j],B11[j]-B12[j],I[1,j],B11[j]+B12[j],lwd = 1.2,lty=2,col=COL[1])
    segments(I[1,j]-0.05,B11[j]-B12[j],I[1,j]+0.05,B11[j]-B12[j],lwd = 1.2,lty=1,col=COL[1])
    segments(I[1,j]-0.05,B11[j]+B12[j],I[1,j]+0.05,B11[j]+B12[j],lwd = 1.2,lty=1,col=COL[1])
    segments(I[2,j],B21[j]-B22[j],I[2,j],B21[j]+B22[j],lwd = 1.2,lty=2,col=COL[2])
    segments(I[2,j]-0.05,B21[j]-B22[j],I[2,j]+0.05,B21[j]-B22[j],lwd = 1.2,lty=1,col=COL[2])
    segments(I[2,j]-0.05,B21[j]+B22[j],I[2,j]+0.05,B21[j]+B22[j],lwd = 1.2,lty=1,col=COL[2])
    segments(I[3,j],B31[j]-B32[j],I[3,j],B31[j]+B32[j],lwd = 1.2,lty=2,col=COL[3])
    segments(I[3,j]-0.05,B31[j]-B32[j],I[3,j]+0.05,B31[j]-B32[j],lwd = 1.2,lty=1,col=COL[3])
    segments(I[3,j]-0.05,B31[j]+B32[j],I[3,j]+0.05,B31[j]+B32[j],lwd = 1.2,lty=1,col=COL[3])
    segments(I[4,j],B41[j]-B42[j],I[4,j],B41[j]+B42[j],lwd = 1.2,lty=2,col=COL[4])
    segments(I[4,j]-0.05,B41[j]-B42[j],I[4,j]+0.05,B41[j]-B42[j],lwd = 1.2,lty=1,col=COL[4])
    segments(I[4,j]-0.05,B41[j]+B42[j],I[4,j]+0.05,B41[j]+B42[j],lwd = 1.2,lty=1,col=COL[4])
  }
  if (k==1){
    legend('top',horiz = T,legend = TeX(c('EV','E0','CL','T')),cex=1.3,col=COL,lty=c(2,2,2,2),lwd=c(1.2,1.2,1.2,1.2),pch=c(21,21,21,21),pt.bg=COL)
  }
  dev.off()
}

