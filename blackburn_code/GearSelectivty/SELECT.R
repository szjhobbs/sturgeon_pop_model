#######################
### SELECT FUNCTION ###
#######################

NetFit=function(Data,Meshsize,x0,rtype="norm.loc",rel.power=NULL) {
  if(sum(sort(Meshsize)==Meshsize)!=length(Meshsize))
    stop("Mesh size must be ascending order")
  if(is.null(rel.power)) rel.power=rep(1,length(Meshsize))
  Counts=Data[,-1]
  if(ncol(Counts)!=length(Meshsize))
    stop("Number of mesh sizes should be ",ncol(Counts))
  CountPropns=Counts/apply(Counts,1,sum,na.rm=TRUE)
  fullfit.l=sum(Counts*log(CountPropns),na.rm=TRUE)
  r=selncurves(rtype) #Get selection curve function
  fit=optim(x0,nllhood,Data=Data,Meshsize=Meshsize,r=r,rel.power=rel.power,
            hessian=T,control=list(trace=F))
  cat("Parameters=",fit$par,",    Deviance=",2*(fullfit.l+fit$value),"\n")
  invisible(c(fit,deviance=deviance,rtype=rtype,rel.power=list(rel.power),
              Meshsize=list(Meshsize),Data=list(Data))) }

nllhood=function(theta,Data,Meshsize,r,rel.power) {
  lens=Data[,1]; Counts=Data[,-1]
  rmatrix=outer(lens,Meshsize,r,theta)
  rmatrix[is.na(Counts)]=NA  #No fitted retention for missing meshsizes
  rmatrix=t(t(rmatrix)*rel.power)
  phi=rmatrix/apply(rmatrix,1,sum,na.rm=TRUE)
  nll=-sum(Counts*log(phi),na.rm=TRUE)
  return(nll) }

Estimates=function(fit) {
  require("msm")
  x=fit$par; varx=solve(fit$hess) 
  names=c("Mode(mesh1)","Std dev.(mesh1)")
  switch(fit$rtype,
         "norm.loc"={ pars=x; varpars=varx },
         "norm.sca"={ pars=x; varpars=varx },
         "lognorm"={
           pars=c(exp(x[1]-x[2]^2),sqrt(exp(2*x[1]+x[2]^2)*(exp(x[2]^2)-1)))
           varpars=deltamethod(list(~exp(x1-x2^2),
                                    ~sqrt(exp(2*x1+x2^2)*(exp(x2^2)-1))),x,varx,ses=F)},
         "binorm.sca"={
           pars=c(x[1:4],exp(x[5])/(1+exp(x[5])))
           names=c("Mode1(mesh1)","Std dev.1(mesh1)",
                   "Mode2(mesh1)","Std dev.2(mesh1)","P(mode1)")
           varpars=deltamethod(list(~x1,~x2,~x3,~x4,~exp(x5)/(1+exp(x5))),
                               x,varx,ses=F)},
         "bilognorm"={
           pars=c(exp(x[1]-x[2]^2),sqrt(exp(2*x[1]+x[2]^2)*(exp(x[2]^2)-1)),
                  exp(x[3]-x[4]^2),sqrt(exp(2*x[3]+x[4]^2)*(exp(x[4]^2)-1)),
                  exp(x[5])/(1+exp(x[5])))
           names=c("Mode1(mesh1)","Std dev.1(mesh1)",
                   "Mode2(mesh1)","Std dev.2(mesh1)","P(mode1)")
           varpars=deltamethod(
             list(~exp(x1-x2^2),~sqrt(exp(2*x1+x2^2)*(exp(x2^2)-1)),
                  ~exp(x3-x4^2),~sqrt(exp(2*x3+x4^2)*(exp(x4^2)-1)),
                  ~exp(x5)/(1+exp(x5))),x,varx,ses=F)},  
         "tt.logistic"={
           pars=c(-x[1]/x[2],2*(log(3))/x[2],exp(x[3])/(1+exp(x[3])))
           names=c("L50","SR","p")
           varpars=deltamethod(list(~-x1/x2,~2*log(3)/x2,~exp(x3)/(1+exp(x3))),
                               x,varx,ses=F)},
         stop(paste("\n",fit$rtype, "not recognised, possible curve types are \n", 
                    "\"norm.loc\", \"norm.sca\", \"lognorm\" \n", 
                    "\"binorm.sca\", \"bilognorm\", and \"tt.logistic\"")) 
  )#End of switch
  estimates=cbind(pars,sqrt(diag(varpars)))
  colnames(estimates)=c("par","s.e.")
  rownames(estimates)=names
  return(estimates) }

PlotCurves=function(fit,Meshsize=NULL,plotlens=NULL,standardize=TRUE,...) {
  r=selncurves(fit$rtype) #Get selection curve function
  if(is.null(plotlens)) plotlens=fit$Data[,1]
  if(is.null(Meshsize)) Meshsize=fit$Meshsize
  plot.title=switch(fit$rtype,
                    "norm.loc"="Normal (common spread)",
                    "norm.sca"="Normal",
                    "lognorm"="Lognormal",
                    "binorm.sca"="Bi-normal",
                    "bilognorm"="Bi-lognormal",
                    "tt.logistic"="Control and logistic","")
  rmatrix=outer(plotlens,Meshsize,r,fit$par)
  rmatrix=t(t(rmatrix)*fit$rel.power)
  if(standardize) rmatrix=rmatrix/max(rmatrix)
  matplot(plotlens,rmatrix,type="l",las=1,ylim=c(0,1),
          xlab="Length (cm)",ylab="Relative retention",...) 
  #abline(h=seq(0,1,0.25),lty=3)
  lenrmatrix=cbind(plotlens,rmatrix)
  colnames(lenrmatrix)=c("Length",Meshsize)
  invisible(lenrmatrix) }

Summary=function(fit,label="Deviance residuals",
                 xlabel="Length (cm)",ylabel="Mesh size (cm)",cex=1) {
  r=selncurves(fit$rtype) #Get selection curve function
  lens=fit$Data[,1]; nlens=length(lens)
  Meshsize=fit$Meshsize; nmeshes=length(Meshsize)
  O=fit$Data[,-1]; #Matrix of observed counts
  rmatrix=outer(lens,Meshsize,r,fit$par)
  rmatrix[is.na(O)]=NA #No fitted retention for missing meshsizes
  rmatrix=t(t(rmatrix)*fit$rel.power)
  print(rmatrix)
  # print(rowSums(rmatrix))
  # print(max(rowSums(rmatrix)))
  print(rowSums(rmatrix) / max(rowSums(rmatrix)))
  phi=rmatrix/apply(rmatrix,1,sum,na.rm=TRUE)
  # print(phi)
  E=apply(O,1,sum,na.rm=TRUE)*phi #Matrix of expected counts
  print(E)
  Pearson.resids=(O-E)/sqrt(E)
  Pearson.chisq=sum(Pearson.resids^2,na.rm=TRUE)
  wk=O*log(O/E); wk[is.na(wk)]=0
  Dev.resids=sign(O-E)*sqrt(2*(E-O+wk))
  print(Dev.resids)
  Deviance=sum(Dev.resids^2,na.rm=TRUE) 
  full.l=sum(-O+O*log(O),na.rm=TRUE)
  null.E=matrix(apply(O,1,mean,na.rm=TRUE),nrow=nlens,ncol=nmeshes)
  null.l=sum(-null.E+O*log(null.E),na.rm=TRUE)
  model.l=sum(-E+O*log(E),na.rm=TRUE)
  NonZeroDat=O[apply(O,1,sum,na.rm=TRUE)>0,]
  d.o.f.=nrow(NonZeroDat)*(nmeshes-1)-length(fit$par)-sum(is.na(NonZeroDat))
  out=rbind(null.l,model.l,full.l,Deviance,Pearson.chisq,d.o.f.)
  AreLensUnique=(length(lens)==length(unique(lens)))
  if(nmeshes>2&AreLensUnique) {
    plot(1,1,xlim=range(lens),xlab=xlabel,ylab=ylabel,
         ylim=range(Meshsize)+(cex/50)*c(-1,1)*(max(Meshsize)-min(Meshsize)),
         yaxt="n",type="n",main=label)
    axis(2,Meshsize,Meshsize,las=1)   
    for(i in 1:nlens)
      for(j in 1:nmeshes)
        points(lens[i],Meshsize[j],pch=ifelse(Dev.resids[i,j]>0,16,1),
               cex=3*abs(Dev.resids[i,j])*cex/(abs(max(Dev.resids)))) }
  else
    if(nmeshes==2) {
      Dev.resids.len=sign(Dev.resids[,2])*sqrt(apply(Dev.resids^2,1,sum))
      plot(lens,Dev.resids.len,type=ifelse(AreLensUnique,"h","p"),las=1,
           main=label,xlab=xlabel,ylab=ylabel,cex=cex)
      abline(h=0) }
  return(out)
}


selncurves=function(rtype) {
  switch(rtype,
         "norm.loc"={
           r=function(lens,Meshsize,th) {
             relsize=Meshsize/Meshsize[1]
             seln=exp(-(lens-th[1]*relsize)^2/(2*th[2]^2)) 
             return(seln) } },
         "norm.sca"={
           r=function(lens,Meshsize,th) {
             relsize=Meshsize/Meshsize[1]
             seln=exp(-(lens-th[1]*relsize)^2/(2*th[2]^2*relsize^2)) 
             return(seln) } },
         "lognorm"={
           r=function(lens,Meshsize,th) {
             relsize=Meshsize/Meshsize[1]
             seln=(relsize/lens)*exp(th[1]-th[2]^2/2)
             seln=seln*exp( -(log(lens)-th[1]-log(relsize))^2/(2*th[2]^2) )
             return(seln) } },
         "binorm.sca"={
           r=function(lens,Meshsize,th) {
             relsize=Meshsize/Meshsize[1]
             seln1=exp(-(lens-th[1]*relsize)^2/(2*th[2]^2*relsize^2))
             seln2=exp(-(lens-th[3]*relsize)^2/(2*th[4]^2*relsize^2))
             p=exp(th[5])/(1+exp(th[5])) # i.e., th[5]=logit(p)
             seln=p*seln1+(1-p)*seln2
             return(seln) } }, 
         "bilognorm"={
           r=function(lens,Meshsize,th) {
             relsize=Meshsize/Meshsize[1]
             seln1=(relsize/lens)*exp(th[1]-th[2]^2/2)
             seln1=seln1*exp( -(log(lens)-th[1]-log(relsize))^2/(2*th[2]^2) )
             seln2=(relsize/lens)*exp(th[3]-th[4]^2/2)
             seln2=seln2*exp( -(log(lens)-th[3]-log(relsize))^2/(2*th[4]^2) )
             p=exp(th[5])/(1+exp(th[5])) # i.e., th[5]=logit(p)
             seln=p*seln1+(1-p)*seln2
             return(seln) } },
         "tt.logistic"={
           r=function(lens,Meshsize,th) {
             control=(Meshsize==Meshsize[1])
             p=exp(th[3])/(1+exp(th[3])) # i.e., th[3]=logit(p)
             wk=exp(th[1]+th[2]*lens)
             lselect=wk/(1+wk)
             seln=(1-p)*control+p*lselect*(1-control)
             return(seln) } },    
         stop(paste("\n",rtype, "not recognised, possible curve types are \n", 
                    "\"norm.loc\", \"norm.sca\", \"lognorm\" \n", 
                    "\"binorm.sca\", \"bilognorm\", and \"tt.logistic\"")) 
  )#End of switch
  return(r) }


########################################
#### Analyses CDFW trammel net data ####
########################################

# load library 
library(msm)

# Import sorted catch data (5-cm length bins) by meshsize 
All = read.csv("S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/Data/Adult Tagging Study/FL.csv", header = F)

# Specify mesh sizes: 6", 7", 8" converted to cm
Meshsize=c(15.2, 17.8, 20.3)

# Unequal fishing power
pwr=c(rep(1,2),rep(2, 1))
pwr

# Or use this for fishing power proportional to meshsize
# pwr=Meshsize

par(mfrow=c(5,2),mar=c(4.1,4.1,1,1))

ALL.1=NetFit(All,Meshsize,c(100,200),rtype="norm.loc",rel.power=pwr)
Estimates(ALL.1);Summary(ALL.1);PlotCurves(ALL.1,plotlens=seq(5,200,0.1)) # lowest 

ALL.2=NetFit(All,Meshsize,c(100,50),rtype="norm.sca",rel.power=pwr)
Estimates(ALL.2); Summary(ALL.2); PlotCurves(ALL.2,plotlens=seq(5,200,0.1))

# ALL.5=NetALL(ALL,Meshsize,c(40,10),rtype="gamma") 
# Estimates(ALL.5); Summary(ALL.5); PlotCurves(ALL.5,plotlens=seq(5,200,0.1))

ALL.3=NetFit(All,Meshsize,c(10,1),rtype="lognorm",rel.power=pwr)
Estimates(ALL.3); Summary(ALL.3); PlotCurves(ALL.3,plotlens=seq(5,200,0.1)) 

ALL.4=NetFit(All,Meshsize,c(40,6,10,10,1),rtype="binorm.sca")
Estimates(ALL.4); Summary(ALL.4); PlotCurves(ALL.4,plotlens=seq(5,200,0.1)) 

All.6=NetFit(All,Meshsize,c(4,2,2,5,1),rtype="bilognorm",rel.power=pwr)
Estimates(All.6); Summary(All.6); PlotCurves(All.6,plotlens=seq(5,200,0.1))

# relative selecltiity
# expected counts
# deviance residuals


###########################################################################
#####Other Old Models#######


ALL=NetFit(All,Meshsize,c(2,0.2,0.1,20,6),rtype="bilognorm")
Estimates(ALL); Summary(ALL); PlotCurves(ALL,plotlens=seq(40,90,0.1))

###Anderson

Anr = read.table("ANR.tl.dat",head=F); 
Meshsize=c(1.27,1.905,2.54,3.81,5.08,6.35,7.62,10.16)
#Equal fishing power
pwr=rep(1,8)
#Or use this for fishing power proportional to meshsize
#pwr=Meshsize

par(mfrow=c(5,2),mar=c(4.1,4.1,1,1))
ANR.1=NetFit(Anr,Meshsize,c(10,5),rtype="norm.loc",rel.power=pwr)
Estimates(ANR.1);Summary(ANR.1);PlotCurves(ANR.1,plotlens=seq(40,90,0.1))

ANR.2=NetFit(Anr,Meshsize,c(4,1),rtype="norm.sca",rel.power=pwr)
Estimates(ANR.2); Summary(ANR.2); PlotCurves(ANR.2,plotlens=seq(40,90,0.1))

#ANR=NetANR(Anr,Meshsize,c(4,0.1),rtype="gamma") #Gamma not yet implemented

ANR.3=NetFit(Anr,Meshsize,c(10,1),rtype="lognorm",rel.power=pwr)
Estimates(ANR.3); Summary(ANR.3); PlotCurves(ANR.3,plotlens=seq(40,90,0.1))

ANR.4=NetFit(Anr,Meshsize,c(4,1,20,5,1),rtype="binorm.sca")
Estimates(ANR.4); Summary(ANR.4); PlotCurves(ANR.4,plotlens=seq(40,90,0.1))


ANR=NetFit(Anr,Meshsize,c(2,0.2,0.1,20,6),rtype="bilognorm",r)
Estimates(ANR); Summary(ANR); PlotCurves(ANR,plotlens=seq(40,90,0.1))

####Arrowrock

Arr = read.table("ARR.tl.dat",head=F); 
Meshsize=c(1.27,1.905,2.54,3.81,5.08,6.35,7.62,10.16)
#Equal fishing power
pwr=rep(1,8)
#Or use this for fishing power proportional to meshsize
#pwr=Meshsize

par(mfrow=c(5,2),mar=c(4.1,4.1,1,1))
ARR.1=NetFit(Arr,Meshsize,c(10,5),rtype="norm.loc",rel.power=pwr)
Estimates(ARR.1); Summary(ARR.1); PlotCurves(ARR.1,plotlens=seq(40,90,0.1))

ARR.2=NetFit(Arr,Meshsize,c(10,5),rtype="norm.sca",rel.power=pwr)
Estimates(ARR.2); Summary(ARR.2); PlotCurves(ARR.2,plotlens=seq(40,90,0.1))

#ARR=NetARR(Arr,Meshsize,c(4,0.1),rtype="gamma") #Gamma not yet implemented

ARR.3=NetFit(Arr,Meshsize,c(10,1),rtype="lognorm",rel.power=pwr)
Estimates(ARR.3); Summary(ARR.3); PlotCurves(ARR.3,plotlens=seq(40,90,0.1))

ARR.4=NetFit(Arr,Meshsize,c(2,1,10,5,0.5),rtype="binorm.sca")
Estimates(ARR.4); Summary(ARR.4); PlotCurves(ARR.4,plotlens=seq(40,90,0.1))


ARR=NetFit(Arr,Meshsize,c(2,0.2,0.1,20,6),rtype="bilognorm",r)
Estimates(ARR); Summary(ARR); PlotCurves(ARR,plotlens=seq(40,90,0.1))

####Luckypeak

Lpr = read.table("LPR.tl.dat",head=F); 
Meshsize=c(1.27,1.905,2.54,3.81,5.08,6.35,7.62,10.16)
#Equal fishing power
pwr=rep(1,8)
#Or use this for fishing power proportional to meshsize
#pwr=Meshsize

par(mfrow=c(5,2),mar=c(4.1,4.1,1,1))
LPR.1=NetFit(Lpr,Meshsize,c(10,5),rtype="norm.loc",rel.power=pwr)
Estimates(LPR.1); Summary(LPR.1); PlotCurves(LPR.1,plotlens=seq(40,90,0.1))

LPR.2=NetFit(Lpr,Meshsize,c(4,1),rtype="norm.sca",rel.power=pwr)
Estimates(LPR.2); Summary(LPR.2); PlotCurves(LPR.2,plotlens=seq(40,90,0.1))

#LPR=NetLPR(Lpr,Meshsize,c(4,0.1),rtype="gamma") #Gamma not yet implemented

LPR.3=NetFit(Lpr,Meshsize,c(10,1),rtype="lognorm",rel.power=pwr)
Estimates(LPR.3); Summary(LPR.3); PlotCurves(LPR.3,plotlens=seq(40,90,0.1))

LPR.4=NetFit(Lpr,Meshsize,c(6,0.8,12,5,0.5),rtype="binorm.sca")
Estimates(LPR.4); Summary(LPR.4); PlotCurves(LPR.4,plotlens=seq(40,90,0.1))


LPR=NetFit(Lpr,Meshsize,c(2,0.2,0.1,20,6),rtype="bilognorm",r)
Estimates(LPR); Summary(LPR); PlotCurves(LPR,plotlens=seq(40,90,0.1))

####LPO

Lpo = read.table("LPO.tl.dat",head=F); 
Meshsize=c(1.27,1.905,2.54,3.81,5.08,6.35,7.62,10.16)
#Equal fishing power
pwr=rep(1,8)
#Or use this for fishing power proportional to meshsize
#pwr=Meshsize

par(mfrow=c(5,2),mar=c(4.1,4.1,1,1))
LPO.1=NetFit(Lpo,Meshsize,c(10,5),rtype="norm.loc",rel.power=pwr)
Estimates(LPO.1); Summary(LPO.1); PlotCurves(LPO.1,plotlens=seq(40,90,0.1))

LPO.2=NetFit(Lpo,Meshsize,c(4,1),rtype="norm.sca",rel.power=pwr)
Estimates(LPO.2); Summary(LPO.2); PlotCurves(LPO.2,plotlens=seq(40,90,0.1))

#LPO=NetLPO(Lpo,Meshsize,c(4,0.1),rtype="gamma") #Gamma not yet implemented

LPO.3=NetFit(Lpo,Meshsize,c(10,1),rtype="lognorm",rel.power=pwr)
Estimates(LPO.3); Summary(LPO.3); PlotCurves(LPO.3,plotlens=seq(40,90,0.1))

LPO.4=NetFit(Lpo,Meshsize,c(5,1,15,7,0.5),rtype="binorm.sca")
Estimates(LPO.4); Summary(LPO.4); PlotCurves(LPO.4,plotlens=seq(40,90,0.1))


LPO=NetFit(Lpo,Meshsize,c(2,0.2,0.1,20,6),rtype="bilognorm",r)
Estimates(LPO); Summary(LPO); PlotCurves(LPO,plotlens=seq(40,90,0.1))

####Spirit

Spt = read.table("Spt.tl.dat",head=F); 
Meshsize=c(1.27,1.905,2.54,3.81,5.08,6.35,7.62,10.16)
#Equal fishing power
pwr=rep(1,8)
#Or use this for fishing power proportional to meshsize
#pwr=Meshsize

par(mfrow=c(5,2),mar=c(4.1,4.1,1,1))
SPT.1=NetFit(Spt,Meshsize,c(10,5),rtype="norm.loc",rel.power=pwr)
Estimates(SPT.1); Summary(SPT.1); PlotCurves(SPT.1,plotlens=seq(40,90,0.1))

SPT.2=NetFit(Spt,Meshsize,c(4,1),rtype="norm.sca",rel.power=pwr)
Estimates(SPT.2); Summary(SPT.2); PlotCurves(SPT.2,plotlens=seq(40,90,0.1))

#SPT=NetSPT(Spt,Meshsize,c(4,0.1),rtype="gamma") #Gamma not yet implemented

SPT.3=NetFit(Spt,Meshsize,c(10,1),rtype="lognorm",rel.power=pwr)
Estimates(SPT.3); Summary(SPT.3); PlotCurves(SPT.3,plotlens=seq(40,90,0.1))

SPT.4=NetFit(Spt,Meshsize,c(6,0.8,12,5,0.5),rtype="binorm.sca")
Estimates(SPT.4); Summary(SPT.4); PlotCurves(SPT.4,plotlens=seq(40,90,0.1))


SPT=NetFit(Spt,Meshsize,c(2,0.2,0.1,20,6),rtype="bilognorm",r)
Estimates(SPT); Summary(SPT); PlotCurves(SPT,plotlens=seq(40,90,0.1))

####Hayden

Hyd = read.table("HYD.tl.dat",head=F); 
Meshsize=c(1.27,1.905,2.54,3.81,5.08,6.35,7.62,10.16)
#Equal fishing power
pwr=rep(1,8)
#Or use this for fishing power proportional to meshsize
#pwr=Meshsize

par(mfrow=c(5,2),mar=c(4.1,4.1,1,1))
HYD.1=NetFit(Hyd,Meshsize,c(10,5),rtype="norm.loc",rel.power=pwr)
Estimates(HYD.1); Summary(HYD.1); PlotCurves(HYD.1,plotlens=seq(40,90,0.1))

HYD.2=NetFit(Hyd,Meshsize,c(4,1),rtype="norm.sca",rel.power=pwr)
Estimates(HYD.2); Summary(HYD.2); PlotCurves(HYD.2,plotlens=seq(40,90,0.1))

#HYD=NetHYD(Hyd,Meshsize,c(4,0.1),rtype="gamma") #Gamma not yet implemented

HYD.3=NetFit(Hyd,Meshsize,c(10,1),rtype="lognorm",rel.power=pwr)
Estimates(HYD.3); Summary(HYD.3); PlotCurves(HYD.3,plotlens=seq(40,90,0.1))

HYD.4=NetFit(Hyd,Meshsize,c(6,0.8,12,5,0.5),rtype="binorm.sca")
Estimates(HYD.4); Summary(HYD.4); PlotCurves(HYD.4,plotlens=seq(40,90,0.1))


HYD=NetFit(Hyd,Meshsize,c(2,0.2,0.1,20,6),rtype="bilognorm",r)
Estimates(HYD); Summary(HYD); PlotCurves(HYD,plotlens=seq(40,90,0.1))






