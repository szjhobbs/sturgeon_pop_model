##################################
## von Bertanlaffy growth model ##
##################################
## S. Blackburn: blac2622@vandals.uidaho.edu
## Using back-calculated length at age (BCLA) from 2014-2016 CDFW White Sturgeon ageing data (n = 374) 
## to estimate von Bertanaffy growth function (VBGF)
## Not partitioned between sexes 
## Not treated as a repeated-measured model 

#######################################################################
##
##  Initial preparation 
##
########################################################################

# Clear console and global environment
rm(list = ls())
cat("\014")     # or ctrl-L in RStudio

# Load packages
library(FSA)
library(nlstools)
library(AICcmodavg)
library(dplyr)
library(magrittr)
library(readxl)

# import data 
wst <- read_excel("S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/R code/vonB/BCLA.xlsx")

#######################################################################
##
##  Fit Growth Model
##
########################################################################

# Create axis label objects 
clrs <- c("black","blue")
xlbl <- "Age (yrs)"
ylbl <- "Fork Length (cm)"
# Create colors that are transparent for help with overplotting
clrs2 <- col2rgbt(clrs,1/20)

# Examine lenght-at-age plot
plot(fl~age,data=wst,pch=19,col=clrs2[2],xlab=xlbl,ylab=ylbl)

# Create function with "typical" von Bertalanffy growth function
vb <- vbFuns("Typical",msg=TRUE)
vb

# Generate starting values ... automatic
wst.vbs <- vbStarts(fl~age,data=wst,type="Typical",plot=TRUE)
# Generate starting values ... manual iteration
wst.vbs <- vbStarts(fl~age,data=wst,type="Typical",plot=TRUE,
                    fixed=list(Linf=160,K=0.3,t0=0))
wst.vbs

# Fit the VBGF to data
wst.vbf <- nls(fl~vb(age,Linf,K,t0),data=wst,start=wst.vbs)
# Examine residual plot
residPlot(wst.vbf) # looks better than using fork-length-at-capture data 

# Extract results
summary(wst.vbf,correlation=TRUE)
( wst.vbc <- coef(wst.vbf) )
cbind(Est=wst.vbc,confint(wst.vbf))

# Bootstrap estimates of uncertainty
wst.vbb <- nlsBoot(wst.vbf,niter=1000)
str(wst.vbb)
headtail(wst.vbb$coefboot)
cbind(EST=wst.vbc,confint(wst.vbb,plot=TRUE,rows=1,cols=3))

# Uncertainty on predictions
ageX <- c(9)
predict(wst.vbf,data.frame(age=ageX))
wst.vbbp <- apply(wst.vbb$coefboot,MARGIN=1,FUN=vb,t=ageX)
c(pred=predict(wst.vbf,data.frame(age=ageX)),
  quantile(wst.vbbp,c(0.025,0.975)))

plot(fl~age,data=wst,xlab=xlbl,ylab=ylbl,pch=19,col=clrs2[2])
curve(vb(x,wst.vbc),from=1,to=30,n=500,lwd=2,col=clrs[2],add=TRUE)

# Predict length from ages 1 to 30 years
nd <- c(1:50)
predict(wst.vbf,data.frame(age=nd))



