#####################
#### Maturity ######
#####################

# load packages 
library(FSA)
library(magrittr)
library(dplyr)
library(lubridate)
library(car)
library(readxl)
library(trtools)

# Model I
#######################
### Maturity ogive  ###
#######################

# Chapman et al. 1996 
# data from table 2
# pooled skipped-spawners (group-2) and mature-reproducing (group-3) to produce maturity ogive 

df9 <- read_xlsx("S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/R code/Maturity/pmat3.xlsx")
headtail(df9)
      
myreg <- glm(cbind(mature, immature) ~ length, data = df9, family = binomial(link = logit), weights = n)
bcL9 <- bootCase(myreg,B=5000)
cbind(Ests=coef(myreg),confint(bcL9))

predict(myreg, data.frame(length=c(80,210)),type="response")
predP <- function(cf,x) exp(cf[1]+cf[2]*x)/(1+exp(cf[1]+cf[2]*x))
p32 <- apply(bcL9,1,predP,x=170)
quantile(p32,c(0.025,0.975))

newdat <- with(myreg, data.frame(length = as.numeric(95:250))) # set-up data 

newdata <- cbind(newdat, predict(myreg, newdata = newdat, type = "response",
                                 se = TRUE))

( newdat <- within(newdata, {
  
  LL <-  (fit - (1.96 * se.fit))
  UL <-  (fit + (1.96 * se.fit))
}) ) 

lrWST <- function(cf,p) (log(p/(1-p))-cf[1])/cf[2]

( L50 <- lrWST(coef(myreg),0.5) )           # length at which 50% of fish are mature; 165 cm
bL50 <- apply(bcL9,1,lrWST,p=0.5)
( L50ci <- quantile(bL50,c(0.025,0.975)) )  # CI

( L90 <- lrWST(coef(myreg),0.9) ) # length at which 90% of fish are mature; 222 cm

# Model II
############################
### Reproductive ogives  ###
############################
# Chapman et al. 1996 
# data from table 2
# pooled skipped-spawners (group-2) with immature fish (group-1) to generate reproductive ogive   

df8 <- read_xlsx("S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/R code/Maturity/pmat4.xlsx")
headtail(df8)

myreg2 <- glm(cbind(ripe, not_ripe)~ length, data = df8, family = binomial(link = logit), weights = n)
bcL8 <- bootCase(myreg2,B=5000)
cbind(Ests=coef(myreg2),confint(bcL8))

predict(myreg2, data.frame(length=c(189)),type="response")

newdat <- with(myreg2, data.frame(length = as.numeric(95:250))) # set-up data 

newdata <- cbind(newdat, predict(myreg2, newdata = newdat, type = "response",
                                 se = TRUE))

( newdat <- within(newdata, {
  
  LL <-  (fit - (1.96 * se.fit))
  UL <-  (fit + (1.96 * se.fit))
}) ) 

( L50 <- lrWST(coef(myreg2),0.5) )           # length at which 50% of fish are mature; 165 cm
bL50 <- apply(bcL8,1,lrWST,p=0.5)
( L50ci <- quantile(bL50,c(0.025,0.975)) )  # CI

# Model III
######################################
### Estimate prob of spawning  ###
######################################
# Chapman et al. 1996 
# data from table 2
# change of maturity 

df9 <- read_xlsx("S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/R code/Maturity/pmat3.xlsx")
headtail(df9)

myreg3 <- glm(cbind(mature, immature) ~ length, data = df9, family = binomial(link = logitnr(0.15)))
# obtain starting values with the natural response parameter set to 0
tmp <- glm(cbind(mature, immature) ~ length, 
           family = binomial(link = logitnr(0)), data = df9)
# use starting values with a fixed natural response parameter of 0.15
tmp <- glm(cbind(mature, immature) ~ length, 
           family = binomial(link = logitnr(0.15)), data = df9, start = coef(tmp))
summary(tmp)

# Model IV
######################################
### Estimate prob of spawning  ###
######################################
# Chapman et al. 1996 
# data from table 2
# change of maturity 

df <- read_xlsx("S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/R code/Maturity/chapman.xlsx")
str(df)
headtail(df)

myreg4 <- glm(pmat~length, data = df, family = binomial)
bcL2 <- bootCase(myreg4,B=5000)
cbind(Ests=coef(myreg4),confint(bcL2))

newdat <- with(myreg4, data.frame(length = as.numeric(95:250))) # set-up data 

newdata <- cbind(newdat, predict(myreg4, newdata = newdat, type = "response",
                                 se = TRUE))

( newdat <- within(newdata, {
  
  LL <-  (fit - (1.96 * se.fit))
  UL <-  (fit + (1.96 * se.fit))
}) ) 


write.csv(newdata,"S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/R code/Maturity/chapman_pred.csv")

