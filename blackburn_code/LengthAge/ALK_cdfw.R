##################################
######### Age Length Key #########
##################################
# Ogle (2016); pages 87-105

# load packages 
library(FSA)
library(magrittr)
library(dplyr)
library(nnet)
library(nlstools)
library(readxl)

cdfw <- read_excel("S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/R code/Age Analysis/cdfw_alk.xlsx")
headtail(cdfw)
str(cdfw)
table(cdfw$agecap) # N = 1003

# apply 5 cm wide length interval
cdfw %<>% mutate(lcat5 = lencat(lencap, w = 5))
headtail(cdfw)     # make sure lcat5 appears as new column 
table(cdfw$lcat5) # only 1 fish in the 200 lcat5 category 

# make sure data.frame includes unaged fish(3 false and 3 true)
is.na(headtail(cdfw)$age)

# extract unaged fish
cdfw.unaged <- filter(cdfw, is.na(agecap))
table(cdfw.unaged$lencap) 
table(cdfw.unaged$lcat5) # look at # of unaged fish in each lenght bin 

cdfw.aged <-filter(cdfw, !is.na(agecap))
summary(cdfw.aged$agecap)
mean(cdfw.aged$agecap)

# check to see if extraction worked
all(is.na(cdfw.unaged$agecap)) # better be TRUE
headtail(cdfw.unaged)
table(cdfw.unaged$lcat5)     

any(is.na(cdfw.aged$agecap))   # better be FALSE 
headtail(cdfw.aged)            # n = 372
table(cdfw.aged$lcat5) 

# construct data.frame with the frequency of fish in each length catergory and age combo
( alk.freq <- xtabs(~lcat5 + agecap, data = cdfw.aged) )
write.csv(alk.freq,"C:/Users/Quuist Grad/Desktop/cdfw_alkfreq.csv" )

# computes the sum of rows in the frequency table
rowSums(alk.freq) # large amount of small fish in sample

# row proportions
( alk <- prop.table(alk.freq, margin = 1) )
round(alk, 3) # rounded for display purposes only
# write.csv(alk,"C:/Users/Quuist Grad/Desktop/POOPY.csv" )

## Visualize ALK ##

# Area plot represenation of the observed age-length key for WST in the SSJ 
alkPlot(alk, type = "area", pal = "gray", showLegend = TRUE, leg.cex = 0.7, xlab = "Fork length (cm)")

alkplot.sm <-alkPlot(alk, type = "bubble", xlab = "Fork length (cm)")

## Applying an ALK ##

## unadjusted data ## 

# Age distribution 
( len.n <- xtabs(~lcat5, data = cdfw) )
( tmp <- sweep(alk, MARGIN = 1, FUN = "*", STATS = len.n) )

# number of fish allocated to each age
( ad1 <- colSums(tmp) )

# Proportion of fish at each age
round(prop.table(ad1), 3)

age_dist <- alkAgeDist(alk, lenA.n = rowSums(alk.freq), len.n = len.n)
age_dist
write.csv(age_dist,"C:/Users/Quuist Grad/Desktop/FART.csv")

( age_len2 <- alkMeanVar(alk, lencap~lcat5 + agecap, data = cdfw.aged, len.n = len.n) )
write.csv(age_len2,"C:/Users/Quuist Grad/Desktop/agelen5_cdfw.csv")

cdfw.unaged.mod <- alkIndivAge(alk, agecap~lencap, data = cdfw.unaged) #apply ALK
headtail(cdfw.unaged.mod)

cdfw.fnl <-rbind(cdfw.aged, cdfw.unaged.mod)
headtail(cdfw.fnl)
write.csv(cdfw.fnl,"C:/Users/Quuist Grad/Desktop/cdfwfnl5_cdfw.csv")

cdfw.sumlen <- cdfw.fnl %>% group_by(agecap) %>%
  summarize(n = validn(lencap), mn = mean(lencap, na.rm = TRUE),
            sd =sd(lencap, na.rm = TRUE), se = se (lencap, na.rm = TRUE)) %>%
  as.data.frame()
write.csv(cdfw.sumlen,"C:/Users/Quuist Grad/Desktop/sumlen5_cdfw.csv")

plot(lencap~agecap, data = cdfw.fnl, pch = 19, col = rgb(0,0,0,1/10),
     xlab = "Age", ylab = "Fork Length (cm)", ylim = c(0,300))
lines(mn~agecap, data = cdfw.sumlen, lwd = 2, lty = 2)

cdfw.fnl
write.csv(cdfw.fnl,"C:/Users/Quuist Grad/Desktop/cdfw_fnl.csv")

describe(cdfw.fnl$agecap, type = 2)
describe(cdfw.aged$agecap, type = 2)
write.csv(cdfw.aged,"C:/Users/Quuist Grad/Desktop/cdfw_aged.csv")

SumWST <- cdfw.aged %>% group_by(agecap) %>%
  summarize(n=validn(lencap), mnlen = mean(lencap, na.rm = T),
            selen=se(lencap, na.rm = T)) %>%
  as.data.frame()
write.csv(SumWST,"C:/Users/Quuist Grad/Desktop/SumWST.csv")

###################
## Adjusted data ##
###################

# import data 
cdfw_adjusted <- read_excel("S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/R code/Age Analysis/cdfw_adjusted.xlsx")
cdfw_adjusted

# Age distribution 
( len.n <- xtabs(~lcat5, data = cdfw_adjusted) )
( tmp <- sweep(alk, MARGIN = 1, FUN = "*", STATS = len.n) )

# number of fish allocated to each age
( ad1 <- colSums(tmp) )

# Proportion of fish at each age
round(prop.table(ad1), 3)

age_dist <- alkAgeDist(alk, lenA.n = rowSums(alk.freq), len.n = len.n)
age_dist
write.csv(age_dist,"C:/Users/Quuist Grad/Desktop/adjusted.csv")

( age_len2 <- alkMeanVar(alk, lencap~lcat5 + agecap, data = cdfw.aged, len.n = len.n) )
write.csv(age_len2,"C:/Users/Quuist Grad/Desktop/agelen5_cdfw_adjusted.csv")

cdfw.unaged.mod <- alkIndivAge(alk, agecap~lencap, data = cdfw.unaged) #apply ALK
headtail(cdfw.unaged.mod)

cdfw.fnl <-rbind(cdfw.aged, cdfw.unaged.mod)
headtail(cdfw.fnl)
write.csv(cdfw.fnl,"C:/Users/Quuist Grad/Desktop/cdfwfnl5_cdfw_adjusted.csv")

cdfw.sumlen <- cdfw.fnl %>% group_by(agecap) %>%
  summarize(n = validn(lencap), mn = mean(lencap, na.rm = TRUE),
            sd =sd(lencap, na.rm = TRUE), se = se (lencap, na.rm = TRUE)) %>%
  as.data.frame()
write.csv(cdfw.sumlen,"C:/Users/Quuist Grad/Desktop/sumlen5_cdfw_adjusted.csv")

plot(lencap~agecap, data = cdfw.fnl, pch = 19, col = rgb(0,0,0,1/10),
     xlab = "Age", ylab = "Fork Length (cm)", ylim = c(0,300))
lines(mn~agecap, data = cdfw.sumlen, lwd = 2, lty = 2)

cdfw.fnl

