# load packages

library(FSA)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(reshape2)

##########################################
### Mean back-calculated length at age ###
##########################################

## USING ALL AGED FISH ##

all.wide <- read.csv("S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/R code/vonB/all_aged.csv")
View(all.wide)

cdfw <-subset(all.wide, study == "CDFW")
str(cdfw)

cdfw$lencap <- as.numeric(cdfw$lencap)

# view data in one-fish-per-line format
headtail(cdfw, n = 2) # no need to convert data because already radial 
                          # increment measure total distance from the center of structure


# convert data to one-measurement-per-line format (wide to long)
all.r <- gather(cdfw, agei, radi, anu1:anu29) %>%
  arrange(fish, agei)
headtail(all.r)

# adjust for format, NA values, and plus growth 
str_sub(all.r$agei,start=1,end=3) <- ""
all.r %<>% mutate(agei=as.numeric(agei))%>%
  filterD(!is.na(radi)) %>%
  filterD(agei<=agecap) 

headtail(all.r)

## back-calculation - Dahl-Lea

all.r %<>% mutate(DL.len = (radi/radcap)*lencap)

all.bc <-  all.r%>%
  group_by(agecap) %>%
  summarize(n=validn(DL.len),mn=mean(DL.len),sd=sd(DL.len)) %>%
  as.data.frame()

View(all.bc)
View(all.r)

sumTable(DL.len~agecap*agei,data=all.r,digits=1)

all.bcr <-  all.r%>%
  group_by(agei) %>%
  summarize(n=validn(DL.len),mn=mean(DL.len),sd=sd(DL.len)) %>%
  as.data.frame()
View(all.bcr)

## summary table for Dahl-Lea
summary.all.fish <- sumTable(DL.len~agecap*agei,data=all.r,digits=1)
write.csv(summary.all.fish,"C:/Users/Quuist Grad/Desktop/allfish.csv")


# convert data to one-fish-per-line format (long to wide)

all.rr <- dcast(all.r,   fish + lencap + agecap  ~  agei, value.var = "DL.len")
head(all.rr)
write.csv(all.rr,"C:/Users/Quuist Grad/Desktop/allfishwide.csv")




                            
                            