#### Sorting data

# Load libraries 
require(FSA)
library(readxl)

# Import dataset 
All.fish <- read_excel("S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/Data/Adult Tagging Study/Sturgeon2014_2016.xlsx")
str(All.fish)

tmp <-All.fish %>% group_by(Meshsize, Species) %>%
  summarize(n=n(), val.n=validn(FL),
            mean=round(mean(FL, na.rm = T),1),
            sd=round(sd(FL, na.rm = T), 2),
            min=min(FL, na.rm = T),
            mdn=quantile(FL, 0.5, na.rm=T),
            max=max(FL, na.rm = T)) %>%
  as.data.frame()

# Interested in just White Sturgeon 
All <- subset(All.fish, Species =="White")
summary(All)
All.FL <- lencat(~FL,data=All, startcat=50, w=5, as.fact = T) # 5-cm length bins
All.FL
FL <- xtabs(~LCat + Meshsize, data = All.FL)
rowSums(FL)

write.csv(FL, "S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/Data/Adult Tagging Study/FL.csv")


            
