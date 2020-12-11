
#####################
#### FECUNDITY ######
#####################

# import dataset
eggs <- read.csv("S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/R code/Fecundity/eggs_devore_chapman.csv")
View(eggs)


## Using DeVore et al. 1995 equation 
## eggs = 0.072 * FL(cm) ^ 2.94
egg_devore <- lm(devore ~ len, data = eggs)
egg.new <- expand.grid(len = 104:293)
c <- predict(egg_devore,  egg.new, interval = "confidence")
p <- predict(egg_devore, egg.new, se.fit = TRUE)

# save as csv 
eggs_d <- cbind(egg.new, p, c) 
headtail(eggs_d)
write.csv(eggs_d,"S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/R code/Fecundity/predicted_eggs_devore.csv")
          
## Using Chapman et al. 1996 equation 
## eggs = 5,648 eggs/kg
## standard weight equation to estimate weight from fork length 
egg_chapman <- lm(chapman ~ len, data = eggs)
egg.new2 <- expand.grid(len = 0:293)
c <- predict(egg_chapman,  egg.new2, interval = "confidence")
p <- predict(egg_chapman, egg.new2, se.fit = TRUE)

# save as csv
eggs_c <- cbind(egg.new2, p, c) 
tail(eggs_c)
write.csv(eggs_c,"S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/R code/Fecundity/predicted_eggs_chapman.csv")
