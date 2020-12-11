###################################
#### Stochastic Matrix Model ### 
###################################
# S. Blackburn: blac2622@vandals.uidaho.edu 

# References: 
# Caswell, H. 2001. Matrix Population Models: Construction, Analysis, and Interpretation 
# Morris, W. F. and D. F. Doak. 2002. Quanatitative Conservation Biology. 
# Ogle, D. 2016. Introductory Fisheries Analyses with R. 
# Stubben, C. J. and B. G. Milligan. 2007. Estimating and analyzing demographic models uisng the popbio package in R. 

# Using truncated dataset of age 19 and younger White Sturgeon (WST) sampled during CDFW adult sturgeon study (2014-2016)
# to run series of population projections to calculate population growth rates as well as elasticity and sensitivity analysis. 
# This matrix model simulates stochasticity and is density-indepedent. 
# Fecundity in this model is based the on probablity of maturity (Chapman 1989) and assumes that 15% of mature 
# WST females are spawning. 

# Clear console and global environment
rm(list = ls())
cat("\014")     # or ctrl-L in RStudio 

###################
#                 #
#   Import Data   # 
#                 #
###################
# Initial Abundance ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Based on CDFW population abudance DuBois and Gingras (2011) and Hildebrand et al. (2016)

initial_abundance = 48000

# Initial Age Distribution ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Based on age-at-length key (NOT adjusted to trammel net selectivity) and scaled by initial_abundance.
# Then used linear model with log-transformed count data (exponential decay fxn)
# to estimate age 1 and age 2 abundace. Then took 1/2 for females.
# Current age-0 abundance represents the number of eggs potentially in the system in a given year 
# which is based on number of spawning females and their associated fecundity. 

initial_age_dist = structure(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 
                               16, 17, 18,19,    
                               141491743, 3192.023, 2874.605, 743.4951,919.6688,1887.2559,
                               2643.7050,3778.5699,4249.9032,3177.8537,1654.6577,
                               1068.1057,597.0728,1214.5035,561.4314,858.8469,
                               38.1710, 439.7614,148.104767, 148.1116858),
                             .Dim = c(20L, 2L), .Dimnames = list(NULL, c("age", "freq")))

# Observed Survival Probabilities------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Estimated from age-key, using Chapman-Robson peak plus one crieterion 
# and adjusted the SE for overdispersion (using chat variance inflation).
# Age age-0 and age-1 values are from literature.  
# Conditional mortality, mortality in the absence of exploitation, was applied to fishes 
# not in the harvest slot length limit

prob_survival_obs = structure(list(age = 0:19, prob = c(0.002, 0.25, 0.835265413, 0.835265413, 
                                                        0.835265413, 0.835265413, 0.835265413, 0.835265413, 0.835265413, 0.835265413, 0.707049, 
                                                        0.707049, 0.707049, 0.707049, 0.707049, 0.707049, 0.835265413, 0.835265413, 
                                                        0.835265413, 0.835265413), SE = c(0.003, 0.05, 0.0377, 0.0377, 0.0377, 0.0377, 0.0377, 0.0377, 0.0377, 0.0377, 
                                                                                          0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.0377, 
                                                                                          0.0377, 0.0377, 0.0377)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame") 

# Probability of Reproduction ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Estimated with logisitic regression from Chapman et al. 1996 summarized data. First age of maturity for females was
# 10 years old (~104 cm; Chapman et al. 1996). SEs predicted from the logistic model.  

prob_spawn = structure(list(Age = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
                                    13, 14, 15, 16, 17, 18, 19), 
                            
                            prob = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                     0.003726142, 0.01198, 0.021455, 0.046198, 0.078354, 
                                     0.090394, 0.125679, 0.128829, 0.1419388, 0.144958), 
                            
                            SE = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                   0.00075, 0.00258, 0.00429, 0.00873,
                                   0.01629, 0.01866, 0.02364, 0.02547,
                                   0.02826,0.02898)), 
                       .Names = c("Age", "prob", "SE"), row.names = c(NA, 20L), class = "data.frame")
# Fecundity ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# From the linear regression of length on number of eggs. SEs predicted from model.  

number_eggs = structure(list(Age = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
                                     13, 14, 15, 16, 17, 18, 19), count = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 33297.98, 87236.71, 112408.1, 
                                                                            141175.4, 166346.9, 191518.3, 216689.7, 267032.4994,313779.4042, 335354.8988
                                     ), SE = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                               11069.74, 9886.758, 8626.421, 7640.275, 
                                               6711.24, 6144.04, 5877.155, 5951.129, 
                                               6354.067, 7029.626)), .Names = c("Age", "count", "SE"), row.names = c(NA, 20L), class = "data.frame")

# End Data Input ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##############################################
#                                            #
#  Deterministic post-breeding Matrix Model  #
#                                            #
##############################################
# This model does NOT include stochasticity (i.e., recruitment, vital rates)
# Last age class does NOT die 

# A post-breeding census means we capture newborns in our census, they are 1st age class
# Row 1: how do we get new age-0 individuals in the next time step?
# Animals we catch right AFTER (post) their birthday can contribute newborn (age 0) animals
# to next year's census by surviving to their next birthday, then reproducing ON that birthday.
# Ex: fecundity of 9-year-olds (10th fecundity row element in matrix) is:
# survival from 9-10 (i.e., S9, 10th survival element), times fecundity on 10th bday (i.e., f10, 11th repro element)

require(popbio)

# Data loaded:
# initial_abundance    estimated population abundance
# initial_age_dist     estimated initial age distribution
# prob_survival_obs    survival probabilities for current situation
# prob_survival_unexp  survival probabilities for unexploited population
# number_eggs          fecundity estimates for mature fish
# prob_spawn           estimated probability of spawning for mature fish

A = matrix(rep(0, 19*20), byrow = T, nrow = 19)

fecundity = number_eggs$count[2:20]*prob_spawn$prob[2:20]*0.5	# f0-f19; 50% sex ratio
fecundity = c(fecundity, fecundity[19])							# need to add f20, same as f19

row1 = prob_survival_obs$prob*fecundity		 

p.surv.obs = prob_survival_obs$prob[1:19]		    # Survival probablities of ages 0-19 
p.surv.obs.final = prob_survival_obs$prob[20]	  # Manually add 19+ age class so that last age class does not die 
diag(A) = p.surv.obs

A[19, 20] = p.surv.obs.final
A = rbind(row1, A)
print(A)

ev = eigen(A)                    # Eigen values
Mod(ev$values)                   # Absolute value of eigen values
lmax = which.max(Re(ev$values))  # Position of primary eigen value
Re(ev$values)[lmax]              # Primary eigen value 1.050753; does NOT include recruitment stochasicity (later)

# you can also extract lambda via:
lambda(A)

# Create stable age distribition (SAD) for easy comparasion to transient (i.e., intitial_age_dist) dymanics
# SAD should be higher than transient dymanics because it negates cohort/year-class effects 
SAD <- eigen.analysis(A)$stable.stage
stable_age_dist <- 10000 * SAD
stable_age_dist

#######################
# Practice Simulation #
#######################
# practice calculating the log growth rate:
# log(growth rate) = mean(log[N(t+1)/N(t)])
#      length(determ_proj$pop.sizes)
#      my_pop.changes = determ_proj$pop.sizes[-1]/determ_proj$pop.sizes[-length(determ_proj$pop.sizes)] 
# Actually, these are already created by pop.projection in pop.changes
iters = c(20, 100, 1000, 10000) # number of simulations to run
my_lambdas = c()
my_lambdas_CIs = c()

for(i in 1:length(iters)){
  determ_proj = pop.projection(A, initial_age_dist[,2], iterations = iters[i])
  my_log_lambdaS = mean(log(determ_proj$pop.changes))
  my_log_lambdaS_SE = 1.96*sqrt(var(determ_proj$pop.changes/iters[i]))
  my_log_lambdaS_CI = c(my_log_lambdaS-my_log_lambdaS_SE, my_log_lambdaS+my_log_lambdaS_SE)
  
  my_lambdas = c(my_lambdas, exp(my_log_lambdaS))
  my_lambdas_CIs = rbind(my_lambdas_CIs, exp(my_log_lambdaS_CI))
}
mean(my_lambdas)

# In the case of 20 iterations/years is higher (1.180829)
# than the theoretical lambda (1.050753).  M&D say you need about 
# 50,000 simulations to accurately estimate the mean and variance.

# This SE is for stochastic simulation (where Ai changes each year)
# so I don't think it will be valid for iterating the same A 
# (independent of time) since variance will go to 0 as #iters goes to inf
# End practice simulation------------------------------------------------------------

################################
#                              #
#   Stochastic Matrix Model    #
#                              #
################################
# Calculate stochastic log growth rate by simulation (pg. 234 in Morris and Doak)
# also the same approach Ng and Syslo used in thier thesises  

# Approach:
# 1. Simulate Ai iid population matrices, i = 1:n, n is the number of simulations,
#    where the random iid vital rates are drawn from the following distrubitions:
#     i.  prob_survival ~ Beta(a, b) with a and b such that E(prob_survival) = mean, 
#         and Var(prob_survival) = SE^2
#     ia. include recruitment stochasticity (i.e., age-0 survival)
#     ii. prob_spawn ~ Beta(a, b) with a and b such that E(prob_spawn) = mean, 
#         and Var(prob_spawn) = SE^2
#    iii. number_eggs ~ StretchBeta(a, b, min, max) with same type of expectation
#         and variance as above.  support = [min, max] = [0, 2* maxobs]
#    Use functions betaval() and stretchbetaval() to generate from Beta and
#    StretchBeta distributions, respectively, with the appropriate parameters.
# 2. Assemble vital rates into post-breeding matrices which includes newborns (age-0) in the census 
#    as the 1st age class
# 3. Use pop.projection() function to iterate each population j = 1:t times by
#    multiplying matrix Ai by the population vector at time j. Because the same Ai
#    will be used accross the t times, we are interested when t is large (to look
#    at long-term changes in the population)
#3a. Calculate observed lambda for each matrix at time t by comparing the population
#    change from time t-1 to time t (which is already done by pop.proj in pop.changes)
# 4. Calculate the mean log lambda for each Ai
# 5. For the overall value, look at the geometric mean of the lambdas.
# 6. Evaluate sensitivity and elasticity values
# 7. Look at how lambda changes over time

#################################
#                               #
#  1: Simulate iid vital rates  #
#                               #
#################################
iters = 100 # make this 5,000 for final runs 

###########################
# Observed Survival Rates #
###########################
# Simulate from observed survival rates:
surv_obs_sims = c()
for(i in 1:20){
  age_sims = replicate(n = iters, betaval(mn = prob_survival_obs[i, 2], 
                                          sdev = prob_survival_obs[i, 3]))
  surv_obs_sims = rbind(surv_obs_sims, age_sims)
}

# simulate environmental stochasticity for age-0 survival only; WST only having successfull recruitment every 5 years (on average)
recruit.rate = surv_obs_sims[1,]                     # isolate age-0 simulated survival 
recruit.dat = c(rep(0,iters*6), recruit.rate)        # on avg. successful recruitent every 7 years 
V = sample(recruit.dat, iters, replace=T)            # randomnly sample from recruit.dat
surv_obs_sims[1,] = V                                # replace age-0 simulated survival rates 
head(surv_obs_sims)                                  # check to make sure new vector was succesfully added 

# check the simulations
surv_obs_check = cbind(prob_survival_obs, 
                       sim_prob = round(apply(surv_obs_sims, 1, mean), 4),
                       sim_SE = round(apply(surv_obs_sims, 1, sd), 4)) 
row.names(surv_obs_sims) = NULL

# look at age-0 in particular and make sure there are 20 age classes (age 0-19); we will add age-20+ later
surv_obs_check 

###########################
# Probability of Spawning #
###########################
# Simulate probability of spawning
prob_spawn_sim = c()
for(i in 1:20){
  spawn_sims = replicate(n = iters, betaval(mn = prob_spawn[i, 2], 
                                            sdev = prob_spawn[i, 3]))
  prob_spawn_sim = rbind(prob_spawn_sim, spawn_sims)
}
# check the simulations
prob_spawn_check = cbind(prob_spawn, 
                         sim_prob = round(apply(prob_spawn_sim, 1, mean), 4),
                         sim_SE = round(apply(prob_spawn_sim, 1, sd), 4))
row.names(prob_spawn_sim) = NULL

##################
# Number of eggs #
##################
# Simulate the number of eggs 
num_eggs_sim = c()
for(i in 1:20){
  eggs_sims = replicate(n = iters, 
                        stretchbetaval(mn = number_eggs[i, 2], 
                                       std = number_eggs[i, 3],
                                       minb = 0, maxb = 3*number_eggs[i, 2],
                                       runif(1)))
  num_eggs_sim = rbind(num_eggs_sim, eggs_sims)
}

# check the simulations
num_eggs_check = cbind(round(number_eggs, 2), 
                       sim_num = round(apply(num_eggs_sim, 1, mean), 2),
                       sim_SE = round(apply(num_eggs_sim, 1, sd), 2)) 
row.names(num_eggs_sim) = NULL

#############
# Fecundity #
#############
# Simulate fecundity rate from prob_spawning_sims and number_eggs_sims
fecund_sim = num_eggs_sim*prob_spawn_sim*0.5   # 50% sex ratio
tail(fecund_sim)                               # check to make sure fecundity look right 

########################################
#                                      #
#  2: Make Random Projection Matrices  #
#                                      #
########################################
# Make post-breeding Leslie matrices which include WST caught right AFTER (post) their "birthday" (i.e., Jan 1)

random_matrices = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_obs_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_obs[20, 2], sdev = prob_survival_obs[20, 3]) # last age class does NOT die
    random_matrices[[i]] = as.matrix(rbind(
      fecund_sim[,i]*surv_obs_sims[,i], A))
    }

head(random_matrices) # make sure structure looks right 
########################################
#                                      #
#  3: Apply pop.projection             #
#                                      #
########################################
# For each element in random_matrices, perform the following function:
period = 20 # How many years over which to perform projection? 

sim_projections = lapply(seq_along(random_matrices), function(i){
  pop.projection(random_matrices[[i]], 
                 n = initial_age_dist[, "freq"], 
                 iterations = period)})

testfun(random_matrices[[100]], 
        n = initial_age_dist, 
        iterations = period)




sim_projections[[1]]["lambda"]





# NB: probably only need $pop.changes (proportion change in population size) from the projections
########################################
#                                      #
#  4: Calculate mean log lambdas       #
#                                      #
########################################
# For each population projection above, get the average
# log-lambda, to look at the "sampling distribution" of the log lambdas

# just the lambdas
log_lambdaS = sapply(seq_along(sim_projections), function(i){
  mean(log(sim_projections[[i]]$pop.changes))
})
mean(log_lambdaS)

# Aside: Cox calculated mean of theoretical lambdas but don't include these in final analysis 
theo_lambda = sapply(seq_along(sim_projections), function(i){
  sim_projections[[i]]$lambda
})
mean(theo_lambda)

###########################################
#                                         #
#  5: Calculate geometric mean of lambdas #
#                                         #
###########################################
# Geometric mean is just the mean of the log lambdas, exponentiated:
( mean_lambdaS = exp(mean(log_lambdaS)) )

# Presumably we can find the 95% CIs in the same way:
( LB_lambdaS = exp(quantile(log_lambdaS, 0.025)) ) 
( UB_lambdaS = exp(quantile(log_lambdaS, 0.975)) ) 

##################################################
#                                                #
#  6: Sensitivity and Elasticity Analysis        #
#                                                #
##################################################
sens <- stoch.sens(random_matrices)
sens

##################################################
#                                                #
#  7: Lambdas over time                          #
#                                                #
##################################################

# Put da lambdas in a fancy matrix
lambdas.time <- matrix(data=NA, nrow=iters, ncol=(period-1))
for(i in 1:iters){
  lambdas.time[i,] <- sim_projections[[i]]$pop.changes
}
lambdas.time
write.csv(lambdas.time,"S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/R code/Matrix/lambdas_time.csv")

# Plot lambdas through timeeeeeeeeeeeeee
plot(c(1:(period-1)), lambdas.time[1,], xlab='Year', ylab='Lambda', type='l', col='grey', ylim=c(0,2))
for(i in 2:iters){
  lines(c(1:(period-1)), lambdas.time[i,], col='grey', type='l')
}

## Take the lambdas from year 2 on ##
lambdas2 <-lambdas.time[,2:ncol(lambdas.time)]

# Geometric mean for each iteration from year 2 on...
MeanLambdas2 <-c()
for(i in 1:nrow(lambdas2)){
  MeanLambdas2[i]<-exp(mean(log(lambdas2[i,])))
}
mean(MeanLambdas2)

## Take the lambdas from year 15 on ##
lambdas15 <-lambdas.time[,15:ncol(lambdas.time)]

# Geometric mean for each iteration from year 15 on...
MeanLambdas15 <-c()
for(i in 1:nrow(lambdas15)){
  MeanLambdas5[i]<-exp(mean(log(lambdas5[i,])))
}
mean(MeanLambdas15)

## Now take the lambdas from year 40 on ##
lambdas40 <-lambdas.time[,40:ncol(lambdas.time)]

# Geometric mean for each iteration from year 40 on...
MeanLambdas40 <-c()
for(i in 1:nrow(lambdas40)){r
  MeanLambdas40[i]<-exp(mean(log(lambdas40[i,])))
}
mean(MeanLambdas40)
