###################################
#### Stochastic Matrix Model ### 
###################################
# References: 
# Caswell, H. 2001. Matrix Population Models: Construction, Analysis, and Interpretation 
# Morris, W. F. and D. F. Doak. 2002. Quanatitative Conservation Biology. 
# Ogle, D. Introductory Fisheries Analyses with R. 
# Stubben, C. J. and B. G. Milligan. 2007. Estimating and analyzing demographic models uisng the popbio package in R. 

# Using truncated dataset of age 19 and younger White Sturgeon sampled during CDFW adult sturgeon study (2014-2016)
# to run series of population projections to calculate deterministic growth rates as well as elasticity and sensitivity analysis. 
# This matrix model simulates stochasticity and is density-indepedent. 
# Fecundity in this model is based the on probablity of maturity (Chapman 1989) and assumed that 15% of mature 
# females are spawning. 

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
# Based on age-at-length key applied  to trammel-net selectivity 
# corrected size-class proportions and then scaled by initial_abundance.
# Then used linear model with log-transformed count data (exponential decay fxn)
# to estimate age 0-2 abundace. Then took 1/2 for females. 
# Age-0 abundance was estimated based on the fecundity of female WST and their abundance 

initial_age_dist = structure(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 
                               16, 17, 18,19,    
                               219387277, 3242.553, 2013.134, 762.9124, 892.9627,
                               # 60387277, 3242.553, 2013.134, 762.9124, 892.9627,
                               2157.2744, 2050.1174, 2769.1556, 3056.7626, 2294.4497,
                               1206.4047, 885.4213, 565.3075, 1548.0006, 935.9705,
                               1771.2220, 118.7429, 1533.6657, 348.5073, 1107.0916),
                             .Dim = c(20L, 2L), .Dimnames = list(NULL, c("age", "freq")))

# Survival Probabilities------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Observed survival probabilities 
# Estimated from age-key, selectivity adjusted data
# Used Chapman-Robson peak + 1 
# Finally, adjusted the SE for overdispersion (using chat variance inflation).
# Age 0-2 values are from literature.  

prob_survival_obs = structure(list(age = 0:19, prob = c(0.002, 0.25, 0.840, 0.94576, 
                                                        0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.81316,
                                                        0.81316, 0.81316, 0.81316, 0.81316, 0.81316, 0.94576, 0.94576, 
                                                        0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                          0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                  0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")
# Unexploitated survival probabilities 
prob_survival_nomu = structure(list(age = 0:19, prob = c(0.002, 0.25, 0.840, 0.94576, 
                                                  0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576,
                                                  0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 
                                                  0.94576, 0.94576), 
                                    SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                            0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281,
                                           0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")

# Survival probabilities for a mu of 0.30 
prob_survival_30mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 
                                                          0.67851,0.67851, 0.67851, 0.67851, 0.67851, 0.67851, 
                                                          0.94576, 0.94576, 0.94576, 0.94576), 
                                    SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                           0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 0.04281, 0.04281, 0.04281)), 
                                           .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")                                                                            

# Survival probabilities for a mu of 0.29 
prob_survival_29mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 
                                                          0.68606,0.68606, 0.68606, 0.68606, 0.68606, 0.68606, 
                                                          0.94576, 0.94576, 0.94576, 0.94576), 
                                    SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                           0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 0.04281, 0.04281, 0.04281)), 
                               .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")         

# Survival probabilities for a mu of 0.28 
prob_survival_28mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 
                                                          0.6937,0.6937, 0.6937, 0.6937, 0.6937, 0.6937, 
                                                          0.94576, 0.94576, 0.94576, 0.94576), 
                                    SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                           0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 0.04281, 0.04281, 0.04281)), 
                               .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")         

# Survival probabilities for a mu of 0.27 
prob_survival_27mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 
                                                          0.70142, 0.70142, 0.70142, 0.70142, 0.70142, 0.70142, 
                                                          0.94576, 0.94576, 0.94576, 0.94576), 
                                    SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                           0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 0.04281, 0.04281, 0.04281)), 
                               .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")         

# Survival probabilities for a mu of 0.26 
prob_survival_26mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 
                                                          0.70923, 0.70923, 0.70923, 0.70923, 0.70923, 0.70923, 
                                                          0.94576, 0.94576, 0.94576, 0.94576), 
                                    SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                           0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 0.04281, 0.04281, 0.04281)), 
                               .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")         

                                                                                                                                                      
# Survival probabilities for a mu of 0.25 
prob_survival_25mu = structure(list(age = 0:19, prob = c(0.002, 0.25, 0.840, 0.94576, 
                                                  0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.71712,
                                                  0.71712, 0.71712, 0.71712, 0.71712, 0.71712, 0.94576, 0.94576, 
                                                  0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                            0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                            0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")

# Survival probabilities for a mu of 0.24 
prob_survival_24mu = structure(list(age = 0:19, prob = c(0.002, 0.25, 0.840, 0.94576, 
                                                         0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.72511,
                                                         0.72511, 0.72511, 0.72511, 0.72511, 0.72511, 0.94576, 0.94576, 
                                                         0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                   0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                   0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")


# Survival probabilities for a mu of 0.23 
prob_survival_23mu = structure(list(age = 0:19, prob = c(0.002, 0.25, 0.840, 0.94576, 
                                                         0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.73318,
                                                         0.73318, 0.73318, 0.73318, 0.73318, 0.73318, 0.94576, 0.94576, 
                                                         0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                   0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                   0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")

# Survival probabilities for a mu of 0.22 
prob_survival_22mu = structure(list(age = 0:19, prob = c(0.002, 0.25, 0.840, 0.94576, 
                                                         0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.74134,
                                                         0.74134, 0.74134, 0.74134, 0.74134, 0.74134, 0.94576, 0.94576, 
                                                         0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                   0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                   0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")
# Survival probabilities for a mu of 0.21 
prob_survival_21mu = structure(list(age = 0:19, prob = c(0.002, 0.25, 0.840, 0.94576, 
                                                         0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.74959,
                                                         0.74959, 0.74959, 0.74959, 0.74959, 0.74959, 0.94576, 0.94576, 
                                                         0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                   0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                   0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")
# Survival probabilities for a mu of 0.20 
prob_survival_20mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                  0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.75793,
                                                  0.75793, 0.75793, 0.75793, 0.75793, 0.75793, 0.94576, 0.94576, 
                                                  0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                            0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                            0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")
# Survival probabilities for a mu of 0.19 
prob_survival_19mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.76637,
                                                          0.76637, 0.76637, 0.76637, 0.76637, 0.76637, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                    0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")

# Survival probabilities for a mu of 0.18 
prob_survival_18mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.7749,
                                                          0.7749, 0.7749, 0.7749, 0.7749, 0.7749, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                    0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")

# Survival probabilities for a mu of 0.17 
prob_survival_17mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.78353,
                                                          0.78353, 0.78353, 0.78353, 0.78353, 0.78353, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                    0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")

# Survival probabilities for a mu of 0.16 
prob_survival_16mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.79225,
                                                          0.79225, 0.79225, 0.79225, 0.79225, 0.75793, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                    0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")
# Survival probabilities for a mu of 0.15 
prob_survival_15mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.80107,
                                                          0.80107, 0.80107, 0.80107, 0.80107, 0.80107, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                    0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")
# Survival probabilities for a mu of 0.12 
prob_survival_12mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.82812,
                                                          0.82812, 0.82812, 0.82812, 0.82812, 0.82812, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 0.04281, 0.04281, 0.04281)),
                               .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")

# Survival probabilities for a mu of 0.11 
prob_survival_11mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.83733,
                                                          0.83733, 0.83733, 0.83733, 0.83733, 0.83733, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 0.04281, 0.04281, 0.04281)),
                               .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")


# Survival probabilities for a mu of 0.10 
prob_survival_10mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.84665,
                                                          0.84665, 0.84665, 0.84665, 0.84665, 0.84665, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                    0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")

# Survival probabilities for a mu of 0.09 
prob_survival_09mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.85608,
                                                          0.85608, 0.85608, 0.85608, 0.85608, 0.85608, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                    0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")

# Survival probabilities for a mu of 0.08 
prob_survival_08mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.86561,
                                                          0.86561, 0.86561, 0.86561, 0.86561, 0.86561, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                    0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")

# Survival probabilities for a mu of 0.07 
prob_survival_07mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.87524,
                                                          0.87524, 0.87524, 0.87524, 0.87524, 0.87524, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                    0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")
# Survival probabilities for a mu of 0.06 
prob_survival_06mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.88498,
                                                          0.88498, 0.88498, 0.88498, 0.88498, 0.88498, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                    0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")
# Survival probabilities for a mu of 0.05 
prob_survival_05mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.89484,
                                                          0.89484, 0.89484, 0.89484, 0.89484, 0.89484, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                    0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")
# Survival probabilities for a mu of 0.04 
prob_survival_04mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.9048,
                                                          0.9048, 0.9048, 0.9048, 0.9048, 0.9048, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                    0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")

# Survival probabilities for a mu of 0.03 
prob_survival_03mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.91487,
                                                          0.91487, 0.91487, 0.91487, 0.91487, 0.91487, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                    0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")
# Survival probabilities for a mu of 0.02 
prob_survival_02mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.92505,
                                                          0.92505, 0.92505, 0.92505, 0.92505, 0.92505, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                    0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")

# Survival probabilities for a mu of 0.01 
prob_survival_01mu = structure(list(age = 0:19, prob =  c(0.002, 0.25, 0.840, 0.94576, 
                                                          0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.94576, 0.93535,
                                                          0.93535, 0.93535, 0.93535, 0.93535, 0.93535, 0.94576, 0.94576, 
                                                          0.94576, 0.94576), SE = c(0.003, 0.05, 0.168, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 0.04281, 
                                                                                    0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.01932, 0.04281, 
                                                                                    0.04281, 0.04281, 0.04281)), .Names = c("age", "prob", "SE"), row.names = c(NA, -20L), class = "data.frame")

# Probability of Reproduction ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Estimated with logisitic regression from Chapman et al. 1996 summarized data. First age of maturity for females was
# 10 years old (~104 cm; Chapman et al. 1996). SEs predicted from the logistic model.  

prob_spawn = structure(list(Age = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
                                    13, 14, 15, 16, 17, 18, 19), 
                            
                            prob = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                     0.00375, 0.0129, 0.02145,
                                     0.04365, 0.08145, 0.0933,
                                     0.1182, 0.12735, 0.1413, 0.1449), 
                            
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
# This model does NOT include stochasticity (e.g., recruitment, vital rates)
# Last age class does NOT die 

# This means we capture newborns in our census, they are 1st age class
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

p.surv.obs = prob_survival_obs$prob[1:19]		    # Ssurvival probablities of ages 0-19 
p.surv.obs.final = prob_survival_obs$prob[20]	  # Manually add 20+ age class so that last age class does not die 
diag(A) = p.surv.obs

A[19, 20] = p.surv.obs.final
A = rbind(row1, A)
print(A)

ev = eigen(A)                    # Eigen values
Mod(ev$values)                   # Absolute value of eigen values
lmax = which.max(Re(ev$values))  # Position of primary eigen value
Re(ev$values)[lmax]              # Primary eigen value 1.17; does NOT include recruitment stochasicity (later)

# you can also extract lambda via:
lambda(A)

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
#     ia. include recruitment stochasticity (age-0 survival only)
#     ii. prob_spawn ~ Beta(a, b) with a and b such that E(prob_spawn) = mean, 
#         and Var(prob_spawn) = SE^2
#    iii. number_eggs ~ StretchBeta(a, b, min, max) with same type of expectation
#         and variance as above.  support = [min, max] = [0, 2* maxobs]
#    Use functions betaval() and stretchbetaval() to generate from Beta and
#    StretchBeta distributions, respectively, with the appropriate parameters.
# 2. Assemble vital rates into post-breeding matrices. This means we include newborns (age-0) in our census, 
#    they are 1st age class
# 3. Use pop.projection() function to iterate each population j = 1:t times by
#    multiplying matrix Ai by the population vector at time j. Because the same Ai
#    will be used accross the t times, we are interested when t is small (to look
#    at short-term changes in the population)
#3a. Calculate observed lambda for each matrix at time t by comparing the population
#    change from time t-1 to time t (which is already done by pop.proj in pop.changes)
# 4. Calculate the mean log lambda for each Ai
# 5. For the overall value, look at the geometric mean of the lambdas.

#################################
#                               #
#  1: Simulate iid vital rates  #
#                               #
#################################
iters = 100 # make this 1,000 or 5,000 for final runs 

###########################
# Observed Survival Rates #
###########################

### Simulate from observed survival rate ###
surv_obs_sims = c()
for(i in 1:20){
  age_sims = replicate(n = iters, betaval(mn = prob_survival_obs[i, 2], 
                                          sdev = prob_survival_obs[i, 3]))
  surv_obs_sims = rbind(surv_obs_sims, age_sims)
}

# simulate environmental stochasticity for age-0 survival only; WST only having successfull recruitment every 5 years (on average)
recruit.rate = surv_obs_sims[1,]                     # isolate age-0 simulated survival 
recruit.dat = c(rep(0,iters*9), recruit.rate)        # on avg. successful recruitent every 5 years 
V = sample(recruit.dat, iters, replace=T)            # randomnly sample from recruit.dat
surv_obs_sims[1,] = V                                # replace age-0 simulated survival rates 
head(surv_obs_sims)                                  # check to make sure new vector was succesfully added 

# check the simulations
surv_obs_check = cbind(prob_survival_obs, 
                       sim_prob = round(apply(surv_obs_sims, 1, mean), 4),
                       sim_SE = round(apply(surv_obs_sims, 1, sd), 4)) 
row.names(surv_obs_sims) = NULL

surv_obs_check # look at age 0 in particular and make sure there are 20 age classes (age 0-20)

###########################################
## Simulate from unexploited population ###
###########################################
surv_nomu_sims = c()
for(i in 1:20){
  age_sims_nomu = replicate(n = iters, betaval(mn = prob_survival_nomu[i, 2], 
                                               sdev = prob_survival_nomu[i, 3]))
  surv_nomu_sims = rbind(surv_nomu_sims, age_sims_nomu)
}

# simulate environmental stochasticity for age-0 survival only; WST only having successfull recruitment every 5 years (on average)
recruit.rate = surv_nomu_sims[1,]                     # isolate age-0 simulated survival 
recruit.dat = c(rep(0,iters*9), recruit.rate)       # on avg. successful recruitent every 5 years 
V = sample(recruit.dat, iters, replace=T)            # randomnly sample from recruit.dat
surv_nomu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_nomu_check = cbind(prob_survival_nomu, 
                        sim_prob = round(apply(surv_nomu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_nomu_sims, 1, sd), 4)) 
row.names(surv_nomu_sims) = NULL

#######################################
## Simulate from 0.30 exploitation  ###
#######################################
prob_survival_30mu
surv_30mu_sims = c()
for(i in 1:20){
  age_sims_30mu = replicate(n = iters, betaval(mn = prob_survival_30mu[i, 2], 
                                               sdev = prob_survival_30mu[i, 3]))
  surv_30mu_sims = rbind(surv_30mu_sims, age_sims_30mu)
}

surv_30mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_30mu_check = cbind(prob_survival_30mu, 
                        sim_prob = round(apply(surv_30mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_30mu_sims, 1, sd), 4)) 
row.names(surv_30mu_sims) = NULL

#######################################
## Simulate from 0.29 exploitation  ###
#######################################
prob_survival_29mu
surv_29mu_sims = c()
for(i in 1:20){
  age_sims_29mu = replicate(n = iters, betaval(mn = prob_survival_29mu[i, 2], 
                                               sdev = prob_survival_29mu[i, 3]))
  surv_29mu_sims = rbind(surv_29mu_sims, age_sims_29mu)
}

surv_29mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_29mu_check = cbind(prob_survival_29mu, 
                        sim_prob = round(apply(surv_29mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_29mu_sims, 1, sd), 4)) 
row.names(surv_29mu_sims) = NULL

#######################################
## Simulate from 0.28 exploitation  ###
#######################################
prob_survival_28mu
surv_28mu_sims = c()
for(i in 1:20){
  age_sims_28mu = replicate(n = iters, betaval(mn = prob_survival_28mu[i, 2], 
                                               sdev = prob_survival_28mu[i, 3]))
  surv_28mu_sims = rbind(surv_28mu_sims, age_sims_28mu)
}

surv_28mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_28mu_check = cbind(prob_survival_28mu, 
                        sim_prob = round(apply(surv_28mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_28mu_sims, 1, sd), 4)) 
row.names(surv_28mu_sims) = NULL

#######################################
## Simulate from 0.27 exploitation  ###
#######################################
prob_survival_27mu
surv_27mu_sims = c()
for(i in 1:20){
  age_sims_27mu = replicate(n = iters, betaval(mn = prob_survival_27mu[i, 2], 
                                               sdev = prob_survival_27mu[i, 3]))
  surv_27mu_sims = rbind(surv_27mu_sims, age_sims_27mu)
}

surv_27mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_27mu_check = cbind(prob_survival_27mu, 
                        sim_prob = round(apply(surv_27mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_27mu_sims, 1, sd), 4)) 
row.names(surv_27mu_sims) = NULL

#######################################
## Simulate from 0.26 exploitation  ###
#######################################
prob_survival_26mu
surv_26mu_sims = c()
for(i in 1:20){
  age_sims_26mu = replicate(n = iters, betaval(mn = prob_survival_26mu[i, 2], 
                                               sdev = prob_survival_26mu[i, 3]))
  surv_26mu_sims = rbind(surv_26mu_sims, age_sims_26mu)
}

surv_26mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_26mu_check = cbind(prob_survival_26mu, 
                        sim_prob = round(apply(surv_26mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_26mu_sims, 1, sd), 4)) 
row.names(surv_26mu_sims) = NULL

#######################################
## Simulate from 0.25 exploitation  ###
#######################################
prob_survival_25mu
surv_25mu_sims = c()
for(i in 1:20){
  age_sims_25mu = replicate(n = iters, betaval(mn = prob_survival_25mu[i, 2], 
                                               sdev = prob_survival_25mu[i, 3]))
  surv_25mu_sims = rbind(surv_25mu_sims, age_sims_25mu)
}

surv_25mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_25mu_check = cbind(prob_survival_25mu, 
                        sim_prob = round(apply(surv_25mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_25mu_sims, 1, sd), 4)) 
row.names(surv_25mu_sims) = NULL

#######################################
## Simulate from 0.24 exploitation  ###
#######################################
prob_survival_24mu
surv_24mu_sims = c()
for(i in 1:20){
  age_sims_24mu = replicate(n = iters, betaval(mn = prob_survival_24mu[i, 2], 
                                               sdev = prob_survival_24mu[i, 3]))
  surv_24mu_sims = rbind(surv_24mu_sims, age_sims_24mu)
}

surv_24mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_24mu_check = cbind(prob_survival_24mu, 
                        sim_prob = round(apply(surv_24mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_24mu_sims, 1, sd), 4)) 
row.names(surv_24mu_sims) = NULL

#######################################
## Simulate from 0.23 exploitation  ###
#######################################
prob_survival_23mu
surv_23mu_sims = c()
for(i in 1:20){
  age_sims_23mu = replicate(n = iters, betaval(mn = prob_survival_23mu[i, 2], 
                                               sdev = prob_survival_23mu[i, 3]))
  surv_23mu_sims = rbind(surv_23mu_sims, age_sims_23mu)
}

surv_23mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_23mu_check = cbind(prob_survival_23mu, 
                        sim_prob = round(apply(surv_23mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_23mu_sims, 1, sd), 4)) 
row.names(surv_23mu_sims) = NULL

#######################################
## Simulate from 0.22 exploitation  ###
#######################################
prob_survival_22mu
surv_22mu_sims = c()
for(i in 1:20){
  age_sims_22mu = replicate(n = iters, betaval(mn = prob_survival_22mu[i, 2], 
                                               sdev = prob_survival_22mu[i, 3]))
  surv_22mu_sims = rbind(surv_22mu_sims, age_sims_22mu)
}

surv_22mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_22mu_check = cbind(prob_survival_22mu, 
                        sim_prob = round(apply(surv_22mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_22mu_sims, 1, sd), 4)) 
row.names(surv_22mu_sims) = NULL

#######################################
## Simulate from 0.21 exploitation  ###
#######################################
prob_survival_21mu
surv_21mu_sims = c()
for(i in 1:20){
  age_sims_21mu = replicate(n = iters, betaval(mn = prob_survival_21mu[i, 2], 
                                               sdev = prob_survival_21mu[i, 3]))
  surv_21mu_sims = rbind(surv_21mu_sims, age_sims_21mu)
}

surv_21mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_21mu_check = cbind(prob_survival_21mu, 
                        sim_prob = round(apply(surv_21mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_21mu_sims, 1, sd), 4)) 
row.names(surv_21mu_sims) = NULL

#######################################
## Simulate from 0.20 exploitation  ###
#######################################
prob_survival_20mu
surv_20mu_sims = c()
for(i in 1:20){
  age_sims_20mu = replicate(n = iters, betaval(mn = prob_survival_20mu[i, 2], 
                                               sdev = prob_survival_20mu[i, 3]))
  surv_20mu_sims = rbind(surv_20mu_sims, age_sims_20mu)
}

surv_20mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_20mu_check = cbind(prob_survival_20mu, 
                        sim_prob = round(apply(surv_20mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_20mu_sims, 1, sd), 4)) 
row.names(surv_20mu_sims) = NULL

#######################################
## Simulate from 0.19 exploitation  ###
#######################################
prob_survival_19mu
surv_19mu_sims = c()
for(i in 1:20){
  age_sims_19mu = replicate(n = iters, betaval(mn = prob_survival_19mu[i, 2], 
                                               sdev = prob_survival_19mu[i, 3]))
  surv_19mu_sims = rbind(surv_19mu_sims, age_sims_19mu)
}

surv_19mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_19mu_check = cbind(prob_survival_19mu, 
                        sim_prob = round(apply(surv_19mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_19mu_sims, 1, sd), 4)) 
row.names(surv_19mu_sims) = NULL

#######################################
## Simulate from 0.18 exploitation  ###
#######################################
prob_survival_18mu
surv_18mu_sims = c()
for(i in 1:20){
  age_sims_18mu = replicate(n = iters, betaval(mn = prob_survival_18mu[i, 2], 
                                               sdev = prob_survival_18mu[i, 3]))
  surv_18mu_sims = rbind(surv_18mu_sims, age_sims_18mu)
}

surv_18mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_18mu_check = cbind(prob_survival_18mu, 
                        sim_prob = round(apply(surv_18mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_18mu_sims, 1, sd), 4)) 
row.names(surv_18mu_sims) = NULL

#######################################
## Simulate from 0.17 exploitation  ###
#######################################
prob_survival_17mu
surv_17mu_sims = c()
for(i in 1:20){
  age_sims_17mu = replicate(n = iters, betaval(mn = prob_survival_17mu[i, 2], 
                                               sdev = prob_survival_17mu[i, 3]))
  surv_17mu_sims = rbind(surv_17mu_sims, age_sims_17mu)
}

surv_17mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_17mu_check = cbind(prob_survival_17mu, 
                        sim_prob = round(apply(surv_17mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_17mu_sims, 1, sd), 4)) 
row.names(surv_17mu_sims) = NULL

#######################################
## Simulate from 0.16 exploitation  ###
#######################################
prob_survival_16mu
surv_16mu_sims = c()
for(i in 1:20){
  age_sims_16mu = replicate(n = iters, betaval(mn = prob_survival_16mu[i, 2], 
                                               sdev = prob_survival_16mu[i, 3]))
  surv_16mu_sims = rbind(surv_16mu_sims, age_sims_16mu)
}

surv_16mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_16mu_check = cbind(prob_survival_16mu, 
                        sim_prob = round(apply(surv_16mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_16mu_sims, 1, sd), 4)) 
row.names(surv_16mu_sims) = NULL

#######################################
## Simulate from 0.15 exploitation  ###
#######################################
prob_survival_15mu
surv_15mu_sims = c()
for(i in 1:20){
  age_sims_15mu = replicate(n = iters, betaval(mn = prob_survival_15mu[i, 2], 
                                               sdev = prob_survival_15mu[i, 3]))
  surv_15mu_sims = rbind(surv_15mu_sims, age_sims_15mu)
}

surv_15mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_15mu_check = cbind(prob_survival_15mu, 
                        sim_prob = round(apply(surv_15mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_15mu_sims, 1, sd), 4)) 
row.names(surv_15mu_sims) = NULL

#######################################
## Simulate from 0.12 exploitation  ###
#######################################
prob_survival_12mu
surv_12mu_sims = c()
for(i in 1:20){
  age_sims_12mu = replicate(n = iters, betaval(mn = prob_survival_12mu[i, 2], 
                                               sdev = prob_survival_12mu[i, 3]))
  surv_12mu_sims = rbind(surv_12mu_sims, age_sims_12mu)
}

surv_12mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_12mu_check = cbind(prob_survival_12mu, 
                        sim_prob = round(apply(surv_12mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_12mu_sims, 1, sd), 4)) 
row.names(surv_12mu_sims) = NULL

#######################################
## Simulate from 0.11 exploitation  ###
#######################################
prob_survival_11mu
surv_11mu_sims = c()
for(i in 1:20){
  age_sims_11mu = replicate(n = iters, betaval(mn = prob_survival_11mu[i, 2], 
                                               sdev = prob_survival_11mu[i, 3]))
  surv_11mu_sims = rbind(surv_11mu_sims, age_sims_11mu)
}

surv_11mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_11mu_check = cbind(prob_survival_11mu, 
                        sim_prob = round(apply(surv_11mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_11mu_sims, 1, sd), 4)) 
row.names(surv_11mu_sims) = NULL

#######################################
## Simulate from 0.10 exploitation  ###
#######################################
prob_survival_10mu
surv_10mu_sims = c()
for(i in 1:20){
  age_sims_10mu = replicate(n = iters, betaval(mn = prob_survival_10mu[i, 2], 
                                               sdev = prob_survival_10mu[i, 3]))
  surv_10mu_sims = rbind(surv_10mu_sims, age_sims_10mu)
}

surv_10mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_10mu_check = cbind(prob_survival_10mu, 
                        sim_prob = round(apply(surv_10mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_10mu_sims, 1, sd), 4)) 
row.names(surv_10mu_sims) = NULL

#######################################
## Simulate from 0.09 exploitation  ###
#######################################
prob_survival_09mu
surv_09mu_sims = c()
for(i in 1:20){
  age_sims_09mu = replicate(n = iters, betaval(mn = prob_survival_09mu[i, 2], 
                                               sdev = prob_survival_09mu[i, 3]))
  surv_09mu_sims = rbind(surv_09mu_sims, age_sims_09mu)
}

surv_09mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_09mu_check = cbind(prob_survival_09mu, 
                        sim_prob = round(apply(surv_09mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_09mu_sims, 1, sd), 4)) 
row.names(surv_09mu_sims) = NULL

#######################################
## Simulate from 0.08 exploitation  ###
#######################################
prob_survival_08mu
surv_08mu_sims = c()
for(i in 1:20){
  age_sims_08mu = replicate(n = iters, betaval(mn = prob_survival_08mu[i, 2], 
                                               sdev = prob_survival_08mu[i, 3]))
  surv_08mu_sims = rbind(surv_08mu_sims, age_sims_08mu)
}

surv_08mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_08mu_check = cbind(prob_survival_08mu, 
                        sim_prob = round(apply(surv_08mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_08mu_sims, 1, sd), 4)) 
row.names(surv_08mu_sims) = NULL

#######################################
## Simulate from 0.07 exploitation  ###
#######################################
prob_survival_07mu
surv_07mu_sims = c()
for(i in 1:20){
  age_sims_07mu = replicate(n = iters, betaval(mn = prob_survival_07mu[i, 2], 
                                               sdev = prob_survival_07mu[i, 3]))
  surv_07mu_sims = rbind(surv_07mu_sims, age_sims_07mu)
}

surv_07mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_07mu_check = cbind(prob_survival_07mu, 
                        sim_prob = round(apply(surv_07mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_07mu_sims, 1, sd), 4)) 
row.names(surv_07mu_sims) = NULL

#######################################
## Simulate from 0.06 exploitation  ###
#######################################
prob_survival_06mu
surv_06mu_sims = c()
for(i in 1:20){
  age_sims_06mu = replicate(n = iters, betaval(mn = prob_survival_06mu[i, 2], 
                                               sdev = prob_survival_06mu[i, 3]))
  surv_06mu_sims = rbind(surv_06mu_sims, age_sims_06mu)
}

surv_06mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_06mu_check = cbind(prob_survival_06mu, 
                        sim_prob = round(apply(surv_06mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_06mu_sims, 1, sd), 4)) 
row.names(surv_06mu_sims) = NULL

#######################################
## Simulate from 0.05 exploitation  ###
#######################################
prob_survival_05mu
surv_05mu_sims = c()
for(i in 1:20){
  age_sims_05mu = replicate(n = iters, betaval(mn = prob_survival_05mu[i, 2], 
                                               sdev = prob_survival_05mu[i, 3]))
  surv_05mu_sims = rbind(surv_05mu_sims, age_sims_05mu)
}

surv_05mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_05mu_check = cbind(prob_survival_05mu, 
                        sim_prob = round(apply(surv_05mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_05mu_sims, 1, sd), 4)) 
row.names(surv_05mu_sims) = NULL

#######################################
## Simulate from 0.04 exploitation  ###
#######################################
prob_survival_04mu
surv_04mu_sims = c()
for(i in 1:20){
  age_sims_04mu = replicate(n = iters, betaval(mn = prob_survival_04mu[i, 2], 
                                               sdev = prob_survival_04mu[i, 3]))
  surv_04mu_sims = rbind(surv_04mu_sims, age_sims_04mu)
}

surv_04mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_04mu_check = cbind(prob_survival_04mu, 
                        sim_prob = round(apply(surv_04mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_04mu_sims, 1, sd), 4)) 
row.names(surv_04mu_sims) = NULL

#######################################
## Simulate from 0.03 exploitation  ###
#######################################
prob_survival_03mu
surv_03mu_sims = c()
for(i in 1:20){
  age_sims_03mu = replicate(n = iters, betaval(mn = prob_survival_03mu[i, 2], 
                                               sdev = prob_survival_03mu[i, 3]))
  surv_03mu_sims = rbind(surv_03mu_sims, age_sims_03mu)
}

surv_03mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_03mu_check = cbind(prob_survival_03mu, 
                        sim_prob = round(apply(surv_03mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_03mu_sims, 1, sd), 4)) 
row.names(surv_03mu_sims) = NULL

#######################################
## Simulate from 0.02 exploitation  ###
#######################################
prob_survival_02mu
surv_02mu_sims = c()
for(i in 1:20){
  age_sims_02mu = replicate(n = iters, betaval(mn = prob_survival_02mu[i, 2], 
                                               sdev = prob_survival_02mu[i, 3]))
  surv_02mu_sims = rbind(surv_02mu_sims, age_sims_02mu)
}

surv_02mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_02mu_check = cbind(prob_survival_02mu, 
                        sim_prob = round(apply(surv_02mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_02mu_sims, 1, sd), 4)) 
row.names(surv_02mu_sims) = NULL

#######################################
## Simulate from 0.01 exploitation  ###
#######################################
prob_survival_01mu
surv_01mu_sims = c()
for(i in 1:20){
  age_sims_01mu = replicate(n = iters, betaval(mn = prob_survival_01mu[i, 2], 
                                               sdev = prob_survival_01mu[i, 3]))
  surv_01mu_sims = rbind(surv_01mu_sims, age_sims_01mu)
}

surv_01mu_sims[1,] = V                                # replace age-0 simulated survival rates 

# check the simulations
surv_01mu_check = cbind(prob_survival_01mu, 
                        sim_prob = round(apply(surv_01mu_sims, 1, mean), 4),
                        sim_SE = round(apply(surv_01mu_sims, 1, sd), 4)) 
row.names(surv_01mu_sims) = NULL

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
fecund_sims = num_eggs_sim*prob_spawn_sim*0.5   # 50% sex ratio
tail(fecund_sims)                               # check to make sure fecundity look right 

########################################
#                                      #
#  2: Make Random Projection Matrices  #
#                                      #
########################################
# Make post-breeding Leslie matrices meaning WST caught right AFTER (post) their "birthday" 
# can contribute newborn (age 0) animals to next year's census by surviving to their next birthday, 
# then reproducing ON that birthday.
# Ex: fecundity of 9-year-olds (10th fecundity row element in matrix) is:
# survival from 9-10 (S9, 10th survival element), times fecundity on 10th bday (f10, 11th repro element)

###############
## Observed ###
###############
random_matrices_obs = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_obs_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_obs[20, 2], sdev = prob_survival_obs[20, 3]) # last age class does not die
  random_matrices_obs[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_obs_sims[,i], A))
}

head(random_matrices_obs) # structure looks right 

####################
## Unexploitated ###
####################
random_matrices_nomu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_nomu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_nomu[20, 2], sdev = prob_survival_nomu[20, 3]) # last age class does not die
  random_matrices_nomu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_nomu_sims[,i], A))
}

##########################
## Exploitated at 0.30 ###
##########################
random_matrices_30mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_30mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_30mu[20, 2], sdev = prob_survival_30mu[20, 3]) # last age class does not die
  random_matrices_30mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_30mu_sims[,i], A))
}

##########################
## Exploitated at 0.29 ###
##########################
random_matrices_29mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_29mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_29mu[20, 2], sdev = prob_survival_29mu[20, 3]) # last age class does not die
  random_matrices_29mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_29mu_sims[,i], A))
}

##########################
## Exploitated at 0.28 ###
##########################
random_matrices_28mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_28mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_28mu[20, 2], sdev = prob_survival_28mu[20, 3]) # last age class does not die
  random_matrices_28mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_28mu_sims[,i], A))
}

##########################
## Exploitated at 0.27 ###
##########################
random_matrices_27mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_27mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_27mu[20, 2], sdev = prob_survival_27mu[20, 3]) # last age class does not die
  random_matrices_27mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_27mu_sims[,i], A))
}

##########################
## Exploitated at 0.26 ###
##########################
random_matrices_26mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_26mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_26mu[20, 2], sdev = prob_survival_26mu[20, 3]) # last age class does not die
  random_matrices_26mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_26mu_sims[,i], A))
}

##########################
## Exploitated at 0.25 ###
##########################
random_matrices_25mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_25mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_25mu[20, 2], sdev = prob_survival_25mu[20, 3]) # last age class does not die
  random_matrices_25mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_25mu_sims[,i], A))
}

##########################
## Exploitated at 0.24 ###
##########################
random_matrices_24mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_24mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_24mu[20, 2], sdev = prob_survival_24mu[20, 3]) # last age class does not die
  random_matrices_24mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_24mu_sims[,i], A))
}

##########################
## Exploitated at 0.23 ###
##########################
random_matrices_23mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_23mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_23mu[20, 2], sdev = prob_survival_23mu[20, 3]) # last age class does not die
  random_matrices_23mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_23mu_sims[,i], A))
}

##########################
## Exploitated at 0.22 ###
##########################
random_matrices_22mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_22mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_22mu[20, 2], sdev = prob_survival_22mu[20, 3]) # last age class does not die
  random_matrices_22mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_22mu_sims[,i], A))
}

##########################
## Exploitated at 0.21 ###
##########################
random_matrices_21mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_21mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_21mu[20, 2], sdev = prob_survival_21mu[20, 3]) # last age class does not die
  random_matrices_21mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_21mu_sims[,i], A))
}

##########################
## Exploitated at 0.20 ###
##########################
random_matrices_20mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_20mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_20mu[20, 2], sdev = prob_survival_20mu[20, 3]) # last age class does not die
  random_matrices_20mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_20mu_sims[,i], A))
}

##########################
## Exploitated at 0.19 ###
##########################
random_matrices_19mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 19 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_19mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_19mu[20, 2], sdev = prob_survival_19mu[20, 3]) # last age class does not die
  random_matrices_19mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_19mu_sims[,i], A))
}

##########################
## Exploitated at 0.18 ###
##########################
random_matrices_18mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 19 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_18mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_18mu[20, 2], sdev = prob_survival_18mu[20, 3]) # last age class does not die
  random_matrices_18mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_18mu_sims[,i], A))
}

##########################
## Exploitated at 0.18 ###
##########################
random_matrices_17mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 19 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_17mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_17mu[20, 2], sdev = prob_survival_17mu[20, 3]) # last age class does not die
  random_matrices_17mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_17mu_sims[,i], A))
}

##########################
## Exploitated at 0.16 ###
##########################
random_matrices_16mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 19 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_16mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_16mu[20, 2], sdev = prob_survival_16mu[20, 3]) # last age class does not die
  random_matrices_16mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_16mu_sims[,i], A))
}

##########################
## Exploitated at 0.15 ###
##########################
random_matrices_15mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 19 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_15mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_15mu[20, 2], sdev = prob_survival_15mu[20, 3]) # last age class does not die
  random_matrices_15mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_15mu_sims[,i], A))
}

##########################
## Exploitated at 0.12 ###
##########################
random_matrices_12mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 19 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_12mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_12mu[20, 2], sdev = prob_survival_12mu[20, 3]) # last age class does not die
  random_matrices_12mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_12mu_sims[,i], A))
}

##########################
## Exploitated at 0.11 ###
##########################
random_matrices_11mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 19 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_11mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_11mu[20, 2], sdev = prob_survival_11mu[20, 3]) # last age class does not die
  random_matrices_11mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_11mu_sims[,i], A))
}

##########################
## Exploitated at 0.10 ###
##########################
random_matrices_10mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_10mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_10mu[20, 2], sdev = prob_survival_10mu[20, 3]) # last age class does not die
  random_matrices_10mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_10mu_sims[,i], A))
}

##########################
## Exploitated at 0.09 ###
##########################
random_matrices_09mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_09mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_09mu[20, 2], sdev = prob_survival_09mu[20, 3]) # last age class does not die
  random_matrices_09mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_09mu_sims[,i], A))
}

##########################
## Exploitated at 0.08 ###
##########################
random_matrices_08mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_08mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_08mu[20, 2], sdev = prob_survival_08mu[20, 3]) # last age class does not die
  random_matrices_08mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_08mu_sims[,i], A))
}

##########################
## Exploitated at 0.07 ###
##########################
random_matrices_07mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_07mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_07mu[20, 2], sdev = prob_survival_07mu[20, 3]) # last age class does not die
  random_matrices_07mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_07mu_sims[,i], A))
}

##########################
## Exploitated at 0.06 ###
##########################
random_matrices_06mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_06mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_06mu[20, 2], sdev = prob_survival_06mu[20, 3]) # last age class does not die
  random_matrices_06mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_06mu_sims[,i], A))
}

##########################
## Exploitated at 0.05 ###
##########################
random_matrices_05mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_05mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_05mu[20, 2], sdev = prob_survival_05mu[20, 3]) # last age class does not die
  random_matrices_05mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_05mu_sims[,i], A))
}

##########################
## Exploitated at 0.04 ###
##########################
random_matrices_04mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_04mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_04mu[20, 2], sdev = prob_survival_04mu[20, 3]) # last age class does not die
  random_matrices_04mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_04mu_sims[,i], A))
}

##########################
## Exploitated at 0.03 ###
##########################
random_matrices_03mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_03mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_03mu[20, 2], sdev = prob_survival_03mu[20, 3]) # last age class does not die
  random_matrices_03mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_03mu_sims[,i], A))
}

##########################
## Exploitated at 0.02 ###
##########################
random_matrices_02mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_02mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_02mu[20, 2], sdev = prob_survival_02mu[20, 3]) # last age class does not die
  random_matrices_02mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_02mu_sims[,i], A))
}

##########################
## Exploitated at 0.01 ###
##########################
random_matrices_01mu = list()

for(i in 1:iters){
  A = matrix(rep(0, 19*20), byrow = T, nrow = 19) # 20 columns by 19 rows; will add top extra row (fecundity)
  diag(A) = surv_01mu_sims[1:19,i]                 # survival rates age 0 through 19
  A[19,20] = betaval(mn = prob_survival_01mu[20, 2], sdev = prob_survival_01mu[20, 3]) # last age class does not die
  random_matrices_01mu[[i]] = as.matrix(rbind(
    fecund_sims[,i]*surv_01mu_sims[,i], A))
}

########################################
#                                      #
#  3: Apply pop.projection             #
#                                      #
########################################
# For each element in random_matrices, perform the following function:
period = 50 # How many years over which to perform projection? 

sim_projections_obs = lapply(seq_along(random_matrices_obs), function(i){
  pop.projection(random_matrices_obs[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_nomu = lapply(seq_along(random_matrices_nomu), function(i){
  pop.projection(random_matrices_nomu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_30mu = lapply(seq_along(random_matrices_30mu), function(i){
  pop.projection(random_matrices_30mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_29mu = lapply(seq_along(random_matrices_29mu), function(i){
  pop.projection(random_matrices_29mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_28mu = lapply(seq_along(random_matrices_28mu), function(i){
  pop.projection(random_matrices_28mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_27mu = lapply(seq_along(random_matrices_27mu), function(i){
  pop.projection(random_matrices_27mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_26mu = lapply(seq_along(random_matrices_26mu), function(i){
  pop.projection(random_matrices_26mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_25mu = lapply(seq_along(random_matrices_25mu), function(i){
  pop.projection(random_matrices_25mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_24mu = lapply(seq_along(random_matrices_24mu), function(i){
  pop.projection(random_matrices_24mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_23mu = lapply(seq_along(random_matrices_23mu), function(i){
  pop.projection(random_matrices_23mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_22mu = lapply(seq_along(random_matrices_22mu), function(i){
  pop.projection(random_matrices_22mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_21mu = lapply(seq_along(random_matrices_21mu), function(i){
  pop.projection(random_matrices_21mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_20mu = lapply(seq_along(random_matrices_20mu), function(i){
  pop.projection(random_matrices_20mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_19mu = lapply(seq_along(random_matrices_19mu), function(i){
  pop.projection(random_matrices_19mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_18mu = lapply(seq_along(random_matrices_18mu), function(i){
  pop.projection(random_matrices_18mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_17mu = lapply(seq_along(random_matrices_17mu), function(i){
  pop.projection(random_matrices_17mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_16mu = lapply(seq_along(random_matrices_16mu), function(i){
  pop.projection(random_matrices_16mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_15mu = lapply(seq_along(random_matrices_15mu), function(i){
  pop.projection(random_matrices_15mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_12mu = lapply(seq_along(random_matrices_12mu), function(i){
  pop.projection(random_matrices_12mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_11mu = lapply(seq_along(random_matrices_11mu), function(i){
  pop.projection(random_matrices_11mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_10mu = lapply(seq_along(random_matrices_10mu), function(i){
  pop.projection(random_matrices_10mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_09mu = lapply(seq_along(random_matrices_09mu), function(i){
  pop.projection(random_matrices_09mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_08mu = lapply(seq_along(random_matrices_08mu), function(i){
  pop.projection(random_matrices_08mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_07mu = lapply(seq_along(random_matrices_07mu), function(i){
  pop.projection(random_matrices_07mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_06mu = lapply(seq_along(random_matrices_06mu), function(i){
  pop.projection(random_matrices_06mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_05mu = lapply(seq_along(random_matrices_05mu), function(i){
  pop.projection(random_matrices_05mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_04mu = lapply(seq_along(random_matrices_04mu), function(i){
  pop.projection(random_matrices_04mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_03mu = lapply(seq_along(random_matrices_03mu), function(i){
  pop.projection(random_matrices_03mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_02mu = lapply(seq_along(random_matrices_02mu), function(i){
  pop.projection(random_matrices_02mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

sim_projections_01mu = lapply(seq_along(random_matrices_01mu), function(i){
  pop.projection(random_matrices_01mu[[i]], 
                 n = initial_age_dist, 
                 iterations = period)})

########################################
#                                      #
#  4: Calculate mean log lambdas       #
#                                      #
########################################
# For each population projection above, get the average
# log-lambda, to look at the "sampling distribution" of the log lambdas

#############
# Observed ##
#############

# just the lambdas
log_lambdaS_obs = sapply(seq_along(sim_projections_obs), function(i){
  mean(log(sim_projections_obs[[i]]$pop.changes))
})
mean(log_lambdaS_obs)

#############
# No mu ##
#############
# just the lambdas
log_lambdaS_nomu = sapply(seq_along(sim_projections_nomu), function(i){
  mean(log(sim_projections_nomu[[i]]$pop.changes))
})
mean(log_lambdaS_nomu)

#############
# 30 mu ##
#############
# just the lambdas
log_lambdaS_30mu = sapply(seq_along(sim_projections_30mu), function(i){
  mean(log(sim_projections_30mu[[i]]$pop.changes))
})
mean(log_lambdaS_30mu)

#############
# 29 mu ##
#############
# just the lambdas
log_lambdaS_29mu = sapply(seq_along(sim_projections_29mu), function(i){
  mean(log(sim_projections_29mu[[i]]$pop.changes))
})
mean(log_lambdaS_29mu)

#############
# 28 mu ##
#############
# just the lambdas
log_lambdaS_28mu = sapply(seq_along(sim_projections_28mu), function(i){
  mean(log(sim_projections_28mu[[i]]$pop.changes))
})
mean(log_lambdaS_28mu)

#############
# 27 mu ##
#############
# just the lambdas
log_lambdaS_27mu = sapply(seq_along(sim_projections_27mu), function(i){
  mean(log(sim_projections_27mu[[i]]$pop.changes))
})
mean(log_lambdaS_27mu)

#############
# 26 mu ##
#############
# just the lambdas
log_lambdaS_26mu = sapply(seq_along(sim_projections_26mu), function(i){
  mean(log(sim_projections_26mu[[i]]$pop.changes))
})
mean(log_lambdaS_26mu)

#############
# 25 mu ##
#############
# just the lambdas
log_lambdaS_25mu = sapply(seq_along(sim_projections_25mu), function(i){
  mean(log(sim_projections_25mu[[i]]$pop.changes))
})
mean(log_lambdaS_25mu)

#############
# 24 mu ##
#############
# just the lambdas
log_lambdaS_24mu = sapply(seq_along(sim_projections_24mu), function(i){
  mean(log(sim_projections_24mu[[i]]$pop.changes))
})
mean(log_lambdaS_24mu)

#############
# 23 mu ##
#############
# just the lambdas
log_lambdaS_23mu = sapply(seq_along(sim_projections_23mu), function(i){
  mean(log(sim_projections_23mu[[i]]$pop.changes))
})
mean(log_lambdaS_23mu)

#############
# 22 mu ##
#############
# just the lambdas
log_lambdaS_22mu = sapply(seq_along(sim_projections_22mu), function(i){
  mean(log(sim_projections_22mu[[i]]$pop.changes))
})
mean(log_lambdaS_22mu)

#############
# 21 mu ##
#############
# just the lambdas
log_lambdaS_21mu = sapply(seq_along(sim_projections_21mu), function(i){
  mean(log(sim_projections_21mu[[i]]$pop.changes))
})
mean(log_lambdaS_21mu)

#############
# 20 mu ##
#############
# just the lambdas
log_lambdaS_20mu = sapply(seq_along(sim_projections_20mu), function(i){
  mean(log(sim_projections_20mu[[i]]$pop.changes))
})
mean(log_lambdaS_20mu)

#############
# 19 mu ##
#############
# just the lambdas
log_lambdaS_19mu = sapply(seq_along(sim_projections_19mu), function(i){
  mean(log(sim_projections_19mu[[i]]$pop.changes))
})
mean(log_lambdaS_19mu)

#############
# 18 mu ##
#############
# just the lambdas
log_lambdaS_18mu = sapply(seq_along(sim_projections_18mu), function(i){
  mean(log(sim_projections_18mu[[i]]$pop.changes))
})
mean(log_lambdaS_18mu)

#############
# 17 mu ##
#############
# just the lambdas
log_lambdaS_17mu = sapply(seq_along(sim_projections_17mu), function(i){
  mean(log(sim_projections_17mu[[i]]$pop.changes))
})
mean(log_lambdaS_17mu)

#############
# 16 mu ##
#############
# just the lambdas
log_lambdaS_16mu = sapply(seq_along(sim_projections_16mu), function(i){
  mean(log(sim_projections_16mu[[i]]$pop.changes))
})
mean(log_lambdaS_16mu)

#############
# 15 mu ##
#############
# just the lambdas
log_lambdaS_15mu = sapply(seq_along(sim_projections_15mu), function(i){
  mean(log(sim_projections_15mu[[i]]$pop.changes))
})
mean(log_lambdaS_15mu)

#############
# 12 mu ##
#############
# just the lambdas
log_lambdaS_12mu = sapply(seq_along(sim_projections_12mu), function(i){
  mean(log(sim_projections_12mu[[i]]$pop.changes))
})
mean(log_lambdaS_12mu)

#############
# 11 mu ##
#############
# just the lambdas
log_lambdaS_11mu = sapply(seq_along(sim_projections_11mu), function(i){
  mean(log(sim_projections_11mu[[i]]$pop.changes))
})
mean(log_lambdaS_11mu)

#############
# 10 mu ##
#############
# just the lambdas
log_lambdaS_10mu = sapply(seq_along(sim_projections_10mu), function(i){
  mean(log(sim_projections_10mu[[i]]$pop.changes))
})
mean(log_lambdaS_10mu)

#############
# 09 mu ##
#############
# just the lambdas
log_lambdaS_09mu = sapply(seq_along(sim_projections_09mu), function(i){
  mean(log(sim_projections_09mu[[i]]$pop.changes))
})

#############
# 08 mu ##
#############
# just the lambdas
log_lambdaS_08mu = sapply(seq_along(sim_projections_08mu), function(i){
  mean(log(sim_projections_08mu[[i]]$pop.changes))
})

#############
# 07 mu ##
#############
# just the lambdas
log_lambdaS_07mu = sapply(seq_along(sim_projections_07mu), function(i){
  mean(log(sim_projections_07mu[[i]]$pop.changes))
})

#############
# 06 mu ##
#############
# just the lambdas
log_lambdaS_06mu = sapply(seq_along(sim_projections_06mu), function(i){
  mean(log(sim_projections_06mu[[i]]$pop.changes))
})

#############
# 0.05 mu ##
#############
# just the lambdas
log_lambdaS_05mu = sapply(seq_along(sim_projections_05mu), function(i){
  mean(log(sim_projections_05mu[[i]]$pop.changes))
})
mean(log_lambdaS_05mu)

#############
# 0.04 mu ##
#############
# just the lambdas
log_lambdaS_04mu = sapply(seq_along(sim_projections_04mu), function(i){
  mean(log(sim_projections_04mu[[i]]$pop.changes))
})

#############
# 0.03 mu ##
#############
# just the lambdas
log_lambdaS_03mu = sapply(seq_along(sim_projections_03mu), function(i){
  mean(log(sim_projections_03mu[[i]]$pop.changes))
})

#############
# 0.02 mu ##
#############
# just the lambdas
log_lambdaS_02mu = sapply(seq_along(sim_projections_02mu), function(i){
  mean(log(sim_projections_02mu[[i]]$pop.changes))
})

#############
# 0.01 mu ##
#############
# just the lambdas
log_lambdaS_01mu = sapply(seq_along(sim_projections_01mu), function(i){
  mean(log(sim_projections_01mu[[i]]$pop.changes))
})

###########################################
#                                         #
#  5: Calculate geometric mean of lambdas #
#                                         #
###########################################

#######################
### Observed ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_obs = exp(mean(log_lambdaS_obs)) 

# Geometric median is just the mean of the log lambdas, exponentiated:
median_lambdaS_obs = exp(median(log_lambdaS_obs))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_obs = exp(quantile(log_lambdaS_obs, 0.025)) 
UB_lambdaS_obs = exp(quantile(log_lambdaS_obs, 0.975)) 
results_obs <- cbind(mean_lambdaS_obs, median_lambdaS_obs, LB_lambdaS_obs, UB_lambdaS_obs)

#######################
### No exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_nomu = exp(mean(log_lambdaS_nomu)) 

# Geometric median is just the mean of the log lambdas, exponentiated:
median_lambdaS_nomu = exp(median(log_lambdaS_nomu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_nomu = exp(quantile(log_lambdaS_nomu, 0.025)) 
UB_lambdaS_nomu = exp(quantile(log_lambdaS_nomu, 0.975)) 
results_nomu <- cbind(mean_lambdaS_nomu, median_lambdaS_nomu, LB_lambdaS_nomu, UB_lambdaS_nomu)

#######################
### 30 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_30mu = exp(mean(log_lambdaS_30mu)) 

# Geometric median is just the mean of the log lambdas, exponentiated:
median_lambdaS_30mu = exp(median(log_lambdaS_30mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_30mu = exp(quantile(log_lambdaS_30mu, 0.025)) 
UB_lambdaS_30mu = exp(quantile(log_lambdaS_30mu, 0.975)) 
results_30mu <- cbind(mean_lambdaS_30mu, median_lambdaS_30mu, LB_lambdaS_30mu, UB_lambdaS_30mu)

#######################
### 0.29 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_29mu = exp(mean(log_lambdaS_29mu)) 

# Geometric median is just the mean of the log lambdas, exponentiated:
median_lambdaS_29mu = exp(median(log_lambdaS_29mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_29mu = exp(quantile(log_lambdaS_29mu, 0.025)) 
UB_lambdaS_29mu = exp(quantile(log_lambdaS_29mu, 0.975)) 
results_29mu <- cbind(mean_lambdaS_29mu, median_lambdaS_29mu, LB_lambdaS_29mu, UB_lambdaS_29mu)

#######################
### 0.28 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_28mu = exp(mean(log_lambdaS_28mu)) 

# Geometric median is just the mean of the log lambdas, exponentiated:
median_lambdaS_28mu = exp(median(log_lambdaS_28mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_28mu = exp(quantile(log_lambdaS_28mu, 0.025)) 
UB_lambdaS_28mu = exp(quantile(log_lambdaS_28mu, 0.975)) 
results_28mu <- cbind(mean_lambdaS_28mu, median_lambdaS_28mu, LB_lambdaS_28mu, UB_lambdaS_28mu)

#######################
### 0.27 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_27mu = exp(mean(log_lambdaS_27mu)) 

# Geometric median is just the mean of the log lambdas, exponentiated:
median_lambdaS_27mu = exp(median(log_lambdaS_27mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_27mu = exp(quantile(log_lambdaS_27mu, 0.025)) 
UB_lambdaS_27mu = exp(quantile(log_lambdaS_27mu, 0.975)) 
results_27mu <- cbind(mean_lambdaS_27mu, median_lambdaS_27mu, LB_lambdaS_27mu, UB_lambdaS_27mu)

#######################
### 0.26 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_26mu = exp(mean(log_lambdaS_26mu)) 

# Geometric median is just the mean of the log lambdas, exponentiated:
median_lambdaS_26mu = exp(median(log_lambdaS_26mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_26mu = exp(quantile(log_lambdaS_26mu, 0.025)) 
UB_lambdaS_26mu = exp(quantile(log_lambdaS_26mu, 0.975)) 
results_26mu <- cbind(mean_lambdaS_26mu, median_lambdaS_26mu, LB_lambdaS_26mu, UB_lambdaS_26mu)

#######################
### 0.25 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_25mu = exp(mean(log_lambdaS_25mu)) 

# Geometric median is just the mean of the log lambdas, exponentiated:
median_lambdaS_25mu = exp(median(log_lambdaS_25mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_25mu = exp(quantile(log_lambdaS_25mu, 0.025)) 
UB_lambdaS_25mu = exp(quantile(log_lambdaS_25mu, 0.975)) 
results_25mu <- cbind(mean_lambdaS_25mu, median_lambdaS_25mu, LB_lambdaS_25mu, UB_lambdaS_25mu)

#######################
### 0.24 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_24mu = exp(mean(log_lambdaS_24mu)) 

# Geometric median is just the mean of the log lambdas, exponentiated:
median_lambdaS_24mu = exp(median(log_lambdaS_24mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_24mu = exp(quantile(log_lambdaS_24mu, 0.024)) 
UB_lambdaS_24mu = exp(quantile(log_lambdaS_24mu, 0.975)) 
results_24mu <- cbind(mean_lambdaS_24mu, median_lambdaS_24mu, LB_lambdaS_24mu, UB_lambdaS_24mu)

#######################
### 0.23 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_23mu = exp(mean(log_lambdaS_23mu)) 

# Geometric median is just the mean of the log lambdas, exponentiated:
median_lambdaS_23mu = exp(median(log_lambdaS_23mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_23mu = exp(quantile(log_lambdaS_23mu, 0.023)) 
UB_lambdaS_23mu = exp(quantile(log_lambdaS_23mu, 0.975)) 
results_23mu <- cbind(mean_lambdaS_23mu, median_lambdaS_23mu, LB_lambdaS_23mu, UB_lambdaS_23mu)

#######################
### 0.22 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_22mu = exp(mean(log_lambdaS_22mu)) 

# Geometric median is just the mean of the log lambdas, exponentiated:
median_lambdaS_22mu = exp(median(log_lambdaS_22mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_22mu = exp(quantile(log_lambdaS_22mu, 0.022)) 
UB_lambdaS_22mu = exp(quantile(log_lambdaS_22mu, 0.975)) 
results_22mu <- cbind(mean_lambdaS_22mu, median_lambdaS_22mu, LB_lambdaS_22mu, UB_lambdaS_22mu)

#######################
### 0.21 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_21mu = exp(mean(log_lambdaS_21mu)) 

# Geometric median is just the mean of the log lambdas, exponentiated:
median_lambdaS_21mu = exp(median(log_lambdaS_21mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_21mu = exp(quantile(log_lambdaS_21mu, 0.021)) 
UB_lambdaS_21mu = exp(quantile(log_lambdaS_21mu, 0.975)) 
results_21mu <- cbind(mean_lambdaS_21mu, median_lambdaS_21mu, LB_lambdaS_21mu, UB_lambdaS_21mu)

#######################
### 0.20 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_20mu = exp(mean(log_lambdaS_20mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_20mu = exp(median(log_lambdaS_20mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_20mu = exp(quantile(log_lambdaS_20mu, 0.025)) 
UB_lambdaS_20mu = exp(quantile(log_lambdaS_20mu, 0.975)) 
results_20mu <- cbind(mean_lambdaS_20mu, median_lambdaS_20mu, LB_lambdaS_20mu, UB_lambdaS_20mu)

#######################
### 0.19 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_19mu = exp(mean(log_lambdaS_19mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_19mu = exp(median(log_lambdaS_19mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_19mu = exp(quantile(log_lambdaS_19mu, 0.025)) 
UB_lambdaS_19mu = exp(quantile(log_lambdaS_19mu, 0.975)) 
results_19mu <- cbind(mean_lambdaS_19mu, median_lambdaS_19mu, LB_lambdaS_19mu, UB_lambdaS_19mu)

#######################
### 0.18 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_18mu = exp(mean(log_lambdaS_18mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_18mu = exp(median(log_lambdaS_18mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_18mu = exp(quantile(log_lambdaS_18mu, 0.025)) 
UB_lambdaS_18mu = exp(quantile(log_lambdaS_18mu, 0.975)) 
results_18mu <- cbind(mean_lambdaS_18mu, median_lambdaS_18mu, LB_lambdaS_18mu, UB_lambdaS_18mu)

#######################
### 0.17 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_17mu = exp(mean(log_lambdaS_17mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_17mu = exp(median(log_lambdaS_17mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_17mu = exp(quantile(log_lambdaS_17mu, 0.025)) 
UB_lambdaS_17mu = exp(quantile(log_lambdaS_17mu, 0.975)) 
results_17mu <- cbind(mean_lambdaS_17mu, median_lambdaS_17mu, LB_lambdaS_17mu, UB_lambdaS_17mu)

#######################
### 0.16 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_16mu = exp(mean(log_lambdaS_16mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_16mu = exp(median(log_lambdaS_16mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_16mu = exp(quantile(log_lambdaS_16mu, 0.025)) 
UB_lambdaS_16mu = exp(quantile(log_lambdaS_16mu, 0.975)) 
results_16mu <- cbind(mean_lambdaS_16mu, median_lambdaS_16mu, LB_lambdaS_16mu, UB_lambdaS_16mu)

#######################
### 0.15 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_15mu = exp(mean(log_lambdaS_15mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_15mu = exp(median(log_lambdaS_15mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_15mu = exp(quantile(log_lambdaS_15mu, 0.025)) 
UB_lambdaS_15mu = exp(quantile(log_lambdaS_15mu, 0.975)) 
results_15mu <- cbind(mean_lambdaS_15mu, median_lambdaS_15mu, LB_lambdaS_15mu, UB_lambdaS_15mu)

#######################
### 0.12 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_12mu = exp(mean(log_lambdaS_12mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_12mu = exp(median(log_lambdaS_12mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_12mu = exp(quantile(log_lambdaS_12mu, 0.025)) 
UB_lambdaS_12mu = exp(quantile(log_lambdaS_12mu, 0.975)) 
results_12mu <- cbind(mean_lambdaS_12mu, median_lambdaS_12mu, LB_lambdaS_12mu, UB_lambdaS_12mu)

#######################
### 0.11 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_11mu = exp(mean(log_lambdaS_11mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_11mu = exp(median(log_lambdaS_11mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_11mu = exp(quantile(log_lambdaS_11mu, 0.025)) 
UB_lambdaS_11mu = exp(quantile(log_lambdaS_11mu, 0.975)) 
results_11mu <- cbind(mean_lambdaS_11mu, median_lambdaS_11mu, LB_lambdaS_11mu, UB_lambdaS_11mu)

#######################
### 0.10 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_10mu = exp(mean(log_lambdaS_10mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_10mu = exp(median(log_lambdaS_10mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_10mu = exp(quantile(log_lambdaS_10mu, 0.025)) 
UB_lambdaS_10mu = exp(quantile(log_lambdaS_10mu, 0.975)) 
results_10mu <- cbind(mean_lambdaS_10mu, median_lambdaS_10mu, LB_lambdaS_10mu, UB_lambdaS_10mu)

#######################
### 0.09 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_09mu = exp(mean(log_lambdaS_09mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_09mu = exp(median(log_lambdaS_09mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_09mu = exp(quantile(log_lambdaS_09mu, 0.025)) 
UB_lambdaS_09mu = exp(quantile(log_lambdaS_09mu, 0.975)) 
results_09mu <- cbind(mean_lambdaS_09mu, median_lambdaS_09mu, LB_lambdaS_09mu, UB_lambdaS_09mu)

#######################
### 0.08 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_08mu = exp(mean(log_lambdaS_08mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_08mu = exp(median(log_lambdaS_08mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_08mu = exp(quantile(log_lambdaS_08mu, 0.025)) 
UB_lambdaS_08mu = exp(quantile(log_lambdaS_08mu, 0.975)) 
results_08mu <- cbind(mean_lambdaS_08mu, median_lambdaS_08mu, LB_lambdaS_08mu, UB_lambdaS_08mu)

#######################
### 0.07 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_07mu = exp(mean(log_lambdaS_07mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_07mu = exp(median(log_lambdaS_07mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_07mu = exp(quantile(log_lambdaS_07mu, 0.025)) 
UB_lambdaS_07mu = exp(quantile(log_lambdaS_07mu, 0.975)) 
results_07mu <- cbind(mean_lambdaS_07mu, median_lambdaS_07mu, LB_lambdaS_07mu, UB_lambdaS_07mu)

#######################
### 0.06 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_06mu = exp(mean(log_lambdaS_06mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_06mu = exp(median(log_lambdaS_06mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_06mu = exp(quantile(log_lambdaS_06mu, 0.025)) 
UB_lambdaS_06mu = exp(quantile(log_lambdaS_06mu, 0.975)) 
results_06mu <- cbind(mean_lambdaS_06mu, median_lambdaS_06mu, LB_lambdaS_06mu, UB_lambdaS_06mu)

#######################
### 0.05 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_05mu = exp(mean(log_lambdaS_05mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_05mu = exp(median(log_lambdaS_05mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_05mu = exp(quantile(log_lambdaS_05mu, 0.025)) 
UB_lambdaS_05mu = exp(quantile(log_lambdaS_05mu, 0.975)) 
results_05mu <- cbind(mean_lambdaS_05mu, median_lambdaS_05mu, LB_lambdaS_05mu, UB_lambdaS_05mu)

#######################
### 0.04 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_04mu = exp(mean(log_lambdaS_04mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_04mu = exp(median(log_lambdaS_04mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_04mu = exp(quantile(log_lambdaS_04mu, 0.025)) 
UB_lambdaS_04mu = exp(quantile(log_lambdaS_04mu, 0.975)) 
results_04mu <- cbind(mean_lambdaS_04mu, median_lambdaS_04mu, LB_lambdaS_04mu, UB_lambdaS_04mu)

#######################
### 0.03 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_03mu = exp(mean(log_lambdaS_03mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_03mu = exp(median(log_lambdaS_03mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_03mu = exp(quantile(log_lambdaS_03mu, 0.025)) 
UB_lambdaS_03mu = exp(quantile(log_lambdaS_03mu, 0.975)) 
results_03mu <- cbind(mean_lambdaS_03mu, median_lambdaS_03mu, LB_lambdaS_03mu, UB_lambdaS_03mu)

#######################
### 0.02 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_02mu = exp(mean(log_lambdaS_02mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_02mu = exp(median(log_lambdaS_02mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_02mu = exp(quantile(log_lambdaS_02mu, 0.025)) 
UB_lambdaS_02mu = exp(quantile(log_lambdaS_02mu, 0.975)) 
results_02mu <- cbind(mean_lambdaS_02mu, median_lambdaS_02mu, LB_lambdaS_02mu, UB_lambdaS_02mu)

#######################
### 0.01 exploitation ###
#######################
# Geometric mean is just the mean of the log lambdas, exponentiated:
mean_lambdaS_01mu = exp(mean(log_lambdaS_01mu)) 

# Geometric mean is just the mean of the log lambdas, exponentiated:
median_lambdaS_01mu = exp(median(log_lambdaS_01mu))

# Presumably we can find the 95% CIs in the same way:
LB_lambdaS_01mu = exp(quantile(log_lambdaS_01mu, 0.025)) 
UB_lambdaS_01mu = exp(quantile(log_lambdaS_01mu, 0.975)) 
results_01mu <- cbind(mean_lambdaS_01mu, median_lambdaS_01mu, LB_lambdaS_01mu, UB_lambdaS_01mu)

########################################
## Compare stochastic lambdas and CIs ##
########################################

Results <- data.frame(rbind(results_obs, results_30mu, results_29mu, results_28mu, results_27mu, 
                            results_26mu, results_25mu, results_24mu, results_23mu, results_22mu, results_21mu, results_20mu, 
                            results_19mu, results_18mu, results_17mu, results_16mu, results_15mu, results_12mu, results_11mu,
                            results_10mu, results_09mu, results_08mu, results_07mu, results_06mu, results_05mu, results_04mu,
                            results_03mu, results_02mu, results_01mu, results_nomu))
names(Results) = c("Mean Lambda", "Median Lambda", "Lower Bound", "Upper Bound")
Exploitation = c("Obs", "30mu", "29mu", "28mu", "27mu", "26mu", "25mu", "24mu", "23mu", "22mu", "21mu", 
                 "20mu", "19mu", "18mu", "17mu", "16mu", "15mu", "12mu", "11mu","10mu", 
                 "09mu", "08mu", "07mu", "06mu", "05mu", "04mu", "03mu", "02mu", "01mu", "No mu")
Results = cbind(Exploitation, Results)
Results

# write.csv(Results, "S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/R code/Matrix/corrected_TPD_results_midfecund_20years.csv", row.names = F)

##################################################
#                                                #
#  6: Sensitivity and Elasticity Analysis        #
#                                                #
##################################################

# Follows Caswell 14.1 equation on calculating sensitivity and elasticity analysis 
# Allow use of the random_matrices as opposed to just one matrix 
sens <- stoch.sens(random_matrices_obs)
sens
# write.csv(sens,"S:/CNR/Labs-Quist/Blackburn/Projects/WST/California/R code/Matrix/sens_20yrs.csv")

