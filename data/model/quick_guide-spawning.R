# ******************************************************************************
# Created: 03-Apr-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: quick guide to understand data behind S. Blackburn's 'prob_spawn'
#          variable in 'corrected_transient_midfecund_current.R' model code;
#          variable contains Age, prob(ability of spawning), & SE fields; herein
#          I reproduce S. Blackburn's methods & results for this variable;
#          S. Blackburn's code is in mat2.R starting at line 108
# ******************************************************************************

# TODO: experiement with approx_lens, should be consistent with fecundity
# TODO: create function to produce this output (as in prob_spawn, so that
#       ages are included rather than [or in addition to] fork length) &
#       include parameter to vary spawning periodicity

# load data
SpawningProbWST <- readRDS(file = "data/model/SpawningProbWST.rds")

# S. Blackburn used logit model to predict probability of spawning from data in
# literature (see Chapman 1989, Figure 14); Blackburn digitized this figure (see
# file chapman(2).jpg) to derive starting data in SpawningProbWST.rds

mod <- glm(
  formula = PMat ~ ForkLen,
  family = binomial(link = "logit"),
  data = SpawningProbWST
)

# ******************************************************************************
# post running code above I get the warning below - not sure if S. Blackburn got
# the same warning, & to date (03-Apr-2018) I have not handled this warning

# Warning message:
#   In eval(family$initialize) : non-integer #successes in a binomial glm!
# ******************************************************************************

# these are approximate lengths I think S. Blackburn used to get equivalent age,
# though S. Blackburn in narrative mentions starting at 104
approx_lens <- c(102, 118, 125, 136, 149, 153, 163, 168, 181, 188)

# though S. Blackburn predicts on lengths 95-250, for this purpose, I am using
# what I think are S. Blackburn's lengths (at age); nd = new data

# nd <- data.frame(ForkLen = as.numeric(95:250))
nd <- data.frame(ForkLen = approx_lens)

mod_pred <- predict(
  object = mod,
  newdata = nd,
  type = "response",
  se.fit = TRUE
)

# combine for plotting & viewing
dat <- cbind(nd, mod_pred)

# field not needed
dat$residual.scale <- NULL

# per S. Blackburn (narrative & personal comm.), we need to multiply 'fit' by
# assumed spawning periodicity (i.e., 15%), & then to get SE (standard error) we
# use the generic 20% of 'fit' (this per statistician with whom S. Blackburn
# consulted; se.fit is not the SE to use)
dat$midfec <- dat$fit * 0.15
dat$SE <- dat$fit * 0.15 * 0.20

# load prob_spawing from 'corrected_transient_midfecund_current.R'
prob_spawn <- subset(prob_spawn, subset = Age > 9)

# compare probabilities
plot(x = prob_spawn$prob, y = dat$midfec)
abline(a = 0, b = 1, col = 2)

# compare SEs
plot(x = prob_spawn$SE, y = dat$SE)
abline(a = 0, b = 1, col = 2)
