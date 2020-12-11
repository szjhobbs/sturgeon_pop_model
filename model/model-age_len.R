# ******************************************************************************
# Created: 17-Apr-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: file creates age-length key (alk) from aged White Sturgeon (n~360);
#          then uses alk to assign ages to non-aged fish; will bin lengths
#          accordingly, see S. Blackburn's code in 'ALK_cdfw.R'
# ******************************************************************************

# TODO: (17-Apr-2018) create function(s) for this process, particularly scale
#       and running linear regression (to get ages 1 & 2)
# TODO: (17-Apr-2018) try running lin reg on non-scaled count to see if ages
#       1 & 2 compare better to S. Blackburn
# TODO: (17-Apr-2018) incorporate adjusted catch (i.e., gear selectivity)

# NOTE: for now (17-Apr-2018) source 'quick_guide-age_assign.R' & use
#       SturgeonAll dataframe; in the future I'll streamline this process but
#       right now I just want to move things along

# source data & files -----------------------------------------------------

source(file = "data/model/quick_guide-age_assign.R")
source(file = "source/functions-age_len.R")

# some clean up for now
rm(ages, index, n, used, Sturgeon2014_2016)

# subset length & age data ------------------------------------------------

# for this purpose, we'll limit age to no greater than 19 and length remove 217
# cm FL (and this fish was not aged)

SturgeonAll <- subset(SturgeonAll, subset = Age < 20 | is.na(Age) & FL != 217)

# some information about age & length -------------------------------------

with(data = SturgeonAll, expr = {
  print(table(Age, useNA = "ifany"))

  print(table(FL, useNA = "ifany"))

  tail(table(FL, Age, useNA = "ifany"), n = 15)
})

range(SturgeonAll[["FL"]])
sort(SturgeonAll[["FL"]][SturgeonAll[["FL"]] >= 200])
range(SturgeonAll[["RelDate"]])

# variables ---------------------------------------------------------------

# need to set length breaks (bins) for age-len key (for right now, we will hard
# code this, later we'll try to create this from the length range)
len_breaks <- seq(from = 50, to = 205, by = 5)

# create age-length key from raw data (FL) & aged samples (n ~360) --------

# FSA::alkIndivAge() needs bin labels to be numeric, thus setting breakLabs to
# length breaks less the last bin
alkey <- MakeALKey(
  dat = SturgeonAll,
  len = FL,
  age = Age,
  lenBreaks = len_breaks,
  breakLabs = len_breaks[-length(len_breaks)]
)

# some rows are 0 (i.e., no catch in that bin), can play around with bin width,
# but to follow S. Blackburn for now we'll use 5 cm
rowSums(alkey[["ALKey"]])

# use ALKey to assign ages to fish ----------------------------------------

# using ~ length (in this case FL) will add to dataframe a field named 'age';
# rather than do this & then employ rbind (as displayed in the help file), I'll
# extract all ages & then assign ages to 'Age' in SturgeonAll where Age is NA

# type = "SR" is semi-random see Isermann and Knight (2005); allows for using
# ALKey to assign ages to each fish NOT aged by way of scales or fin rays, etc.

# type = "SR" is default, which is what S. Blackburn used; for now we'll use
# this but will need to read Isermann and Knight (2005) to see if "CR" is more
# appropriate (17-Apr-2018)

# 'seed' was not used by S. Blackburn but is important to reproduce same results
# each time code is run (per help file..."allows repeatability of results")

ages <- FSA::alkIndivAge(
  key = alkey[["ALKey"]],
  formula = ~ FL,
  data = SturgeonAll,
  type = "SR",
  seed = 123
)[["age"]]

table(ages, useNA = "ifany")

# use ages to replace NAs with age value ----------------------------------

# in SturgeonAll, most Age values are NA so we'll use values in ages variable

test <- SturgeonAll[["Age"]]

is_age_na <- is.na(SturgeonAll[["Age"]])

SturgeonAll[["Age"]][is_age_na] <- ages[is_age_na]

# # un-comment to check
# table(SturgeonAll[["Age"]], useNA = "ifany")
# View(cbind(Old = test, New = SturgeonAll[["Age"]]))

# clean up
rm(test, is_age_na)

# get proportion of each age ----------------------------------------------

count_at_age <- table(SturgeonAll[["Age"]])
prop_at_age <- prop.table(count_at_age)

# scale & run lin reg to get ages 1 & 2 -----------------------------------

# S. Blackburn scaled by initial abundance of 48K, then employed linear reg to
# get values for ages 1 & 2

init_abun <- 48e+3

age_dist <- init_abun * prop_at_age

# frml <- as.formula(log(age_dist) ~ as.numeric(names(age_dist)))

# from S. Blackburn's equations in "size_selective_data.xlsx" it appears
# log(count), where count is raw (or adjusted using gear selectivity) catch
# data; to achieve age 1 & age 2 values in "initial_age_dist" variable, it
# appears "count" in this case is (or should be) proportion (age3-age19) * 48K,
# we then take the natural log of those numbers & perform linear regression with
# ages (continuous) as the x axis, doing so gets close to S. Blackburn's numbers
# with age 1 & age 2 values still a bit off; I tried log(raw catch) but that
# appeared to only make things worse, below I use log(age_dist) where "age_dist"
# is proportion of raw catch * 48K (initial assumed starting abundance) -
# 18-Apr-2018 (J. DuBois)

xval <- as.numeric(names(age_dist))
yval <- log(age_dist)

mod <- lm(yval ~ xval)

plot(x = xval, y = yval, xlab = "Age", ylab = "log(Freq)")
abline(mod, col = 2)

plot(count_at_age, type = "b")

age1_2 <- exp(predict(object = mod, newdata = list(xval = c(1, 2))))

# take 1/2 for females (per S. Blackburn)
age_dist <- c(age1_2, age_dist) / 2

# current age-0 abundance represents the number of eggs potentially in the
# system in a given year (per S. Blackburn)
age0 <- 141491743
names(age0) <- 0

age_dist <- c(age0, age_dist)

# initial_age_dist from S. Blackburn's code
plot(
  x = initial_age_dist[-1, "freq"],
  y = age_dist[-1],
  xlab = "Blackburn",
  ylab = "CDFW",
  col = c(4, 4, rep(1, times = 17))
)
abline(a = 0, b = 1, col = 2)
