# ******************************************************************************
# Created: 04-Apr-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: quick guide to understand how S. Blackburn used age-length key to
#          assign ages to individual (non-aged) fish; process requires linking
#          fish in age-length key to fish in model's dataset (2014-2016) as age-
#          length key was created from fish sampled during this period, so we
#          don't want to assign an age to a fish aged directly through the fin-
#          ray method; S. Blackburn's code in 'ALK_cdfw.R'
# ******************************************************************************

# TODO: 

# NOTE: best I can do linking assigned ages back to CDFW data is presented
#       herein. I prefer using the SturgeonAll, & then assigning ages based
#       on match with release date and fork length (cm); with this method we
#       match 361 of the 372 ages; we can do more in-depth comparison with
#       S. Blackburn's CDFW_WST.xlsx file, but in the end we still may not
#       match all 372 back to the original CDFW data record (16-Apr-2018)

# load data ---------------------------------------------------------------

# we culled the age & length data from S. Blackburn's
# "CA_FinRayInventory_Ageing_Blackburn.xlsx" (tab 'aging_master') & saved it as
# .rds (AgeLengthWST.rds); we need to match (using date & length) this dataset
# to SturgeonAll (subsetted on 2014-2016)

AgeLengthWST <- readRDS(file = "data/model/AgeLengthWST.rds")
SturgeonAll <- readRDS(file = "data/tagging/SturgeonAll.rds")
Sturgeon2014_2016 <- readRDS(file = "data/model/CDFW-2014_2016.rds")

# for ease of working with this data, subset on years 2014-2016 & only White
# Stureon (should be ~1000 fish)
SturgeonAll <- subset(
  SturgeonAll,
  subset = RelYear %in% 2014:2016 & Species %in% "White"
)

# FIO, 6 are recaptured (from previous seasons?) fish
table(SturgeonAll[["StuType"]], useNA = "ifany")
table(Sturgeon2014_2016[["StuType"]], useNA = "ifany")

# link ages to fish in Sturgeon2014_2016 using FishID ---------------------

index <- match(
  x = Sturgeon2014_2016[["FishID"]],
  table = AgeLengthWST[["FishID"]]
)

# assign age field to White Sturgeon from 2014-2016
Sturgeon2014_2016$Age <- AgeLengthWST[["Age"]][index]

# link ages to fish in SturgeonAll ----------------------------------------

# currently (04-Apr-2018) we can only do this using RelDate & ForkLen

# vector to keep track of which record in AgeLengthWST has already been used
# (i.e., age now assigned in SturgeonAll)
used <- vector(mode = "logical", length = nrow(AgeLengthWST))

n <- nrow(SturgeonAll)

# Reduce(paste, x = AgeLengthWST[, c("RelDate", "ForkLen")])
# 
# 
# Reduce(paste, x = SturgeonAll[1, c("RelDate", "FL")]) %in% 
#   Reduce(paste, x = AgeLengthWST[, c("RelDate", "ForkLen")])
# 
# Reduce(paste, x = AgeLengthWST[, c("RelDate", "ForkLen")]) %in% 
#   Reduce(paste, x = SturgeonAll[1, c("RelDate", "FL")]) & !used

# below works but gets to 361 and not 362 as I had hoped. This is OK and is the
# best I can do for now; just need to decide if I want to have vapply return
# index or actual ages
  
ages <- vapply(seq_len(n), FUN = function(i) {
  
  bool <- Reduce(paste, x = AgeLengthWST[, c("RelDate", "ForkLen")]) %in% 
    Reduce(paste, x = SturgeonAll[i, c("RelDate", "FL")]) & !used 
  
  # if all bool are FALSE, no matches & no need to contiue since which(FALSE)[1]
  # will be NA anyway
  if (!any(bool)) return(NA)
  
  # we want only the first value of > 1 matches
  val <- which(bool, useNames = FALSE)[1]
  
  # if (is.na(val)) stop("val is NA", call. = FALSE)
  
  # if (length(val) == 0) return(NA)
  
  # # proceed only if val is not NA, val should be a single integer to proceed
  # if (is.na(val)) return(NA)
  
  # prevents age (from match based on FL & date) to be used again
  used[val] <<- TRUE
  
  # get the age 
  # as.numeric(AgeLengthWST[val, "Age"])
  AgeLengthWST[["Age"]][val]
  
}, FUN.VALUE = numeric(1L))

# check ages before adding as field to Sturgeon df ------------------------

table(ages, useNA = "ifany")

# create Age field in SturgeonAll -----------------------------------------

SturgeonAll$Age <- ages

# choose a dataset --------------------------------------------------------

# can use Sturgeon2014_2016 or SturgeonAll (some ages will vary)

table(Sturgeon2014_2016[["Age"]], useNA = "ifany") - 
  table(SturgeonAll[["Age"]], useNA = "ifany")
