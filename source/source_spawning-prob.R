# ******************************************************************************
# Created: 30-Mar-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: this file creates datafile for survival rates of age0-age2 white
#          sturgeon; these data come from the literature as no other data
#          exist (currently) for white sturgeon this young; saves data to .rds
#          file for use in the model. original data  provided by S. Blackburn
#          (U. Idaho) in Masters thesis (Table 2)
# ******************************************************************************

# ******************************************************************************
#                           Age Value SE    Source
# S0	Egg to age 1 survival	0	  0.002	0.003	Caroffino et al. (2010)
# S1	Age 1 survival	      1	  0.250	0.008	Pine et al. (2001)
# S3 	Age 2 survival 	      2	  0.840	0.168	Ireland et al. (2002)
# ******************************************************************************

# create dataframe --------------------------------------------------------

SurvivalYoungWST <- data.frame(
  Age = c(0:2),
  Survival = c(0.002, 0.250, 0.840),
  StdErr = c(0.003, 0.008, 0.168),
  Source = c(
    "Caroffino et al. (2010)",
    "Pine et al. (2001)",
    "Ireland et al. (2002)"
  ),
  stringsAsFactors = FALSE
)

# filter data & select desired fields -------------------------------------

# not needed at this time

# clean data: rename fields -----------------------------------------------

# none needed at this time

# add metadata to fields --------------------------------------------------

# this will help when loading the .rds file for future analytics by providing
# some understanding of data in each field

attr(SurvivalYoungWST, which = "metadata") <-
  "survival for young white sturgeon, best available"

attr(SurvivalYoungWST, which = "source_file") <- "S. Blackburn thesis, Table 2"

# save to RDS file --------------------------------------------------------

# for use in future analytics

# can double-check if need be - should all be 0
# Map(function(x) sum(is.na(x)), age_len)

saveRDS(object = SurvivalYoungWST, file = "data/model/SurvivalYoungWST.rds")

# clean up ----------------------------------------------------------------

# clear environment
rm(list = ls())
