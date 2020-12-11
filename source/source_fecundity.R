# ******************************************************************************
# Created: 30-Mar-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: this file loads fecundity data from .xlsx file, filters & then
#          cleans desired data, & then saves data to .rds file for use in the
#          model. original data in "eggs_devore_chapman.xlsx"
#          provided by S. Blackburn (U. Idaho) & data therein from 
#          DeVore et al. 1995 & Chapman et al. 1996 (or 1989?); these provide
#          best available (currently) fecundity data for white sturgeon & these
#          data are used by the population model
# ******************************************************************************

# load .xlsx data ---------------------------------------------------------

dat_file <- "eggs_devore_chapman.xlsx"

fecundity <- readxl::read_xlsx(
  path = file.path("blackburn_code/Fecundity", dat_file),
  sheet = "Sheet1"
)

# filter data & select desired fields -------------------------------------

# not needed at this time

# clean data: rename fields -----------------------------------------------

new_col_names <- c("ForkLen", "Devore", "Chapman")

colnames(fecundity) <- new_col_names

# add metadata to fields --------------------------------------------------

# this will help when loading the .rds file for future analytics by providing
# some understanding of data in each field
comment(fecundity$ForkLen) <- "fork length (cm)"
comment(fecundity$Devore) <- "from DeVore eqn: eggs = 0.072 * ForkLen^2.94"
comment(fecundity$Chapman) <- "from Chapman 1989 (doctoral dissertation)"

attr(fecundity, which = "metadata") <-
  "eggs / length class for white sturgeon from cited literature, best available"

attr(fecundity, which = "source_file") <- "eggs_devore_chapman.xlsx"

# save to RDS file --------------------------------------------------------

# for use in future analytics

# can double-check if need be - should all be 0
# Map(function(x) sum(is.na(x)), age_len)

saveRDS(object = fecundity, file = "data/model/FecundityWST.rds")

# clean up ----------------------------------------------------------------

# clear environment
rm(list = ls())
