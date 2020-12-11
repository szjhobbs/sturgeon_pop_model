# ******************************************************************************
# Created: 29-Jun-2016
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file loads all cpfv data from the data/cpfv directory
# ******************************************************************************

# NOTE: cpfv = commercial passenger fishing vessel (aka party boat)

cpfv_dir <- "data/cpfv/"

# load rds file -----------------------------------------------------------

SturgeonCpfv <- readRDS(file = paste0(cpfv_dir, "SturgeonCpfv.rds"))

# clean up ----------------------------------------------------------------
rm(cpfv_dir)
