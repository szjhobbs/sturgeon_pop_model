# ******************************************************************************
# Created: 28-Jun-2016
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file loads all tagging data from the data/tagging directory
# ******************************************************************************

tagging_dir <- "data/tagging/"

# load csv files ----------------------------------------------------------

# TaggingLocations & Vessels function more like look-up tables, providing names
# to abbreviations in the tagging data dataframes

TaggingLocations <- read.csv(
  file = paste0(tagging_dir, "TaggingLocations.csv"),
  header = TRUE,
  stringsAsFactors = FALSE
)

Vessels <- read.csv(
  file = paste0(tagging_dir, "Vessels.csv"),
  header = TRUE,
  stringsAsFactors = FALSE
)

# load rds files ----------------------------------------------------------

AnglerTagReturn <- readRDS(file = paste0(tagging_dir, "AnglerTagReturn.rds"))
Effort <- readRDS(file = paste0(tagging_dir, "Effort.rds"))
Environmentals <- readRDS(file = paste0(tagging_dir, "Environmentals.rds"))
SturgeonAll <- readRDS(file = paste0(tagging_dir, "SturgeonAll.rds"))

# clean up ----------------------------------------------------------------
rm(tagging_dir)
