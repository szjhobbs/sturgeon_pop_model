# ******************************************************************************
# Created: 29-Jun-2016
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file creates table of annual CPFV CPUE for (at least) fishing
#          blocks within the San Francisco Esturary (SFE). Variables can be
#          modified as desired.
# ******************************************************************************

# Libraries and Source Files ----------------------------------------------

# libraries needed
library(dplyr)

# loads sturgeon cpfv data
source(file = "source/source_load_cpfv.R")

# Create variables for analytics ------------------------------------------

# terms
# sfe: San Francisco Estuary (all fishing blocks east of Golden Gate)
# successful trips: where at least one angler caught and kept 1 fish,
#                   sometimes known as trips with catch

# all sturgeons:  market code = 470
# green sturgeon: market code = 471
# white sturgeon: market code = 472

# some global variables for ease of analytics
bool_sfe <- SturgeonCpfv$Block %in% c(300:308, 488, 489)

# for successful trips
bool_successful <- SturgeonCpfv$Kept > 0

# for white sturgeon only
bool_species <- SturgeonCpfv$Species %in% "WST"

# Analysis: annual white sturgeon CPUE successful trips within SFE --------

sfe_cpue <- SturgeonCpfv %>%
  filter(bool_successful & bool_sfe & bool_species) %>%
  group_by(Year) %>%
  summarise(
    Trips = length(TripID),
    Catch = sum(Kept, na.rm = TRUE),
    Hours = sum(Hours, na.rm = TRUE),
    Anglers = sum(Anglers, na.rm = TRUE),
    AngHours = sum(AnglerHours, na.rm = TRUE),
    # catch per 100 angler hours
    CPUE = 100 * (Catch / AngHours)
  )

# to view in console
as.data.frame(sfe_cpue)

# session clean up --------------------------------------------------------

rm(bool_successful, bool_sfe, bool_species)
