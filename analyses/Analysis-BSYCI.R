# ******************************************************************************
# Created: 05-Jul-2016
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file loads Bay Study data from data/baystudy & then uses this
#          data to calculate the year-class index (YCI) for White Sturgeon
#          within the San Francisco Estuary (SFE); data are collected year-round
#          by the Bay Study unit & currently data are contained in the database
#          "Fish CPUE and Index calc_March2016.mdb"; Bay Study unit has QAQCed
#          all data & 2016 data might be available March 2017
# ******************************************************************************

# Modified: 28-Sep-2016 (J. DuBois)

# TODO: add coding for graphing of index
# TODO: ensure testing is done on output when combining nets or series or both
#       (e.g., including both nets 1 & 2 for series 1)

# File Paths --------------------------------------------------------------

# data_import <- "C:/Data/jdubois/RDataConnections/BayStudyFish"
# source_files <- "C:/Data/jdubois/RSourcedCode"

# Libraries and Source Files ----------------------------------------------

# load libraries
library(ggplot2)

# source files
source(file = "source/source_load_bsyci.R")

# clean up (as needed)
# rm(data_import, source_files, lib_loc)

# Variables for analytics -------------------------------------------------

# fields in dataframes on which to match records
fields1 <- c("Year", "Survey", "Bay", "Net", "Series")

# Match records -----------------------------------------------------------

# FreqByAgeCat contains records for only when White Sturgeon were caught -
# however to calculate mean cpue with need records with 0 catch, matching
# records of FreqByAgeCat with IndexTows will allow for this
mr_index_tows <- MatchRecords(
  df1 = IndexTows,
  df2 = FreqByAgeCat,
  onFields = fields1
)

# Calculate mean cpue & corresponding index -------------------------------

# creates a dataframe of mean cpue values along with corresponding index (cpue *
# bay weight) by age category
BSCpueIndex <- MeanBSCpue(
  split(
    summary(mr_index_tows),
    AgeCat,
    val = CPUE,
    dfSelect = "DF2"
  )
)

# Calculate YCI -----------------------------------------------------------

# ********************************************************************
# to calculate historical YCI for White Sturgeon we use the following:
# Age0 abundance index is mean from Apr-Oct (i.e., surveys 4-10)
# Age1 abundance index is mean from Feb-Oct (i.e., surveys 2-10)
# net = 2 (otter trawl)
# series = 1 (historic index stations)

# though Age2 (or Age2+) is not used in YCI historically data from
# Feb-Oct (i.e., surveys 2-10) have been used

# where a survey was not conducted for a particular year the index is
# NA, where a survey did not catch White Sturgeon the index is 0
# ********************************************************************

# below will provide YCI as it has been calculated historically (output is a
# list & first element of this list (YCI) contains the annual YCI along with
# variance & standard error)
yci_historic <- CalcYCI(
  dat = SumIndex(dat = BSCpueIndex, net = 2, series = 1),
  AgeCat == "Age0" & Survey %in% 4:10,
  AgeCat == "Age1" & Survey %in% 2:10,
  AgeCat == "Age2+" & Survey %in% 2:10
)

yci_historic

# Plot: annual YCI --------------------------------------------------------

# NOTE: still tinkering with this section & best way to plot YCI
#       (J. DuBois 28-Sep-2016)

# for plotting
yci_historic$YCI$Year <- as.numeric(yci_historic$YCI$Year)

# annual YCI with +/- 1 SE
ggplot(data = yci_historic$YCI, mapping = aes(x = Year, y = YCI)) +
  # geom_point() +
  geom_bar(stat = "identity") +
  geom_errorbar(
    mapping = aes(ymax = YCI + YCI_SE, ymin = YCI -YCI_SE),
    width = 0
  ) +
  scale_y_continuous(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0))

# Plot: mean cpue ---------------------------------------------------------

# still tinkering with plots for mean cpue (J. DuBois 28-Sep-2016)

# ggplot(data = BSCpueIndex, mapping = aes(x = Year, y = CPUE_Avg)) +
#   geom_bar(mapping = aes(fill = factor(Survey)), stat = "identity") +
#   facet_grid(facets = AgeCat ~ .)
# 
# ggplot(data = BSCpueIndex, mapping = aes(x = factor(Bay), y = CPUE_Avg)) +
#   geom_bar(mapping = aes(fill = AgeCat), stat = "identity") +
#   facet_wrap(facets = ~Year, ncol = 6)
# 
# ggplot(data = BSCpueIndex, mapping = aes(x = Year, y = factor(Survey))) +
#   geom_tile(mapping = aes(fill = CPUE_Avg)) +
#   facet_wrap(facets = ~AgeCat)
