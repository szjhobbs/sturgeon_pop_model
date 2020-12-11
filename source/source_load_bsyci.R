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

# Modified: 27-Sep-2016 (J. DuBois)

# File Paths --------------------------------------------------------------

# data_import <- "C:/Data/jdubois/RDataConnections/BayStudyFish"
# source_files <- "C:/Data/jdubois/RSourcedCode"

# Libraries and Source Files ----------------------------------------------

# load libraries
# library(dplyr)

# source files
source(file = "source/methods_record_matching.R")
source(file = "source/methods_split.R")
source(file = "source/source_bay_study.R")

# clean up (as needed)
# rm(data_import, source_files, lib_loc)

# Load data ---------------------------------------------------------------

# data directory
dir_data <- "data/baystudy"

# assign data in .rds files to global env
vapply(list.files(path = dir_data), function(bsFiles) {
  
  # for naming in global env
  nm <- sub(pattern = ".rds", replacement = "", x = bsFiles)
  
  # read data in
  out <- readRDS(paste(dir_data, bsFiles, sep = "/"))
  
  # load in global env
  assign(nm, value = out, envir = .GlobalEnv)
  
  NA_character_
  # character()
  
}, FUN.VALUE = character(1L), USE.NAMES = FALSE)

# clean up
rm(dir_data)

# Add field: variable for calculating CPUE --------------------------------

# TowArea: IIf([Net]=2,[Distance]*6338)
# TowVolume: IIf([Net]=2,Null,IIf([Net]=1,[TotalMeter]*0.2875,
#                [TotalMeter]*0.0103))

# need to add the above fields (from "qryIndexTows") to IndexTows; opting to do
# this here because I'll only need to add one field rather than in the query
# where I'd have to add 2 with lots of blanks (nulls)

bool_net_1 <- IndexTows$Net %in% 1      # net = 1
bool_net_2 <- IndexTows$Net %in% 2      # net = 2
bool_net_o <- !bool_net_1 & !bool_net_2 # o = other

# add field with all NAs - could not think of a better name than "TowValue" but
# essentially this is either TowArea or TowVolume depending upon net type
IndexTows$TowValue <- NA

# fill new field with values
IndexTows$TowValue[bool_net_1] <- IndexTows$TotalMeter[bool_net_1] * 0.2875
IndexTows$TowValue[bool_net_2] <- IndexTows$Distance[bool_net_2] * 6338
IndexTows$TowValue[bool_net_o] <- IndexTows$TotalMeter[bool_net_o] * 0.0103

# section clean up
rm(bool_net_1, bool_net_2, bool_net_o)

# Create index variables for matching -------------------------------------

# this section contains all variables needed for matching fields from one
# dataframe to another; avoids the need (in most casese) for merging dataframes

# index to assign fields Bay & Series in FreqByAgeCat from StationConstants as
# needed (using match instead of MatchRecords because using only 1 field)
index_series_bay <- substitute(
  match(
    FreqByAgeCat$Station,
    StationConstants$Station
  )
)

# index for using value in IndexTows to calculate CPUE in FreqByAgeCat; field in
# IndexTows already considers net type & this will facilitate calculation of
# CPUE in FreqByAgeCat
index_tow_value <- substitute(
  MatchRecords(
    df1 = FreqByAgeCat,
    df2 = IndexTows,
    onFields = c("Year", "Survey", "Station", "Net", "Tow")
  )$Match
)

# Summary: by age category ------------------------------------------------

# the road to calculating cpue is based on age category (AgeCat), so here we
# need to sum the adjusted frequency for each age category (not length)

# below works & requires dplyr library
# FreqByAgeCat <- BSAdjFreq %>%
#   group_by(Year, Survey, Station, Net, Tow, AgeCat) %>%
#   summarise(
#     SumAdjFreq = sum(AdjFreq)
#   )

# this works & does not require loading dplyr
FreqByAgeCat <- aggregate(
  AdjFreq ~ Year + Survey + Station + Net + Tow + AgeCat,
  data = BSAdjFreq,
  FUN = sum
)

# renaming for convenience as further code developed using "SumAdjFreq"
colnames(FreqByAgeCat)[colnames(FreqByAgeCat) %in% "AdjFreq"] <- "SumAdjFreq"

# Add field: CPUE ---------------------------------------------------------

# add new fields to FreqByAgeCat for convenience & to eventually calculate YCI
# index for each record
FreqByAgeCat <- within(data = FreqByAgeCat, expr = {
  
  # calculate cpue based on net type (2 and then everything else)
  # CPUE <- ifelse(
  #   test = Net %in% 2,
  #   yes = (SumAdjFreq / IndexTows$TowArea[index_tow_value]) * 10000,
  #   no = (SumAdjFreq / IndexTows$TowVolume[index_tow_value]) * 10000
  # )
  
  CPUE <- (SumAdjFreq / IndexTows$TowValue[eval(index_tow_value)]) * 10000
  
  # add fields for Bay & Series - merely copying over from StationConstants 
  # table for ease of use in FreqByAgeCat (prefer this over merging as merging 
  # distorts column order and row sorting)
  Bay <- StationConstants$Bay[eval(index_series_bay)]
  Series <- StationConstants$Series[eval(index_series_bay)]
  
})
