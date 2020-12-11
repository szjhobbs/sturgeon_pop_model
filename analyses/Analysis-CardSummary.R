# ******************************************************************************
# Created: 24-Jun-2016
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file creates annual summary table of returned cards (catch,
#          fished no catch, or did not fish), purchased cards, &
#          counts of GST & WST caught & released and WST kept.
# ******************************************************************************

# running all code in this file will produce a table like the one below (though
# data may be slightly different as card data will update annually)

# Issued (purchased): cards were free through 2012
# NoEffort: number of anglers reporting did not fish
# NoCatch: number of anglers reporting fished but caught no sturgeon
# Catch: number of anglers reporting caught sturgeon
# GST: number of green sturgeon caught (reported by anglers)
# WSTr: number of white sturgeon caught & released (reported by anglers)
# WSTk: number of white sturgeon caught & kept (reported by anglers)

# do not un-comment table below ****************************
#    Year Issued NoEffort NoCatch Catch GST WSTr WSTk
# 1  2007  37680       NA    5062  1859 316 4666 1431
# 2  2008  53777       NA    5280  2051 240 5103 1786
# 3  2009  72499       NA    6342  2208 240 5807 1871
# 4  2010  66357     1478    4262  1756 163 4372 1424
# 5  2011 112000     4354    5703  2259 120 4276 2047
# 6  2012 112800     5378    5200  2051 176 4222 1745
# 7  2013  50915     3127    5207  2286 170 4462 2035
# 8  2014  49260     3254    6150  2638 186 4652 2325
# 9  2015  48337     4364    6980  2843 197 6165 2397
# 10 2016  42273        1       5     0  NA   NA   NA
# **********************************************************

# Libraries and Source Files ----------------------------------------------

# loads all Card data from 2007 to present
source(file = "source/source_load_card.R")

# Summary: 2007-2011 ------------------------------------------------------

# Because this data is pre-ALDS, the summary herein will be pretty piecemeal.
# First we'll use CardTally0711 to get at numbers for did not fish, fished no
# catch, & Cards issued.

# NOTE: new on the 2010 Card was a box the angler could check if he/she did not
# fish for sturgeon that year. So the "No Fishing" summary is not really
# relevant until 2010.

# Cards Issued, No Data (fished no catch), & No Fishing (did not fish); Tally
# column in resulting dataframe is sum of tally type for that year
tally0711 <- aggregate(
  formula = Tally ~ Year + TallyType,
  data = CardTally0711,
  FUN = sum
)

# Cards with catch
cards_catch_0711 <- tapply(
  RetCards0711$CardNum,
  INDEX = RetCards0711$Year,
  FUN = length
)

# below should be same result as cards_catch_0711 but off by 5 (2007), 3 (2008),
# & 2 (2010) due to 10 anglers in RetCards0711 not having corresponding catch in
# Sturgeon0711 - CDFW has reported data in cards_catch_0711, so use these nums
tapply(
  Sturgeon0711$CardNum,
  INDEX = Sturgeon0711$Year,
  FUN = function(x) length(unique(x))
)

# for resulting dataframe below count field is called "CardNum" (can be changed
# if desired); NOTE: in 2009 angler reported keeping 1 green
stu_catch_0711 <- aggregate(
  formula = CardNum ~ Year + Species + Fate,
  data = Sturgeon0711,
  FUN = length
)

# Summary: 2012-present ---------------------------------------------------

# See Notes-FishingCode.txt

# Useage level codes (2013-current)*:
#    U   = used card (& caught fish)
#    NU  = did not use card (i.e., did not fish)
#    UNS = used card (but did not catch fish; i.e., got skunked)

# *NOTE: in 2012 codes were only U or NU

# so this works but one problem: for most of 2012 the codes were either fished 
# or did not fish, there was no fished but got skunked. So we need to do some 
# finagling. We need to for 2012, identify of the anglers who fished, which ones
# caught fish. Use SturgeonAlds, as this contains sturgeon catch data.
with(data = RetCardsAlds, expr = {
  table(ItemYear, Code, useNA = "ifany")
})

# to deal with this, follow steps 1-4 below

# 1: which angler caught fish - catch is reported in StuCardAll - so any angler
# "showing up" in this dataframe caught fish
bool_fish_caught <- RetCardsAlds$LicenseReportID %in%
  SturgeonAlds$LicenseReportID

# 2: create new field (for useage level code) to accurately get at U & UNS (see 
# Notes-FishingCode.txt for details) & assign data in new field according to
# TRUE or FALSE in bool_fish_caught & useage code

RetCardsAlds$NewCode <- RetCardsAlds$Code

RetCardsAlds$NewCode[!bool_fish_caught & RetCardsAlds$Code %in% 'U'] <- 'UNS'
RetCardsAlds$NewCode[bool_fish_caught & RetCardsAlds$Code %in% 'UNS'] <- 'U'

# 3: count by item year and code (NewCode) fields
tallyAlds <- with(data = RetCardsAlds, expr = {
  table(ItemYear, NewCode, useNA = "ifany")
})

# convert to dataframe if desired (note however ItemYear & NewCode are factors)
tallyAlds <- as.data.frame(tallyAlds)

# convert fields accordingly *****************************************

# good idea to convert year to integer
tallyAlds$ItemYear <- as.integer(as.character(tallyAlds$ItemYear))

# run below if desired
# tallyAlds$NewCode <- as.character(tallyAlds$NewCode)

# ********************************************************************

str(tallyAlds)

# 4: run using original code FIO and for comparison
with(data = RetCardsAlds, expr = {
  table(ItemYear, Code, bool_fish_caught, useNA = "ifany")
})

# 5: purchased cards: need only active cards for this analysis and where 
# LicenseID is not a duplicate (note the difference in observations [rows]
# between purch_cards & PurCardsAlds)
purch_cards <- PurCardsAlds[PurCardsAlds$StatusCodeDesc %in% "Active" &
                                !duplicated(PurCardsAlds$LicenseID), ]

cards_issued <- as.data.frame(
  table(ItemYear = purch_cards$ItemYear, useNA = "ifany")
)

# good idea to convert year to integer
cards_issued$ItemYear <- as.integer(as.character(cards_issued$ItemYear))

# below use as needed FIO ********************************************
# with(data = RetCardsAlds, expr = {
#   table(
#     ItemYear, Code, CustomerSourceCode, bool_fish_caught,
#     useNA = "ifany"
#   )
# })
# 
# RetCardsAlds[bool_fish_caught & RetCardsAlds$Code %in% "UNS", ]
# 
# RetCardsAlds[!bool_fish_caught & RetCardsAlds$Code %in% "U" &
#                RetCardsAlds$ItemYear > 2012, ]
# 
# RetCardsAlds[!bool_fish_caught & RetCardsAlds$Code %in% "U" &
#                RetCardsAlds$ItemYear > 2012 &
#                RetCardsAlds$CustomerSourceCode %in% "CC", ]
# ********************************************************************

# for resulting dataframe below count field is called "LicenseReportID" (can be
# changed if desired)
stu_catch_alds <- aggregate(
  formula = LicenseReportID ~ ItemYear + SturgeonType + Fate,
  data = SturgeonAlds,
  FUN = length
)

# Summary: creating summary table -----------------------------------------

# this section creates an overall summary table combining the 0711 & alds data
# summaries 

# create _07 summary table (contains years 2007-2011)
data_sum_0711 <- reshape2::dcast(
  data = tally0711,
  formula = Year ~ TallyType,
  value.var = "Tally"
)

# adding necessary fields to _07 dataframe
data_sum_0711$Catch <- cards_catch_0711

data_sum_0711$Green <- stu_catch_0711$CardNum[
  stu_catch_0711$Species %in% "Green" &
    stu_catch_0711$Fate %in% "released"
]

data_sum_0711$WhiteRel <- stu_catch_0711$CardNum[
  stu_catch_0711$Species %in% "White" &
    stu_catch_0711$Fate %in% "released"
]

data_sum_0711$WhiteKept <- stu_catch_0711$CardNum[
  stu_catch_0711$Species %in% "White" &
    stu_catch_0711$Fate %in% "kept"
]

# create _12 summary table (contains years 2012-present)
data_sum_alds <- reshape2::dcast(
  data = tallyAlds,
  formula = ItemYear ~ NewCode,
  value.var = "Freq"
)

data_sum_alds <- merge(cards_issued, data_sum_alds)

data_sum_alds$Green <- c(
  stu_catch_alds$LicenseReportID[
    stu_catch_alds$SturgeonType %in% "Green" &
      stu_catch_alds$Fate %in% "released"
    ],
  NA
)

data_sum_alds$WhiteRel <- c(
  stu_catch_alds$LicenseReportID[
    stu_catch_alds$SturgeonType %in% "White" &
      stu_catch_alds$Fate %in% "released"
    ],
  NA
)

data_sum_alds$WhiteKept <- c(
  stu_catch_alds$LicenseReportID[
    stu_catch_alds$SturgeonType %in% "White" &
      stu_catch_alds$Fate %in% "kept"
    ],
  NA
)

# _0711 & _alds ready for rbind()ing - reorder columns and change colnames
data_sum_0711 <- data_sum_0711[, c(1, 2, 4, 3, 5:8)]
data_sum_alds <- data_sum_alds[, c(1:3, 5, 4, 6:8)]

col_names <- c(
  "Year", "Issued", "NoEffort", "NoCatch",
  "Catch", "GST", "WSTr", "WSTk"
)

colnames(data_sum_0711) <- col_names
colnames(data_sum_alds) <- col_names

data_sum_all <- rbind(data_sum_0711, data_sum_alds)

# clean up for data_sum_all
data_sum_all[data_sum_all$Year %in% c(2007:2009), "NoEffort"] <- NA
data_sum_all[data_sum_all$Year %in% 2011, "Issued"] <- 112000
data_sum_all

# session clean up --------------------------------------------------------

rm(bool_fish_caught, cards_catch_0711, col_names)
