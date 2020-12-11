# ******************************************************************************
# Created: 30-Mar-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: this file loads age-length data from .xlsx file, filters & then
#          cleans desired data, & then saves data to .rds file for use in the
#          model. original data in "CA_FinRayInventory_Ageing_Blackburn.xlsx"
#          provided by S. Blackburn (U. Idaho) & ages therein estimated by
#          S. Blackburn & others by way of finrays; clean data will contain
#          only data for White Sturgeon caught per CDFW sampling 2014-2016
# Modified: (16-Apr-2018) based on additional information provided by S. Black-
#           burn; info will help match better the ages with CDFW catch (tagging)
#           data from 2014-2016
# ******************************************************************************

# load .xlsx data ---------------------------------------------------------

dat_file <- "CA_FinRayInventory_Ageing_Blackburn.xlsx"

age_len <- readxl::read_xlsx(
  path = file.path("blackburn_code/LengthAge", dat_file),
  sheet = "aging_master"
)

dat_file <- "CDFW_WST.xlsx"

age_id <- readxl::read_xlsx(
  path = file.path("blackburn_code/LengthAge", dat_file),
  sheet = "CDFW_WST"
)

# loads S. Blackburn's extract of CDFW tagging containing FishID field
dat_file <- "Sturgeon2014_2016.xlsx"

cdfw_2014_2016 <- readxl::read_xlsx(
  path = file.path("blackburn_code/LengthAge", dat_file),
  sheet = "Sheet1"
)

# clean up
rm(dat_file)

# filter data & select desired fields -------------------------------------

# desired fields
fields <- c(
  "ID", "datecap", "date_aged",
  "lencap", "agecap", "Study"
)

age_len <- subset(age_len, subset = Study %in% "CDFW", select = fields)

# from age_id, we do not need records whee ageing_ID is na

nrow(age_id[!is.na(age_id[["ageing_ID"]]), ])

age_id <- age_id[!is.na(age_id[["ageing_ID"]]), ]

# match id records --------------------------------------------------------

head(age_id)
head(age_len)

index <- match(x = age_len[["ID"]], table = age_id[["ageing_ID"]])

# use index to create fish id field in age_len; this field should be the link to
# CDFW data containing all 1000 fish (sampled 2014-2016)
age_len$FishID <- age_id[["FishID"]][index]

# clean data: remove NA from age & convert to numeric ---------------------

# plus we want to limit to age 29 (per S. Blackburn)

# agecap is character, desire numeric
table(age_len$agecap, useNA = "ifany") # 1 as NA, 1 as '3/4?'

# force a coersion
age_len$agecap <- as.numeric(age_len$agecap)

# remove any records of age = NA & any age > 29
age_len <- subset(age_len, subset = !is.na(agecap) & agecap < 30)

# clean data: re-format date_aged -----------------------------------------

# date_aged is character-numeric, desire date of POSIXct format
sum(is.na(age_len$date_aged))

age_len$date_aged <- as.Date(
  as.numeric(age_len$date_aged),
  origin = "1899-12-30"
)

# just need date from datecap
# test <- age_len$datecap
age_len$datecap <- as.Date(age_len$datecap)

# clean data: drop 'Study' field ------------------------------------------

# not needed as all records from CDFW

age_len$Study <- NULL

# clean data: rename fields -----------------------------------------------

new_col_names <- c("AgeID", "RelDate", "DateAged", "ForkLen", "Age", "FishID")

colnames(age_len) <- new_col_names

# add metadata to fields --------------------------------------------------

# this will help when loading the .rds file for future analytics by providing
# some understanding of data in each field
comment(age_len$AgeID) <- "ID of aged sample in .xlsx file"
comment(age_len$RelDate) <- "date fish was caught, then released; CDFW date"
comment(age_len$DateAged) <- "date finray sample was aged"
comment(age_len$ForkLen) <- "fork length (cm) measured & recorded at capture"
comment(age_len$Age) <- "age assigned based on finray analysis"
comment(age_len$FishID) <- "link to CDFW tagging data; record number"

attr(age_len, which = "metadata") <-
  "data for white sturgeon collected during CDFW sampling 2014-2016"

attr(age_len, which = "source_file") <-
  "CA_FinRayInventory_Ageing_Blackburn.xlsx"

# save to RDS file --------------------------------------------------------

# for use in future analytics

# can double-check if need be - should all be 0
# Map(function(x) sum(is.na(x)), age_len)

saveRDS(object = age_len, file = "data/model/AgeLengthWST.rds")

# filter CDFW tagging data for White Sturgeon only
saveRDS(
  object = subset(cdfw_2014_2016, subset = Species %in% "White"),
  file = "data/model/CDFW-2014_2016.rds"
)

# clean up ----------------------------------------------------------------

# clear environment
rm(list = ls())
