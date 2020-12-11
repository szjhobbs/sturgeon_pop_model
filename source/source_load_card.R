# ******************************************************************************
# Created: 23-Jun-2016
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file loads all card data from the data/card directory
# ******************************************************************************

card_dir <- "data/card/"

# load csv files ----------------------------------------------------------

LocationCodes <- read.csv(
  file = paste0(card_dir, "LocationCodes.csv"),
  header = TRUE,
  stringsAsFactors = FALSE
)

# load rds files for ALDS card data ---------------------------------------

PurCardsAlds <- readRDS(file = paste0(card_dir, "PurCardsAlds.rds"))
RetCardsAlds <- readRDS(file = paste0(card_dir, "RetCardsAlds.rds"))
SturgeonAlds <- readRDS(file = paste0(card_dir, "SturgeonAlds.rds"))

# load rds files for 0711 card data ---------------------------------------

CardTally0711 <- readRDS(file = paste0(card_dir, "CardTally0711.rds"))
RetCards0711 <- readRDS(file = paste0(card_dir, "RetCards0711.rds"))
Sturgeon0711 <- readRDS(file = paste0(card_dir, "Sturgeon0711.rds"))

# clean up ----------------------------------------------------------------
rm(card_dir)
