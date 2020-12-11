# ******************************************************************************
# Created: 24-Jun-2016
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file saves data to .csv files as desired
# ******************************************************************************

# get the source function SaveAsCsv()
source(file = "source/functions_data_to_csv.R")

# Sturgeon Report Card data saved to .csv files
SaveAsCsv(sourceFile = "source_load_card.R", newDir = "CardData")

# Sturgeon mark-recapture data saved to .csv files
SaveAsCsv(sourceFile = "source_load_tagging.R", newDir = "TaggingData")

# Sturgeon cpfv (party boat) data saved to .csv files
SaveAsCsv(sourceFile = "source_load_cpfv.R", newDir = "CpfvData")

# clean up
rm(SaveAsCsv)
