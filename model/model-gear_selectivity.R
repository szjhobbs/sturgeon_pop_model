# ******************************************************************************
# Created: 17-Apr-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: file uses Millar (gear selectivity) code to adjust trammel net catch
#          of the various mesh sizes; this is an important first step as S.
#          Blackburn modeled population rate (lambda) using adjusted catch;
#          see Blackburn data (& some methodology) in "size_selective_data.xlsx"
# ******************************************************************************

# TODO: (18-Apr-2018) nail down subsetting (e.g., using all ages & sizes)

# TODO: (18-Apr-2018) develop function to create starting dataframe needed
#       gear selectivity models (ideally in form of 'dat' below)

# TODO: (18-Apr-2018) develop function to run all Millar modesl & then 
#       select 'best' model ('best' based on lowest deviance)

# TODO: (18-Apr-2018) with best model selected, run PlotCurves() to get
#       relative retentions (standardize or not?)

# TODO: (18-Apr-2018) develop functio to adjust catch based on matrix
#       created from PlotCurves(); use Blackburn method to adjust catch?

# source data -------------------------------------------------------------

# SturgeonAll <- readRDS(file = "data/tagging/SturgeonAll.rds")

source(file = "data/model/quick_guide-age_assign.R")

# some clean up for now
rm(ages, index, n, used, Sturgeon2014_2016)

# frequency by mesh size & length (bin) -----------------------------------

# SturgeonAll should be subsetted as below, but if sourcing from
# "quick_guide-age_assign.R", year & species already subsetted

# subset = Species %in% "White" &
#   RelYear %in% 2014:2016 &
#   MeshSize %in% 6:8 &
#   (Age < 20 | is.na(Age)) &
#   FL != 217

# for now want only White Stu data from 2014-2016; some MeshSize recorded as 0
# (which is NA) thus the restriction on mesh size to known panels of 6, 7, 8
SturgeonAll <- subset(
  SturgeonAll,
  subset = MeshSize %in% 6:8 &
    (Age < 20 | is.na(Age)) &
    FL != 217
)

fl_range <- range(SturgeonAll[["FL"]])

len_breaks <- seq(from = 50, to = 205, by = 5)

len_bins <- cut(
  SturgeonAll[["FL"]],
  breaks = len_breaks,
  labels = len_breaks[-length(len_breaks)],
  include.lowest = FALSE,
  right = FALSE
)


freq <- aggregate(
  formula = Species ~ len_bins + MeshSize,
  data = SturgeonAll,
  FUN = length
)

# I believe below should work for Millar gear-selectivity model (but need to
# check, 17-Apr-2018)
dat <- reshape2::dcast(
  data = freq,
  formula = len_bins ~ MeshSize,
  # fun.aggregate = sum,
  # margins = "MeshSize",
  fill = 0,
  value.var = "Species"
)

# employ gear selectivity functions ---------------------------------------

# need numeric length bins for model
dat[["len_bins"]] <- as.numeric(as.character(dat[["len_bins"]]))

# need mesh size converted to cm
mesh_size <- as.numeric(colnames(dat)[2:4]) * 2.54

# 2 8" panels fished for every 1 6" or 7" panel; this is not always consistent &
# something that maybe (though difficult) to confirm; difficult because in
# recent years panels have been in short supply so a net config may be (for
# example) 6,6,7,8 rather than the historical (since 1990) & assumed 8,6,7,8
# config; the relative power should sum to 1, although using c(1, 1, 2) does not
# seem to make a difference
mesh_rel_power <- c(0.25, 0.25, 0.50)
# mesh_rel_power <- c(1, 1, 2)

# fit model then run PlotCurve() to get relative retentions
norm_loc <- NetFit(
  Data = dat,
  Meshsize = mesh_size,
  x0 = c(100, 200),
  rtype = "norm.loc",
  rel.power = mesh_rel_power
)

rel_ret <- PlotCurves(fit = norm_loc, standardize = TRUE)

# NOTE: see "size_selective_data.xlsx" for S. Blackburn method; not entirely
# sure of S. Blackburn's method but it seems to be useful in terms of not
# over-inflating (adjusted) catch; should consider S. Blackburn's method but
# also may want to contact Millar for advice...
