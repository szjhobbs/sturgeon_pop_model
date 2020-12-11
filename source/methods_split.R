# ******************************************************************************
# Created: 23-Sep-2016
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: this file extends methods for R's split() function
# ******************************************************************************

# modified: 27-Sep-2016 (J. DuBois)

split.summary.match_records <- function(x, ..., val = NULL,
                                        dfSelect = c("DF1", "DF2")) {
  # this function splits data in x & as selected either 1 or 2 (DF) & 
  # pairs the split data with appropriate matched record count (e.g., if data is
  # split on AgeCat & CPUE then function returns this data along with the sample
  # size as set by the OnFields in MatchRecords())
  
  # Args:
  #    x:         the results of summary.match_records()
  #    ...:       any number of fields within selected dataframe on
  #               which to split data
  #    val:       the value (more like field) on which to perform analytics
  #               e.g., mean, sd (should add check for numeric?)
  #    dfSelect:  either DF1 or DF2 (default: DF1) may change in future to allow
  #               for selecting dataframe by name (e.g., FreqByAgeCat) rather
  #               than DF1 or DF2
  
  # Returns:
  #    list for further process using $Count in analytics
  #    (say calculation of the mean & standard deviation)
  
  # for dataframe selection (either 1 or 2 depending upon how records were
  # matched)
  DF <- match.arg(dfSelect)
  
  # get variable names on which to subset dataframe in dat, -1 to remove 'c'
  var_names <- as.character(substitute(c(...)))[-1]

  # get field on which to perform analytics
  val <- as.character(substitute(val))
  
  # for getting specific columns in dataframe
  get_df_cols <- if (length(val) == 0) var_names else c(var_names, val)
  
  # get data of desired dataframe (either DF1 or DF2) for splitting below
  dat <- get(x[[DF]])
  
  # split list on fields defined in MatchRecords()
  # lst <- split(dat, f = dat[x$OnFields], drop = TRUE)
  lst <- split(dat, f = dat[x[["OnFields"]]], drop = TRUE)
  
  # get count of records from MatchRecords(), rename GetMatRecordFreq - but
  # using this function keeps frequency (count) in same order as split() above
  nn <- GetMatRecordFreq(obj = x, dat = dat)
  
  # tried without Map or lapply but idea is to combine each element in lst with
  # its appropriate Count value
  res <- Map(f = function(x) {
    
    list(
      Dat = lst[[x]][get_df_cols],
      Count = unname(nn[x])
    )
    
  }, names(lst))
  
  # providing some metadata
  res$ByFields <- var_names
  attr(res$ByFields, "comment") <- "fields on which to summarize ValField"
  
  res$ValField <- val
  attr(res$ValField, "comment") <- "numeric field on which to get desc stats"
  
  # adding more info to function output
  res$OnFields <- x[["OnFields"]]
  
  class(res) <- "split_smr" # smr = summary match records
  
  # function output (a list)
  res
}
# end split.summary.match_records
