# ******************************************************************************
# Created: 31-Aug-2016
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: this file loads methods & functions used for record matching & then
#          record processing (e.g., splitting data & then deriving mean on 
#          desired variable(s))
# ******************************************************************************

# modified: 27-Sep-2016 (J. DuBois)

# TODO: ?

# methods: MatchRecords ---------------------------------------------------

# establishes generic S3 function
MatchRecords <- function(x, ...) {
  UseMethod("MatchRecords")
}
# end MatchRecords generic

# S3 method for data.frame
MatchRecords.data.frame <- function(df1, df2, onFields, ...) {
  
  # probably not needed but want to ensure df2 is also a dataframe ***
  bool_df1 <- is.data.frame(df1)
  bool_df2 <- is.data.frame(df2)
  
  if (!bool_df1) stop("'df1' must be a data.frame", call. = FALSE)
  if (!bool_df2) stop("'df2' must be a data.frame", call. = FALSE)
  # ******************************************************************
  
  # TODO: need default & error checking for onFields
  
  # Reduce works like a charm! Quick too! - pastes records of fields 
  # Reduce(paste, df1[, onFields, drop = FALSE]) - this works too to keep
  # data.frame type, but I think df1[onFields] should work too
  df1_cols <- Reduce(paste, df1[onFields])
  df2_cols <- Reduce(paste, df2[onFields])
  
  # match records as "pasted" data from specified fields
  mr <- match(df1_cols, table = df2_cols, ...)
  
  # method output
  out <- list(
    Match = mr,
    Fields = onFields,
    DF1 = deparse(substitute(df1)),
    DF2 = deparse(substitute(df2)),
    EnvDF1 = NULL,
    EnvDF2 = NULL
  )
  
  # set class as match records (mr)
  class(out) <- "match_records"
  
  # method output
  out
}
# end MatchRecords.data.frame

# extending summary function (method for match_records class)
summary.match_records <- function(object) {
  # this method extends R's summary() function for the match_records class
  
  # Args:
  #    object: object of class match_records
  
  # Returns:
  #    list with summarized information about matched records
  
  # not needed *******************************************************
  # cls <- class(object)
  # 
  # if (cls != "match_records") stop("match_records", call. = FALSE)
  # ******************************************************************
  
  # for additional output information
  index_na <- is.na(object$Match)
  
  # get count of number of records in dat1 matching records in dat2
  tbl <- table(object$Match)
  
  # method ouput
  out <- list(
    FreqMatches = tbl,
    Matches = object$Match,
    NumMatched = sum(!index_na),
    NumNotMatched = sum(index_na),
    RecordNum = as.integer(names(tbl)),
    OnFields = object$Fields,
    # RevOnInputFields = revFields,
    CountOfFreq = length(tbl),
    DF1 = object$DF1,
    DF2 = object$DF2
  )
  
  # assign class here
  class(out) <- "summary.match_records"
  
  # method output
  out
}
# end summary.match_records

# functions: MatchRecords -------------------------------------------------

# TODO: add some kind of error handling for GetMatRecordFreq()

GetMatRecordFreq <- function(obj, dat = NULL) {
  # this function is designed to be used in conjuction with split() function, 
  # which uses interaction() when splitting on desired variables; this function
  # produces --- in order according to interaction --- the frequency (count) of
  # records matched when using MatchRecords()
  
  # Args:
  #    obj: list of class "summary.match_records"
  #    dat: dataframe on which to run interation() using obj[["OnFields"]]]
  #         should not really be NULL (default) & will need to re-code this
  #         at some point to either allow NULL or select data.frame as option
  
  # Returns:
  #    count of number of records matched per each interaction in the order
  #    of the results of interation()
  
  # the obj itself as access to the dataframe by name but I just need to decide
  # how I want to access the data frame - have an arg with 1 or 2 to select the
  # dataframe? Need to test if DF1 and DF2 were reversed
  
  # for ordering frequency accordingly
  # could do something like this...
  # interaction(get(obj$DF2)[obj$OnFields], drop = TRUE)
  f_fields <- interaction(dat[obj[["OnFields"]]], drop = TRUE)
  
  # get frequency in proper order
  freq <- as.integer(obj[["FreqMatches"]])[order(unique(f_fields))]
  
  # for selecting appropriate frequency in subsequent function
  names(freq) <- unique(f_fields)[order(unique(f_fields))]
  
  # function output
  freq
}
# end GetMatRecordFreq
