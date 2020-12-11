# ******************************************************************************
# Created: 02-Sep-2016
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: this file loads functions necessary to calculate the Bay Study YCI &
#          to provide a dataframe on which to run further analytics on CPUE vals
# ******************************************************************************

# TODO: possibly find a home for general functions, as these could be used in
#       other applications (J. DuBois 27-Sep-2016)

# general functions -------------------------------------------------------

ParseListNames <- function(lstNames, newNames) {
  # this function creates a dataframe from list names separated by '.'
  
  # Args:
  #    lstNames: names from a list
  #    newNames: names for the columns in dataframe output
  
  # Returns:
  #    dataframe created from list names separated by '.'
  
  # TODO: function will error if not all fields can be converted to numeric -
  #       must supply fix for this error (J. DuBois 27-Sep-2016)
  
  # split list names on dot
  res <- strsplit(lstNames, split = "\\.")
  
  # rbind needed in this case 'cause results of do.call() are matrix
  out <- data.frame(do.call(rbind, res), stringsAsFactors = FALSE)
  
  # convert all to numeric (original datatype)
  out[] <- lapply(out, FUN = as.numeric)
  
  # name columns in df
  names(out) <- newNames
  
  # function output
  out
}
# end ParseListNames

CreateFormula <- function(valVar, byVars) {
  # this function creates a formula for use in aggregate or like functions, 
  # formula is based off a single value variable (valVar) and >=1 by variables
  # (byVars)
  
  # Args:
  #    valVar: the value (field) on which to calculate (mean, sd, or like)
  #    byVars: variables (fields) on which to summarize calculated field
  
  # Returns:
  #    formula for use in functions like aggregate, etc.
  
  # TODO: implement arg to supply separator for by_vars (J. DuBois 27-Sep-2016)
  
  # ensuring value variable is length 1
  if (length(valVar) != 1) {
    stop("Must supply only one variable to 'valVar' arg.", call. = FALSE)
  }
  
  # collapse is hardcoded for now but may implement as arg in this function
  by_vars <- paste(byVars, collapse = '+')
  
  # function output - the formula object
  as.formula(paste(valVar, '~', by_vars))
}
# end CreateFormula

IncludeZeroes <- function(FUN) {
  # this function allows 0s to be easily included when calculating something
  # like mean, sd, or the like
  
  # Args:
  #    FUN: any function like mean, sd where including 0s affects calculation
  
  # Returns:
  #    a function which accepts a function (like mean) & n of 0s to be used
  
  fn <- match.fun(FUN)
  
  # function output
  function(x, num0) {
    
    zeroes <- num0 - length(x)
    
    # cannot be negative for rep() below
    if (zeroes < 0) {
      zeroes <- 0
      warning("'num0 - length(x)' < 0. Using 0 instead.", call. = FALSE)
    }
    
    # second function output
    fn(c(x, rep(0, times = zeroes)))
  }
}
# end IncludeZeroes

Aggregate <- function(formula, data, FUN, nn = 0) {
  # this function is sort of a wrapper for R's aggregate() but allows for
  # including 0s in fn() calculation
  
  # Args:
  #    formula: value ~ variables to be passed to aggregate()
  #    data:    the dataframe in which to find value & variables
  #    FUN:     function from IncludeZeroes()
  #    nn:      number of zeroes to include, default 0
  
  # Returns:
  #    dataframe based on supplied data & formula
  
  # get function to be use for calculation
  fn <- match.fun(FUN = FUN)
  
  # generate output
  out <- aggregate(formula = formula, data = data, FUN = fn, num0 = nn)
  
  # function removes the attributes added by aggregat(), output of FUN in 
  # aggregate is matrix with x, y dimensions - remove these dimensions for
  # convenience in future analytics
  out <- RemoveAttr(dat = out)
  
  # include sample size in output 
  out[["N"]] <- nn
  
  # function output
  out
}
# end Aggregate

RemoveAttr <- function(dat, attrCol = NULL) {
  # quick function to remove matrix attribute set when more than one function in
  # FUN arg (in aggregate function) is supplied
  
  # Args:
  #    dat:     dataframe as a result of aggregate()
  #    attrCol: if known the column with attributes (dim)
  
  # Returns:
  #    dat but with attributes removed
  
  # if needed to test each field for dim
  ac <- if (length(as.character(substitute(NULL))) == 0) {
    
    res <- lapply(names(dat), FUN = function(x) {
      if(!is.null(dim(dat[[x]]))) x
    })
    
    unlist(res)
    
  } else {
    
    as.character(substitute(attrCol))
    
  }
  
  # does not work
  # attr(dat[[ac]], which = "dim") <- NULL
  
  # for subsetting below
  i <- which(colnames(dat) %in% ac)
  
  # gets matrix from dat
  dt <- dat[[ac]]
  
  # setting new names for dataframe output
  colnames(dt) <- paste(ac, colnames(dat[[ac]]), sep = "_")
  
  # combine into dataframe as desired - function output
  data.frame(dat[-i], dt, stringsAsFactors = FALSE)
}
# end RemoveAttr

# bay study specific functions --------------------------------------------

CalcYCI <- function(dat, ...) {
  # this function calcalates the mean bay study index, which is then used to
  # calculate the YCI by summing age0 & age1 (year + 1) mean values for each
  # year
  
  # Args:
  #    dat: the dataframe containing bay study summed index (CPUE * BayWeight)
  #    ...: argurments passed to IndexForYCI2 (used in subsetting which
  #         surveys or months are included in the mean)
  
  # Returns:
  #    list with Bay Study YCI & annual mean index for each age category
  
  # subsetting for which values to include in mean index
  val <- IndexForYCI2(dat = dat, ...)
  
  # aggregate na.action defaults to na.omit - which will remove records with NAs
  mean_index <- aggregate(
    SumIndex ~ Year + AgeCat,
    data = dat[val, ],
    FUN = MeanIndex
  )
  
  # for ease of manipulation below
  mean_index <- RemoveAttr(dat = mean_index)
  
  # for display purposes mostly
  yci <- reshape2::dcast(
    data = mean_index,
    Year ~ AgeCat,
    value.var = "SumIndex_Mean"
  )
  
  # function output
  list(
    YCI = CalcYCIStats(dat = mean_index),
    AgeIndex = yci
  )
}
# end CalcYCI

CalcYCIStats <- function(dat) {
  # this function calculates the year class index & provides measure of spread
  # like standard error & variance
  
  # Args:
  #    dat: dataframe with mean index
  
  # Returns:
  #    dataframe with calculated YCI along with variance, standard error, & N
  
  # for subsetting below
  nn <- length(unique(dat[["Year"]]))
  records <- 2:(nn + 1) # offsetting by 1 year
  
  # YCI calculated using age0 & age1 data
  splt_dat <- split(dat, f = dat[["AgeCat"]])
  
  # year class index
  yci <- splt_dat[["Age0"]][["SumIndex_Mean"]] +
    splt_dat[["Age1"]][["SumIndex_Mean"]][records]
  
  # yci variance over one year
  yci_var <- splt_dat[["Age0"]][["SumIndex_Var"]] +
    splt_dat[["Age1"]][["SumIndex_Var"]][records]
  
  # yci count over one year
  yci_n <- splt_dat[["Age0"]][["SumIndex_N"]] +
    splt_dat[["Age1"]][["SumIndex_N"]][records]
  
  # yci standard error (se)
  yci_se <- sqrt(yci_var / yci_n)
  
  # function output
  data.frame(
    Year = unique(dat[["Year"]]),
    YCI = yci,
    YCI_VAR = yci_var,
    YCI_SE = yci_se,
    YCI_N = yci_n,
    stringsAsFactors = FALSE
  )
}
# end CalcYCIStats

MeanBSCpue <- function(x) {
  # this function calculates mean CPUE values for each tow where sturgeon were 
  # caught; it also provides standard deviation & N; calculation includes 0
  # catch records
  
  # Args:
  #    x: object of class of "split_smr"
  
  # Returns:
  #    dataframe with mean cpue values & calculated index by record, where
  #    index is calculated by multiplying mean cpue by BayWeigth value
  
  # TODO: may be simpler & safer way to get out2 below
  
  # will ensure various items required in this function are available
  if (class(x) != "split_smr") {
    stop("'x' must be of class 'split_smr'.", call. = FALSE)
  }
  
  if (!exists("BayWeightingFactors")) {
    stop("Dataframe 'BayWeightingFactors' must be loaded first.", call. = FALSE)
  }
  
  # for selecting correct elements for Aggregate() below
  check_names <- c("ByFields", "ValField", "OnFields")
  lst <- x[!(names(x) %in% check_names)]
  
  # fmla <- CreateFormula(valVar = x$ValField, byVars = x$ByFields)
  
  # calculates mean cpue for each record
  out <- Map(
    f = function(y) Aggregate(
      formula = CreateFormula(valVar = x$ValField, byVars = x$ByFields),
      data = y[["Dat"]],
      FUN = MeanSd0,
      nn = y[["Count"]]
    ),
    lst
  ) # end Map
  
  # for matching records below needed to get correct BayWeight
  dat <- ParseListNames(lstNames = names(lst), newNames = x$OnFields)
  
  # gets record numbers matched by Bay & Net fields
  mr <- MatchRecords(
    df1 = dat,
    df2 = BayWeightingFactors,
    onFields = c("Net", "Bay")
  )$Match
  
  # for now below works - basically used to add Index to out (list) & then
  # combine output in clean dataframe to be rbind()ed in do.call below
  out2 <- lapply(seq_along(out), function(i) {
    out[[i]][["Index"]] <- out[[i]][["CPUE_Avg"]] *
      BayWeightingFactors[["BayWgt"]][mr][i]
    
    data.frame(dat[i, ], out[[i]], row.names = NULL)
  })
  
  # function output
  do.call(rbind, out2)
}
# end MeanBSCpue

MeanIndex <- function(x) {
  # this is a simple function to combine several R descriptive stats functions 
  # under 'one roof'; idea that this function will be used in R's aggregate
  # function or the like
  
  # Args:
  #    x: a numeric vector
  
  # Returns:
  #    named vector of mean, standard deveation, variance, and count
  
  # function output
  c(
    Mean = mean(x),
    Sd = sd(x),
    Var = var(x),
    N = length(x),
    N0 = length(x[x == 0]) # count of 0 values
  )
}
# end MeanIndex

SumIndex <- function(dat, net = 2, series = 1) {
  # this function supplies an interim dataframe where the index is summed by
  # Year, Survey, & AgeCat; the returned dataframe is used for further analytics
  # to obtain the YCI (using mean index of selected surveys for Age0 & Age1)
  
  # Args:
  #    dat:    dataframe containing Index field (Index as CPUE * BayWeight)
  #    net:    gear type where fish were caugth; 1 = MWT, 2 = OT (default)
  #    series: field on which to group stations; 1 (default) = historic stations
  
  # Returns:
  #    dataframe where index is supplied for every Year, Survey, AgeCat combo;
  #    where the survey wasn't conducted for a particular year the index is set
  #    NA, otherwise index is 0 or > 0.
  
  # subset dat on desired net & series
  dat <- dat[dat$Net %in% net & dat$Series %in% series, ]
  
  # for splitting index values
  col_names <- c("Year", "Survey", "AgeCat")
  
  dat_split <- split(
    dat[["Index"]],
    f = dat[col_names],
    drop = FALSE # TRUE
  )
  
  # used for convenient output below
  var_names <- names(dat_split)
  
  # sum index values accordingly
  sum_index <- vapply(
    dat_split,
    FUN = sum,
    FUN.VALUE = numeric(1),
    na.rm = TRUE
  )
  
  # combine in dataframe for convenient output
  out <- data.frame(
    do.call(rbind, strsplit(var_names, split = "\\.")),
    sum_index,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  
  names(out) <- c(col_names, "SumIndex")
  
  # below needed to set to NA any value where sampling did not occur, because
  # dat_split set drop = T this would include all records which would give
  # incorrect results (i.e., would yield 0 when in fact there was no sample)
  mr_year_survey <- MatchRecords(
    df1 = out,
    df2 = IndexTows,
    onFields = c("Year", "Survey")
  )$Match
  
  # where survey was not conducted for a particular year
  out[["SumIndex"]][is.na(mr_year_survey)] <- NA
  
  # function output
  out
}
# end SumIndex

MeanSd0 <- IncludeZeroes(FUN = function(x) {
  # this function creates a function for including 0s when calculating mean & sd
  
  # Args:
  #    FUN: a function (in this case a vector of functions)
  
  # Returns:
  #    a function on which to include a value for sample size on which to calculate
  #    mean, sd
  
  c(Avg = mean(x), Sd = sd(x), Max = max(x), Non0 = length(x[x > 0]))
})
# end MeanSd0

IndexForYCI2 <- function(dat, ..., lstEval = NULL) {
  # this function creates a boolean vector indicating if index value should be
  # included in mean value ultimately used to calculate YCI
  
  # Args:
  #    dat:     dataframe in which to evaluate boolean expressions
  #    ...:     any number of expressions evaluating to boolean
  #    lstEval: a list (substituted) of boolean expressions to be evaluated
  #             herein
  
  # Returns:
  #    a vector of boolean values
  
  # NOTE: will keep name IndexForYCI2 though it's a bit cryptic
  
  # if lstEval is not supplied
  if (is.null(lstEval)) lstEval <- eval(substitute(alist(...)))
  
  # for holding function output
  v <- vector(mode = "logical", length = nrow(dat))
  
  # evaluate lstEval
  bool <- lapply(lstEval, function(x) eval(x, envir = dat))
  
  # if boolean expression is TRUE reassign to v
  lapply(bool, FUN = function(b) v[b] <<- TRUE)
  
  # maybe something like below for warning if all evaluated to FALSE -- trying
  # to prevent lstEval from containing irrelevant info ??
  # lapply(bool, sum)
  
  # if (sum(v) == 0) warning("All evaluated as FALSE.", call. = FALSE)
  # **************************************************************************
  
  # function output
  v
}
# end IndexForYCI2
