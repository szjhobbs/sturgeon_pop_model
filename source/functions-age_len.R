
# TODO: clean up MakeALKey function

MakeALKey <- function(dat, len, age, lenBreaks, breakLabs = NULL, dia = FALSE) {
  # function creates an age-length (al) key when supplied with dataframe of
  # lengths and ages (J. DuBois 14-Apr-2016)
  
  # modified 17-Apr-2018 for ease & speed
  
  # Args:
  #   dat:       dataframe of fish lengths & ages
  #   len:       column in dat containing length values
  #   age:       column in dat containing age values
  #   lenBreaks: sequence of breaks on which to bin len values
  #   breakLabs: optional (default = NULL) labels for length breaks
  #   dia:       diagnostics (default = FALSE); useful for seeing info about
  #              data behind alkey before constructing said key
  
  # Returns:
  #   age-length key with first column of bins and all other columns
  #   proportions of ages at each length bin
  
  # get length and age columns
  l <- as.character(substitute(len))
  a <- as.character(substitute(age))
  
  # add Bin field to dat for ease of manipulating al key by length, if desired; 
  # using defaults in cut() of right = TRUE & include.lowest = FALSE - hmm - not
  # sure why I did this, as I want left included but not right - made change
  # 03-May-2016 (J. DuBois)
  # dat$Bins <- cut(
  #   dat[, l],
  #   breaks = lenBreaks,
  #   labels = breakLabs,
  #   include.lowest = FALSE,
  #   right = FALSE
  # )
  
  bins <- cut(
    dat[[l]],
    breaks = lenBreaks,
    labels = breakLabs,
    include.lowest = FALSE,
    right = FALSE
  )
  
  # construct age-length (al) key
  # al_key_long <- data.frame(
  #   prop.table(
  #     table(Bins = dat[, "Bins"], Age = dat[, a]),
  #     margin = 1
  #   ),
  #   stringsAsFactors = FALSE
  # )
  # 
  # al_key_long <- data.frame(
  #   prop.table(table(Bins = bins, Age = dat[[a]]), margin = 1),
  #   stringsAsFactors = FALSE
  # )
  
  alk <- prop.table(table(Bins = bins, Age = dat[[a]]), margin = 1)
  
  alk[is.na(alk)] <- 0
  
  out <- list(
    Bins = dimnames(alk)[["Bins"]],
    Ages = dimnames(alk)[["Age"]],
    ALKey = alk
  )
  
  return(out)
  
  # reshape to short age-length key
  # al_key_short <- reshape2::dcast(
  #   data = al_key_long,
  #   formula = Bins ~ Age,
  #   value.var = "Freq"
  # )
  
  # remove all NaN or NA
  al_key_short[is.na(al_key_short)] <- 0
  
  # function output (the AL key)
  # if (dia) {
  #   alk_stats <- GetAlkDiag(
  #     alk = al_key_short,
  #     len = dat[, l],
  #     age = dat[, a]
  #   )
  #   
  #   return(alk_stats)
  # }
  # 
  # function output if not dia = TRUE
  al_key_short
}
# end MakeALKey

