# ******************************************************************************
# Created: 28-Jun-2016
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains function(s) that loads data (e.g., tagging, card)
#          & then saves this loaded data as .csv files in a new user-selected
#          & user-named directory. Idea is to allow user to save as .csv files
#          for easier viewing & for convenience.
# ******************************************************************************

SaveAsCsv <- function(sourceFile, newDir) {
  # this function loads a source file locally, and then for every data.frame
  # loaded will create a .csv file given a user selected file path and user
  # named directory in newDir
  
  # Args:
  #    soureFile: name of the .R file which will load the data
  #    newDir:    user-supplied name of directory to which .csv
  #               files will be saved
  
  # Returns:
  #    nothing, but does message the user where & which .csv files were created
  
  # source data locally - will not see in global env
  source(
    file = paste("source", sourceFile, sep = "/"),
    local = TRUE
  )
  
  # directory selected by user (prompts user)
  selected_dir <- choose.dir()
  
  # adds directory CardData to selected_dir
  dir.create(path = paste(selected_dir, newDir, sep = "\\"))
  
  cat("At ", selected_dir, "\\", newDir, "\n", sep = "")
  
  # applied to all local objects & if data.frame will save as .csv
  sapply(X = ls(), FUN = function(x) {
    
    # variable to house message to user
    # out <- NA_character_
    
    if (is.data.frame(get(x))) {
      
      # create complete csv file name and filepath
      csv_file <- paste(x, "csv", sep = ".")
      dir_file <- paste(
        selected_dir,
        newDir,
        csv_file,
        sep = "\\"
      )
      
      # write dataframe to csv in selected directory
      write.csv(
        x = get(x),
        file = dir_file,
        # append = FALSE,
        row.names = FALSE
      )
      
      # message when complete
      # out <- " written to .csv file"
      
      # message to user
      # message(x, out)
      message(x, " written to .csv file")
      
    }
    # end if
    
  }, simplify = FALSE, USE.NAMES = TRUE)
  # end
  invisible(NULL)
}
# end SaveAsCsv

# function testing below --------------------------------------------------

# SaveAsCsv(sourceFile = "source_load_card.R", newDir = "Test")
