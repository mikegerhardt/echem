# Electrochemistry package
# Written by Mike Gerhardt
# 10/17/2015

#' Imports a Gamry data file.
#' 
#' \code{importGamry} imports a Gamry .dta file and returns a data frame with 
#' each column corresponding to a column in the data file.
#' 
#' This function needs to know which lines in the .dta file to import. The 
#' default behavior is to count each line in the file and assume that the
#' number that appears the most times is where the data is stored in the file.
#' Sometimes this fails because Gamry adds extensive header documentation to
#' their files. The ncols parameter can be used to manually specify which lines
#' to import based on how many columns each line has.
#' 
#' @param inputfile File to be imported.
#' @param ncols Number of expected data columns. The default of 0 has the
#'   function make an educated guess about how many columns there are.
#'   
#' @return A data frame with all the data in the .dta file.
#' @author Mike Gerhardt
#' @export

importGamry <- function(inputfile = file.choose(), ncols = 0){
  alldata <- readLines(inputfile)
  fields <- sapply(strsplit(alldata, split = "\t"), length)
  
  if(ncols == 0){  
    fields.to.remove <- grep(which.max(tabulate(fields)),
                             fields, invert = TRUE)
  } else {
    fields.to.remove <- grep(ncols, fields, invert = TRUE)
  }
  
  usable.data <- alldata[-fields.to.remove] # this line removes all the random header fields etc
  
  # name.repeats finds all the times Gamry repeats the names of the columns
  # This happens when doing multiple CV cycles, and it screws up read.table
  # because read.table then imports everything as factors (bad)
  
  if(usable.data[1] %in% usable.data[-1]){
    # if the first line of usable.data is repeated elsewhere in usable.data
    # then it must be removed prior to read.table
    name.repeats <- grep(usable.data[1], usable.data)[-1]
    importable.data <- usable.data[-name.repeats]  
  } else {
    # if the first line of usable.data doesn't appear again, don't do anything
    importable.data <- usable.data
  }
  
  outdf <- read.table(textConnection(importable.data), header = TRUE)
  return(outdf)
}