# Electrochemistry package
# Mike Gerhardt

#' Imports all cyclic voltammetry data
#' 
#' Imports data from any cyclic voltammetry curves collected with the Gamry
#' potentiostat and outputs them in a list of data frames.
#' 
#' This function finds all files marked with \code{identifier}, assumes they
#' are cyclic voltammograms, and calls \code{import_cv} on each. It puts
#' out a list, with each list element containing the full filename from which
#' the data was imported and the data itself in a data frame.
#' This function should NOT be used with cyclic voltammograms performed on
#' a full cell to obtain polarization curves. Use \code{import_cell_cv} 
#' instead.
#' 
#' @param filepath The directory in which the files are located.
#' @param identifier A character string. All file names in \code{filepath} are 
#'   searched for this value, and any file names with a match are imported.
#' @param ... More parameters to pass to import_cv.
#'
#' @return Returns a list. Each list element contains the full path and file
#'   name of the imported file and a data frame with that file's data.
#' @seealso \code{\link{import_cv}}
#' @author Mike Gerhardt
#' @export

import_all_cyclic_voltammetry <- function(filepath, identifier = ".DTA", ...){

  if(!substr(filepath, nchar(filepath), nchar(filepath)) %in% c("\\", "/")){
    # This if statement checks to see if filepath ends with "\\" or "/"
    # since I often forget to add those
    filepath <- paste(filepath, "\\", sep = "")
  }
  
  cv_filenames <- list.files(pattern = identifier,
                             path = filepath,
                             full.names = TRUE)
  cv_data <- lapply(cv_filenames, import_cv, ...)
  return(cv_data)
}