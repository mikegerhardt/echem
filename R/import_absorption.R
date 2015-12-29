#' Import absorption spectrum.
#' 
#' This function imports an absorption spectrum collected by the Ocean Optics
#' Flame spectrometer in our lab. It just needs to be told where the data
#' begins by the data_tag parameter.
#' 
#' @param filename The file to import.
#' @param data_tag Identifies where in the file the data begins, defaults to
#'   the right option for the Ocean Optics Flame spectrometer in our lab.
#' @param columns What to name the columns in the data frame.
#' @return Returns a data frame with column names given by the \code{columns}
#'   input parameter.
#' @author Mike Gerhardt
#' @export


import_absorption <- function(filename = file.choose(),
                              data_tag = ">>>>>Begin Spectral Data<<<<<",
                              columns = c("Wavelength", "Absorption")){
  filelines <- readLines(filename)
  beginning_line <- grep(data_tag, filelines)
  absorption_spectrum <- read.table(textConnection(filelines), 
                                    skip = beginning_line,
                                    col.names = columns)
  return(absorption_spectrum)
}