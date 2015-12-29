# Given a set of OCP files, produce an OCP vs SoC data frame
#
#' Import open circuit potential vs state of charge
#' 
#' This function, given a set of open circuit potential measurement files from
#' Gamry Framework, collects the last voltage data point from each file, and
#' puts them into a data frame to make plotting open circuit potential versus
#' state of charge straightforward
#' 
#' The function requires all open circuit potential files to have a unique
#' character string \code{filepattern} in their filenames. The function also
#' currently requires the user to specify the state of charge at which each
#' data file was collected. The function looks in the working directory for any
#' files with the tag \code{filepattern} and then uses \code{mixedsort} to
#' intelligently sort the files (so that, for example, #10 does not come
#' between #1 and #2).
#' 
#' @param filepattern A unique tag which appears in the filename of each data
#'   file you wish to import
#' @param state.of.charge A sequence of numbers which should be of the same
#'   length as the number of files you wish to import. Each number in the 
#'   sequence should correspond to the state of charge at which you made each
#'   open circuit potential measurement.
#'   
#' @return Returns a data frame containing open circuit potential and state of
#'   charge.
#' @author Mike Gerhardt
#' 
#' @seealso \code{\link[gtools]{mixedsort}}
#' @export
#'  
import_ocp <- function(filepattern = "OCP_before_CV", state.of.charge = seq(from = 10, to = 100, by = 10)){
  voltages <- ldply(mixedsort(list.files(pattern = filepattern)), getocp)
  ocpoutputdf <- data.frame(soc = state.of.charge, open.circuit = voltages)
  colnames(ocpoutputdf) <- c("soc", "ocp")
  return(ocpoutputdf)
}

#' @describeIn import_ocp This function returns the last recorded voltage from
#'   an open circuit potential data file.

getocp <- function(filename){
  whatscan <- list(NULL, time = 0, volts = 0, NULL, NULL, NULL, 
                   NULL, NULL, NULL, NULL, NULL)
  skipto <- length(count.fields(filename)) + 1
  voltage <- scan(file = filename, skip = skipto, what = whatscan)[[3]]
  return(voltage)
}

