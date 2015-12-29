#' Import cyclic voltammogram
#' 
#' Import a Gamry .dta file from a cyclic voltammetry experiment.
#' 
#' This function should be used to import standard half cell cyclic
#' voltammogram experiments. By default, it assumes you are using a 3mm
#' diameter working electrode and a silver/silver chloride reference electrode.
#'  These can be changed in the function call. If you're trying to import a
#' cyclic voltammogram performed on a full cell, use \code{import_cell_cv}.
#' 
#' @param inputfile File to import
#' @param cols Columns to import. Must match the column names given by the Gamry
#'   software.
#' @param refvoltage Voltage of the reference electrode used in the experiment.
#'   This function will add this value to the measured voltage in the data file.
#' @param earea Electrode area. The default value of 0.0765 is the area of a 
#'   three millimeter disk, in square centimeters.
#' @param multiplier The function will multiply the current by this after 
#'   dividing by \code{earea}. This defaults to 1000 so that the output current
#'   density is in milliamps per square centimeter.
#' @author Mike Gerhardt
#' @seealso \code{\link{import_cell_cv}}
#' @return Returns a data frame with the measured current, the measured voltage
#'   adjusted by \code{refvoltage}, and the current density, calculated by
#'   multiplying the measured current by \code{multiplier} and dividing by
#'   \code{earea}.
#' @export

import_cv <- function(inputfile = file.choose(), cols = c("Vf", "Im"), 
                      refvoltage = 0.21, earea = 0.07065, multiplier = 1000){
  inputdf <- importGamry(inputfile)
  outdf <- inputdf[, cols]
  if("Im" %in% cols){
    # make a current density column
    outdf$currentdensity <- outdf$Im *  multiplier / earea 
  }
  if("Vf" %in% cols){
    # shift voltage column to V vs SHE instead of Ag/AgCl (or whatever the ref is)
    outdf$Vf <- outdf$Vf + refvoltage
  }
  return(outdf)
}
