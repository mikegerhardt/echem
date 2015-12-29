# Electrochemistry package
# Written by Mike Gerhardt
# 10/17/2015
#' Imports a single constant-voltage cycle.
#' 
#' This function takes a single charge or discharge cycle from a cyclic
#' charge/discharge loop run from Gamry Framework and outputs the time,
#' voltage, and current into a data frame.
#' 
#' This function calls \code{importGamry} and strips the output down to
#' time, charge, voltage, and current columns. Unlike \code{import_cc_cycle}
#' this function can't take a sequence of cycles and output during each
#' cycle the time elapsed from the start of the first cycle.
#' 
#' @param inputfile File to import.
#' @param cols The names of the columns to import. These must match the names
#'   given by Gamry Framework to the data in the file.
#' 
#' @return Returns a data frame containing the time, voltage, current, and
#' charge sampled during the cycle.
#' 
#' @author Mike Gerhardt
#' @seealso \code{\link{import_all_cc_cycles}}, \code{\link{import_cycling_stats}},
#'   \code{\link{cv_cycle_stats}}

import_cv_cycle <- function(inputfile = file.choose(), cols = c("T", "Q", "Vf", "Im"), ...){
  inputdf <- importGamry(inputfile, ...)
  outdf <- inputdf[, cols]
  return(outdf)
}