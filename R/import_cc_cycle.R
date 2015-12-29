#' Imports a single constant-current cycle.
#' 
#' This function takes a single charge or discharge cycle from a cyclic
#' charge/discharge loop run from Gamry Framework and outputs the time,
#' voltage, and current into a data frame.
#' 
#' This function calls \code{importGamry} and strips the output down to
#' time, voltage, and current columns. It then multiplies time by current
#' to get charge passed and adds that as a fourth column. If \code{getrealtime}
#' is TRUE then the function will find the last line of the Gamry data file,
#' where the "start time offset" is listed - this is how much time has passed
#' since the first cycle in the sequence.
#' 
#' @param inputfile File to import.
#' @param cols The names of the columns to import. These must match the names
#'   given by Gamry Framework to the data in the file.
#' @param getrealtime If TRUE, searches for the start time offset (see details)
#' 
#' @return Returns a data frame containing the time, voltage, current, and
#' charge sampled during the cycle.
#' 
#' @author Mike Gerhardt
#' @seealso \code{import_all_cc_cycles}, \code{import_cycling_stats},
#'   \code{cv_cycle_stats}
#' @export

import_cc_cycle <- function(inputfile = file.choose(), cols = c("T", "Vf","Im"),
                            getrealtime = TRUE, ...){
  # function for importing constant current cycles
  
  inputdf <- importGamry(inputfile, ...)
  outdf <- inputdf[, cols]
  
  # make a "charge" column so you can plot voltage vs capacity for different currents
  outdf$Q <- outdf[,"T"] * mean(outdf[,"Im"])
  
  if(getrealtime){
    # This chunk finds the "Start Time Offset" bit (the last line of the file)
    # and makes a "realtime" column which just adds start time offset to T.
    last_line <- tail(readLines(inputfile), 1)
    last_line_chopped <- as.numeric(unlist(strsplit(last_line, split = "\t")))
    time_offset <- last_line_chopped[!is.na(last_line_chopped)]
    outdf$realtime <- outdf[,"T"] + time_offset
  }
  
  return(outdf)
}