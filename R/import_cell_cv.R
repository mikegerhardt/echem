# cell polarization curve import function (cyclic voltammetry version)
# Written by MRG 11/27/2015

#'  Imports full cell cyclic voltammogram
#' 
#'  This function imports a cyclic voltammogram performed on a full cell for the
#'  purpose of measuring a polarization curve.
#' 
#'  This function, by default, performs two filters on the data to smooth it.
#'  First, a moving average filter is applied to dampen pulses from non-steady
#'  flow pumps such as diaphragm pumps. The moving average is applied over one
#'  pump cycle, which is why the function takes pump RPM as an input. The 
#'  second filter splits the polarization curve in half and averages the top
#'  and bottom halves, to adjust for state of charge changes while running the
#'  cyclic voltammogram.
#'  
#'  
#' @param fname File to import.
#' @param area Geometric area of the cell electrodes, for correction from
#'    current/power to current/power density.
#' @param rpm Rotation rate of the pumps, in rotations per minute. This is
#'    used by the moving average filter to average the collected data over
#'    one pump pulse.
#' @param cols Data columns to import. Must match the names given by Gamry
#'    Framework software.
#' @param movingavg If TRUE (default is TRUE), the function will apply a
#'    moving average to the measured current to smooth out oscillations in
#'    the data from a pulsing pump.
#' @param traceavg If TRUE (default is TRUE), the function will attempt to
#'    split the polarization curve into two traces, a top and bottom trace, and
#'    average the two. This is useful if the cyclic voltammogram is changing
#'    the state of charge of the cell.
#'    
#' @return Returns a data frame with current, voltage, and power columns. The
#'    filename of each file is also reported in the state of charge ("soc")
#'    column for later analysis.
#' @author Mike Gerhardt


import_cell_cv <- function(fname = file.choose(), area = 5, rpm = 50, cols = c("T", "Vf", "Im"),
                           movingavg = TRUE, traceavg = TRUE, ...){
  
  # old code had ncols set to 9
  
  inputdf <- importGamry(fname, ...)
  outdf <- inputdf[, cols]
  outdf$current <- -outdf$Im/area
  outdf$power <- outdf$current * outdf$Vf
  
  
  #--------------------
  # Moving Average code
  #--------------------
  
  if(movingavg){
    sampletime <- outdf$T[[2]] - outdf$T[[1]]
    
    pts.per.rot <- 60/(rpm * sampletime) + 1 #number of points in a rotation
    #plus 1 is for the moving avg filter
    fparams <- rep(1/pts.per.rot, pts.per.rot)
    
    outdf$scurrent <- as.vector(filter(outdf$current, fparams, sides = 2))
    
    outdf$spower <- outdf$scurrent* outdf$Vf  
  }
  
  #-------------------------
  # Trace Average code
  #-------------------------
  
  if(traceavg){
    outdf <- polcurve_averager(outdf, currentid = if(movingavg) "scurrent" else "current")
    
  }
  
  outdf$soc <- fname
  return(outdf)
}