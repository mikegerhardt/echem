# cell polarization curve import function (cyclic voltammetry version)
# Written by MRG 11/27/2015

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