# Function to average the top and bottom traces of a polarization curve
# Written by MRG 11/27/2015

#' @describeIn import_cell_cv Function which averages the top and bottom traces
#'   of a polarization curve.
#' @export

polcurve_averager <- function(inputdf, voltageid = "Vf", currentid = "scurrent"){
  
  inputlength <- length(inputdf[, voltageid])
  
  # Find the voltage/current peaks?
  firstpeak <- which.max(inputdf[1:floor(inputlength/2), voltageid])
  secondpeak <- which.max(inputdf[floor(inputlength/2):inputlength, voltageid]) + floor(inputlength/2)
  midpoint <- (firstpeak + secondpeak)/2
  
  firsttrace <- inputdf[firstpeak:midpoint, currentid]
  secondtrace <- rev(inputdf[(midpoint+1):secondpeak, currentid])
  
  if(length(firsttrace) > length(secondtrace)){
    firsttrace <- firsttrace[1:length(secondtrace)]
  }
  
  if(length(secondtrace) > length(firsttrace)){
    secondtrace <- secondtrace[1:length(firsttrace)]
  }
  
  avgtrace <- (firsttrace + secondtrace)/2
  voltagetrace <- inputdf[firstpeak:(firstpeak + length(avgtrace) - 1), voltageid]
  outputdf <- data.frame(avgcurrent = avgtrace, avgvoltage = voltagetrace, avgpower = avgtrace * voltagetrace)
  return(outputdf)
}

#' @describeIn import_cell_cv Function which smooths out the pulses in the
#'   polarization curve due to diaphragm pumps
#' @export

polcurve_smoother <- function(inputdf, rpm){
  sampletime <- inputdf$T[[2]] - inputdf$T[[1]]
  
  pts.per.rot <- 60/(rpm * sampletime) + 1 #number of points in a rotation
  #plus 1 is for the moving avg filter
  fparams <- rep(1/pts.per.rot, pts.per.rot)
  smoothed_current <- as.vector(filter(inputdf$current, fparams, sides = 2))
  smoothed_power <- smoothed_current * inputdf$Vf
  smoothed_polcurve <- data.frame(scurrent = smoothed_current,
                                  svoltage = inputdf$Vf,
                                  spower = smoothed_power)
  return(smootehd_polcurve)
}