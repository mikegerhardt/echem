# Polarization curve plotter
# for electrochemistry package
# Written by Mike Gerhardt
# 12/28/2015
#' Plot polarization curves
#' 
#' This function takes a data frame with current and voltage data and
#' makes a polarization curve plot
#' 
#' This function is designed to be used on data frames generated with
#' \code{import_cell_cv}. But in reality as long as the data frames have named
#' current, voltage, and power columns, and those columns are named, this
#' function should be able to generate polarization curves from those data
#' frames.
#' 
#' @param inputdf Data frame with the data to plot.
#' @param xdata Name of the column under which the current data are stored.
#' @param ydata Name of the column under which the voltage/power data are stored.
#'
#' @return Returns a ggplot polarization curve, which should be manipulable
#'   by functions in the ggplot package.
#' @author Mike Gerhardt
#' 
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link{import_cell_cv}}
#' @export
plot_polcurve <- function(inputdf, xdata = "avgcurrent", ydata = "avgvoltage"){
  pcplot <- (ggplot(data = inputdf, 
                      aes_string(x = xdata, y = ydata),
                    environment = environment())
               + geom_line(aes(color = soc, breaks = soc))
               + theme_classic(18)
               + xlab(expression(paste(Current~Density~~(A/cm^2)))) 
             + ylab(expression(paste(Cell~Voltage~~(V))))
  )
  return(pcplot)
}

#' @describeIn plot_polcurve Plot power density versus current density, to see
#'   peak galvanic power density.
#' @export
plot_powercurve <- function(inputdf, xdata = "avgcurrent", ydata = "avgpower"){
  pcplot <- (ggplot(data = inputdf, 
                    aes_string(x = xdata, y = ydata))
             + geom_line(aes(color = soc, breaks = soc))
             + theme_classic(18)
             + xlab(expression(paste(Current~Density~~(A/cm^2)))) + ylab(expression(paste(Power~Density~~(W/cm^2))))
  )
  return(pcplot)
}