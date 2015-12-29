# Polarization curve plotter
# for electrochemistry package
# Written by Mike Gerhardt
# 12/28/2015

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

plot_powercurve <- function(inputdf, xdata = "avgcurrent", ydata = "avgpower"){
  pcplot <- (ggplot(data = inputdf, 
                    aes_string(x = xdata, y = ydata))
             + geom_line(aes(color = soc, breaks = soc))
             + theme_classic(18)
             + xlab(expression(paste(Current~Density~~(A/cm^2)))) + ylab(expression(paste(Power~Density~~(W/cm^2))))
  )
  return(pcplot)
}