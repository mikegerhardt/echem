# Charge/discharge capacity plotter
# for electrochemistry package
# Written by Mike Gerhardt
# 12/28/2015

#' Plot charge and discharge capacity versus cycle number
#' 
#' This function takes a data frame with charge and discharge capacity data and
#' makes a plot of these capacities versus cycle number. Useful for qualitative
#' investigations of capacity fade.
#' 
#' This function should only be used on data frames generated with
#' \code{import_cycling_stats} as it will probably fail otherwise. It assumes
#' that the Type column exists in the input data frame and colors the plot by
#' Type. The function will return a ggplot, which can be further edited through
#' any of the ggplot methods.
#' 
#' @param inputdf Data frame with the data to plot. Should be a data frame made
#'   with \code{import_cycling_stats}
#' @param xdata Name of the column under which the cycle numbers are stored.
#' @param ydata Name of the column under which the charge/discharge capacities
#'   are stored.
#'
#' @return Returns a ggplot object plotting charge/discharge capacity versus
#'   cycle number.
#' @author Mike Gerhardt
#' 
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link{import_cycling_stats}}

plot_capacities <- function(inputdf, xdata = "Cycle", ydata = "Charge"){
  cap_plot <- (ggplot(inputdf, aes_string(x = xdata, 
                                    y = ydata), 
                      environment = environment()) 
               + geom_point(aes(color = factor(Type)))
               + theme_bw(18)
               + scale_color_discrete(labels = c("Charge", "Discharge"),
                                      name = "")
               + ylab("Charge (C)")
               + expand_limits(y = 0)
  )
  return(cap_plot)
}