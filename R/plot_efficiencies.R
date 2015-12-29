# Current/energy efficiency plotter
# for electrochemistry package
# Written by Mike Gerhardt
# 12/28/2015

#' Plot current and energy efficiency versus cycle number
#' 
#' This function takes a data frame with current and energy efficiency data and
#' makes a plot of these efficiencies versus cycle number.
#' 
#' This function is designed to be used on data frames generated with
#' \code{import_cycling_stats} and assumes that the Cycle and ceffy columns exist.
#' The function will return a ggplot, which can be further edited through
#' any of the ggplot methods.
#' 
#' @param inputdf Data frame with the data to plot. Should be a data frame made
#'   with \code{import_cycling_stats}
#' @param energyeffy Defaults to TRUE. If TRUE, plot the energy efficiency as
#'   well as the current efficiency.
#'
#' @return Returns a ggplot object plotting current/energy efficiency versus
#'   cycle number.
#' @author Mike Gerhardt
#' 
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link{import_cycling_stats}}

plot_efficiencies <- function(inputdf, energyeffy = TRUE){
  effyplot<- (ggplot(inputdf, aes(x = Cycle, y = ceffy, color = "black")) 
     + geom_point()
     + (if(energyeffy == TRUE) geom_point(aes(x = Cycle, y = eeffy, color = "blue")))
     + theme_bw(18)
     + scale_color_discrete(labels = c("Current Effy", "Energy Effy"),
                            name = "")
     + ylab("Efficiency")
     + ylim(0, 1)
  )
  return(effyplot)
}