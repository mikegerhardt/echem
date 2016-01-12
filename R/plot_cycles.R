#'  Plot a set of constant-current charge-discharge cycles
#'  
#'  This function takes a set of voltage vs. time curves from a cyclic
#'  charge-discharge experiment and plots them.
#'  
#'  This function is designed to be used on data frames generated with
#'  \code{import_all_cc_cycles} and may not work otherwise. The function will
#'  return a ggplot object which can be modified with ggplot methods.
#'  
#'  @param inputdf Data frame, typically generated with \code{import_all_cc_cycles}
#'  
#'  @author Mike Gerhardt
#'  @return Returns a ggplot object containing all charge/discharge cycles
#'    specified by \code{cyclelist}
#'  
#'  @export
#'  @seealso \code{\link{import_all_cc_cycles}}

plot_cycles <- function(inputdf, cyclelist = "all"){
  
  if(cyclelist != "all"){
    # Subsets the input data frame as directed by cyclelist
    cycledf <- inputdf[inputdf$cycle %in% cyclelist, ]
  } else cycledf <- inputdf
  # If no cyclelist argument is given, just plot all the cycles.
  
  cyclebreaker <- sapply(cycledf$Type, 
                        function(x) if(x == "Charge") 0 else 0.5)
  cycledf <- cbind(cycledf, cyclebreaks = cyclebreaker + cycledf$cycle)

  cycleplot <- (ggplot(cycledf, aes(x = Q, y = Vf, color = cycle))
                + theme_bw(24)
                + geom_line(breaks = cyclebreaks)
                + xlab("Charge (C)")
                + ylab("Cell Voltage (V)")
                + scale_color_discrete(name = "Cycle")
  )
  return(cycleplot)
}
