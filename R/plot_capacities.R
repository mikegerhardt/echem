# Charge/discharge capacity plotter
# for electrochemistry package
# Written by Mike Gerhardt
# 12/28/2015

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