# Current/energy efficiency plotter
# for electrochemistry package
# Written by Mike Gerhardt
# 12/28/2015

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