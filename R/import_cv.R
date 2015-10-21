import_cv <- function(inputfile = file.choose(), cols = c("Vf", "Im"), 
                      refvoltage = 0.21, earea = 0.07065, multiplier = 1000){
  inputdf <- importGamry(inputfile)
  outdf <- inputdf[, cols]
  if("Im" %in% cols){
    # make a current density column
    outdf$currentdensity <- outdf$Im *  multiplier / earea 
  }
  if("Vf" %in% cols){
    # shift voltage column to V vs SHE instead of Ag/AgCl (or whatever the ref is)
    outdf$Vf <- outdf$Vf + refvoltage
  }
  return(outdf)
}
