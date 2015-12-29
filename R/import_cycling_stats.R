# Electrochemistry package
# Written by Mike Gerhardt
# 10/17/2015

#' Import constant current cycling statistics.
#' 
#' Imports a Gamry .DTA file from a cycling experiment. These files are
#' generated during cyclic charge/discharge experiments out of the PWR
#' package and contain cycling efficiencies.
#' 
#' When a cyclic charge/discharge loop is run using Gamry Framework 6, it
#' outputs each charge/discharge curve as its own .DTA file, as well as a
#' single .DTA file with each cycle's charge and discharge capacities,
#' energies, and starting voltages. This function imports that file and puts
#' it into a data frame.
#' 
#' @param inputfile File to be imported.
#' @param cols Columns to be imported (must match the names of columns
#'   in the data file)
#' @param efficiencies If TRUE, current and energy efficiency will be
#' calculated from the data and reported (default). If FALSE, efficiency
#' calculation is skipped.
#' 
#' @return A data frame with the specified columns, plus (optionally) current
#'   and energy efficiencies.
#'   
#' @author Mike Gerhardt
#'  
import_cycling_stats <- function(inputfile = file.choose(),
                                 cols = c("Type", "Cycle", 
                                          "Charge", "Duration", 
                                          "Vstart", "Vend", 
                                          "Energy"),
                                 efficiencies = TRUE, ...){
  inputdf <- importGamry(inputfile, ...)
  outdf <- inputdf[, cols]
  
  # The next block calculates current and energy efficiencies, there is a tag to shut it off in function options
  if(efficiencies == TRUE){
    charge_cycles <- subset(outdf, Type == 0)
    discharge_cycles <- subset(outdf, Type == 1)
    
    ceffy <- discharge_cycles$Charge / charge_cycles$Charge
    eeffy <- -discharge_cycles$Energy / charge_cycles$Energy
    
    # Order outdf by cycle type (charge/discharge). Then I can add in ceffy and eeffy and they'll repeat over
    # the whole df, and I don't have to split up the df into charge/discharge dfs.
    outdf <- outdf[with(outdf, order(Type)), ]
    outdf$ceffy <- ceffy
    outdf$eeffy <- eeffy
  }
  
  return(outdf)
}