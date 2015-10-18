# Electrochemistry package draft
# Written by Mike Gerhardt
# 10/17/2015



importGamry <- function(inputfile = file.choose(), ncols = 0){
  alldata <- readLines(inputfile)
  fields <- sapply(strsplit(alldata, split = "\t"), length)
  
  if(ncols == 0){  
    fields.to.remove <- grep(which.max(tabulate(fields)),
                             fields, invert = TRUE)
  } else {
    fields.to.remove <- grep(ncols, fields, invert = TRUE)
  }
  
  usable.data <- alldata[-fields.to.remove] # this line removes all the random header fields etc
  
  # name.repeats finds all the times Gamry repeats the names of the columns
  # This happens when doing multiple CV cycles, and it screws up read.table
  # because read.table then imports everything as factors (bad)
  
  if(usable.data[1] %in% usable.data[-1]){
    # if the first line of usable.data is repeated elsewhere in usable.data
    # then it must be removed prior to read.table
    name.repeats <- grep(usable.data[1], usable.data)[-1]
    importable.data <- usable.data[-name.repeats]  
  } else {
    # if the first line of usable.data doesn't appear again, don't do anything
    importable.data <- usable.data
  }
  
  outdf <- read.table(textConnection(importable.data), header = TRUE)
  return(outdf)
}

import_cc_cycle <- function(inputfile = file.choose(), cols = c("T", "Vf","Im")){
  # function for importing constant current cycles
  
  inputdf <- importGamry(inputfile)
  outdf <- inputdf[, cols]
  
  # make a "charge" column so you can plot voltage vs capacity for different currents
  outdf$Q <- outdf[,"T"] * mean(outdf[,"Im"])
  return(outdf)
}

import_cv_cycle <- function(inputfile = file.choose(), cols = c("T", "Q", "Vf", "Im")){
  inputdf <- importGamry(inputfile)
  outdf <- inputdf[, cols]
  return(outdf)
}

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

import_cycling_stats <- function(inputfile = file.choose(),
                                 cols = c("Type", "Cycle", "Charge", "Duration", "Vstart", "Vend", "Energy"),
                                 efficiencies = TRUE){
  inputdf <- importGamry(file.choose())
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