cv_cycle_stats <- function(inputdf,
                           chargeid = "Charge",
                           dischargeid = "Discharge",
                           cycleid = "cycle",
                           voltageid = "Vf",
                           capacityid = "Q"){
  
  charge_cycles = subset(inputdf, type == chargeid)
  discharge_cycles = subset(inputdf, type == dischargeid)
  
  # get cycle numbers
  cycle_numbers = unique(inputdf[, cycleid])
  
  # get capacities for each charge & discharge
  charge_capacities <- by(charge_cycles, 
                          charge_cycles[, cycleid], 
                          function(x) abs(tail(x, 1)[, capacityid]))
  
  discharge_capacities <- by(discharge_cycles, 
                             discharge_cycles[, cycleid], 
                             function(x) abs(tail(x, 1)[, capacityid]))
  ceffy <- discharge_capacities / charge_capacities
  
  #get charge and discharge voltages
  charge_voltages <- by(charge_cycles,
                        charge_cycles[, cycleid],
                        function(x) mean(x[, voltageid]))
  discharge_voltages <- by(discharge_cycles,
                           discharge_cycles[, cycleid],
                           function(x) mean(x[, voltageid]))
  charge_energies <- charge_capacities * charge_voltages
  discharge_energies <- discharge_capacities * discharge_voltages
  eeffy <- discharge_energies / charge_energies
  
  outdf <- data.frame(cbind(cycle_numbers, 
                            charge_capacities, 
                            discharge_capacities,
                            ceffy,
                            charge_voltages,
                            discharge_voltages,
                            charge_energies,
                            discharge_energies,
                            eeffy))
  
  names(outdf) <- c("cycle", 
                    "charge", 
                    "discharge",
                    "ceffy",
                    "charge_v", 
                    "discharge_v", 
                    "charge_e", 
                    "discharge_e",
                    "eeffy")
  return(outdf)
  
}