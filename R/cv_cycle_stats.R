# Electrochemistry package
# Mike Gerhardt 12-28-2015
#
#' Compute efficiencies for constant voltage cycling
#' 
#' This function takes as input a data set of constant voltage cycles and
#' returns a data frame listing cycle number, capacity, current efficiency,
#' energy efficiency, and charging and discharging voltages.
#' 
#' At time of writing, Gamry Framework doesn't allow constant voltage cycling
#' with its cyclic charge/discharge package, meaning that a single .DTA file
#' with the capacities, efficiencies, etc for constant voltage cycling is never
#' generated. This function collects that data from constant voltage cycles.
#' The corollary for constant current cycles has not been written; instead
#' the \code{import_cycling_stats()} function can be used.
#' 
#' @param inputdf Data frame containing all constant voltage cycles, preferably
#'   imported via \code{import_all_cv_cycles()} or similar
#' @param chargeid Identifies charge cycles based on subsetting the input data
#'   frame on the "type" column.
#' @param dischargeid Identifies discharge cycles based on subsetting the input
#'   data frame.
#' @param cycleid The name of the column under which cycle number is stored in
#'   the input data frame.
#' @param voltageid The name of the column under which the charging voltage is
#'   stored in the input data frame.
#' @param capacityid The name of the column under which the charge capacity is
#'   stored in the input data frame.
#' @return Returns a data frame with columns for cycle, charge and discharge
#'   capacities, current efficiency, charging and discharging voltages,
#'   charging and discharging energies, and energy efficiency.
#'
#' @author Mike Gerhardt
#' @seealso \code{\link{import_all_cv_cycles}}, \code{\link{import_cycling_stats}}

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