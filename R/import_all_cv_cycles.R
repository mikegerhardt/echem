# needs ImportGamry
# These functions import a set of chronocoulometry curves labeled "charge" and
# "discharge". The "Capacity" one just trims the last charge point off each and
# is useful if you don't want to look at all the data.



import_all_cv_cycles <- function(filepath, 
                                 allcyclesid = "charge", 
                                 dischargeid = "discharge",
                                 # getcapacities = TRUE, 
                                 ...){
  home_directory <- getwd()
  setwd(filepath)
  
  all_cycles_list <- list.files(pattern = allcyclesid)
  discharge_cycles_list <- mixedsort(list.files(pattern = dischargeid))
  discharge_cycles_location <- all_cycles_list %in% discharge_cycles_list
  charge_cycles_list <- mixedsort(all_cycles_list[!discharge_cycles_location])
  
  # import all the charge cycles
  cycle_numbers <- seq(from = 1, to = length(charge_cycles_list), by = 1)
  charge_cycles_data <- lapply(charge_cycles_list, FUN = import_cv_cycle, ...)
  
  # import all discharge cycles
  discharge_cycles_data <- lapply(X = discharge_cycles_list, FUN = import_cc_cycle, ...)
  
  charge_cycles_data <- mapply(cbind, charge_cycles_data, "cycle" = cycle_numbers, SIMPLIFY = FALSE)
  discharge_cycles_data <- mapply(cbind, discharge_cycles_data, "cycle" = cycle_numbers, SIMPLIFY = FALSE)
  
#   if(getcapacities == TRUE){
#     # Add a column reporting the charge/discharge capacity if I asked for it (default is to report it)
#     charge_capacities <- unlist(lapply(charge_cycles_data, 
#                                        function(x) tail(x, 1)$Q))
#     
#     discharge_capacities <- unlist(lapply(discharge_cycles_data,
#                                           function(x) tail(x, 1)$Q))
#     
#     charge_cycles_data <- mapply(cbind, charge_cycles_data, "capacity" = charge_capacities, SIMPLIFY = FALSE)
#     discharge_cycles_data <- mapply(cbind, discharge_cycles_data, "capacity" = discharge_capacities, SIMPLIFY = FALSE)
#   }
  
  # set all charge cycles to "Charge" type and all discharge cycles to "Discharge" type
  charge_cycles_df <- ldply(charge_cycles_data)
  charge_cycles_df$type <- "Charge"
  discharge_cycles_df <- ldply(discharge_cycles_data)
  discharge_cycles_df$type <- "Discharge"
  
  # smush everything into a data.frame
  
  all_cycles_df <- rbind(charge_cycles_df, discharge_cycles_df)
  setwd(home_directory)
  return(all_cycles_df)
}

