import_all_cc_cycles <- function(filepath = "mrgPWRCAPACITY/CHARGE_DISCHARGE/",
                                 allcyclesid = "CHARGE",
                                 dischargeid = "DISCHARGE", ...){
  home_directory <- getwd()
  setwd(filepath)
  
  # set up a list of filenames to import
  all_cycles_list <- list.files(pattern = "CHARGE")
  discharge_cycles_list <- mixedsort(list.files(pattern = "DISCHARGE"))
  discharge_cycle_location <- all_cycles_list %in% discharge_cycles_list
  charge_cycles_list <- mixedsort(all_cycles_list[!discharge_cycle_location])
  
  # grab all the charge cycles, tag them as "Charge" type, and add cycle number
  cycle_numbers <- seq(from = 1, to = length(charge_cycles_list), by = 1)
  charge_cycles_data <- lapply(charge_cycles_list, FUN = import_cc_cycle, ...)
  #charge_cycles_data <- lapply(X = charge_cycles_data, FUN = function(x){x$type <- "Charge"}) 
  #charge_cycles_data <- mdply(.fun = function(x, y){x$cycle <- y} ,charge_cycles_data, cycle_numbers)
  
  #grab all discharge cycles, tag as "Discharge", add cycle number
  discharge_cycles_data <- lapply(X = discharge_cycles_list, FUN = import_cc_cycle, ...)
  
  for(i in cycle_numbers){
    
    # this loop adds a cycle number to each data frame in each list. I couldn't figure out how to do with
    # mapply or mdply or whatever.
    
    charge_cycles_data[[i]]$cycle <- i
    discharge_cycles_data[[i]]$cycle <- i
  }
  
  charge_cycles_df <- ldply(charge_cycles_data)
  charge_cycles_df$type <- "Charge"
  discharge_cycles_df <- ldply(discharge_cycles_data)
  discharge_cycles_df$type <- "Discharge"

  # smush everything into a data.frame
  
  all_cycles_df <- rbind(charge_cycles_df, discharge_cycles_df)
  
  return(all_cycles_df)
  
  setwd(home_directory)
}