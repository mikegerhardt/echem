import_all_cc_cycles <- function(filepath = "mrgPWRCAPACITY/CHARGE_DISCHARGE/",
                                 allcyclesid = "CHARGE",
                                 dischargeid = "DISCHARGE", ...){
  if(!substr(filepath, nchar(filepath), nchar(filepath)) %in% c("\\", "/")){
    # This if statement checks to see if filepath ends with "\\" or "/"
    # since I often forget to add those
    filepath <- paste(filepath, "\\", sep = "")
  }
  
  # set up a list of filenames to import
  all_cycles_list <- list.files(pattern = allcyclesid,
                                path = filepath)
  discharge_cycles_list <- lapply(mixedsort(grep(pattern = dischargeid, all_cycles_list, value = TRUE)),
                                  function(x) paste(filepath, x, sep = ""))
  #discharge_cycle_location <- all_cycles_list %in% discharge_cycles_list
  charge_cycles_list <- lapply(mixedsort(grep(pattern = dischargeid, all_cycles_list, value = TRUE, invert = TRUE)),
                               function(x) paste(filepath, x, sep = ""))
  
  # import all the charge cycles
  # cycle_numbers <- seq(from = 1, to = length(charge_cycles_list), by = 1)
  charge_cycles_df <- ldply(charge_cycles_list, 
                               .fun = function(x){
                                 cycledata <- import_cc_cycle(x)
                                 numberplusdigits <- tail(unlist(strsplit(x, split = "#")), 1)
                                 cyclenumber <- as.numeric(regmatches(numberplusdigits, 
                                                                      regexpr("\\d+",numberplusdigits)))
                                 cycledf <- cbind(cycledata, cycle = cyclenumber)
                                 return(cycledf)
                               },
                               ...)
  
  # import all discharge cycles
  discharge_cycles_df <- ldply(discharge_cycles_list, 
                               .fun = function(x){
                                 cycledata <- import_cc_cycle(x)
                                 numberplusdigits <- tail(unlist(strsplit(x, split = "#")), 1)
                                 cyclenumber <- as.numeric(regmatches(numberplusdigits, 
                                                                      regexpr("\\d+",numberplusdigits)))
                                 cycledf <- cbind(cycledata, cycle = cyclenumber)
                                 return(cycledf)
                               },
                               ...)
#   for(i in cycle_numbers){
#     
#     # this loop adds a cycle number to each data frame in each list. I couldn't figure out how to do with
#     # mapply or mdply or whatever.
#     
#     charge_cycles_data[[i]]$cycle <- i
#     discharge_cycles_data[[i]]$cycle <- i
#   }
#   
  
  # set all charge cycles to "Charge" type and all discharge cycles to "Discharge" type
  
  # charge_cycles_df <- ldply(charge_cycles_data)
  if(length(charge_cycles_list > 0)) charge_cycles_df$type <- "Charge"
  # discharge_cycles_df <- ldply(discharge_cycles_data)
  if(length(discharge_cycles_list > 0)) discharge_cycles_df$type <- "Discharge"

  # smush everything into a data.frame
  
  all_cycles_df <- rbind(charge_cycles_df, discharge_cycles_df)

  return(all_cycles_df)
  
  
}