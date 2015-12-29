# needs ImportGamry
# These functions import a set of chronocoulometry curves labeled "charge" and
# "discharge". The "Capacity" one just trims the last charge point off each and
# is useful if you don't want to look at all the data.
# This was last edited 12-28-2015 and there were big changes to it.

#' @describeIn import_all_cc_cycles Import all constant voltage cycling curves
#'   (current vs time)
#' @export


import_all_cv_cycles <- function(filepath, 
                                 allcyclesid = "charge", 
                                 dischargeid = "discharge",
                                 # getcapacities = TRUE, 
                                 ...){
  
  all_cycles_list <- list.files(pattern = allcyclesid,
                                path = filepath,
                                full.names = TRUE)
  discharge_cycles_list <- mixedsort(grep(pattern = dischargeid, all_cycles_list, value = TRUE))
  charge_cycles_list <- mixedsort(grep(pattern = dischargeid, all_cycles_list, value = TRUE, invert = TRUE))
  
  # import all the charge cycles
  charge_cycles_df <- ldply(charge_cycles_list, 
                            .fun = function(x){
                              cycledata <- import_cv_cycle(x)
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
  

  # set all charge cycles to "Charge" type and all discharge cycles to "Discharge" type
  charge_cycles_df$type <- "Charge"
  discharge_cycles_df$type <- "Discharge"
  
  # smush everything into a data.frame
  
  all_cycles_df <- rbind(charge_cycles_df, discharge_cycles_df)
  return(all_cycles_df)
}

