# Electrochemistry package
# Last update 12-28-2015

#' Import each cycle in a folder.
#' 
#' These functions imports all the constant current or constant voltage cycling
#' data in a given folder. The functions rely on differences in filename to
#' distinguish charge curves from discharge curves.
#' 
#' Each of these functions calls the appropriate import_cc_cycle or 
#' import_cv_cycle fuction on each file matching the descriptions you give in
#' the function call.
#' 
#' @aliases import_all_cv_cycles
#' @param filepath Path where the charge/discharge cycle files are located
#' @param allcyclesid A character string which is compared to each filename
#'   in \code{filepath}. Any file having this string is considered a cycle.
#' @param dischargeid A character string which is compared to each cycle
#'   found by \code{allcyclesid}. All cycles having this pattern are marked
#'   discharge cycles, and the remaining cycles are assumed to be charge
#'   cycles.
#' @param ... arguments to pass to each import_cycle function.
#' @return Returns a data frame containing each cycle in a folder, with a
#'   cycle number tag allowing you to subset the data by cycle if you wish.
#' @author Mike Gerhardt
#' @seealso \code{\link{import_cc_cycle}}, \code{\link{import_cv_cycle}}
#' @export

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
  # Don't make all_cycles_list use full.names = TRUE
  # If you do, both the CHARGE and DISCHARGE tags will appear in each element
  # of all_cycles_list. Better to split the cycles up first and then paste the full
  # filename back in.
  
  discharge_cycles_list <- mixedsort(grep(pattern = dischargeid, all_cycles_list, value = TRUE))
  discharge_cycles_list <- lapply(discharge_cycles_list, function(x) paste(filepath, x, sep = ""))

  charge_cycles_list <- mixedsort(grep(pattern = dischargeid, all_cycles_list, value = TRUE, invert = TRUE))
  charge_cycles_list <- lapply(charge_cycles_list, function(x) paste(filepath, x, sep = ""))
  
  # import all the charge cycles

  charge_cycles_df <- ldply(charge_cycles_list, 
                               .fun = function(x, ncols = 12){
                                 cycledata <- import_cc_cycle(x, ncols = ncols)
                                 numberplusdigits <- tail(unlist(strsplit(x, split = "#")), 1)
                                 cyclenumber <- as.numeric(regmatches(numberplusdigits, 
                                                                      regexpr("\\d+",numberplusdigits)))
                                 cycledf <- cbind(cycledata, cycle = cyclenumber)
                                 return(cycledf)
                               },
                               ...)
  
  # import all discharge cycles
  discharge_cycles_df <- ldply(discharge_cycles_list, 
                               .fun = function(x, ncols = 12){
                                 cycledata <- import_cc_cycle(x, ncols = ncols)
                                 numberplusdigits <- tail(unlist(strsplit(x, split = "#")), 1)
                                 cyclenumber <- as.numeric(regmatches(numberplusdigits, 
                                                                      regexpr("\\d+",numberplusdigits)))
                                 cycledf <- cbind(cycledata, cycle = cyclenumber)
                                 return(cycledf)
                               },
                               ...)

  
  # set all charge cycles to "Charge" type and all discharge cycles to "Discharge" type

  if(length(charge_cycles_list > 0)) charge_cycles_df$type <- "Charge"
  if(length(discharge_cycles_list > 0)) discharge_cycles_df$type <- "Discharge"

  # smush everything into a data.frame
  
  all_cycles_df <- rbind(charge_cycles_df, discharge_cycles_df)

  return(all_cycles_df)
  
  
}