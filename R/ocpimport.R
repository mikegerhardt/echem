# written by Mike Gerhardt
# 12/4/2015
# ocpimport is for OCP vs SoC graphs.

getocp <- function(filename){
  whatscan <- list(NULL, time = 0, volts = 0, NULL, NULL, NULL, 
                   NULL, NULL, NULL, NULL, NULL)
  skipto <- length(count.fields(filename)) + 1
  voltage <- scan(file = filename, skip = skipto, what = whatscan)[[3]]
  return(voltage)
}

ocpimport <- function(filepattern = "OCP_vs_SoC", 
                      state.of.charge = seq(from = 10, to = 100, by = 10)){
  voltages <- ldply(mixedsort(list.files(pattern = filepattern)), getocp)
  ocpoutputdf <- data.frame(soc = state.of.charge, open.circuit = voltages)
  colnames(ocpoutputdf) <- c("soc", "ocp")
  return(ocpoutputdf)
  
}