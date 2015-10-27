import_absorption <- function(filename = file.choose(),
                              data_tag = ">>>>>Begin Spectral Data<<<<<",
                              columns = c("Wavelength", "Absorption")){
  filelines <- readLines(filename)
  beginning_line <- grep(data_tag, filelines)
  absorption_spectrum <- read.table(textConnection(filelines), 
                                    skip = beginning_line,
                                    col.names = columns)
  return(absorption_spectrum)
}