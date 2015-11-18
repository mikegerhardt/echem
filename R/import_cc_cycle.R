import_cc_cycle <- function(inputfile = file.choose(), cols = c("T", "Vf","Im"),
                            getrealtime = TRUE, ...){
  # function for importing constant current cycles
  
  inputdf <- importGamry(inputfile, ...)
  outdf <- inputdf[, cols]
  
  # make a "charge" column so you can plot voltage vs capacity for different currents
  outdf$Q <- outdf[,"T"] * mean(outdf[,"Im"])
  
  if(getrealtime){
    # This chunk finds the "Start Time Offset" bit (the last line of the file)
    # and makes a "realtime" column which just adds start time offset to T.
    last_line <- tail(readLines(inputfile), 1)
    last_line_chopped <- as.numeric(unlist(strsplit(last_line, split = "\t")))
    time_offset <- last_line_chopped[!is.na(last_line_chopped)]
    outdf$realtime <- outdf[,"T"] + time_offset
  }
  
  return(outdf)
}