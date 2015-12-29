#' Save a ggplot graph
#' 
#' Exports a ggplot graph with a set width and height
#' 
#' @param fname Filename to which export should occur. If the file doesn't yet
#'   exist, it is created.
#' @param ggname Name of the gg object to export.
#' @author Mike Gerhardt
#' @export

export_graph <- function(fname, ggname, width  = 600, height = 360){
  png(filename = fname, width = width, height = height)
  print(ggname)
  dev.off()
}