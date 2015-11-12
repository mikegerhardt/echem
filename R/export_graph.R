export_graph <- function(fname, ggname, width  = 600, height = 360){
  png(filename = fname, width = width, height = height)
  print(ggname)
  dev.off()
}