export_graph <- function(fname, ggname){
  png(filename = fname, width = 600, height = 480)
  print(ggname)
  dev.off()
}