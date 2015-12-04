import_all_cell_cv <- function(filepath = ".", fileid = "mrg_CV", ...){
  if(!substr(filepath, nchar(filepath), nchar(filepath)) %in% c("\\", "/")){
    # This if statement checks to see if filepath ends with "\\" or "/"
    # since I often forget to add those
    filepath <- paste(filepath, "\\", sep = "")
  }
  files_to_import <- list.files(path = filepath, pattern = fileid)
  
  all_cell_cv <- ldply(files_to_import, function(x){
    cbind(import_cell_cv(x), soc = x)
  }, ...)