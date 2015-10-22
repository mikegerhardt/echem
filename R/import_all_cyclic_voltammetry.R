import_all_cyclic_voltammetry <- function(filepath, identifier = ".DTA", ...){

  if(!substr(filepath, nchar(filepath), nchar(filepath)) %in% c("\\", "/")){
    # This if statement checks to see if filepath ends with "\\" or "/"
    # since I often forget to add those
    filepath <- paste(filepath, "\\", sep = "")
  }
  
  cv_filenames <- list.files(pattern = identifier,
                             path = filepath)
  cv_filenames_full <- lapply(cv_filenames, function(x) paste(filepath, x, sep = ""))
  cv_data <- lapply(cv_filenames_full, import_cv)
  return(cv_data)
}