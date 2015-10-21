import_all_cyclic_voltammetry <- function(filepath, identifier = ".DTA", ...){
  home_directory <- getwd()
  setwd(filepath)
  cv_filenames <- list.files(pattern = identifier)
  cv_data <- lapply(cv_filenames, import_cv)
  return(cv_data)
}