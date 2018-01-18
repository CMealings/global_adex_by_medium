reqPackages <- function(packages) {
  
  ## Install and load necessary packages
  ## Version 1.1
  
  for (pkg in packages) {
    if(!pkg %in% installed.packages()[, 1])
      install.packages(pkg)
  }
  
  lapply(packages, require, character.only = TRUE)
}