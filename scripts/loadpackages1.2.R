reqPackages <- function(packages, shinyio = c(FALSE, TRUE)) {
  
  ## Install and load necessary packages
  ## Version 1.2
  
  if (missing(shinyio))
    shinyio <- FALSE
  
  if (!shinyio) {
    for (pkg in packages) {
      if(!pkg %in% installed.packages()[, 1])
        install.packages(pkg)
    }
    
    lapply(packages, require, character.only = TRUE)
  }
  
  ## shinyapps.io requires you to call the packages individually here, otherwise it won't find and install them...
  
  require(shiny)
  require(plotly)
  require(XLConnect)
  require(shinythemes)
  require(ggplot2)
  require(reshape2)
  require(abind)
  require(forcats)
  require(showtext)
  require(webshot)
  
}