# =======================  Script Information  =================================
#! Check if necessary packages are available
#    download, install, and attach required packages if necessary
#===============================================================================
library(utils)
necessary = c("dplyr", "CosmosToR", "SDMTools")
if(!all(loaded<-is.element(paste("package:", necessary, sep=""), search()))) {
  installed = necessary %in% installed.packages()[, 'Package']
  if (length(necessary[!installed]) >= 1)
    install.packages(necessary[!installed], repos = "http://cran.rstudio.com/", dep = T)
  #invisible(rapply(as.list(necessary[!loaded]), library))
}