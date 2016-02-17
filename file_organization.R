#' use this to delete ouput files from dropbox directories
#' 

library(magrittr)

#' use this to delete all output files in a directory
#' setwd("C:/Dropbox2/Dropbox/SpinUp")
#' setwd("C:/Atlantis/SpinUp_53yr_runs")
#' list.files(getwd(), pattern="NGulfOut*.*", full.names=TRUE, recursive=TRUE) %>% file.remove
#' 
#' use this to open all at_harvest files from a directory

