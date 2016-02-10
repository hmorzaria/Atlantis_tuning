#' use this to delete ouput files from dropbox directories
#' 

library(magrittr)
setwd("C:/Dropbox2/Dropbox/Common_scenarios")

setwd("C:/Atlantis/SpinUp_53yr_runs")


list.files(getwd(), pattern="NGulfOut*.*", full.names=TRUE, recursive=TRUE) %>% file.remove

