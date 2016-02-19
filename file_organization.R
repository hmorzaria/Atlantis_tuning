#' use this to delete ouput files from dropbox directories
#' 

library(magrittr)
pathfiles="E:/Atlantis/SpinUp_53yrs_V2/outputs_SpinUp_Feb18_2016/"

#use this code to open all log.txt files
setwd(pathfiles)
location.folders <- list.dirs(full.names = TRUE,recursive=TRUE)
indx.folders = grep('OUT', location.folders)
input.folder = location.folders[indx.folders]

for (eachfolder in 1:length(input.folder)){
#change path
  input.folder[eachfolder] %>% strsplit(.,"[.]")%>% unlist %>% 
    .[2] %>% paste(pathfiles,.,sep="") %>% setwd(.)
 #list input files
  input.files = list.files(getwd(),  pattern="*log.txt$", full.names=TRUE) 
  #call notepad
  shell(paste("start", "notepad++",input.files))
 }

# use this to delete all output files in a directory
# setwd("C:/Dropbox2/Dropbox/SpinUp")
# setwd("C:/Atlantis/SpinUp_53yr_runs")
# list.files(getwd(), pattern="NGulfOut*.*", full.names=TRUE, recursive=TRUE) %>% file.remove
# 
# use this to open all at_harvest files from a directory
# 
