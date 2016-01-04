
#read MortPerPredSpec
#hmorzarialuna@gmail.com

library(ncdf)
library(reshape)
#library(sm)  # for pause()
library(grDevices)
library(gridExtra)
library(RColorBrewer)

rm(list=ls())


#-------------
# USER CAN DEFINE INPUT FILES, STARTYEAR, TIMESTEP, SAVE DIRECTORY, ETC. HERE:  
#Here are some things that are hardcoded, but change here if you want. 
#


#pathfiles="E:/AtlantisV2/Final_30yr_NoF/AtlantisGOC_30yr_NoF_V2_NEWFlagOldDiet/"
#DateRun ="July_28"
#folders = c("Jul_28_FIXPREY_V2_NoF_Starve")

pathfiles="C:/Atlantis/Dropbox/Spin_up_runs_2015/outputs_Nov_30_2015/"
DateRun ="PRM_56"
folders = c("OUT_AtlantisGOC_53yr_SpinUp_10")

this.species = c('FVD') # SSK','^FVS','^FPL indicate which species to subset, the "^"eliminates strings not at the beginning

my.directory = "C:/Atlantis/Dropbox/1Archivos/NOAA/Atlantis model files/Atlantis_NEW_code/Plot code"

for (eachfolder in 1:length(folders)) {
filepath = paste(pathfiles,folders[eachfolder],sep="")

setwd(filepath)

This.mort = read.table("NGulfOutSpecificPredMort.txt",header=T) #nrows = 2 use this to only read certain rows

for (eachspecies in 1:length(this.species)){

this.species.mort= This.mort[, grep(this.species[eachspecies], names(This.mort))] #subsets all columns with SSK


max.mort = apply(this.species.mort, 2, max) 

print(paste(this.species[eachspecies],"mortality", sep ="_"))
print(max.mort)

print("Maximum predation")
max.col = which(max.mort == max(max.mort), arr.ind = TRUE)
print(names(max.col))
print(max(max.mort))



setwd(my.directory)

write.csv(this.species.mort, file=paste(this.species[eachspecies],folders[eachfolder],"MortSpec.csv",sep="_"))


  }
    }
