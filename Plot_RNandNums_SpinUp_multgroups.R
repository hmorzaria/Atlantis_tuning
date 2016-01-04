#PlotOutNC_NandRN


#   This plots Biomass (all groups) and reserve N and Nums (all Vertebrates) over time. 
#   See/check/change the user defined block of parameters below. 
#   It expects three things in the working directory: 
#    FuncGroupNamesInPlotOrder.csv  , with four columns: 1)NetCDFName (like "Planktiv_S_Fish_N"); 2) CODE, like "FVO"; and 3) EMOCCName, like "Migrating Bird", and 4) Virgin Biomass/ Current Biomass, like 10. (No headers; Put "NA" if you want some subplots to be blank)
#    VertGroupNamesInPlotOrder.csv , with 2 columns: 1) NetCDFVertName, like "Planktiv_S_Fish1_ResN",and 2) EMOCCVertName, like "Small Planktivore" (No headers)
#    VertGroupNamesInPlotOrderNums.csv , with 2 columns: 1) NetCDFVertName, like "Planktiv_S_Fish1_Nums",and 2) EMOCCVertName, like "Small Planktivore" (No headers)
 #   AssessOrSurveyData.csv, with 1st column as year, columns 2-max are biomass of each functional group in each year. NAs are ok. 



#ToDO:This should really use matplot() to make plots faster
# Isaac Kaplan isaac.kaplan@noaa.gov 
#---------

X11()
library(ncdf)
library(reshape)
library(grDevices)
library(gridExtra)
library(RColorBrewer)
library(plyr)
library(ggplot2)
library(gdata)

rm(list=ls())

#-------------
# USER CAN DEFINE INPUT FILES, STARTYEAR, TIMESTEP, SAVE DIRECTORY, ETC. HERE:  
#Here are some things that are hardcoded, but change here if you want. 
#

X11()
pathfiles="E:/AtlantisV2/SpinUp_53yrs_V2/outputs_SpinUp_Sept15_2015/"
setwd("E:/AtlantisV2/SpinUp_53yrs_V2/")


DateRun ="Final"

#This is just to keep track of the runs, need to paste biom_indx in the folder with the run name
folders = c("OUT_AtlantisGOC_53yr_SpinUp_1","OUT_AtlantisGOC_53yr_SpinUp_2","OUT_AtlantisGOC_53yr_SpinUp_3","OUT_AtlantisGOC_53yr_SpinUp_4","OUT_AtlantisGOC_53yr_SpinUp_5","OUT_AtlantisGOC_53yr_SpinUp_6","OUT_AtlantisGOC_53yr_SpinUp_7","OUT_AtlantisGOC_53yr_SpinUp_8","OUT_AtlantisGOC_53yr_SpinUp_9","OUT_AtlantisGOC_53yr_SpinUp_10")
#folders = c("OUT_AtlantisGOC_53yr_SpinUp_7","OUT_AtlantisGOC_53yr_SpinUp_8","OUT_AtlantisGOC_53yr_SpinUp_9","OUT_AtlantisGOC_53yr_SpinUp_10")
#folders = c("OUT_AtlantisGOC_53yr_SpinUp_10", "OUT_AtlantisGOC_53yr_SpinUp_8")#,"OUT_AtlantisGOC_53yr_SpinUp_10")

AtlantisBoxesToUse = 0:65 # Isaac added this Feb 2014. You can put box 0 here. 
#AtlantisBoxesToUse<- c(3,9,39)
#AtlantisBoxesToUse<- c(11,29,38)
boxesToUse<- AtlantisBoxesToUse +1 # Isaac added this Feb 2014.  Box 0 becomes box 1, Box 1 becomes box 2, because Indexing in R starts with 1, not 0.
year0<-1955   # or whatever is appropriate. 
fishingStartYear<-1955 #All this does is draw a red vertical line on the plots. 
#numStepsPerYear<-1  #Number of output intervals per year. With output intervals of 122 days, this is = 3. 
wantSubtitle<-FALSE  # Set this to true, to print graph subtitles, allows you to make sure the NetCDF name matches the Common Name used in the title. 
plotRowsPerPage<-4   #number of rows of subplots you want per page
plotColsPerPage<-3   # number of columns of subplots you want per page
plotsPerPage<-plotRowsPerPage*plotColsPerPage
setwd("E:/Archivos/1Archivos/NOAA/Atlantis model files/Atlantis_NEW_code/Plot code") # set working directory, place with  CSV file (see below)
FuncGroupNamesInPlotOrder<-read.table("FuncGroupNamesInPlotOrder.csv",as.is = TRUE,header=TRUE,sep=",") # For the Biomass vs. Time plots here. Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 


#AssessOrSurveyData<-read.table("AssessOrSurveyData1985.csv",as.is = TRUE,header=TRUE,sep=",") # Assessment or survey data. Column 1 is year, other cols are MT biomass per year
#print('**** AssessOrSurveyData1985.csv must have functional groups in same order...')
#print('    ...as FuncGroupNamesInPlotOrder.csv (including NAs)***** ')
VertNamesInPlotOrder<-read.table("VertNamesInPlotOrder.csv",as.is = TRUE,header=FALSE,col.names=c("NetCDFVertName","Name"),sep=",") # For the rN vs. time plots below. It should have # rows=10*numVertebrates (one per age class). Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 
VertNamesInPlotOrder2<-read.table("VertNamesInPlotOrder2.csv",as.is = TRUE,header=FALSE,col.names=c("NetCDFVertName","Name"),sep=",") # For the rN vs. time plots below. It should have # rows=10*numVertebrates (one per age class). Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 
VertNamesInPlotOrderNums<-read.table("VertNamesInPlotOrderNums.csv",as.is = TRUE,header=FALSE,col.names=c("NetCDFVertName","Name"),sep=",") # For the Nums vs. time plots below. It should have # rows=10*numVertebrates (one per age class). Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 



# END OF DEFINITION BLOCK. USER CAN PROBABLY IGNORE EVERYTHING BELOW
#-----------

#-------------------------------------------------------
#PLOT RESERVE NITROGEN OVER TIME: 


print('rN over time')
rowsCols<-dim(VertNamesInPlotOrder)
numCohortsAllSpp<-rowsCols[1]
plotNum<-0


result.matrix = as.data.frame(matrix(0,nrow=0,ncol=5))

       for(eachfolder in 1:length(folders)) {
  
         rNPlotpath<-paste(pathfiles,folders[eachfolder],sep="")
         setwd(rNPlotpath)
         print('**** OPENING THE NETCDF FILE CAN TAKE 5 MINUTES ***** ')
         print('**** YOU CAN TRY USING save() and load() TO AVOID THIS IN THE FUTURE***** ')
         ThisNC.nc<-open.ncdf("NGulfOut.nc")
        # save(ThisNC.nc,file="GOC.Rdata")
         print.ncdf(ThisNC.nc)
         
         
         volumeData <- get.var.ncdf( ThisNC.nc,"volume") # extract the data from the variable. The variable contains lots of other metainfo like units, name, etc.
         volDims<-dim(volumeData)  # Just use volume to see how many time steps are in the data
         volBottomCells<-volumeData[volDims[1],,] # Because dz (height) of lowest boxes is 1m,I can say that Area is equal to the volume of the lowest boxes 
         dz <- get.var.ncdf( ThisNC.nc,"dz") 
         numDepths<-dim(dz)[1]
         zBottomCell<- dz[numDepths,1,1]
         areaData<-volBottomCells/zBottomCell
 
         
         
         for (funcGroup in 1:(numCohortsAllSpp/10) )     # loop over all functional groups (there are 10 age classes per functinal group)
         {
           maxYForPlot<-1.1
           for (ageclass in 1:10) {# loop over all 10 age classes
           
     #thisVar <- ThisNC.nc$var[[ SppRnIndices[(funcGroup-1)*10+ageclass] ]]  # Extract variable
     thisData <- get.var.ncdf( ThisNC.nc,VertNamesInPlotOrder$NetCDFVertName[((funcGroup-1)*10+ageclass)])  # Extract data from variable
     thisData[thisData==0]<-NA  # Replace 0's with NA
     thisDataMeanMg<-apply(thisData,3,mean,na.rm = TRUE)#Get mean size over time, averaging over depth and location 
     thisY<-thisDataMeanMg/thisDataMeanMg[1]  # Normalize by initial value
    speciesName<-VertNamesInPlotOrder$Name[funcGroup*10]
     # Calculate max Y value needed to show in plot
    age.frame = as.data.frame(thisY)
    frame.rows = 1 - nrow(age.frame)
     age.frame$species = speciesName
     age.frame$age.class = ageclass
     age.frame$Time = 0:frame.rows
     age.frame$run = folders[eachfolder]
 result.matrix=rbind(age.frame,result.matrix)
           
  }
    }
         
       }
#-----------------
result.matrix$species = as.factor(result.matrix$species)
result.matrix$run = as.factor(result.matrix$run)
result.matrix$age.class = as.factor(result.matrix$age.class)
result.matrix$Time = as.numeric(result.matrix$Time)


plot_list = list()

setwd(pathfiles)

for(eachgroup in 1:length(levels(result.matrix$species)))
  {

  print(eachgroup)
  
  species = levels(result.matrix$species)
  this.group = as.character(species[eachgroup])
  this.species= subset(result.matrix, result.matrix$species %in% c(this.group))
  this.species = drop.levels(this.species)

  maxYForPlot<-1.1
  maxYObserved<-max(this.species$thisY,na.rm=TRUE)
  maxYForPlot<-max(c(maxYForPlot,1.1*maxYObserved),na.rm=TRUE)
  
  #maxRN= rep(1.2,31)
  #maxRN= rep(1.5,31)
  #maxRN= rep(0.8,31)
  #maxRN= rep(0.5,31)
  
#  lines(x=c(0,max(simYears)),y=c(1.2,1.2),lty='solid',lwd=0.75,col='black')
#  lines(x=c(0,max(simYears)),y=c(0.8,0.8),lty='solid',lwd=0.75,col='black')
#  lines(x=c(0,max(simYears)),y=c(1.5,1.5),lty='dashed',lwd=1,col='black')
#  lines(x=c(0,max(simYears)),y=c(0.5,0.5),lty='dashed',lwd=1,col='black')
  
  
 p <- ggplot(this.species, aes(x = Time, y = thisY, group=interaction(run,age.class),colour=age.class,linetype=age.class)) 
  geom.p <- p + geom_line(size=0.65)
  sp.graph <- geom.p + labs(x= ("Time"), y =("rN / initial rN")) + ylim(0,maxYForPlot) +
    ggtitle(this.group) +
    geom_hline(yintercept=1.2,linetype='dashed') +
  geom_hline(yintercept=0.8,linetype='dashed') +
  geom_hline(yintercept=1.5,linetype='solid') +
  geom_hline(yintercept=0.5,linetype='solid')
  
  graph1 <-  sp.graph + theme(plot.title = element_text(size = 9), panel.grid.minor = element_line(colour = NA), panel.background = element_rect(fill = NA, colour = NA),
                              axis.line=element_line(colour = "black"), axis.text.x = element_text(vjust = 1, size = 8, colour = "black"), axis.text.y = element_text(hjust=1, size=8, colour = "black")) +
    theme(axis.text.y = element_text(angle=90)) + #, axis.text.x = theme_blank()) + 
    theme(legend.position = "none")

  plot_list = c(plot_list, list(graph1))
  
  if (eachgroup==12 | eachgroup==24 | eachgroup==35) 
    { ## print 12 plots on a page
    
    pdf(paste("RNratio_prms_AtlantisV2_",eachgroup,".pdf",sep=""), width = 9.5, height = 11)
    
    print(do.call(grid.arrange, c(plot_list, list(ncol=plotColsPerPage,nrow=plotRowsPerPage))))
    
    plot_list = list() # reset plot 
    
    dev.off()
    
  }
  
    
} 
  
if (length(plot_list) != 0) { 
  print(do.call(grid.arrange, c(plot_list, list(ncol=plotColsPerPage,nrow=plotRowsPerPage))))
  
}
dev.off()

# # #-------------------------------------------------------

print('Numbers')
rowsCols<-dim(VertNamesInPlotOrder)
numCohortsAllSpp<-rowsCols[1]
plotNum<-0


result.matrix = as.data.frame(matrix(0,nrow=0,ncol=5))

for(eachfolder in 1:length(folders)) {
  
  rNPlotpath<-paste(pathfiles,folders[eachfolder],sep="")
  setwd(rNPlotpath)
  print('**** OPENING THE NETCDF FILE CAN TAKE 5 MINUTES ***** ')
  print('**** YOU CAN TRY USING save() and load() TO AVOID THIS IN THE FUTURE***** ')
  ThisNC.nc<-open.ncdf("NGulfOut.nc")
  # save(ThisNC.nc,file="GOC.Rdata")
  print.ncdf(ThisNC.nc)
  
  
  volumeData <- get.var.ncdf( ThisNC.nc,"volume") # extract the data from the variable. The variable contains lots of other metainfo like units, name, etc.
  volDims<-dim(volumeData)  # Just use volume to see how many time steps are in the data
  volBottomCells<-volumeData[volDims[1],,] # Because dz (height) of lowest boxes is 1m,I can say that Area is equal to the volume of the lowest boxes 
  dz <- get.var.ncdf( ThisNC.nc,"dz") 
  numDepths<-dim(dz)[1]
  zBottomCell<- dz[numDepths,1,1]
  areaData<-volBottomCells/zBottomCell
  
  
  
  for (funcGroup in 1:(numCohortsAllSpp/10) )     # loop over all functional groups (there are 10 age classes per functinal group)
  {
    maxYForPlot<-1.1
    for (ageclass in 1:10) {# loop over all 10 age classes
      
      #thisVar <- ThisNC.nc$var[[ SppRnIndices[(funcGroup-1)*10+ageclass] ]]  # Extract variable
      thisData <- get.var.ncdf( ThisNC.nc,VertNamesInPlotOrderNums$NetCDFVertName[((funcGroup-1)*10+ageclass)])  # Extract data from variable
      thisData[thisData==0]<-NA  # Replace 0's with NA
      thisDataNums<-apply(thisData,3,sum,na.rm = TRUE) #Get nums over time, summing over depth and location  
      thisY<-thisDataNums 
      speciesName<-VertNamesInPlotOrder$Name[funcGroup*10]
      # Calculate max Y value needed to show in plot
      age.frame = as.data.frame(thisY)
      frame.rows = 1 - nrow(age.frame)
      age.frame$species = speciesName
      age.frame$age.class = ageclass
      age.frame$Time = 0:frame.rows
      age.frame$run = folders[eachfolder]
      result.matrix=rbind(age.frame,result.matrix)
      
    }
  }
  
}
#-----------------
result.matrix$species = as.factor(result.matrix$species)
result.matrix$run = as.factor(result.matrix$run)
result.matrix$age.class = as.factor(result.matrix$age.class)
result.matrix$Time = as.factor(result.matrix$Time)


plot_list = list()

setwd(pathfiles)
for(eachgroup in 1:length(levels(result.matrix$species)))
{
  
  
  species = levels(result.matrix$species)
  this.group = as.character(species[eachgroup])
  this.species= subset(result.matrix, result.matrix$species %in% c(this.group))
  this.species = drop.levels(this.species)
  
  maxYForPlot<-1.1
  maxYObserved<-max(this.species$thisY,na.rm=TRUE)
  maxYForPlot<-max(c(maxYForPlot,1.1*maxYObserved),na.rm=TRUE)
  
  #maxRN= rep(1.2,31)
  #maxRN= rep(1.5,31)
  #maxRN= rep(0.8,31)
  #maxRN= rep(0.5,31)
  
  #  lines(x=c(0,max(simYears)),y=c(1.2,1.2),lty='solid',lwd=0.75,col='black')
  #  lines(x=c(0,max(simYears)),y=c(0.8,0.8),lty='solid',lwd=0.75,col='black')
  #  lines(x=c(0,max(simYears)),y=c(1.5,1.5),lty='dashed',lwd=1,col='black')
  #  lines(x=c(0,max(simYears)),y=c(0.5,0.5),lty='dashed',lwd=1,col='black')
  
  
  p <- ggplot(this.species, aes(x = Time, y = thisY, group=interaction(run,age.class),colour=age.class,linetype=age.class)) 
  geom.p <- p + geom_line(size=0.65)
  sp.graph <- geom.p + labs(x= ("Time"), y =("Numbers")) + ylim(0,maxYForPlot) +
    ggtitle(this.group) +
    geom_hline(yintercept=1.2,linetype='dashed') +
    geom_hline(yintercept=0.8,linetype='dashed') +
    geom_hline(yintercept=1.5,linetype='solid') +
    geom_hline(yintercept=0.5,linetype='solid')
  
  graph1 <-  sp.graph + theme(plot.title = element_text(size = 9), panel.grid.minor = element_line(colour = NA), panel.background = element_rect(fill = NA, colour = NA),
                              axis.line=element_line(colour = "black"), axis.text.x = element_text(vjust = 1, size = 8, colour = "black"), axis.text.y = element_text(hjust=1, size=8, colour = "black")) +
    theme(axis.text.y = element_text(angle=90)) + #, axis.text.x = theme_blank()) + 
    theme(legend.position = "none")
  
  plot_list = c(plot_list, list(graph1))
  
  if (eachgroup==12 | eachgroup==24 | eachgroup==35) 
  { ## print 12 plots on a page
    
    pdf(paste("Numbers_prms_AtlantisV2_",eachgroup,".pdf",sep=), width = 9.5, height = 11)
    
    print(do.call(grid.arrange, c(plot_list, list(ncol=plotColsPerPage,nrow=plotRowsPerPage))))
    
    plot_list = list() # reset plot 
    
    dev.off()
    
  }
  
  
} 


if (length(plot_list) != 0) { 
  print(do.call(grid.arrange, c(plot_list, list(ncol=plotColsPerPage,nrow=plotRowsPerPage))))
  
}
dev.off()


#-------
#PLOT WET WEIGHT PER INDIVIDUAL, SIZE-AT-AGE OVER TIME: (APPROXIMATE AND BASED ON Rn RESERVE nITROGEN ONLY): 

print('Weight over time')
rowsCols<-dim(VertNamesInPlotOrder)
numCohortsAllSpp<-rowsCols[1]
plotNum<-0



result.matrix = as.data.frame(matrix(0,nrow=0,ncol=5))

for(eachfolder in 1:length(folders)) {
  
  rNPlotpath<-paste(pathfiles,folders[eachfolder],sep="")
  setwd(rNPlotpath)
  print('**** OPENING THE NETCDF FILE CAN TAKE 5 MINUTES ***** ')
  print('**** YOU CAN TRY USING save() and load() TO AVOID THIS IN THE FUTURE***** ')
  ThisNC.nc<-open.ncdf("NGulfOut.nc")
  # save(ThisNC.nc,file="GOC.Rdata")
  print.ncdf(ThisNC.nc)
  
  
  volumeData <- get.var.ncdf( ThisNC.nc,"volume") # extract the data from the variable. The variable contains lots of other metainfo like units, name, etc.
  volDims<-dim(volumeData)  # Just use volume to see how many time steps are in the data
  volBottomCells<-volumeData[volDims[1],,] # Because dz (height) of lowest boxes is 1m,I can say that Area is equal to the volume of the lowest boxes 
  dz <- get.var.ncdf( ThisNC.nc,"dz") 
  numDepths<-dim(dz)[1]
  zBottomCell<- dz[numDepths,1,1]
  areaData<-volBottomCells/zBottomCell
  
  
  
  for (funcGroup in 1:(numCohortsAllSpp/10) )     # loop over all functional groups (there are 10 age classes per functinal group)
  {

    for (ageclass in 1:10) {# loop over all 10 age classes
      
      thisData <- get.var.ncdf( ThisNC.nc,paste(VertNamesInPlotOrder$NetCDFVertName[((funcGroup-1)*10+ageclass)],sep=""))  # Extract data from variable
      thisData<-thisData*20*5.7*(3.65/2.65)/1000000
      thisData[thisData==0]<-NA  # Replace 0's with NA
      thisDataMeanWt<-apply(thisData,3,mean,na.rm = TRUE)#Get mean size over time, averaging over depth and location 
      thisY<-thisDataMeanWt   #/thisDataMeanMg[1]  # Normalize by initial value
      #maxYForPlot<-max(c(maxYForPlot,1.1*max(thisY))) # Calculate max Y value needed to show in plot
      speciesName<-VertNamesInPlotOrder$Name[funcGroup*10]
      
      # Calculate max Y value needed to show in plot
      age.frame = as.data.frame(thisY)
      frame.rows = 1 - nrow(age.frame)
      age.frame$species = speciesName
      age.frame$age.class = ageclass
      age.frame$Time = 0:frame.rows
      age.frame$run = folders[eachfolder]
      result.matrix=rbind(age.frame,result.matrix)
      
    }
  }
  
}
#-----------------
result.matrix$species = as.factor(result.matrix$species)
result.matrix$run = as.factor(result.matrix$run)
result.matrix$age.class = as.factor(result.matrix$age.class)
result.matrix$Time = as.numeric(result.matrix$Time)


plot_list = list()

setwd(pathfiles)

for(eachgroup in 1:length(levels(result.matrix$species)))
{
  
  print(eachgroup)
  
  species = levels(result.matrix$species)
  this.group = as.character(species[eachgroup])
  this.species= subset(result.matrix, result.matrix$species %in% c(this.group))
  this.species = drop.levels(this.species)
  
  #maxYForPlot<-1.1

  
  #maxRN= rep(1.2,31)
  #maxRN= rep(1.5,31)
  #maxRN= rep(0.8,31)
  #maxRN= rep(0.5,31)
  
  #  lines(x=c(0,max(simYears)),y=c(1.2,1.2),lty='solid',lwd=0.75,col='black')
  #  lines(x=c(0,max(simYears)),y=c(0.8,0.8),lty='solid',lwd=0.75,col='black')
  #  lines(x=c(0,max(simYears)),y=c(1.5,1.5),lty='dashed',lwd=1,col='black')
  #  lines(x=c(0,max(simYears)),y=c(0.5,0.5),lty='dashed',lwd=1,col='black')
  
  
  p <- ggplot(this.species, aes(x = Time, y = thisY, group=interaction(run,age.class),colour=age.class,linetype=age.class)) 
  geom.p <- p + geom_line(size=0.65)
  sp.graph <- geom.p + labs(x= ("Time"), y =("Wet Weight per Individual (kg)")) + 
    ggtitle(this.group) 
    
  
  graph1 <-  sp.graph + theme(plot.title = element_text(size = 9), panel.grid.minor = element_line(colour = NA), panel.background = element_rect(fill = NA, colour = NA),
                              axis.line=element_line(colour = "black"), axis.text.x = element_text(vjust = 1, size = 8, colour = "black"), axis.text.y = element_text(hjust=1, size=8, colour = "black")) +
    theme(axis.text.y = element_text(angle=90)) + #, axis.text.x = theme_blank()) + 
    theme(legend.position = "none")
  
  plot_list = c(plot_list, list(graph1))
  
  if (eachgroup==12 | eachgroup==24 | eachgroup==35) 
  { ## print 12 plots on a page
    
    pdf(paste("WetWeight_prms_AtlantisV2_",eachgroup,".pdf",sep=""), width = 9.5, height = 11)
    
    print(do.call(grid.arrange, c(plot_list, list(ncol=plotColsPerPage,nrow=plotRowsPerPage))))
    
    plot_list = list() # reset plot 
    
    dev.off()
    
  }
  
  
} 

if (length(plot_list) != 0) { 
  print(do.call(grid.arrange, c(plot_list, list(ncol=plotColsPerPage,nrow=plotRowsPerPage))))
  
}
dev.off()