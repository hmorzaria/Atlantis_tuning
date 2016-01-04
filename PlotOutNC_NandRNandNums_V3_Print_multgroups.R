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
#Modified July 2014 Hem Morzaria hmorzarialuna@gmail.com
#---------

library(ncdf)
library(reshape)
library(grDevices)
library(gridExtra)
library(RColorBrewer)
library(plyr)
library(ggplot2)

rm(list=ls())

#-------------
# USER CAN DEFINE INPUT FILES, STARTYEAR, TIMESTEP, SAVE DIRECTORY, ETC. HERE:  
#Here are some things that are hardcoded, but change here if you want. 
#

pathfiles="E:/AtlantisV2/Final_30yr_NoF/FinalPRMS/"
#E:\AtlantisV2\Final_30yr_NoF\FinalPRMS\Aug31_PRM37
DateRun ="Aug31_2014"
folders = c("Aug27_PRM28","Aug31_PRM37")#, "OUT_AtlantisGOC_30yr_NoF - 2","OUT_AtlantisGOC_30yr_NoF - 3","OUT_AtlantisGOC_30yr_NoF - 4","OUT_AtlantisGOC_30yr_NoF - 5","OUT_AtlantisGOC_30yr_NoF - 6","OUT_AtlantisGOC_30yr_NoF - 7","OUT_AtlantisGOC_30yr_NoF - 8")#,"Jul_26_FIXPREY_V2_NoF_Starve_3")


AtlantisBoxesToUse = 0:65 # Isaac added this Feb 2014. You can put box 0 here. 
#AtlantisBoxesToUse<- c(3,9,39)
#AtlantisBoxesToUse<- c(11,29,38)
boxesToUse<- AtlantisBoxesToUse +1 # Isaac added this Feb 2014.  Box 0 becomes box 1, Box 1 becomes box 2, because Indexing in R starts with 1, not 0.
year0<-2008   # or whatever is appropriate. 
fishingStartYear<-2008 #All this does is draw a red vertical line on the plots. 
numStepsPerYear<-1  #Number of output intervals per year. With output intervals of 122 days, this is = 3. 
numTimeSteps<-31
wantSubtitle<-FALSE  # Set this to true, to print graph subtitles, allows you to make sure the NetCDF name matches the Common Name used in the title. 
plotRowsPerPage<-4   #number of rows of subplots you want per page
plotColsPerPage<-3   # number of columns of subplots you want per page
plotsPerPage<-plotRowsPerPage*plotColsPerPage
setwd("E:/Archivos/1Archivos/NOAA/Atlantis model files/Atlantis_NEW_code/Plot code") # set working directory, place with  CSV file (see below)
FuncGroupNamesInPlotOrder<-read.table("FuncGroupNamesInPlotOrder.csv",as.is = TRUE,header=TRUE,sep=",") # For the Biomass vs. Time plots here. Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 

simYears = 53

#AssessOrSurveyData<-read.table("AssessOrSurveyData1985.csv",as.is = TRUE,header=TRUE,sep=",") # Assessment or survey data. Column 1 is year, other cols are MT biomass per year
#print('**** AssessOrSurveyData1985.csv must have functional groups in same order...')
#print('    ...as FuncGroupNamesInPlotOrder.csv (including NAs)***** ')
VertNamesInPlotOrder<-read.table("VertNamesInPlotOrder.csv",as.is = TRUE,header=FALSE,col.names=c("NetCDFVertName","Name"),sep=",") # For the rN vs. time plots below. It should have # rows=10*numVertebrates (one per age class). Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 
VertNamesInPlotOrder2<-read.table("VertNamesInPlotOrder2.csv",as.is = TRUE,header=FALSE,col.names=c("NetCDFVertName","Name"),sep=",") # For the rN vs. time plots below. It should have # rows=10*numVertebrates (one per age class). Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 
VertNamesInPlotOrderNums<-read.table("VertNamesInPlotOrderNums.csv",as.is = TRUE,header=FALSE,col.names=c("NetCDFVertName","Name"),sep=",") # For the Nums vs. time plots below. It should have # rows=10*numVertebrates (one per age class). Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 



# END OF DEFINITION BLOCK. USER CAN PROBABLY IGNORE EVERYTHING BELOW
#-----------



setwd("E:/AtlantisV2/Final_30yr_NoF/FinalPRMS/")

 csvdat <- list.files(pattern="NGulf*")
 lm.res <- list()
 
 BiomassN = as.data.frame(matrix(0,nrow=0,ncol=4))
 
 for (i in seq(along=csvdat)){
   
   Biom  = read.table(csvdat[i],header=T)
   
   Biomass = melt(Biom,id.vars = "Time")
   names(Biomass) = c("Time","Group","Biomass")
   # these are groups ordered by TL
   tl.groups = c("BB","DIN","DL","DR","DC","MA","MB","PB","PL","PS","SG","BC","BFF","BG","BML","BMS","ZS","FDS","BMD","CEP","BD","ZL","BFD","BO","PWN","ZG","SSK","ZM","WHB","FMN","REP","FVO","BFS","FBP","FDB","FDC","FDD","FDE","SP","FVT","FVD","SB","FVV","FDM","FPO","FDF","FMM","FDP","FPL","SHP","SHR","FDO","PIN","SHD","WDG","FPS","SHC","FVB","FVS","SHB","WHT","WHS")
   Biomass$Group <- ordered(Biomass$Group, levels = c("BB","DIN","DL","DR","DC","MA","MB","PB","PL","PS","SG","BC","BFF","BG","BML","BMS","ZS","FDS","BMD","CEP","BD","ZL","BFD","BO","PWN","ZG","SSK","ZM","WHB","FMN","REP","FVO","BFS","FBP","FDB","FDC","FDD","FDE","SP","FVT","FVD","SB","FVV","FDM","FPO","FDF","FMM","FDP","FPL","SHP","SHR","FDO","PIN","SHD","WDG","FPS","SHC","FVB","FVS","SHB","WHT","WHS"))
   
   Biomass$run = folders[i]
   
   BiomassN = rbind(BiomassN, Biomass)
 }
 
 

#-----------------------------------------------------
# MAKE PLOTS OF ABUNDANCE OVER TIME


rowscols<-dim(FuncGroupNamesInPlotOrder) # count how many species are in this data
numFuncGroups<-rowscols[1]
plotNum<-0
simYears<-(year0+(seq(1,numTimeSteps)/numStepsPerYear))  # Get some x values for the plot (time)
print(numTimeSteps)

plot_list = list()

pdf("Biomass_prms_AtlantisV2.pdf", width = 9.5, height = 11)

par(mfcol=c(plotRowsPerPage,plotColsPerPage))
par(mar=c(2.5,1,1.5,2))
par(omi=c(.4,.44,0,.4))

#LOOP OVER ALL FUNCTIONAL GROUPS
#for (funcGroup in 1:numFuncGroups )  
  for (funcGroup in 48:60 )  
{ 
  Groups.tl = levels(BiomassN$Group) 
  print(Groups.tl[funcGroup])
  
this.name = subset(FuncGroupNamesInPlotOrder, CODE%in% Groups.tl[funcGroup])  
  print(this.name$Name)

 group.name =  this.name$Name
  
  this.group = subset(BiomassN, Group%in% Groups.tl[funcGroup])
  
  this.group$run = as.factor(this.group$run)
  thisY = this.group$Biomass
  
  last.valueY = length(thisY)
    max.valueB = max(thisY)
  max.valueB = as.numeric(max.valueB) 
  first.valueB = thisY[1]
  
  X1985B =  this.name$X1985B
  unfishedB = this.name$unfishedB
  
  Bratio = max.valueB/X1985B
  Bratio = round(Bratio,6)

  testVec<-c(1.1*max(thisY),unfishedB,X1985B) # Calculate max Y value needed to show in plot       
  maxYForPlot<-1.1*(max(testVec[!is.na(testVec)]))
  yLabString<-'Biomass, metric tons' 
  
  p <- ggplot(this.group, aes(x = Time, y = Biomass, group=run,colour=run)) 
  geom.p <- p + geom_line(size=1)
  sp.graph <- geom.p + labs(x= ("Time"), y =(yLabString)) + ylim(0,maxYForPlot) +
    
    geom_hline(aes(yintercept=X1985B), colour="red", linetype="dashed",size=1) +
  geom_hline(aes(yintercept=unfishedB), colour="green", linetype="dashed",size=1) + ggtitle(this.name$Name)
  
  graph1 <-  sp.graph + theme(plot.title = element_text(size = 9), panel.grid.minor = element_line(colour = NA), panel.background = element_rect(fill = NA, colour = NA),
                               axis.line=element_line(colour = "black"), axis.text.x = element_text(vjust = 1, size = 8, colour = "black"), axis.text.y = element_text(hjust=1, size=8, colour = "black")) +
    theme(axis.text.y = element_text(angle=90)) + #, axis.text.x = theme_blank()) + 
    theme(legend.position = "none")
    
  plot_list = c(plot_list, list(graph1))

  if (funcGroup==12 | funcGroup==24 | funcGroup==36 | funcGroup==48 | funcGroup==60) { ## print 8 plots on a page
    
    print(do.call(grid.arrange, c(plot_list, list(ncol=plotColsPerPage,nrow=plotRowsPerPage))))
    
    plot_list = list() # reset plot 
    
  }


}
 
if (length(plot_list) != 0) { 
  print(do.call(grid.arrange, c(plot_list, list(ncol=plotColsPerPage,nrow=plotRowsPerPage))))
  
}
dev.off()


# #-------------------------------------------------------
# #PLOT RESERVE NITROGEN OVER TIME: 
# 
# pathToSavePlots="E:/AtlantisV2/Final_30yr_NoF/FinalPRMS/"
# 
# #postscript(file=rNPlotNameString, onefile=FALSE, horizontal=FALSE) #, height=8, width=8, pointsize=16).
# pdf("rNPlot_PRM.pdf", width = 9.5, height = 11)
# 
# par(mfcol=c(plotRowsPerPage,plotColsPerPage))
# par(mar=c(2.5,1,1.5,2))
# par(omi=c(.4,.44,0,.4))
# 
# for (i in 1:length(folders)){
#   
#   print('**** OPENING THE NETCDF FILE CAN TAKE 5 MINUTES ***** ')
#   print('**** YOU CAN TRY USING save() and load() TO AVOID THIS IN THE FUTURE***** ')
#   
#   filepath = paste(pathfiles,folders[i],sep="")
#   
#   setwd(filepath)
#   ThisNC.nc[[i]]<-open.ncdf("NGulfOut.nc")
#   save(ThisNC.nc,file="GOC.Rdata")
#   print.ncdf(ThisNC.nc)
#   
#   
#   paste("volumeData_",folders[i]) <- get.var.ncdf( ThisNC.nc,"volume") # extract the data from the variable. The variable contains lots of other metainfo like units, name, etc.
#   volDims<-dim(volumeData)  # Just use volume to see how many time steps are in the data
#   volBottomCells<-volumeData[volDims[1],,] # Because dz (height) of lowest boxes is 1m,I can say that Area is equal to the volume of the lowest boxes 
#   dz <- get.var.ncdf( ThisNC.nc,"dz") 
#   numDepths<-dim(dz)[1]
#   zBottomCell<- dz[numDepths,1,1]
#   areaData<-volBottomCells/zBottomCell
#   
# 
# print('rN over time')
# rowsCols<-dim(VertNamesInPlotOrder)
# numCohortsAllSpp<-rowsCols[1]
# plotNum<-0
# 
# 
# for (funcGroup in 1:(numCohortsAllSpp/10) )     # loop over all functional groups (there are 10 age classes per functinal group)
# {
#   
#   if (((funcGroup-1)%%plotsPerPage)==0)   # if we are on Functional Group number 1, 7, 13,19, etc. 
#   {  
#     plotNum<-plotNum+1
#     
#     par(mfrow=c(plotRowsPerPage,plotColsPerPage),mar=c(4,4,2,2)) # lay out the 3x2 blank plots
#   }
#   
# 
# 
# 
# #-----------------
#  # All this loop does is  find the max Y value for this rN  plot (10 age classes, one functional group)   
#      maxYForPlot<-1.1  # set up a y limit max, which will change below
#      for (ageclass in 1:10) # loop over all 10 age classes
#      {
#      #thisVar <- ThisNC.nc$var[[ SppRnIndices[(funcGroup-1)*10+ageclass] ]]  # Extract variable
#      thisData <- get.var.ncdf( ThisNC.nc,VertNamesInPlotOrder$NetCDFVertName[((funcGroup-1)*10+ageclass)])  # Extract data from variable
#      thisData[thisData==0]<-NA  # Replace 0's with NA
#      thisDataMeanMg<-apply(thisData,3,mean,na.rm = TRUE)#Get mean size over time, averaging over depth and location 
#      thisY<-thisDataMeanMg/thisDataMeanMg[1]  # Normalize by initial value
#      maxYObserved<-max(thisY,na.rm=TRUE)
#      maxYForPlot<-max(c(maxYForPlot,1.1*maxYObserved),na.rm=TRUE) # Calculate max Y value needed to show in plot
#     }
# 
# #-----------------
# 
#      for (ageclass in 1:10) # loop over all 10 age classes
#      {
#      #thisVar <- ThisNC.nc$var[[ SppRnIndices[(funcGroup-1)*10+ageclass] ]]  # Extract variable
#      thisData <- get.var.ncdf( ThisNC.nc,VertNamesInPlotOrder$NetCDFVertName[((funcGroup-1)*10+ageclass)])  # Extract data from variable
#      thisData[thisData==0]<-NA  # Replace 0's with NA
#      thisDataMeanMg<-apply(thisData,3,mean,na.rm = TRUE)#Get mean size over time, averaging over depth and location 
#      thisY<-thisDataMeanMg/thisDataMeanMg[1]  # Normalize by initial value
#      #maxYForPlot<-max(c(maxYForPlot,1.1*max(thisY))) # Calculate max Y value needed to show in plot
#      rainbowColors<-rainbow(10)    # Make a rainbow color palette for plotting. 
#      speciesName<-VertNamesInPlotOrder$Name[funcGroup*10]
#       
#      simYears<-(0+(seq(1,length(thisY))/numStepsPerYear)) 
#      
#      if (ageclass==1)  # If we are plotting age class 1, start a new plot
#          {
#             plot(simYears,thisY,cex.lab=1, cex.axis=1,lwd=1.5,type='l',ylab='rN / initial rN',main=speciesName,cex.main=1, xlab='Year',ylim=c(0,maxYForPlot),lty=ageclass,col = rainbowColors[ageclass])
#              print(speciesName)
#          }
#          else  # If we are doing age class 2 or higher, just add lines to the existing plot made for Age class 1. 
#          {
#            lines(simYears,thisY,lwd=1.5,type='l',lty=ageclass,col = rainbowColors[ageclass])
#              
#          }
#     }  
#      lines(x=c(0,max(simYears)),y=c(1.2,1.2),lty='solid',lwd=0.75,col='black')
#      lines(x=c(0,max(simYears)),y=c(0.8,0.8),lty='solid',lwd=0.75,col='black')
#     lines(x=c(0,max(simYears)),y=c(1.5,1.5),lty='dashed',lwd=1,col='black')
#     lines(x=c(0,max(simYears)),y=c(0.5,0.5),lty='dashed',lwd=1,col='black')
# 
#      if (wantSubtitle)
#      {
#      title(sub=VertNamesInPlotOrder$CODE[funcGroup*10])
#      }
# #    if (((funcGroup)%%plotsPerPage)==0)    
# #    {
# #         dev.off()  # close the device
# #    }
# 
#     
# }
# 
# dev.off()
# 
# 
# #-------------------------------------------------------
# #PLOT Numbers at age over timE: 
# 
# print('Nums over time')
# rowsCols<-dim(VertNamesInPlotOrder)
# numCohortsAllSpp<-rowsCols[1]
# plotNum<-0
# 
# 
# NumsPlotNameString<-paste(pathToSavePlots,"NumsPlot",folders[i],"_",DateRun,".pdf",sep="")
# print(NumsPlotNameString)
# #postscript(file=NumsPlotNameString, onefile=FALSE, horizontal=FALSE) #, height=8, width=8, pointsize=16).
# pdf(NumsPlotNameString)
# 
# for (funcGroup in 1:(numCohortsAllSpp/10) )     # loop over all functional groups (there are 10 age classes per functinal group)
# {
#   
#   if (((funcGroup-1)%%plotsPerPage)==0)   # if we are on Functional Group number 1, 7, 13,19, etc. 
#   {  
#     plotNum<-plotNum+1
#     par(mfrow=c(plotRowsPerPage,plotColsPerPage),mar=c(4,4,2,2)) # lay out the 3x2 blank plots
#   }     
# 
# 
# #-----------------
#  # All this loop does is  find the max Y value for this Nums  plot (10 age classes, one functional group)   
#      maxYForPlot<-1.1  # set up a y limit max, which will change below
#      for (ageclass in 1:10) # loop over all 10 age classes
#      {
#      #thisVar <- ThisNC.nc$var[[ SppRnIndices[(funcGroup-1)*10+ageclass] ]]  # Extract variable
#      thisData <- get.var.ncdf( ThisNC.nc,VertNamesInPlotOrderNums$NetCDFVertName[((funcGroup-1)*10+ageclass)])  # Extract data from variable
#      thisData[thisData==0]<-NA  # Replace 0's with NA
#      thisDataNums<-apply(thisData,3,sum,na.rm = TRUE)#Get nums over time, summing over depth and location 
#      thisY<-thisDataNums
#      maxYObserved<-max(thisY,na.rm=TRUE)
#      maxYForPlot<-max(c(maxYForPlot,1.1*maxYObserved),na.rm=TRUE) # Calculate max Y value needed to show in plot
#     }
# 
# #-----------------
# 
#      for (ageclass in 1:10) # loop over all 10 age classes
#      {
#      #thisVar <- ThisNC.nc$var[[ SppRnIndices[(funcGroup-1)*10+ageclass] ]]  # Extract variable
#      thisData <- get.var.ncdf( ThisNC.nc,VertNamesInPlotOrderNums$NetCDFVertName[((funcGroup-1)*10+ageclass)])  # Extract data from variable
#      thisData[thisData==0]<-NA  # Replace 0's with NA
#      thisDataNums<-apply(thisData,3,sum,na.rm = TRUE)#Get nums over time, summing over depth and location  
#      thisY<-thisDataNums  # Normalize by initial value
#      #maxYForPlot<-max(c(maxYForPlot,1.1*max(thisY))) # Calculate max Y value needed to show in plot
#      
#    rainbowColors<-rainbow(10)    # Make a rainbow color palette for plotting. 
#      speciesName<-VertNamesInPlotOrder$Name[funcGroup*10]
#       
#    simYears<-(0+(seq(1,length(thisY))/numStepsPerYear)) 
#          if (ageclass==1)  # If we are plotting age class 1, start a new plot
#          {
#             plot(simYears,thisY,cex.lab=1, cex.axis=1,lwd=1.5,type='l',ylab='Numbers',main=speciesName,cex.main=1, xlab='Year',ylim=c(0,maxYForPlot ),lty=ageclass,col = rainbowColors[ageclass])
#              print(speciesName)
#          }
#          else  # If we are doing age class 2 or higher, just add lines to the existing plot made for Age class 1. 
#          {
#            lines(simYears,thisY,lwd=1.5,type='l',lty=ageclass,col = rainbowColors[ageclass])
#              
#          }
#     }  
#      
#      if (wantSubtitle)
#      {
#      title(sub=VertNamesInPlotOrder$CODE[funcGroup*10])
#      }
# #    if (((funcGroup)%%plotsPerPage)==0)    
# #    {
# #         dev.off()  # close the device
# #    }
# 
#     
# }
# 
# dev.off()
# 
# 
# #-------
# #PLOT WET WEIGHT PER INDIVIDUAL, SIZE-AT-AGE OVER TIME: (APPROXIMATE AND BASED ON Rn RESERVE nITROGEN ONLY): 
# 
# print('rN over time')
# rowsCols<-dim(VertNamesInPlotOrder)
# numCohortsAllSpp<-rowsCols[1]
# plotNum<-0
# 
# WetWeightAtAGePlotNameString<-paste(pathToSavePlots,"WetWeightAtAgeApproxPlot", folders[i],"_",DateRun,".pdf",sep="")
# print(WetWeightAtAGePlotNameString)
# #postscript(file=rNPlotNameString, onefile=FALSE, horizontal=FALSE) #, height=8, width=8, pointsize=16).
# pdf(WetWeightAtAGePlotNameString)
# 
# for (funcGroup in 1:(numCohortsAllSpp/10) )     # loop over all functional groups (there are 10 age classes per functinal group)
# {
#   
#   if (((funcGroup-1)%%plotsPerPage)==0)   # if we are on Functional Group number 1, 7, 13,19, etc. 
#   {  
#     plotNum<-plotNum+1
#     
#     par(mfrow=c(plotRowsPerPage,plotColsPerPage),mar=c(4,4,2,2)) # lay out the 3x2 blank plots
#   }
#   
#   
#   
#   #-----------------
#   # All this loop does is  find the max Y value for this rN  plot (10 age classes, one functional group)   
#   maxYForPlot<-0.05  # set up a y limit max, which will change below
#   for (ageclass in 1:10) # loop over all 10 age classes
#   {
#     #thisVar <- ThisNC.nc$var[[ SppRnIndices[(funcGroup-1)*10+ageclass] ]]  # Extract variable
#     thisData <- get.var.ncdf( ThisNC.nc,paste(VertNamesInPlotOrder$NetCDFVertName[((funcGroup-1)*10+ageclass)],sep=""))  # Extract data from variable
#     thisData<-thisData*20*5.7*(3.65/2.65)/1000000
#     thisData[thisData==0]<-NA  # Replace 0's with NA
#     thisDataMeanWt<-apply(thisData,3,mean,na.rm = TRUE)#Get mean size over time, averaging over depth and location 
#     thisY<-thisDataMeanWt   #/thisDataMeanMg[1]  # Normalize by initial value
#     maxYObserved<-max(thisY,na.rm=TRUE)
#     maxYForPlot<-max(c(maxYForPlot,1.1*maxYObserved),na.rm=TRUE) # Calculate max Y value needed to show in plot
#   }
#   
#   #-----------------
#   
#   for (ageclass in 1:10) # loop over all 10 age classes
#   {
#     #thisVar <- ThisNC.nc$var[[ SppRnIndices[(funcGroup-1)*10+ageclass] ]]  # Extract variable
#     thisData <- get.var.ncdf( ThisNC.nc,paste(VertNamesInPlotOrder$NetCDFVertName[((funcGroup-1)*10+ageclass)],sep=""))  # Extract data from variable
#     thisData<-thisData*20*5.7*(3.65/2.65)/1000000
#     thisData[thisData==0]<-NA  # Replace 0's with NA
#     thisDataMeanWt<-apply(thisData,3,mean,na.rm = TRUE)#Get mean size over time, averaging over depth and location 
#     thisY<-thisDataMeanWt   #/thisDataMeanMg[1]  # Normalize by initial value
#     #maxYForPlot<-max(c(maxYForPlot,1.1*max(thisY))) # Calculate max Y value needed to show in plot
#     rainbowColors<-rainbow(10)    # Make a rainbow color palette for plotting. 
#     speciesName<-VertNamesInPlotOrder$Name[funcGroup*10]
#     simYears<-(0+(seq(1,length(thisY))/numStepsPerYear)) 
#     if (ageclass==1)  # If we are plotting age class 1, start a new plot
#     {
#       plot(simYears,thisY,cex.lab=1, cex.axis=1,lwd=1.5,type='l',ylab='Wet Weight per Individual (kg)',main=speciesName,cex.main=1, xlab='Year',ylim=c(0,maxYForPlot ),lty=ageclass,col = rainbowColors[ageclass])
#       print(speciesName)
#     }
#     else  # If we are doing age class 2 or higher, just add lines to the existing plot made for Age class 1. 
#     {
#       lines(simYears,thisY,lwd=1.5,type='l',lty=ageclass,col = rainbowColors[ageclass])
#       
#     }
#   }  
#   # lines(x=c(0,max(simYears)),y=c(1,1),lty='dashed',lwd=2,col='black')
#   if (wantSubtitle)
#   {
#     title(sub=VertNamesInPlotOrder$CODE[funcGroup*10])
#   }
#   #if (((funcGroup)%%plotsPerPage)==0)    
#   #{
#   #     dev.off()  # close the device
#   #}
#   
#   
# }
# 
# dev.off()
# 
# }
