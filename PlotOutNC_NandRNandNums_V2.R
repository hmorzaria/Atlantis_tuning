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

#pathfiles="E:/AtlantisV2/outputsApr28/" #path where run results are saved
#folders = c("OUT_Atlantis2_V4","OUT_Atlantis2_V5","OUT_Atlantis2_V6","OUT_Atlantis2_V7","OUT_Atlantis2_V8")

#pathfiles="E:/AtlantisV2/outputsPoterooApr28/"
#folders = c("OUT_Atlantis2_V13","OUT_Atlantis2_V14","OUT_Atlantis2_V15","OUT_Atlantis2_V16")

#pathfiles="E:/AtlantisV2/outputsMay2/"
#folders = c("OUT_Atlantis2_V1","OUT_Atlantis2_V2","OUT_Atlantis2_V3","OUT_Atlantis2_V4")

#pathfiles="E:/AtlantisV2/outputsMay6/"
#folders = c("OUT_Atlantis2_V1","OUT_Atlantis2_V2","OUT_Atlantis2_V3","OUT_Atlantis2_V4","OUT_Atlantis2_V5","OUT_Atlantis2_V6","OUT_Atlantis2_V7","OUT_Atlantis2_V8")

#pathfiles="E:/AtlantisV2/outputsMay12/"
#folders = c("OUT_Atlantis2_V5")#,"OUT_Atlantis2_V2","OUT_Atlantis2_V3","OUT_Atlantis2_V4","OUT_Atlantis2_V5")

#pathfiles="E:/AtlantisV2/outputsMay13/"
#folders = c("OUT_Atlantis2_V1")#,"OUT_Atlantis2_V2","OUT_Atlantis2_V3","OUT_Atlantis2_V4","OUT_Atlantis2_V5")

#pathfiles="E:/AtlantisV2/outputsMay14/"
#folders = c("OUT_Atlantis2_V2")#,"OUT_Atlantis2_V2","OUT_Atlantis2_V3","OUT_Atlantis2_V4","OUT_Atlantis2_V5")

# pathfiles="E:/AtlantisV2/outputsMay19/"
# DateRun ="May19" 
# #folders = c("OUT_Atlantis2_V1","OUT_Atlantis2_V2","OUT_Atlantis2_V3","OUT_Atlantis2_V4","OUT_Atlantis2_V5")
# folders = c("OUT_Atlantis2_V5")

#pathfiles="E:/AtlantisV2/outputsMay21/"
#DateRun ="May21" 
#folders = c("OUT_Atlantis2_V1","OUT_Atlantis2_V2","OUT_Atlantis2_V3","OUT_Atlantis2_V4","OUT_Atlantis2_V5","OUT_Atlantis2_V6","OUT_Atlantis2_V7","OUT_Atlantis2_V8")

 pathfiles="E:/AtlantisV2/outputsSalicorniaJune5/"
 DateRun ="June5" 
# #folders = c("OUT_Atlantis2_V1","OUT_Atlantis2_V2","OUT_Atlantis2_V3","OUT_Atlantis2_V4","OUT_Atlantis2_V5")
 folders = c("OUT_Atlantis_2_GOC_1")

AtlantisBoxesToUse = 0:65 # Isaac added this Feb 2014. You can put box 0 here. 
#AtlantisBoxesToUse<- c(3,9,39)
#AtlantisBoxesToUse<- c(11,29,38)
boxesToUse<- AtlantisBoxesToUse +1 # Isaac added this Feb 2014.  Box 0 becomes box 1, Box 1 becomes box 2, because Indexing in R starts with 1, not 0.
year0<-2008   # or whatever is appropriate. 
fishingStartYear<-2008 #All this does is draw a red vertical line on the plots. 
numStepsPerYear<-12  #Number of output intervals per year. With output intervals of 122 days, this is = 3. 
wantSubtitle<-TRUE  # Set this to true, to print graph subtitles, allows you to make sure the NetCDF name matches the Common Name used in the title. 
plotRowsPerPage<-3   #number of rows of subplots you want per page
plotColsPerPage<-2   # number of columns of subplots you want per page
plotsPerPage<-plotRowsPerPage*plotColsPerPage
setwd("E:/Archivos/1Archivos/NOAA/Atlantis model files/Atlantis_NEW_code/Plot code") # set working directory, place with  CSV file (see below)
FuncGroupNamesInPlotOrder<-read.table("FuncGroupNamesInPlotOrder.csv",as.is = TRUE,header=TRUE,sep=",") # For the Biomass vs. Time plots here. Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 

#AssessOrSurveyData<-read.table("AssessOrSurveyData1985.csv",as.is = TRUE,header=TRUE,sep=",") # Assessment or survey data. Column 1 is year, other cols are MT biomass per year
#print('**** AssessOrSurveyData1985.csv must have functional groups in same order...')
#print('    ...as FuncGroupNamesInPlotOrder.csv (including NAs)***** ')
VertNamesInPlotOrder<-read.table("VertNamesInPlotOrder.csv",as.is = TRUE,header=FALSE,col.names=c("NetCDFVertName","Name"),sep=",") # For the rN vs. time plots below. It should have # rows=10*numVertebrates (one per age class). Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 
VertNamesInPlotOrder2<-read.table("VertNamesInPlotOrder2.csv",as.is = TRUE,header=FALSE,col.names=c("NetCDFVertName","Name"),sep=",") # For the rN vs. time plots below. It should have # rows=10*numVertebrates (one per age class). Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 
VertNamesInPlotOrderNums<-read.table("VertNamesInPlotOrderNums.csv",as.is = TRUE,header=FALSE,col.names=c("NetCDFVertName","Name"),sep=",") # For the Nums vs. time plots below. It should have # rows=10*numVertebrates (one per age class). Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 

# TrophicLevels<-read.csv("fg_trophic_levels.csv",header=TRUE) 
# TrophicLevels = TrophicLevels[,2:3]
# names(TrophicLevels) = c("Group", "TL")

for (i in 1:length(folders)){
  
filepath = paste(pathfiles,folders[i],sep="")
pathToSavePlots<-paste(pathfiles,folders[i],"/",sep="")

setwd(filepath)

print('**** OPENING THE NETCDF FILE CAN TAKE 5 MINUTES ***** ')
print('**** YOU CAN TRY USING save() and load() TO AVOID THIS IN THE FUTURE***** ')
ThisNC.nc<-open.ncdf("NGulfOut.nc")
save(ThisNC.nc,file="GOC.Rdata")
print.ncdf(ThisNC.nc)


volumeData <- get.var.ncdf( ThisNC.nc,"volume") # extract the data from the variable. The variable contains lots of other metainfo like units, name, etc.
volDims<-dim(volumeData)  # Just use volume to see how many time steps are in the data
volBottomCells<-volumeData[volDims[1],,] # Because dz (height) of lowest boxes is 1m,I can say that Area is equal to the volume of the lowest boxes 
dz <- get.var.ncdf( ThisNC.nc,"dz") 
numDepths<-dim(dz)[1]
zBottomCell<- dz[numDepths,1,1]
areaData<-volBottomCells/zBottomCell

 # END OF DEFINITION BLOCK. USER CAN PROBABLY IGNORE EVERYTHING BELOW
 #-----------
This.biomindx = read.table("NgulfOutBiomIndx.txt",header=T)#EmoccOutHistoricalIdentical F2MSY EmoccOutF2MSY.nc EmoccOut169longer EmoccOutRun1  EmoccOutSept29a.nc NewCodeStatQuoMortC 18quadB.nc   EmoccOutSept29aItQStatusQuoLowRec.nc Oct3b EmoccOutOct3bPart EmoccOutOct2a  moccOutOct1aPart EmoccOutSept29a EmoccOutSept28a EmoccOut40yrSpinupNoFish.nc EmoccOutHydro3.nc") #) EmoccOutAug28.nc")  #AMS: amsFCoutput12Mar.nc "EmoccOutJan30bNoFish.nc EmoccOutFeb2BF.nc EmoccOutJan24a.nc Open NetCDF file


VirginBToCurrentB <- FuncGroupNamesInPlotOrder$VirgBToCurrentB
unfishedB<- FuncGroupNamesInPlotOrder$unfishedB
X1985B<- FuncGroupNamesInPlotOrder$X1985B
Biom = This.biomindx[,1:63]

plotsPerPage<-plotRowsPerPage*plotColsPerPage

graphics.off()

#-----------------------------------------------------
# MAKE PLOTS OF ABUNDANCE OVER TIME


numTimeSteps<-nrow(Biom)
rowscols<-dim(FuncGroupNamesInPlotOrder) # count how many species are in this data
numFuncGroups<-rowscols[1]

plotNum<-0
simYears<-(0+(seq(1,numTimeSteps)/numStepsPerYear))  # Get some x values for the plot (time)

#simYears<-1:nrow(Biom)  # Get some x values for the plot (time)


Biom$Group <- factor(Biom$Group, levels = c("BB","BB","BO","DIN","DL","DR","DC","MA","MB","PB","PL","PS","SG","BC","BFF","BG","BML","BMS","ZS","FDS","BMD","CEP","BD","ZL","BFD","BO","PWN","ZG","SSK","ZM","WHB","FMN","REP","FVO","BFS","FBP","FDB","FDC","FDD","FDE","SP","FVT","FVD","SB","FVV","FDM","FPO","FDF","FMM","FDP","FPL","SHP","SHR","FDO","PIN","SHD","WDG","FPS","SHC","FVB","FVS","SHB","WHT","WHS"))

#-----------------------------------------------------
# MAKE PLOTS OF ABUNDANCE OVER TIME


#numTimeSteps<-volDims[3]
print(numTimeSteps)
rowscols<-dim(FuncGroupNamesInPlotOrder) # count how many species are in this data
numFuncGroups<-rowscols[1]

plotNum<-0
simYears<-(year0+(seq(1,numTimeSteps)/numStepsPerYear))  # Get some x values for the plot (time)
print(length(simYears))

BiomassPlotNameString<-paste(pathToSavePlots, "BiomassPlot",folders[i],"_",DateRun,".pdf",sep="") # create a string to use as the file name
pdf(BiomassPlotNameString)

#LOOP OVER ALL FUNCTIONAL GROUPS
for (funcGroup in 1:numFuncGroups )      
{ 
  
  Groups.tl = levels(Biomass$Group) 
  print(Groups.tl[funcGroup])
  
  this.name = subset(FuncGroupNamesInPlotOrder, CODE%in% Groups.tl[funcGroup])
  
  print(this.name$Name)
  
  # If we are on Functional Group number 1, 7, 13,19, [i.e. want a fresh page for subplots], set up the plot page 
  if (((funcGroup-1)%%plotsPerPage)==0)  
  {  
    plotNum<-plotNum+1  
    #Outfile <- paste("Biomass_Plots_", dirname, ".pdf", sep="")
    #pdf(Outfile) 
    #BiomassPlotNameString<-paste(pathToSavePlots, "BiomassPlot",plotNum,".eps",sep="") # create a string to use as the file name
    #postscript(file=BiomassPlotNameString, onefile=FALSE, horizontal=FALSE) # give the plot that filename
    #BiomassPlotNameString<-paste(pathToSavePlots, "BiomassPlot",plotNum,".pdf",sep="") # create a string to use as the file name
    #pdf(BiomassPlotNameString)
    par(mfrow=c(plotRowsPerPage,plotColsPerPage)) # lay out the 3x2 blank plots
  }

     
     this.group = subset(Biomass, Group%in% Groups.tl[funcGroup])
             
     thisY = this.group$Biomass
  
    last.valueY = length(thisY)
     
    last.valueB = thisY[last.valueY]
    first.valueB = thisY[1]
  
 
  
  X1985B =  this.name$X1985B
  unfishedB = this.name$unfishedB
  
  Bratio = last.valueB/X1985B
  Bratio = round(Bratio,6)
  
  #MAKE THE PLOT
        #assessOrSurveyMT<-AssessOrSurveyData[,funcGroup+1]  # add 1 because 1st column is year, and then the columns are in order for the plots. 
        YYear1 <- this.group$Biomass[1]
        #testVec<-c(maxYForPlot,1.1*max(thisY),unfishedB[funcGroup]),assessOrSurveyMT) # Calculate max Y value needed to show in plot       
        #YYear1 <- thisY[1]
        testVec<-c(1.1*max(thisY),unfishedB,X1985B) # Calculate max Y value needed to show in plot       
        maxYForPlot<-max(testVec[!is.na(testVec)])
        yLabString<-'Biomass, metric tons' 
        #line type lty options: (0=blank, 1=solid, 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash) or as one of the character strings "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash", where "blank" uses ‘invisible lines’ (i.e., doesn't draw them)
        plot(simYears,thisY,lwd=2,type='l',cex.lab=1.5,cex.axis=2, col='black',lty='solid',ylab=yLabString,main=this.name$Name,xlab='Year',ylim=c(0,maxYForPlot ),cex.main=2)
        if (wantSubtitle)
        {
          title(sub=paste(this.name$CODE,"_end B/1985 Biom ratio=",Bratio,sep=""))
        }
        lines(x=c(0,max(simYears)),y=c(unfishedB,unfishedB),lty='dashed',lwd=2,col='red')
      lines(x=c(0,max(simYears)),y=c(X1985B,X1985B),lty='solid',lwd=2,col='green')
        #lines(x=c(fishingStartYear, fishingStartYear ),y=c(0,maxYForPlot),lty='dashed',lwd=2,col='red')
        #lines(x=AssessOrSurveyData$Year,y=assessOrSurveyMT,lwd=3,col='black')
         
     
    
#    # If we are on Functional Group number 6, 12, 18,24, [i.e. the plotpage is full], close the plot
#    if (((funcGroup)%%plotsPerPage)==0)    
#    {
#         dev.off()  # close the device
#    }
    
}
dev.off()

#-------------------------------------------------------
#PLOT RESERVE NITROGEN OVER TIME: 


print('rN over time')
rowsCols<-dim(VertNamesInPlotOrder)
numCohortsAllSpp<-rowsCols[1]
plotNum<-0

rNPlotNameString<-paste(pathToSavePlots,"rNPlot",folders[i],"_",DateRun,".pdf",sep="")
print(rNPlotNameString)
#postscript(file=rNPlotNameString, onefile=FALSE, horizontal=FALSE) #, height=8, width=8, pointsize=16).
pdf(rNPlotNameString)

for (funcGroup in 1:(numCohortsAllSpp/10) )     # loop over all functional groups (there are 10 age classes per functinal group)
{
  
  if (((funcGroup-1)%%plotsPerPage)==0)   # if we are on Functional Group number 1, 7, 13,19, etc. 
  {  
    plotNum<-plotNum+1
    
    par(mfrow=c(plotRowsPerPage,plotColsPerPage)) # lay out the 3x2 blank plots
  }
  





#-----------------
 # All this loop does is  find the max Y value for this rN  plot (10 age classes, one functional group)   
     maxYForPlot<-1.1  # set up a y limit max, which will change below
     for (ageclass in 1:10) # loop over all 10 age classes
     {
     #thisVar <- ThisNC.nc$var[[ SppRnIndices[(funcGroup-1)*10+ageclass] ]]  # Extract variable
     thisData <- get.var.ncdf( ThisNC.nc,VertNamesInPlotOrder$NetCDFVertName[((funcGroup-1)*10+ageclass)])  # Extract data from variable
     thisData[thisData==0]<-NA  # Replace 0's with NA
     thisDataMeanMg<-apply(thisData,3,mean,na.rm = TRUE)#Get mean size over time, averaging over depth and location 
     thisY<-thisDataMeanMg/thisDataMeanMg[1]  # Normalize by initial value
     maxYObserved<-max(thisY,na.rm=TRUE)
     maxYForPlot<-max(c(maxYForPlot,1.1*maxYObserved),na.rm=TRUE) # Calculate max Y value needed to show in plot
    }

#-----------------

     for (ageclass in 1:10) # loop over all 10 age classes
     {
     #thisVar <- ThisNC.nc$var[[ SppRnIndices[(funcGroup-1)*10+ageclass] ]]  # Extract variable
     thisData <- get.var.ncdf( ThisNC.nc,VertNamesInPlotOrder$NetCDFVertName[((funcGroup-1)*10+ageclass)])  # Extract data from variable
     thisData[thisData==0]<-NA  # Replace 0's with NA
     thisDataMeanMg<-apply(thisData,3,mean,na.rm = TRUE)#Get mean size over time, averaging over depth and location 
     thisY<-thisDataMeanMg/thisDataMeanMg[1]  # Normalize by initial value
     #maxYForPlot<-max(c(maxYForPlot,1.1*max(thisY))) # Calculate max Y value needed to show in plot
     rainbowColors<-rainbow(10)    # Make a rainbow color palette for plotting. 
     speciesName<-VertNamesInPlotOrder$Name[funcGroup*10]
      
     simYears<-(0+(seq(1,length(thisY))/numStepsPerYear)) 
     
     if (ageclass==1)  # If we are plotting age class 1, start a new plot
         {
            plot(simYears,thisY,cex.lab=1.5, cex.axis=2,lwd=0.5,type='l',ylab='rN / initial rN',main=speciesName,cex.main=2, xlab='Year',ylim=c(0,maxYForPlot ),lty=ageclass,col = rainbowColors[ageclass])
             print(speciesName)
         }
         else  # If we are doing age class 2 or higher, just add lines to the existing plot made for Age class 1. 
         {
           lines(simYears,thisY,lwd=0.5,type='l',lty=ageclass,col = rainbowColors[ageclass])
             
         }
    }  
     lines(x=c(0,max(simYears)),y=c(1.2,1.2),lty='solid',lwd=1,col='black')
     lines(x=c(0,max(simYears)),y=c(0.8,0.8),lty='solid',lwd=1,col='black')
    lines(x=c(0,max(simYears)),y=c(1.5,1.5),lty='dashed',lwd=2,col='black')
    lines(x=c(0,max(simYears)),y=c(0.5,0.5),lty='dashed',lwd=2,col='black')

     if (wantSubtitle)
     {
     title(sub=VertNamesInPlotOrder$CODE[funcGroup*10])
     }
#    if (((funcGroup)%%plotsPerPage)==0)    
#    {
#         dev.off()  # close the device
#    }

    
}

dev.off()

# #-------------------------------------------------------
# #PLOT INDIVIDUAL WEIGHT OVER TIME: 
# 
# print('Individual weight')
# rowsCols<-dim(VertNamesInPlotOrder)
# numCohortsAllSpp<-rowsCols[1]
# plotNum<-0
# 
# for (funcGroup in 1:(numCohortsAllSpp/10) )     # loop over all functional groups (there are 10 age classes per functinal group)
# {
#      
#      if (((funcGroup-1)%%plotsPerPage)==0)   # if we are on Functional Group number 1, 7, 13,19, etc. 
#     {  
#     plotNum<-plotNum+1
#     rNPlotNameString<-paste(pathToSavePlots,"IndWght",plotNum,folders[i],".pdf",sep="")
#     print(rNPlotNameString)
#     pdf(rNPlotNameString)
#     #postscript(file=rNPlotNameString, onefile=FALSE, horizontal=FALSE) #, height=8, width=8, pointsize=16).
#     par(mfrow=c(plotRowsPerPage,plotColsPerPage)) # lay out the 3x2 blank plots
#     }
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
#      thisDataMeanMg<-apply(thisData,3,mean,na.rm = TRUE)
#      
#      thisData2 <- get.var.ncdf( ThisNC.nc,VertNamesInPlotOrder2$NetCDFVertName[((funcGroup-1)*10+ageclass)])  # Extract data from variable
#      thisData2[thisData2==0]<-NA  # Replace 0's with NA
#      thisDataMeanMg2<-apply(thisData2,3,mean,na.rm = TRUE)
#      
#     #Get mean size over time, averaging over depth and location 
#      thisDataTotal = thisDataMeanMg + thisDataMeanMg2
#      thisY<-thisDataTotal/thisDataTotal[1]  # Normalize by initial value
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
#      thisDataMeanMg<-apply(thisData,3,mean,na.rm = TRUE)
#      
#      thisData2 <- get.var.ncdf( ThisNC.nc,VertNamesInPlotOrder2$NetCDFVertName[((funcGroup-1)*10+ageclass)])  # Extract data from variable
#      thisData2[thisData2==0]<-NA  # Replace 0's with NA
#      thisDataMeanMg2<-apply(thisData2,3,mean,na.rm = TRUE)
#      
#     #Get mean size over time, averaging over depth and location 
#      thisDataTotal = thisDataMeanMg + thisDataMeanMg2
#      thisY<-thisDataTotal/thisDataTotal[1]  # Normalize by initial value
#     #maxYForPlot<-max(c(maxYForPlot,1.1*max(thisY))) # Calculate max Y value needed to show in plot
#      rainbowColors<-rainbow(10)    # Make a rainbow color palette for plotting. 
#      speciesName<-VertNamesInPlotOrder$Name[funcGroup*10]
#     simYears<-(0+(seq(1,length(thisY))/numStepsPerYear)) 
#       if (ageclass==1)  # If we are plotting age class 1, start a new plot
#          {
#             plot(simYears,thisY,cex.lab=1.5, cex.axis=2,lwd=0.5,type='l',ylab='ind weight / initial weight',main=speciesName,cex.main=2, xlab='Year',ylim=c(0,maxYForPlot ),lty=ageclass,col = rainbowColors[ageclass])
#              print(speciesName)
#          }
#          else  # If we are doing age class 2 or higher, just add lines to the existing plot made for Age class 1. 
#          {
#            lines(simYears,thisY,lwd=0.5,type='l',lty=ageclass,col = rainbowColors[ageclass])
#              
#          }
#     }  
#      lines(x=c(0,max(simYears)),y=c(1,1),lty='dashed',lwd=2,col='black')
#      lines(x=c(0,max(simYears)),y=c(0.5,0.5),lty='dashed',lwd=2,col='black')
#      if (wantSubtitle)
#      {
#      title(sub=VertNamesInPlotOrder$NetCDFVertName[funcGroup*10])
#      }
#    if (((funcGroup)%%plotsPerPage)==0)    
#    {
#         dev.off()  # close the device
#    }
# 
#     
# }
# 
# dev.off()

#-------------------------------------------------------
#PLOT Numbers at age over timE: 

print('Nums over time')
rowsCols<-dim(VertNamesInPlotOrder)
numCohortsAllSpp<-rowsCols[1]
plotNum<-0


NumsPlotNameString<-paste(pathToSavePlots,"NumsPlot",folders[i],"_",DateRun,".pdf",sep="")
print(NumsPlotNameString)
#postscript(file=NumsPlotNameString, onefile=FALSE, horizontal=FALSE) #, height=8, width=8, pointsize=16).
pdf(NumsPlotNameString)

for (funcGroup in 1:(numCohortsAllSpp/10) )     # loop over all functional groups (there are 10 age classes per functinal group)
{
  
  if (((funcGroup-1)%%plotsPerPage)==0)   # if we are on Functional Group number 1, 7, 13,19, etc. 
  {  
    plotNum<-plotNum+1
    par(mfrow=c(plotRowsPerPage,plotColsPerPage)) # lay out the 3x2 blank plots
  }     


#-----------------
 # All this loop does is  find the max Y value for this Nums  plot (10 age classes, one functional group)   
     maxYForPlot<-1.1  # set up a y limit max, which will change below
     for (ageclass in 1:10) # loop over all 10 age classes
     {
     #thisVar <- ThisNC.nc$var[[ SppRnIndices[(funcGroup-1)*10+ageclass] ]]  # Extract variable
     thisData <- get.var.ncdf( ThisNC.nc,VertNamesInPlotOrderNums$NetCDFVertName[((funcGroup-1)*10+ageclass)])  # Extract data from variable
     thisData[thisData==0]<-NA  # Replace 0's with NA
     thisDataNums<-apply(thisData,3,sum,na.rm = TRUE)#Get nums over time, summing over depth and location 
     thisY<-thisDataNums
     maxYObserved<-max(thisY,na.rm=TRUE)
     maxYForPlot<-max(c(maxYForPlot,1.1*maxYObserved),na.rm=TRUE) # Calculate max Y value needed to show in plot
    }

#-----------------

     for (ageclass in 1:10) # loop over all 10 age classes
     {
     #thisVar <- ThisNC.nc$var[[ SppRnIndices[(funcGroup-1)*10+ageclass] ]]  # Extract variable
     thisData <- get.var.ncdf( ThisNC.nc,VertNamesInPlotOrderNums$NetCDFVertName[((funcGroup-1)*10+ageclass)])  # Extract data from variable
     thisData[thisData==0]<-NA  # Replace 0's with NA
     thisDataNums<-apply(thisData,3,sum,na.rm = TRUE)#Get nums over time, summing over depth and location  
     thisY<-thisDataNums  # Normalize by initial value
     #maxYForPlot<-max(c(maxYForPlot,1.1*max(thisY))) # Calculate max Y value needed to show in plot
     
   rainbowColors<-rainbow(10)    # Make a rainbow color palette for plotting. 
     speciesName<-VertNamesInPlotOrder$Name[funcGroup*10]
      
   simYears<-(0+(seq(1,length(thisY))/numStepsPerYear)) 
         if (ageclass==1)  # If we are plotting age class 1, start a new plot
         {
            plot(simYears,thisY,cex.lab=1.5, cex.axis=2,lwd=0.5,type='l',ylab='Numbers',main=speciesName,cex.main=2, xlab='Year',ylim=c(0,maxYForPlot ),lty=ageclass,col = rainbowColors[ageclass])
             print(speciesName)
         }
         else  # If we are doing age class 2 or higher, just add lines to the existing plot made for Age class 1. 
         {
           lines(simYears,thisY,lwd=0.5,type='l',lty=ageclass,col = rainbowColors[ageclass])
             
         }
    }  
     
     if (wantSubtitle)
     {
     title(sub=VertNamesInPlotOrder$CODE[funcGroup*10])
     }
#    if (((funcGroup)%%plotsPerPage)==0)    
#    {
#         dev.off()  # close the device
#    }

    
}

dev.off()


#-------
#PLOT WET WEIGHT PER INDIVIDUAL, SIZE-AT-AGE OVER TIME: (APPROXIMATE AND BASED ON Rn RESERVE nITROGEN ONLY): 

print('rN over time')
rowsCols<-dim(VertNamesInPlotOrder)
numCohortsAllSpp<-rowsCols[1]
plotNum<-0

WetWeightAtAGePlotNameString<-paste(pathToSavePlots,"WetWeightAtAgeApproxPlot",".pdf",sep="")
print(WetWeightAtAGePlotNameString)
#postscript(file=rNPlotNameString, onefile=FALSE, horizontal=FALSE) #, height=8, width=8, pointsize=16).
pdf(WetWeightAtAGePlotNameString)

for (funcGroup in 1:(numCohortsAllSpp/10) )     # loop over all functional groups (there are 10 age classes per functinal group)
{
  
  if (((funcGroup-1)%%plotsPerPage)==0)   # if we are on Functional Group number 1, 7, 13,19, etc. 
  {  
    plotNum<-plotNum+1
    
    par(mfrow=c(plotRowsPerPage,plotColsPerPage)) # lay out the 3x2 blank plots
  }
  
  
  
  #-----------------
  # All this loop does is  find the max Y value for this rN  plot (10 age classes, one functional group)   
  maxYForPlot<-0.05  # set up a y limit max, which will change below
  for (ageclass in 1:10) # loop over all 10 age classes
  {
    #thisVar <- ThisNC.nc$var[[ SppRnIndices[(funcGroup-1)*10+ageclass] ]]  # Extract variable
    thisData <- get.var.ncdf( ThisNC.nc,paste(VertNamesInPlotOrder$NetCDFVertName[((funcGroup-1)*10+ageclass)],sep=""))  # Extract data from variable
    thisData<-thisData*20*5.7*(3.65/2.65)/1000000
    thisData[thisData==0]<-NA  # Replace 0's with NA
    thisDataMeanWt<-apply(thisData,3,mean,na.rm = TRUE)#Get mean size over time, averaging over depth and location 
    thisY<-thisDataMeanWt   #/thisDataMeanMg[1]  # Normalize by initial value
    maxYObserved<-max(thisY,na.rm=TRUE)
    maxYForPlot<-max(c(maxYForPlot,1.1*maxYObserved),na.rm=TRUE) # Calculate max Y value needed to show in plot
  }
  
  #-----------------
  
  for (ageclass in 1:10) # loop over all 10 age classes
  {
    #thisVar <- ThisNC.nc$var[[ SppRnIndices[(funcGroup-1)*10+ageclass] ]]  # Extract variable
    thisData <- get.var.ncdf( ThisNC.nc,paste(VertNamesInPlotOrder$NetCDFVertName[((funcGroup-1)*10+ageclass)],sep=""))  # Extract data from variable
    thisData<-thisData*20*5.7*(3.65/2.65)/1000000
    thisData[thisData==0]<-NA  # Replace 0's with NA
    thisDataMeanWt<-apply(thisData,3,mean,na.rm = TRUE)#Get mean size over time, averaging over depth and location 
    thisY<-thisDataMeanWt   #/thisDataMeanMg[1]  # Normalize by initial value
    #maxYForPlot<-max(c(maxYForPlot,1.1*max(thisY))) # Calculate max Y value needed to show in plot
    rainbowColors<-rainbow(10)    # Make a rainbow color palette for plotting. 
    speciesName<-VertNamesInPlotOrder$Name[funcGroup*10]
    simYears<-(0+(seq(1,length(thisY))/numStepsPerYear)) 
    if (ageclass==1)  # If we are plotting age class 1, start a new plot
    {
      plot(simYears,thisY,cex.lab=1.1, cex.axis=1.2,lwd=0.5,type='l',ylab='Wet Weight per Individual (kg)',main=speciesName,cex.main=1.3, xlab='Year',ylim=c(0,maxYForPlot ),lty=ageclass,col = rainbowColors[ageclass])
      print(speciesName)
    }
    else  # If we are doing age class 2 or higher, just add lines to the existing plot made for Age class 1. 
    {
      lines(simYears,thisY,lwd=0.5,type='l',lty=ageclass,col = rainbowColors[ageclass])
      
    }
  }  
  # lines(x=c(0,max(simYears)),y=c(1,1),lty='dashed',lwd=2,col='black')
  if (wantSubtitle)
  {
    title(sub=VertNamesInPlotOrder$CODE[funcGroup*10])
  }
  #if (((funcGroup)%%plotsPerPage)==0)    
  #{
  #     dev.off()  # close the device
  #}
  
  
}

dev.off()
#--------------
# JUST LIST ALL THE VARIABLES IN THE NCDF FILE
# THIS CODE JUST PRINTS VARIABLE NAMES TO SCREEN, BUT CAN BE USEFUL FOR MAKING THE .CSV INPUT FILES. 
# print('LISTING ALL VARIABLE NAMES IN THE NETCDF FILE')
# numVarsInNC<-length(ThisNC.nc$var)
# 
# for (funcGroup in 1:numFuncGroups )   
# {
#    #  thisVar <- ThisNC.nc$var[[groupIndex]]
#     #  print(thisVar$name)    
#      thisData <- get.var.ncdf( ThisNC.nc,FuncGroupNamesInPlotOrder$NetCDFName[funcGroup])  # Extract data from variable
#      print(FuncGroupNamesInPlotOrder$NetCDFName[funcGroup])
#      print("dimensions:")
#     print(dim(thisData))
# }
# print('THE LISTING ABOVE IS ALL VARIABLE NAMES IN CDF')
# print('THIS MAY BE USEFUL FOR MAKING .CSV INPUT FILES')
# 
# close.ncdf(ThisNC.nc)
}
