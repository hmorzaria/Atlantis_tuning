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
library(gdata)

rm(list=ls())

#-------------
# USER CAN DEFINE INPUT FILES, STARTYEAR, TIMESTEP, SAVE DIRECTORY, ETC. HERE:  
#Here are some things that are hardcoded, but change here if you want. 
#
X11()
pathfiles="E:/AtlantisV2/Final_30yr_NoF/FinalPRMS/"
#E:\AtlantisV2\Final_30yr_NoF\FinalPRMS\Aug31_PRM37
DateRun ="Final"

#This is just to keep track of the runs, need to paste biom_indx in the folder with the run name
#folders = c("Aug31_PRM49","Sep3_PRM50")#, "OUT_AtlantisGOC_30yr_NoF - 2","OUT_AtlantisGOC_30yr_NoF - 3","OUT_AtlantisGOC_30yr_NoF - 4","OUT_AtlantisGOC_30yr_NoF - 5","OUT_AtlantisGOC_30yr_NoF - 6","OUT_AtlantisGOC_30yr_NoF - 7","OUT_AtlantisGOC_30yr_NoF - 8")#,"Jul_26_FIXPREY_V2_NoF_Starve_3")
folders = c("Jan21_PRM28","Jan21_PRM37","Jan21_PRM48","Jan21_PRM46","Jan21_PRM50","Jan21_PRM51","Jan21_PRM53","Jan21_PRM54","Jan21_PRM55","Jan21_PRM56") #52 is same as 37 but with a tiny ml_amount to OA groups


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

simYears = 31

#AssessOrSurveyData<-read.table("AssessOrSurveyData1985.csv",as.is = TRUE,header=TRUE,sep=",") # Assessment or survey data. Column 1 is year, other cols are MT biomass per year
#print('**** AssessOrSurveyData1985.csv must have functional groups in same order...')
#print('    ...as FuncGroupNamesInPlotOrder.csv (including NAs)***** ')
VertNamesInPlotOrder<-read.table("VertNamesInPlotOrder.csv",as.is = TRUE,header=FALSE,col.names=c("NetCDFVertName","Name"),sep=",") # For the rN vs. time plots below. It should have # rows=10*numVertebrates (one per age class). Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 
VertNamesInPlotOrder2<-read.table("VertNamesInPlotOrder2.csv",as.is = TRUE,header=FALSE,col.names=c("NetCDFVertName","Name"),sep=",") # For the rN vs. time plots below. It should have # rows=10*numVertebrates (one per age class). Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 
VertNamesInPlotOrderNums<-read.table("VertNamesInPlotOrderNums.csv",as.is = TRUE,header=FALSE,col.names=c("NetCDFVertName","Name"),sep=",") # For the Nums vs. time plots below. It should have # rows=10*numVertebrates (one per age class). Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 



# END OF DEFINITION BLOCK. USER CAN PROBABLY IGNORE EVERYTHING BELOW
#-----------



setwd("E:/AtlantisV2/Final_30yr_NoF/FinalPRMS/")

 csvdat <- list.files(pattern="Ngulf*")
 lm.res <- list()
 
 BiomassN = as.data.frame(matrix(0,nrow=0,ncol=4))
 
for(eachfolder in 1:length(folders)) {
   
 Plotpath<-paste(pathfiles,folders[eachfolder],sep="")
  setwd(Plotpath)
  
   Biom  = read.table("NgulfOutBiomIndx.txt",header=T)
   
   Biomass = melt(Biom,id.vars = "Time")
   names(Biomass) = c("Time","Group","Biomass")
   # these are groups ordered by TL
   tl.groups = c("BB","DIN","DL","DR","DC","MA","MB","PB","PL","PS","SG","BC","BFF","BG","BML","BMS","ZS","FDS","BMD","CEP","BD","ZL","BFD","BO","PWN","ZG","SSK","ZM","WHB","FMN","REP","FVO","BFS","FBP","FDB","FDC","FDD","FDE","SP","FVT","FVD","SB","FVV","FDM","FPO","FDF","FMM","FDP","FPL","SHP","SHR","FDO","PIN","SHD","WDG","FPS","SHC","FVB","FVS","SHB","WHT","WHS")
   Biomass$Group <- ordered(Biomass$Group, levels = c("BB","DIN","DL","DR","DC","MA","MB","PB","PL","PS","SG","BC","BFF","BG","BML","BMS","ZS","FDS","BMD","CEP","BD","ZL","BFD","BO","PWN","ZG","SSK","ZM","WHB","FMN","REP","FVO","BFS","FBP","FDB","FDC","FDD","FDE","SP","FVT","FVD","SB","FVV","FDM","FPO","FDF","FMM","FDP","FPL","SHP","SHR","FDO","PIN","SHD","WDG","FPS","SHC","FVB","FVS","SHB","WHT","WHS"))
   
   Biomass$run = folders[eachfolder]
   
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

Groups.tl = levels(BiomassN$Group)


#LOOP OVER ALL FUNCTIONAL GROUPS
#for (funcGroup in 1:numFuncGroups )  
  for (funcGroup in 1:length(Groups.tl)) 
{ 
  
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

time.levels = as.data.frame(this.group$Time[1:31])
X1985B.frame = cbind(time.levels,this.group$Group,rep(X1985B,31))
X1985B.frame$run = "X1985B"
names(X1985B.frame) = names(this.group)


unfishedB.frame = cbind(time.levels,this.group$Group,rep(unfishedB,31))
unfishedB.frame$run = "unfishedB"
names(unfishedB.frame) = names(this.group)

this.group = rbind(this.group,unfishedB.frame,X1985B.frame)

  Bratio = max.valueB/X1985B
  Bratio = round(Bratio,6)

  testVec<-c(1.1*max(thisY),unfishedB,X1985B) # Calculate max Y value needed to show in plot       
  maxYForPlot<-1.1*(max(testVec[!is.na(testVec)]))
  yLabString<-'Biomass, metric tons' 
  
  p <- ggplot(this.group, aes(x = Time, y = Biomass, group=run,colour=run,linetype=run)) 
  geom.p <- p + geom_line(size=1.25)
  sp.graph <- geom.p + labs(x= ("Time"), y =(yLabString)) + ylim(0,maxYForPlot) +
  #scale_colour_manual(values = c("goldenrod","blue3","darkred","black")) +
  #scale_linetype_manual(values=c("solid","solid","dashed", "dashed")) +
  
  scale_colour_manual(values = c("darkmagenta","purple3","navy","dodgerblue3","darkturquoise","lightpink3","firebrick2","darkorange","darkgoldenrod","darkolivegreen2","darkred","black")) +
  scale_linetype_manual(values=c("solid","solid","solid","solid","solid","solid","solid","solid","solid","solid","dashed","dashed")) +
 ggtitle(this.name$Name)
  
  graph1 <-  sp.graph + theme(plot.title = element_text(size = 9), panel.grid.minor = element_line(colour = NA), panel.background = element_rect(fill = NA, colour = NA),
                               axis.line=element_line(colour = "black"), axis.text.x = element_text(vjust = 1, size = 8, colour = "black"), axis.text.y = element_text(hjust=1, size=8, colour = "black")) +
    theme(axis.text.y = element_text(angle=90)) + #, axis.text.x = theme_blank()) + 
    theme(legend.position = "none")
    
  plot_list = c(plot_list, list(graph1))

  if (funcGroup==12 | funcGroup==24 | funcGroup==36 | funcGroup==48 | funcGroup==60 | funcGroup==62) 
    { ## print 12 plots on a page
   
      pdf(paste("Biomass_prms_AtlantisV2_",funcGroup,".pdf",sep=), width = 9.5, height = 11)
    
    print(do.call(grid.arrange, c(plot_list, list(ncol=plotColsPerPage,nrow=plotRowsPerPage))))
    
    plot_list = list() # reset plot 
    
    dev.off()
    
  }


}
 
if (length(plot_list) != 0) { 
  print(do.call(grid.arrange, c(plot_list, list(ncol=plotColsPerPage,nrow=plotRowsPerPage))))
  
}
dev.off()

X11()
print(sp.graph)