# Written by Isaac Kaplan & Hem Morzaria hmorzarialuna@gmail.com
# Date: August 2, 2010
#Revised Nov 13 2013
# Data: Summary results Atlantis scenario output
# This file takes output file from NC_Biomass_reader_cell_Sardine.r
# Biomass for cells within the Midriff Islands
# Obtain Biomass by year graphs for aggregated groups


library(ggplot2)
library(gdata)
library(gridExtra)
library(reshape)

rm(list=ls(all=TRUE))# will remove objects

setwd("E:/Archivos/1Archivos/NOAA/Atlantis model files/Atlantis_NEW_code/Plot code") # set working directory, place with  CSV file (see below)
FuncGroupNamesInPlotOrder<-read.table("FuncGroupNamesInPlotOrder.csv",as.is = TRUE,header=TRUE,sep=",") # For the Biomass vs. Time plots here. Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 

pathfiles="E:/AtlantisV2/SpinUp_53yrs_V2/"



DateRun ="SpinUp"
folders = c("Jan25_PRM28","Jan25_PRM37","Jan25_PRM48","Jan25_PRM46","Jan25_PRM50","Jan25_PRM51","Jan25_PRM53","Jan25_PRM54","Jan25_PRM55","Jan25_PRM56") #52 is same as 37 but with a tiny ml_amount to OA groups
#folders = c("Aug27_PRM28","Aug31_PRM37","Sep3_PRM48","Sep3_PRM46","Sep3_PRM50","Sep3_PRM51") #52 is same as 37 but with a tiny ml_amount to OA groups

result.matrix = as.data.frame(matrix(0,nrow=0,ncol=12))

for (i in 1:length(folders)){
  filepath = paste(pathfiles,folders[i],sep="")
  pathToSavePlots<-paste(pathfiles,folders[i],"/",sep="")
  
  setwd(filepath)
  
This.biomindx = read.table("NgulfOutBiomIndx.txt",header=T)

VirginBToCurrentB <- FuncGroupNamesInPlotOrder$VirgBToCurrentB
unfishedB<- FuncGroupNamesInPlotOrder$unfishedB
X1985B<- FuncGroupNamesInPlotOrder$X1985B
Biom = This.biomindx[,2:63]

Time <- as.character(1954:2008)
Biom$Time = Time
  
Biom2 = Biom[,c(63,1:62)]


Biomass = melt(Biom2,id.vars = "Time")
names(Biomass) = c("Time","CODE","Biomass")

Biomass.name = merge(Biomass,FuncGroupNamesInPlotOrder,by="CODE")

Biomass.name = Biomass.name[,c(1:3,5,7:8)]

first.bio = subset(Biomass.name, Time == c("1954")) 
first.bio = drop.levels(first.bio) 
names(first.bio) = c("CODE","Time","Biomass.Observed","Name","unfishedB","X1985B")

last.bio = subset(Biomass.name, Time == c("2008")) 
last.bio = drop.levels(last.bio)
names(last.bio) = c("CODE","Time","Biomass.Predicted","Name","unfishedB","X1985B")


Biomass.change = merge(first.bio,last.bio,by=c("CODE","Name"))

Biomass.change$change = log10(Biomass.change$Biomass.Predicted / Biomass.change$Biomass.Observed)




Biomass.change$run = folders[i]

result.matrix= rbind(result.matrix, Biomass.change)

}

result.matrix$run = as.factor(result.matrix$run)

write.csv(result.matrix, file="Spinup+_Change_biomass.csv")

change.matrix = as.data.frame(matrix(0,nrow=0,ncol=4))


for (eachgroup in 1:length(levels(result.matrix$CODE))){

  this.group = as.character(result.matrix$CODE[eachgroup])
  this.species= subset(result.matrix, result.matrix$CODE %in% c(this.group))
  
  mean.change = mean(this.species$change)
  max.change = max(this.species$change)
  min.change = min(this.species$change)
  
  change.frame = cbind (mean.change,max.change,min.change)
  change.frame = as.data.frame(change.frame)
  change.frame$CODE = this.group
  change.matrix = rbind(change.frame,change.matrix)
}

change.matrix  = change.matrix [ order(-change.matrix [,1], change.matrix [,4]), ]

change.matrix$CODE <- factor(change.matrix$CODE, as.character(change.matrix$CODE))

X11()

limits <- aes(ymax = change.matrix$max.change, ymin=change.matrix$min.change)

p <- ggplot(change.matrix, aes(x = CODE, y = mean.change)) 
geom.p <- p + geom_point()#geom_bar(stat="identity",fill="gray78") #+ scale_fill_brewer(palette="Set1") + scale_colour_brewer(palette="Set1") 

sp.graph <- geom.p + labs(x= ("Group"), y =("Log10 (B predicted / B observed )")) + ylim(-9,5) +
  geom_pointrange (limits, position="dodge", width=0.65)
#change name if producing multiple graphs
graph1 <-  sp.graph + theme(plot.title = element_text(size = 12), panel.grid.minor = element_line(colour = NA), panel.background = element_rect(fill = NA, colour = NA),
                             axis.line=element_line(colour = "black"), axis.text.x = element_text(size = 7, colour = "black"), axis.text.y = element_text(size=10, colour = "black")) +
  theme(axis.ticks.x = element_blank(), axis.text.y = element_text(angle=90),axis.text.x = element_text(angle=90, hjust = 1,vjust=0.5)) + #, axis.text.x = theme_blank()) + 
  theme(legend.position = "none") +
  geom_hline(yintercept=1,linetype="dashed") +
  geom_hline(yintercept=-1,linetype="dashed") +
  geom_hline(yintercept=2,linetype="dotted") +
  geom_hline(yintercept=-2,linetype="dotted") 
  


ggsave(graph1, file="logbiomass.emf",dpi=400)

changes.log = change.matrix$mean.change

more.one = changes.log > 1
less.one = changes.log < -1

more.one = more.one[more.one==TRUE]
less.one = less.one[less.one==TRUE]

groups.outside = 100*(1-((length(more.one)+length(less.one))/length(changes.log)))
print(paste(groups.outside, "% within 1 order of magnitude",sep=""))

more.one = changes.log > 2
less.one = changes.log < -2

more.one = more.one[more.one==TRUE]
less.one = less.one[less.one==TRUE]

groups.outside = 100*(1-((length(more.one)+length(less.one))/length(changes.log)))
print(paste(groups.outside, "% within 2 order of magnitude",sep=""))
