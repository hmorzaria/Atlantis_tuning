
#read MortPerPredSpec
#hmorzarialuna@gmail.com

library(ncdf)
library(reshape)
#library(sm)  # for pause()
library(grDevices)
library(gridExtra)
library(RColorBrewer)
library(gdata)
library(reshape2)

rm(list=ls())

my.directory = "C:/Atlantis/Dropbox/1Archivos/NOAA/Atlantis model files/Atlantis_NEW_code/Plot code"
setwd(my.directory)
#Prey mortality and predator consumption are calculated for this species
species.prey.predator = "FVD"

#PRMS
folders = c("OUT_AtlantisGOC_53yr_SpinUp_10")

#Output interval 
out.int = 730 #days

Infile1 = "C:/Atlantis/Dropbox/Spin_up_runs_2015/outputs_Nov_30_2015/OUT_AtlantisGOC_53yr_SpinUp_10"
#Infile2 = "E:/AtlantisV2/SpinUp_53yrs_V2/outputs_SpinUp_Apr2_2015/OUT_AtlantisGOC_53yr_SpinUp_8"
#Infile3 = "E:/AtlantisV2/SpinUp_53yrs_V2/outputs_SpinUp_Apr2_2015/OUT_AtlantisGOC_53yr_SpinUp_9"
#Infile4 = "E:/AtlantisV2/SpinUp_53yrs_V2/outputs_SpinUp_Apr2_2015/OUT_AtlantisGOC_53yr_SpinUp_10"

file.list <- c(Infile1)#,Infile5,Infile6,Infile7,Infile8,Infile9,Infile10,Infile11,Infile12)


group.names = read.csv("atlantis_groups.csv",header=TRUE)
names(group.names) = c("name","nombre",  "nombre_abv",	"Atlantis_group","species")
all.groups = levels(group.names$species)

#prey

#prey species
this.species = c(species.prey.predator) #,"PIN","WHS"

#predators

pred.species = c(species.prey.predator)#,"PIN","WHS"


Mort = as.data.frame(matrix(0,nrow=0,ncol=69))

for (eachfolder in 1:length(file.list)) {

  filepath = file.list[eachfolder]

setwd(filepath)

This.mort = read.table("NGulfOutMortPerPred.txt",header=T,skip=1,fill=TRUE) #nrows = 2 use this to only read certain rows

new.mort = This.mort[which(This.mort$Time!="Time:"),] #eliminate header rows
new.mort = drop.levels(new.mort)
last.step = length(levels(new.mort$Time))
last.time = levels(new.mort$Time)[last.step]
last.time.step = This.mort[which(This.mort$Time==last.time),] #last time step

print("Analyzing")
print(file.list[eachfolder])

for (eachspecies in 1:length(this.species)){

last.each = last.time.step[which(last.time.step$Prey==this.species[eachspecies]),]
  
#this.species.mort= last.time.step[, grep(this.species[eachspecies], names(last.time.step))] #subsets all columns with SSK

#this.species.mort = cbind(this.species.mort,last.time.step$Prey)

#max.mort = apply(last.each, 2, max) 

#print(paste(this.species[eachspecies],"mortality", sep ="_"))
#print(max.mort)

print("Maximum predation")
#max.col = which(max.mort == max(max.mort), arr.ind = TRUE)
#print(names(max.col))rint(max(max.mort))

last.each$PRM = file.list[eachfolder]

Mort = rbind(Mort,last.each)
}
setwd(my.directory)


    }


#print(Mort)
Mort2 = melt(Mort,id.vars=c("Time","Prey","startN","mL","mQ","implicitMortality","TotalPredMort","PRM"))
require(ggplot2)
names(Mort2) = c("Time","Prey","startN","mL","mQ","implicitMortality","TotalPredMort","PRM","species","predation.mortality")

write.csv(Mort2, file=paste(this.species[eachspecies],"MortSpec.csv",sep="_"))

p <- ggplot(Mort2, aes(x = species, y = predation.mortality, fill=PRM, colour=PRM)) 
geom.p <- p + geom_bar(stat="identity", position="dodge") #+ scale_fill_brewer(palette="Set1") + scale_colour_brewer(palette="Set1") 

sp.graph <- geom.p + labs(x= ("Species"), y =("Total predation mortality")) + ggtitle(paste("Mortality",this.species[eachspecies],sep=":")) #+


graph1 <-  sp.graph + theme(plot.title = element_text(size = 12), panel.grid.minor = element_line(colour = "grey"), panel.background = element_rect(fill = NA, colour = NA),
                             axis.line=element_line(colour = "black"), axis.text.x = element_text(vjust = 0.1, size = 6, colour = "black"), axis.text.y = element_text(hjust=1, size=9, colour = "black")) +
  theme(axis.text.y = element_text(angle=90),axis.text.x = element_text(angle=90)) + #, axis.text.x = theme_blank()) + 
  theme(legend.position = "none")
X11()
print(graph1)

setwd(my.directory)
ggsave(graph1, file=paste("Prey_mortality_",this.species[eachspecies],".png",sep=""),dpi=300)


#use this to get data from predators

Prey.Mort = as.data.frame(matrix(0,nrow=0,ncol=6))

for (eachfolder in 1:length(file.list)) {
  
  filepath = file.list[eachfolder]
  
  setwd(filepath)
  
  This.mort = read.table("NGulfOutMortPerPred.txt",header=T,skip=1,fill=TRUE) #nrows = 2 use this to only read certain rows
  
  last.time.step = This.mort[which(This.mort$Time=="1.970950e+004"),] #last time step
  last.time.step = drop.levels (last.time.step)
  print("Analyzing")
  print(file.list[eachfolder])
  
  for (predspecies in 1:length(pred.species)){
    
    last.each = last.time.step[,pred.species[predspecies]]
    
    prey.mort = as.data.frame(last.each)
    prey.mort$Time = last.time.step$Time
    prey.mort$Prey =last.time.step$Prey
    
    prey.mort$PRM = file.list[eachfolder]
    prey.mort$predator = pred.species[predspecies]
    Prey.Mort = rbind(Prey.Mort,prey.mort)
  }
 setwd(my.directory)

   
}

str(Prey.Mort)


Prey.Mort$last.each[Prey.Mort$last.each==0] = NA
Prey.Mort2 = na.omit(Prey.Mort)
Prey.Mort2= drop.levels(Prey.Mort2)


print(head(Prey.Mort2))

Prey.Mort3 = aggregate(Prey.Mort2$last.each,by=list(Prey.Mort2$Prey,Prey.Mort2$PRM,Prey.Mort2$predator),FUN=mean, na.rm=TRUE)
names(Prey.Mort3) = c("Prey","PRM","predator","mortality")

Prey.Mort3$PRM = as.factor(Prey.Mort3$PRM)
Prey.Mort3$predator = as.factor(Prey.Mort3$predator)

write.csv(Prey.Mort3, file=paste(this.species[eachspecies],"PredCon.csv",sep="_"))

require(ggplot2)
p <- ggplot(Prey.Mort3, aes(x = Prey, y = mortality, fill=PRM, colour=PRM))
geom.p <- p + geom_bar(stat="identity",position="dodge") #+ scale_fill_brewer(palette="Set1") + scale_colour_brewer(palette="Set1") 

sp.graph <- geom.p + labs(x= ("Prey"), y =("Mean predation mortality")) + ggtitle(paste("Predator:",pred.species[predspecies],sep=" ")) #+

graph1 <-  sp.graph + theme(plot.title = element_text(size = 12), panel.grid.minor = element_line(colour = "grey"), panel.background = element_rect(fill = NA, colour = NA),
                            axis.line=element_line(colour = "black"), axis.text.x = element_text(vjust = 0.2, size = 7, colour = "black"), axis.text.y = element_text(hjust=1, size=9, colour = "black")) +
  theme(axis.text.y = element_text(angle=90),axis.text.x = element_text(angle=90)) + #, axis.text.x = theme_blank()) + 
  
X11()
print(graph1)

setwd(my.directory)

ggsave(graph1, file=paste("Predator_consumption_",pred.species[predspecies],".png",sep=""),dpi=300)
