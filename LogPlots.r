LogPlots <- function(Infile)
{
  
  Infile <- "log.txt"  #NAME OF LOG.TXT FILE FROM ATLANTIS
  Name_File <- "Group_Names.csv" # FILE CONTAINING NAMES OF FUNCTIONAL GROUPS
    
  StartDay <- 150
  StartYear <- 2008


  #READ LOG.TXT FILE BY STRING INTO VARIABLE 'X'
  data <- readLines(Infile)
 
  #READ IN NAMES OF FUNCTIONAL GROUPS
  Group_Names <- read.table(Name_File, header=TRUE, sep=",") 
  
  
  
  for (i in 1:length(data))
  {
   if (data[i] == "")
   {
      data[i] <- c("-999", "-999", "-999")
   }
  }
  

  #FIND THE NUMBER OF TIMESTEPS IN LOG.TXT
  timesteps <- 0
  for (i in 2:length(data))
  {
    current_row <- strsplit(data[i], split=" ")
    prior_row <- strsplit(data[i-1], split=" ")
    
    if(is.na(current_row[[1]][1]))               
    {
      current_row[[1]][1] <- "-999"
    }
    
    if(is.na(prior_row[[1]][1]))
    {
      prior_row[[1]][1] <- "-999"
    }
  
    if(current_row[[1]][1] == "Time:" & prior_row[[1]][1] != "Time:")
    {
     timesteps <- timesteps + 1
    } 
  }
  
  

  #DECLARE ARRAYS TO HOLD DATA FROM LOG.TXT
  Biomass <- array(dim=c(length(Group_Names[ ,1]), timesteps))  #X=GROUP, Y=TIMESTEP -- HOLD TOTAL BIOMASS
  Res_N <- array(dim=c(length(Group_Names[ ,1]), 10, timesteps)) #X=GROUP, Y=AGECLASS, Z=TIMESTEP -- HOLDS RESERVE NITROGEN
  Str_N <- array(dim=c(length(Group_Names[ ,1]), 10, timesteps)) #X=GROUP, Y=AGECLASS, Z=TIMESTEP -- HOLD STRUCTURAL NITROGEN
  Den <- array(dim=c(length(Group_Names[ ,1]), 10, timesteps)) #X=GROUP, Y=AGECLASS, Z=TIMESTEP -- HOLDS NUMBERS
  Timestep <- 0

  
  #FIND FIRST TIME STEP (BEYOND INITIAL CONDITIONS)
  i <- 1
  current_row <- strsplit(data[i], split=" ")
  while (current_row[[1]][1] != "Time:")
  {
   i <- i + 1
   current_row <- strsplit(data[i], split=" ")
   if(is.na(current_row[[1]][1]))
   {
    current_row[[1]] <- c("-999", "-999", "-999")
   }
  }
  
  i <- i + 1
  current_row <- strsplit(data[i], split=" ")
  while (current_row[[1]][1] == "Time:")
  {
   i <- i + 1
   current_row <- strsplit(data[i], split=" ")
   if(is.na(current_row[[1]][1]))
   {
    current_row[[1]] <- c("-999", "-999", "-999")
   }
  }

  i <- i + 1
  current_row <- strsplit(data[i], split=" ")
  while (current_row[[1]][1] != "Time:")
  {
   i <- i + 1
   current_row <- strsplit(data[i], split=" ")
   if(is.na(current_row[[1]][1] == TRUE))
   {
    current_row[[1]] <- c("-999", "-999", "-999")
   }
  }
  
  #i IS NOW THE ROW NUMBER FOR FIRST TIMESTEP
  #BEGIN READING DATA
  
  for (j in 2:timesteps) #J WILL BE COUNTER FOR TIMESTEPS  
  {
    #BIOMASS DATA WILL BE FIRST IN FILE 
    while (current_row[[1]][1] == "Time:")
    {
      current_grp <- current_row[[1]][4]
      grp_index <- which(Group_Names[ ,1] == current_grp)
   
      if (j == 2) #RECORD INITIAL CONDITIONS
      {
        Biomass[grp_index, 1] <-  current_row[[1]][16]
        Timestep[1] <- 0
      }
   
      Biomass[grp_index, j] <-  current_row[[1]][12]
      tmp <- strsplit(current_row[[1]][2], split=",")
      Timestep[j] <- tmp[[1]][1]

      i <- i + 1
   
      current_row <- strsplit(data[i], split=" ")
      if(is.na(current_row[[1]][1]))
      {
        current_row[[1]] <- c("-999", "-999", "-999")
      }
    }
    
    #NOW READ NITROGEN AND DENSITY NUMBERS
    while (current_row[[1]][1] != "Time:" & i < length(data))
    {
      tmp <- strsplit(current_row[[1]][2], split="-")
      
      if(is.na(tmp))
      {
      	tmp <- "-999"
      }
      
      if (tmp[[1]][1] == "sn")
      {
        current_grp <- strsplit(current_row[[1]][1], split="-")
        grp_index <- which(Group_Names[ ,1] == current_grp[[1]][1])
   
        if (j == 2) #RECORD INITIAL CONDITIONS
        {
          tmp <- strsplit(current_row[[1]][18], split=",")
          Res_N[grp_index, as.numeric(current_grp[[1]][2])+1, 1] <-  tmp[[1]][1]
        
          tmp <- strsplit(current_row[[1]][16], split=",")
          Str_N[grp_index, as.numeric(current_grp[[1]][2])+1, 1] <-  tmp[[1]][1]
        
          tmp <- strsplit(current_row[[1]][20], ")")
          Den[grp_index, as.numeric(current_grp[[1]][2])+1, 1] <-  tmp[[1]][1]
        } 

        tmp <- strsplit(current_row[[1]][11], split=",")
        Res_N[grp_index, as.numeric(current_grp[[1]][2])+1, j] <-  tmp[[1]][1]
       
        tmp <- strsplit(current_row[[1]][9], split=",")
        Str_N[grp_index, as.numeric(current_grp[[1]][2])+1, j] <-  tmp[[1]][1]
        
        Den[grp_index, as.numeric(current_grp[[1]][2])+1, j] <-  current_row[[1]][13]
      }
      
      i <- i + 1
      
      current_row <- strsplit(data[i], split=" ")
      if(is.na(current_row[[1]][1]))
      {
        current_row[[1]] <- c("-999", "-999", "-999")
      }
    } #END OF WHILE LOOP FOR READING NITROGEN AND DENSITY NUMBERS 
   
  } #END OF TIMESTEP LOOP FOR READING NUMBERS FROM LOG.TXT
  
  
  
  #CONVERT STRING ARRAYS TO NUMERIC
  Biomass_Num <- array(as.double(Biomass), dim=dim(Biomass))
  Res_Num <- array(as.double(Res_N), dim=dim(Res_N))
  Str_Num <- array(as.double(Str_N), dim=dim(Str_N))
  Den_Num <- array(as.double(Den), dim=dim(Den))
  Time_Num <- array(as.numeric(Timestep))
  
  
  
  ##################  NOW PLOT DATA  ######################
  
  
  #GET NAME OF DIRECTORY FOR NAMING
  
  dirname <- getwd()
  dirname <- strsplit(dirname, split="/")
  dirlength <- length(dirname[[1]])
  dirname <- dirname[[1]][dirlength]

  
  #BIOMASS
  
  Outfile <- paste("Biomass_Plots_", dirname, ".pdf", sep="")
  pdf(Outfile)  

  ylabel <- "Biomass (Metric Tons)"
  xlabel <- "Year"
  
  k <- 1
  while (k <= length(Group_Names[ , 1]))
  {
    par(mfrow=c(3,2))
    
    for (l in 1:6)
    {
      if (k <= length(Group_Names[ , 1]))
      {
        if(is.na(Biomass_Num[k, 1]))
        {
          Biomass_Num[k, ] <- 0
          plottitle <- paste(Group_Names[k, 5], "\n", Group_Names[k, 1], ", ", Group_Names[k, 4], "\n", "(Not in Model)", sep="")
          ymin <- -1
          ymax <- 1
        }else
        {  
          plottitle <- paste(Group_Names[k, 5], "\n", Group_Names[k, 1], ", ", Group_Names[k, 4], sep="") 
          
          virgin <- array(dim=length(Time_Num), Biomass_Num[k,1]*Group_Names[k, 6])

          ymin <- 0.9 * min(Biomass_Num[k, ], virgin)
          ymax <- 1.1 * max(Biomass_Num[k, ], virgin) 
        }
      
        plot((Time_Num + StartDay)/360 + StartYear, Biomass_Num[k, ], ylim=c(0, ymax), ylab = ylabel, xlab=xlabel, main=plottitle, lwd=3, type='l', cex.lab=1, cex.axis=1, col='black',lty='dashed')
        
        ###NEW###
        virgin <- array(dim=length(Time_Num), Biomass_Num[k,1]*Group_Names[k, 6])
        lines((Time_Num + StartDay)/360 + StartYear, virgin, col="green")
      }
      k <- k+1
    }
  
  }

  dev.off()  

  msg=paste("Biomass Plots Saved To:", Outfile)
  print(msg)

  #RESERVE NITROGEN / INITITAL RESERVE NITROGEN , X=GROUP, Y=AGECLASS, Z=TIMESTEP 
  
  Outfile <- paste("Res_N_Plots_", dirname, ".pdf", sep="")

  pdf(Outfile)  

  ylabel <- "Res_N / Initial Res_N"
  xlabel <- "Year"
  
  rainbowColors<-rainbow(10)
  
  k <- 28
  while (k <= length(Group_Names[ , 1]))
  {
    par(mfrow=c(3,2))
    
    for (l in 1:6)
    {
      if (k <=length(Group_Names[ , 1]))
      { 
        if(is.na(Res_Num[k, 1, 1]))
        {
          Res_Num[k, 1, ] <- 0
          plottitle <- paste(Group_Names[k, 5], "\n", Group_Names[k, 1], ", ", Group_Names[k, 4], "\n", "(Not in Model)", sep="")
          plot((Time_Num + StartDay)/360 + StartYear, Res_Num[k, 1, ], ylab = ylabel, xlab=xlabel, main=plottitle, lwd=1,type='l', cex.lab=1, cex.axis=1, col = rainbowColors[m],lty=m)
        }else
        {
          ymin <- 0.9 * min(Res_Num[k, , ]/Res_Num[k, ,1], na.rm =TRUE) 
          ymax <- 1.1 * max(Res_Num[k, , ]/Res_Num[k, ,1], na.rm =TRUE)
 
          for (m in 1:10)
          {
            if (m == 1)
            {
              plottitle <- paste(Group_Names[k, 5], "\n", Group_Names[k, 1], ", ", Group_Names[k, 4], sep="")
              plot((Time_Num + StartDay)/360 + StartYear, Res_Num[k, m, ]/Res_Num[k, m, 1], ylim=c(0, ymax), ylab = ylabel, xlab=xlabel, main=plottitle, lwd=1,type='l', cex.lab=1, cex.axis=1, col = rainbowColors[m],lty=m)
            }else
            {
              lines((Time_Num + StartDay)/360 + StartYear, Res_Num[k, m, ]/Res_Num[k, m, 1], lwd=1,type='l', col = rainbowColors[m],lty=m)
            }      
          }
        }
      }
      k <- k+1 
    }
  }

  dev.off()  

  msg=paste("Reserve N Plots Saved To:", Outfile)
  print(msg)



  #STRUCTURAL NITROGEN / INITITAL STRUCTURAL NITROGEN , X=GROUP, Y=AGECLASS, Z=TIMESTEP 
  
  Outfile <- paste("Str_N_Plots_", dirname, ".pdf", sep="")
  pdf(Outfile)  

  ylabel <- "Str_N / Initial Str_N"
  xlabel <- "Year"
  
  rainbowColors<-rainbow(10)
  
  k <- 28
  while (k <= length(Group_Names[ , 1]))
  {
    par(mfrow=c(3,2))
    
    for (l in 1:6)
    {
      if (k <=length(Group_Names[ , 1]))
      { 
        if(is.na(Str_Num[k, 1, 1]))
        {
          Str_Num[k, 1, ] <- 0
          plottitle <- paste(Group_Names[k, 5], "\n", Group_Names[k, 1], ", ", Group_Names[k, 4], "\n", "(Not in Model)", sep="")
          plot((Time_Num + StartDay)/360 + StartYear, Str_Num[k, 1, ], ylab = ylabel, xlab=xlabel, main=plottitle, lwd=1,type='l', cex.lab=1, cex.axis=1, col = rainbowColors[m],lty=m)
        }else
        {
          ymin <- 0.9 * min(Str_Num[k, , ]/Str_Num[k, ,1], na.rm =TRUE) 
          ymax <- 1.1 * max(Str_Num[k, , ]/Str_Num[k, ,1], na.rm =TRUE)
 
          for (m in 1:10)
          {
            if (m == 1)
            {
              plottitle <- paste(Group_Names[k, 5], "\n", Group_Names[k, 1], ", ", Group_Names[k, 4], sep="")
              plot((Time_Num + StartDay)/360 + StartYear, Str_Num[k, m, ]/Str_Num[k, m, 1], ylim=c(0, ymax), ylab = ylabel, xlab=xlabel, main=plottitle, lwd=1,type='l', cex.lab=1, cex.axis=1, col = rainbowColors[m],lty=m)
            }else
            {
              lines((Time_Num + StartDay)/360 + StartYear, Str_Num[k, m, ]/Str_Num[k, m, 1], lwd=1,type='l', col = rainbowColors[m],lty=m)
            }      
          }
        }
      }
      k <- k+1 
    }
  }

  dev.off()  

  msg=paste("Structural N Plots Saved To:", Outfile)
  print(msg)




  #DENSITY (NUMBERS) , X=GROUP, Y=AGECLASS, Z=TIMESTEP 
  
  Outfile <- paste("Density_Plots_", dirname, ".pdf", sep="")
  pdf(Outfile)  

  ylabel <- "Numbers"
  xlabel <- "Year"
  
  rainbowColors<-rainbow(10)
  
  k <- 28
  while (k <= length(Group_Names[ , 1]))
  {
    par(mfrow=c(3,2))
    
    for (l in 1:6)
    { 
      if (k <=length(Group_Names[ , 1]))
      {
        if(is.na(Den_Num[k, 1, 1]))
        {
          Den_Num[k, 1, ] <- 0
          plottitle <- paste(Group_Names[k, 5], "\n", Group_Names[k, 1], ", ", Group_Names[k, 4], "\n", "(Not in Model)", sep="")
          plot((Time_Num + StartDay)/360 + StartYear, Den_Num[k, 1, ], ylab = ylabel, xlab=xlabel, main=plottitle, lwd=1,type='l', cex.lab=1, cex.axis=1, col = rainbowColors[m],lty=m)
        }else
        {
          ymin <- 0.9 * min(Den_Num[k, , ], na.rm =TRUE)
          ymax <- 1.1 * max(Den_Num[k, , ], na.rm =TRUE) 
 
          for (m in 1:10)
          {
            if (m == 1)
            {
              plottitle <- paste(Group_Names[k, 5], "\n", Group_Names[k, 1], ", ", Group_Names[k, 4], sep="")
              plot((Time_Num + StartDay)/360 + StartYear, Den_Num[k, m, ], ylim=c(0, ymax), ylab = ylabel, xlab=xlabel, main=plottitle, lwd=1,type='l', cex.lab=1, cex.axis=1, col = rainbowColors[m],lty=m)
            }else
            {
              lines((Time_Num + StartDay)/360 + StartYear, Den_Num[k, m, ], lwd=1,type='l', col = rainbowColors[m],lty=m)
            }      
          }
        }
      }
      k <- k+1 
    }
  }

  dev.off()  
  
  msg=paste("Biomass Plots Saved To:", Outfile)
  print(msg)

  graphics.off()
                    
} #END OF FUNCTION
