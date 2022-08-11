## Summary tables ----

#read in resampled data (first number in file is effort, second is the random seed)
#S mono data
mySmonoDistResults <- read.table("dataSim/resampleSmono/distResults-300_1.txt")
Plotresults1m <- read.table("dataSim/resampleSmono/plot1m-300_1.txt")
Plotresults4m <- read.table("dataSim/resampleSmono/plot4m-300_1.txt")

#S quad data

mySquadDistResults <- read.table("dataSim/resampleSquad/distResults-360_1.txt")
PlotresultsSQ <- read.table("dataSim/resampleSquad/plot-360_1.txt")

#count number of units where model couldn't be fitted for ds, and where p couldn't be estimated for double obs plots

problemsSM <- data.frame("method" = character(7),
                       "prop fail" = numeric(7),
                       "mean p" = numeric(7),
                       "S" = integer(7),
                       "mean n_t" = numeric(7))
problemsSQ <- data.frame("method" = character(4),
                       "prop fail" = numeric(4),
                       "mean p" = numeric(4),
                       "S" = integer(4),
                       "mean n_t" = numeric(4))

#SM fill in the table
problemsSM$method <- c("LTS", "Opt", "GrB", 
                       "single1m", "double1m",
                       "single4m", "double4m")

#prepare summaries for 'prop fail' column
dsPropfailSM <- aggregate(mySmonoDistResults$estD[!is.na(mySmonoDistResults$estD)], by = list(mySmonoDistResults$method[!is.na(mySmonoDistResults$estD)]), FUN = length)

problemsSM$prop.fail[1] <- 1- dsPropfailSM$x[dsPropfailSM$Group.1=="LTS"]/100
problemsSM$prop.fail[2] <- 1- dsPropfailSM$x[dsPropfailSM$Group.1=="Opt"]/100
problemsSM$prop.fail[3] <- 1- dsPropfailSM$x[dsPropfailSM$Group.1=="GrB"]/100

problemsSM$prop.fail[4] <- NA
problemsSM$prop.fail[5] <- sum(is.na(Plotresults1m$probOverall))/100
problemsSM$prop.fail[6] <- NA
problemsSM$prop.fail[7] <- sum(is.na(Plotresults4m$probOverall))/100

#mean p column
dsPSM <- aggregate(mySmonoDistResults$p[!is.na(mySmonoDistResults$p)], by = list(mySmonoDistResults$method[!is.na(mySmonoDistResults$p)]), FUN = mean)

problemsSM$mean.p[1] <- dsPSM$x[dsPSM$Group.1=="LTS"]
problemsSM$mean.p[2] <- dsPSM$x[dsPSM$Group.1=="Opt"]
problemsSM$mean.p[3] <- dsPSM$x[dsPSM$Group.1=="GrB"]

problemsSM$mean.p[4] <- NA
problemsSM$mean.p[5] <- mean(Plotresults1m$probOverall[!is.na(Plotresults1m$probOverall)])
problemsSM$mean.p[6] <- NA
problemsSM$mean.p[7] <- mean(Plotresults4m$probOverall[!is.na(Plotresults4m$probOverall)])

#S and nT
data <- read.table("dataSim/resampleSmono/resampleLTS/resamp1.txt")

#unitCount function makes a table of one row per resample, gives the n units in that resample and the total counts of individuals detected (single and double obs for plots, total n for all distance methods)

unitCount <- function(folder1, folder2, filename1, filename2, method){
  #data1 is the plot resample OR the object table for distance data
  data1 <- read.table(paste0("dataSim/", folder1, "/", folder2, "/", filename1, ".txt"))
  if(method=="plot"){
    nUnits <- length(data1$plotID)
    count1 <- sum(data1$kkPrimaryCount+data1$SecObsPrimaryCount, na.rm = TRUE)
    countAll <- sum(data1$kkPrimaryCount+data1$kkSecondaryCount+data1$SecObsPrimaryCount+data1$SecObsSecondaryCount, na.rm = TRUE)
   vector <- c(nUnits, count1, countAll) 
  } else { 
    #data2 is the sample table for distance data - object table will only contain transect id for transects with observations.  sample table will include all transects in the sample
    data2 <-  read.table(paste0("dataSim/", folder1, "/", folder2, "/", filename2, ".txt")) 
    nUnits <- length(data2$Sample.Label)
    count1 <- length(data1$object)
    vector <- c(nUnits, count1)
    
  }
  return(vector)
}

LTSunits_Smono <- data.frame("round" = 1:100, "nUnits" = integer(100), "nCount" = integer(100))
OptUnits_Smono <- data.frame("round" = 1:100, "nUnits" = integer(100), "nCount" = integer(100))
GrBunits_Smono <- data.frame("round" = 1:100, "nUnits" = integer(100), "nCount" = integer(100))

for(i in 1:100){
  filename1 <- paste0("resampObj", i)
  filename2 <- paste0("resampSamp", i)
  info <- unitCount(folder1 = "resampleSmono", folder2 = "resampleGrB", filename1 = filename1, filename2 = filename2, method = "ds")
  GrBunits_Smono[i, 2:3] <- info
}



#nt

#SQ fill in the table
problemsSQ$method <- c("LTS", "Opt",  
                       "plot single", "plot double")

#prepare summaries for 'prop fail' column
dsPropfailSQ <- aggregate(mySquadDistResults$estD[!is.na(mySquadDistResults$estD)], by = list(mySquadDistResults$method[!is.na(mySquadDistResults$estD)]), FUN = length)

problemsSQ$prop.fail[1] <- 1- dsPropfailSQ$x[dsPropfailSQ$Group.1=="LTS"]/100
problemsSQ$prop.fail[2] <- 1- dsPropfailSQ$x[dsPropfailSQ$Group.1=="Opt"]/100

problemsSQ$prop.fail[3] <- NA
problemsSQ$prop.fail[4] <- sum(is.na(PlotresultsSQ$probOverall))/100

#mean p column
dsPSQ <- aggregate(mySquadDistResults$p[!is.na(mySquadDistResults$p)], by = list(mySquadDistResults$method[!is.na(mySquadDistResults$p)]), FUN = mean)

problemsSQ$mean.p[1] <- dsPSQ$x[dsPSQ$Group.1=="LTS"]
problemsSQ$mean.p[2] <- dsPSQ$x[dsPSQ$Group.1=="Opt"]

problemsSQ$mean.p[3] <- NA
problemsSQ$mean.p[4] <- mean(PlotresultsSQ$probOverall[!is.na(PlotresultsSQ$probOverall)])

#S

LTSunits_Squad <- data.frame("round" = 1:100, "nUnits" = integer(100), "nCount" = integer(100))
Optunits_Squad <- data.frame("round" = 1:100, "nUnits" = integer(100), "nCount" = integer(100))

for(i in 1:100){
  filename1 <- paste0("resampObj", i)
  filename2 <- paste0("resampSamp", i)
  info <- unitCount(folder1 = "resampleSquad", folder2 = "resampleLTS", filename1 = filename1, filename2 = filename2, method = "ds")
  LTSunits_Squad[i, 2:3] <- info
}

#nt

