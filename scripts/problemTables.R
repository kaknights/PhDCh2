## Summary tables ----

#read in resampled data (first number in file is effort, second is the random seed)
#S mono data
mySmonoDistResults <- read.table("dataSim/resampleSmono/distResults-300_1.txt")
Plotresults1m <- read.table("dataSim/resampleSmono/plot1m-300_1.txt")
Plotresults4m <- read.table("dataSim/resampleSmono/plot4m-300_1.txt")

#S quad data

mySquadDistResults1 <- read.table("dataSim/resampleSquad/distResults-275_1.txt")
PlotresultsSQ1 <- read.table("dataSim/resampleSquad/plot-275_1.txt")

#Squad data (with replacement)
mySquadDistResults2 <- read.table("dataSim/resampleSquad/distResults-360_1Repl.txt")
PlotresultsSQ2 <- read.table("dataSim/resampleSquad/plot-360_1Repl.txt")


#count number of units where model couldn't be fitted for ds, and where p couldn't be estimated for double obs plots

problemsSM <- data.frame("method" = character(7),
                       "prop fail" = numeric(7),
                       "mean p" = numeric(7),
                       "S" = integer(7),
                       "mean n_t" = numeric(7))
problemsSQ <- data.frame("method" = character(8),
                       "prop fail" = numeric(8),
                       "mean p" = numeric(8),
                       "S" = integer(8),
                       "mean n_t" = numeric(8))

#SM fill in the table
problemsSM$method <- c("LTS", "Opt", "GrB", 
                       "single1m", "double1m",
                       "single4m", "double4m")

# SM prop fail ----
#prepare summaries for 'prop fail' column
dsPropfailSM <- aggregate(mySmonoDistResults$estD[!is.na(mySmonoDistResults$estD)], by = list(mySmonoDistResults$method[!is.na(mySmonoDistResults$estD)]), FUN = length)

problemsSM$prop.fail[1] <- 1- dsPropfailSM$x[dsPropfailSM$Group.1=="LTS"]/100
problemsSM$prop.fail[2] <- 1- dsPropfailSM$x[dsPropfailSM$Group.1=="Opt"]/100
problemsSM$prop.fail[3] <- 1- dsPropfailSM$x[dsPropfailSM$Group.1=="GrB"]/100

problemsSM$prop.fail[4] <- NA
problemsSM$prop.fail[5] <- sum(is.na(Plotresults1m$probOverall))/100
problemsSM$prop.fail[6] <- NA
problemsSM$prop.fail[7] <- sum(is.na(Plotresults4m$probOverall))/100

# SM mean p ----
dsPSM <- aggregate(mySmonoDistResults$p[!is.na(mySmonoDistResults$p)], by = list(mySmonoDistResults$method[!is.na(mySmonoDistResults$p)]), FUN = mean)

problemsSM$mean.p[1] <- round(dsPSM$x[dsPSM$Group.1=="LTS"], 2)
problemsSM$mean.p[2] <- round(dsPSM$x[dsPSM$Group.1=="Opt"], 2)
problemsSM$mean.p[3] <- round(dsPSM$x[dsPSM$Group.1=="GrB"], 2)

problemsSM$mean.p[4] <- NA
problemsSM$mean.p[5] <- round(mean(Plotresults1m$probOverall[!is.na(Plotresults1m$probOverall)]), 2)
problemsSM$mean.p[6] <- NA
problemsSM$mean.p[7] <- round(mean(Plotresults4m$probOverall[!is.na(Plotresults4m$probOverall)]), 2)

#se p
problemsSM$se.p <- numeric(7)
dsPSM <- aggregate(mySmonoDistResults$p[!is.na(mySmonoDistResults$p)], by = list(mySmonoDistResults$method[!is.na(mySmonoDistResults$p)]), FUN = sd)

problemsSM$se.p[1] <- round(dsPSM$x[dsPSM$Group.1=="LTS"], 3)
problemsSM$se.p[2] <- round(dsPSM$x[dsPSM$Group.1=="Opt"], 3)
problemsSM$se.p[3] <- round(dsPSM$x[dsPSM$Group.1=="GrB"], 3)

problemsSM$se.p[4] <- NA
problemsSM$se.p[5] <- round(sd(Plotresults1m$probOverall[!is.na(Plotresults1m$probOverall)]), 3)
problemsSM$se.p[6] <- NA
problemsSM$se.p[7] <- round(sd(Plotresults4m$probOverall[!is.na(Plotresults4m$probOverall)]), 3)

#SM S and nt ----
#data <- read.table("dataSim/resampleSmono/resampleLTS/resamp1.txt")

#unitCount function makes a table of one row per resample, gives the n units in that resample and the total counts of individuals detected (single and double obs for plots, total n for all distance methods)

unitCount <- function(folder1, folder2, filename1, filename2, method){
  #data1 is the plot resample OR the object table for distance data
  data1 <- read.table(paste0("dataSim/", folder1, "/", folder2, "/", filename1, ".txt"))
  if(method=="plot"){
    nUnits <- length(data1$plotID)
    count1 <- sum(data1$kkPrimaryCount, na.rm = TRUE)+sum(data1$SecObsPrimaryCount, na.rm = TRUE)
    countAll <- sum(data1$kkPrimaryCount, na.rm = TRUE)+sum(data1$kkSecondaryCount, na.rm = TRUE)+sum(data1$SecObsPrimaryCount, na.rm = TRUE)+sum(data1$SecObsSecondaryCount, na.rm = TRUE)
   vector <- c(nUnits, count1, countAll) 
  } else { 
    #data2 is the sample table for distance data - object table will only contain transect id for transects with observations.  sample table will include all transects in the sample
    data2 <-  read.table(paste0("dataSim/", folder1, "/", folder2, "/", filename2, ".txt")) 
    nUnits <- length(data2$transectID)
    count1 <- length(data1$obsID)
    vector <- c(nUnits, count1)
    
  }
  return(vector)
}

#make data frames to store info from saved resamples
LTSunits_Smono <- data.frame("round" = 1:100, "nUnits" = integer(100), "nCount" = integer(100))
for(i in 1:100){
  filename1 <- paste0("resampData", i)
  filename2 <- paste0("resampDetails", i)
  info <- unitCount(folder1 = "resampleSmono", folder2 = "resampleLTS", filename1 = filename1, filename2 = filename2, method = "ds")
  LTSunits_Smono[i, 2:3] <- info
}

OptUnits_Smono <- data.frame("round" = 1:100, "nUnits" = integer(100), "nCount" = integer(100))
for(i in 1:100){
  filename1 <- paste0("resampData", i)
  filename2 <- paste0("resampDetails", i)
  info <- unitCount(folder1 = "resampleSmono", folder2 = "resampleOpt", filename1 = filename1, filename2 = filename2, method = "ds")
  OptUnits_Smono[i, 2:3] <- info
}

GrBunits_Smono <- data.frame("round" = 1:100, "nUnits" = integer(100), "nCount" = integer(100))
for(i in 1:100){
  filename1 <- paste0("resampData", i)
  filename2 <- paste0("resampDetails", i)
  info <- unitCount(folder1 = "resampleSmono", folder2 = "resampleGrB", filename1 = filename1, filename2 = filename2, method = "ds")
  GrBunits_Smono[i, 2:3] <- info
}


plot1sUnits_Smono <- data.frame("round" = 1:100, "nUnits" = integer(100), "singleCount" = integer(100), "nAll" = integer(100))
for(i in 1:100){
  filename1 <- paste0("resample-300_1_single", i)
  filename2 <- paste0("resample-300_1_single", i)
  info <- unitCount(folder1 = "resampleSmono", folder2 = "resampleData1m", filename1 = filename1, filename2 = filename2, method = "plot")
  plot1sUnits_Smono[i, 2:4] <- info
}
plot1dUnits_Smono <- data.frame("round" = 1:100, "nUnits" = integer(100), "singleCount" = integer(100), "nAll" = integer(100))
for(i in 1:100){
  filename1 <- paste0("resample-300_1_double", i)
  filename2 <- paste0("resample-300_1_double", i)
  info <- unitCount(folder1 = "resampleSmono", folder2 = "resampleData1m", filename1 = filename1, filename2 = filename2, method = "plot")
  plot1dUnits_Smono[i, 2:4] <- info
}
plot4sUnits_Smono <- data.frame("round" = 1:100, "nUnits" = integer(100), "singleCount" = integer(100), "nAll" = integer(100))
for(i in 1:100){
  filename1 <- paste0("resample-300_1_single", i)
  filename2 <- paste0("resample-300_1_single", i)
  info <- unitCount(folder1 = "resampleSmono", folder2 = "resampleData4m", filename1 = filename1, filename2 = filename2, method = "plot")
  plot4sUnits_Smono[i, 2:4] <- info
}

plot4dUnits_Smono <- data.frame("round" = 1:100, "nUnits" = integer(100), "singleCount" = integer(100), "nAll" = integer(100))
for(i in 1:100){
  filename1 <- paste0("resample-300_1_double", i)
  filename2 <- paste0("resample-300_1_double", i)
  info <- unitCount(folder1 = "resampleSmono", folder2 = "resampleData4m", filename1 = filename1, filename2 = filename2, method = "plot")
  plot4dUnits_Smono[i, 2:4] <- info
}

#fill in problem table with S and n
problemsSM$S <- c(14, 21, 12, 25, 13, 18, 9)
problemsSM$mean.n_t[problemsSM$method=="LTS"] <- round(mean(LTSunits_Smono$nCount))
problemsSM$mean.n_t[problemsSM$method=="Opt"] <- round(mean(OptUnits_Smono$nCount))
problemsSM$mean.n_t[problemsSM$method=="GrB"] <- round(mean(GrBunits_Smono$nCount))
problemsSM$mean.n_t[problemsSM$method=="single1m"] <- round(mean(plot1sUnits_Smono$singleCount))
problemsSM$mean.n_t[problemsSM$method=="double1m"] <- round(mean(plot1dUnits_Smono$nAll))
problemsSM$mean.n_t[problemsSM$method=="single4m"] <- round(mean(plot4sUnits_Smono$singleCount))
problemsSM$mean.n_t[problemsSM$method=="double4m"] <- round(mean(plot4dUnits_Smono$nAll))

write.csv(problemsSM, "dataSim/problemsSM.csv")

#SQ prop fail ----
problemsSQ$method <- c("LTS", "Opt",  
                       "plotS", "plotD", "LTSr", "Optr",  
                       "plotSr", "plotDr")

#prepare summaries for 'prop fail' column
dsPropfailSQ <- aggregate(mySquadDistResults1$estD[!is.na(mySquadDistResults1$estD)], by = list(mySquadDistResults1$method[!is.na(mySquadDistResults1$estD)]), FUN = length)

problemsSQ$prop.fail[1] <- 1- dsPropfailSQ$x[dsPropfailSQ$Group.1=="LTS"]/100
problemsSQ$prop.fail[2] <- 1- dsPropfailSQ$x[dsPropfailSQ$Group.1=="Opt"]/100

problemsSQ$prop.fail[3] <- NA
problemsSQ$prop.fail[4] <- sum(is.na(PlotresultsSQ1$probOverall))/100

dsPropfailSQ2 <- aggregate(mySquadDistResults2$estD[!is.na(mySquadDistResults2$estD)], by = list(mySquadDistResults2$method[!is.na(mySquadDistResults2$estD)]), FUN = length)

problemsSQ$prop.fail[5] <- 1- dsPropfailSQ2$x[dsPropfailSQ2$Group.1=="LTS"]/100
problemsSQ$prop.fail[6] <- 1- dsPropfailSQ2$x[dsPropfailSQ2$Group.1=="Opt"]/100

problemsSQ$prop.fail[7] <- NA
problemsSQ$prop.fail[8] <- sum(is.na(PlotresultsSQ2$probOverall))/100

#SQ mean p ----
dsPSQ <- aggregate(mySquadDistResults1$p[!is.na(mySquadDistResults1$p)], by = list(mySquadDistResults1$method[!is.na(mySquadDistResults1$p)]), FUN = mean)

problemsSQ$mean.p[1] <- dsPSQ$x[dsPSQ$Group.1=="LTS"]
problemsSQ$mean.p[2] <- dsPSQ$x[dsPSQ$Group.1=="Opt"]

problemsSQ$mean.p[3] <- NA
problemsSQ$mean.p[4] <- mean(PlotresultsSQ1$probOverall[!is.na(PlotresultsSQ1$probOverall)])

dsPSQ2 <- aggregate(mySquadDistResults2$p[!is.na(mySquadDistResults2$p)], by = list(mySquadDistResults2$method[!is.na(mySquadDistResults2$p)]), FUN = mean)

problemsSQ$mean.p[5] <- dsPSQ2$x[dsPSQ$Group.1=="LTS"]
problemsSQ$mean.p[6] <- dsPSQ2$x[dsPSQ$Group.1=="Opt"]

problemsSQ$mean.p[7] <- NA
problemsSQ$mean.p[8] <- mean(PlotresultsSQ2$probOverall[!is.na(PlotresultsSQ2$probOverall)])

#se p
problemsSQ$se.p <- numeric(8)

dsSePSQ <- aggregate(mySquadDistResults1$p[!is.na(mySquadDistResults1$p)], by = list(mySquadDistResults1$method[!is.na(mySquadDistResults1$p)]), FUN = sd)

problemsSQ$se.p[1] <- dsSePSQ$x[dsPSQ$Group.1=="LTS"]
problemsSQ$se.p[2] <- dsSePSQ$x[dsPSQ$Group.1=="Opt"]

problemsSQ$se.p[3] <- NA
problemsSQ$se.p[4] <- sd(PlotresultsSQ1$probOverall[!is.na(PlotresultsSQ1$probOverall)])

dsSePSQ2 <- aggregate(mySquadDistResults2$p[!is.na(mySquadDistResults2$p)], by = list(mySquadDistResults2$method[!is.na(mySquadDistResults2$p)]), FUN = sd)

problemsSQ$se.p[5] <- dsSePSQ2$x[dsPSQ$Group.1=="LTS"]
problemsSQ$se.p[6] <- dsSePSQ2$x[dsPSQ$Group.1=="Opt"]

problemsSQ$se.p[7] <- NA
problemsSQ$se.p[8] <- sd(PlotresultsSQ2$probOverall[!is.na(PlotresultsSQ2$probOverall)])

#SQ S and nt ----

LTSunits_Squad <- data.frame("round" = 1:100, "nUnits" = integer(100), "nCount" = integer(100))
for(i in 1:100){
  filename1 <- paste0("resampData", i, "half")
  filename2 <- paste0("resampDetails", i, "half")
  info <- unitCount(folder1 = "resampleSquad", folder2 = "resampleLTS", filename1 = filename1, filename2 = filename2, method = "ds")
  LTSunits_Squad[i, 2:3] <- info
}

OptUnits_Squad <- data.frame("round" = 1:100, "nUnits" = integer(100), "nCount" = integer(100))
for(i in 1:100){
  filename1 <- paste0("resampData", i, "half")
  filename2 <- paste0("resampDetails", i, "half")
  info <- unitCount(folder1 = "resampleSquad", folder2 = "resampleOpt", filename1 = filename1, filename2 = filename2, method = "ds")
  OptUnits_Squad[i, 2:3] <- info
}

LTSrunits_Squad <- data.frame("round" = 1:100, "nUnits" = integer(100), "nCount" = integer(100))
for(i in 1:100){
  filename1 <- paste0("resampData", i, "Repl")
  filename2 <- paste0("resampDetails", i, "Repl")
  info <- unitCount(folder1 = "resampleSquad", folder2 = "resampleLTS", filename1 = filename1, filename2 = filename2, method = "ds")
  LTSrunits_Squad[i, 2:3] <- info
}

OptrUnits_Squad <- data.frame("round" = 1:100, "nUnits" = integer(100), "nCount" = integer(100))
for(i in 1:100){
  filename1 <- paste0("resampData", i, "Repl")
  filename2 <- paste0("resampDetails", i, "Repl")
  info <- unitCount(folder1 = "resampleSquad", folder2 = "resampleLTS", filename1 = filename1, filename2 = filename2, method = "ds")
  OptrUnits_Squad[i, 2:3] <- info
}

plotSqSUnits_Squad <- data.frame("round" = 1:100, "nUnits" = integer(100), "singleCount" = integer(100), "nAll" = integer(100))
for(i in 1:100){
  filename1 <- paste0("resample-275_1_single", i)
  filename2 <- paste0("resample-275_1_single", i)
  info <- unitCount(folder1 = "resampleSquad", folder2 = "resamplePlot", filename1 = filename1, filename2 = filename2, method = "plot")
  plotSqSUnits_Squad[i, 2:4] <- info
}

plotSqDUnits_Squad <- data.frame("round" = 1:100, "nUnits" = integer(100), "singleCount" = integer(100), "nAll" = integer(100))
for(i in 1:100){
  filename1 <- paste0("resample-275_1_double", i)
  filename2 <- paste0("resample-275_1_double", i)
  info <- unitCount(folder1 = "resampleSquad", folder2 = "resamplePlot", filename1 = filename1, filename2 = filename2, method = "plot")
  plotSqDUnits_Squad[i, 2:4] <- info
}

plotSqSrUnits_Squad <- data.frame("round" = 1:100, "nUnits" = integer(100), "singleCount" = integer(100), "nAll" = integer(100))
for(i in 1:100){
  filename1 <- paste0("resample-360_1_single", i, "Repl")
  filename2 <- paste0("resample-360_1_single", i, "Repl")
  info <- unitCount(folder1 = "resampleSquad", folder2 = "resamplePlot", filename1 = filename1, filename2 = filename2, method = "plot")
  plotSqSrUnits_Squad[i, 2:4] <- info
}

plotSqDrUnits_Squad <- data.frame("round" = 1:100, "nUnits" = integer(100), "singleCount" = integer(100), "nAll" = integer(100))
for(i in 1:100){
  filename1 <- paste0("resample-360_1_double", i, "Repl")
  filename2 <- paste0("resample-360_1_double", i, "Repl")
  info <- unitCount(folder1 = "resampleSquad", folder2 = "resamplePlot", filename1 = filename1, filename2 = filename2, method = "plot")
  plotSqDrUnits_Squad[i, 2:4] <- info
}

# n units column is reading 0 for last two dataframes, because the column isn't called plotID.  So just looking manually:
folder1 <- "resampleSquad"
folder2 <- "resamplePlot"

testData <- read.table(paste0("dataSim/", folder1, "/", folder2, "/", filename2, ".txt"))
nUnits <- length(testData$samp)
    
#suspicious of n units for ds samples with replacement
folder1 <- "resampleSquad"
folder2 <- "resampleLTS"

filename <- "resampDetails62half"

testData <- read.table("dataSim/resampleSquad/resampleOpt/resampDetails62Repl.txt")
nUnits <- length(testData$transectID)

#fill in problem table with S and n
problemsSQ$S <- c(4, 6, 30, 16,5, 8, 40, 20)
problemsSQ$mean.n_t[problemsSQ$method=="LTS"] <- round(mean(LTSunits_Squad$nCount))
problemsSQ$mean.n_t[problemsSQ$method=="Opt"] <- round(mean(OptUnits_Squad$nCount))
problemsSQ$mean.n_t[problemsSQ$method=="LTSr"] <- round(mean(LTSrunits_Squad$nCount))
problemsSQ$mean.n_t[problemsSQ$method=="Optr"] <- round(mean(OptrUnits_Squad$nCount))

problemsSQ$mean.n_t[problemsSQ$method=="plotS"] <- round(mean(plotSqSUnits_Squad$singleCount))
problemsSQ$mean.n_t[problemsSQ$method=="plotD"] <- round(mean(plotSqDUnits_Squad$nAll))
problemsSQ$mean.n_t[problemsSQ$method=="plotSr"] <- round(mean(plotSqSrUnits_Squad$singleCount))
problemsSQ$mean.n_t[problemsSQ$method=="plotDr"] <- round(mean(plotSqDrUnits_Squad$nAll))

write.csv(problemsSQ, "dataSim/problemsSQ.csv")
