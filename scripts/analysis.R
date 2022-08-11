# Analysis: ---- A comparison of the performance of distance- and plot-based methods for surveys of high density species: a wildflower case study

#NOTES: need to add in variations for S quad resampling with and without replacement (save seeds etc for reproducibility)
#commit to git hub first.

source("scripts/summaries.R")
#source("scripts/resampleFunction.R")
source("scripts/resampleDistFunc.R")

library(Distance); library(viridis)
mySeed <- 1 #used in naming files below

seed <- read.table("dataSim/listOfSeeds.txt") #only used when sims need to be run out of sequence (recovers the random seed)

# Stackhousia distance ----

# Resamples take a subset of LTS distance survey data, runs distance analysis using LTS, Opt and Grouped methods using the same data (Opt method uses additional datafrom the LTS dataset)
# resampleMany function takes LTS dataframes, creates tables required by ds functionin distance package (for LTS, Opt and GrB methods), runs distance models, stores results in a list.  n = number of resamples, effort = survey time in minutes

smEffort <- 300
set.seed(mySeed)
SmonoDistTimeStart <- Sys.time()
try1 <- resampleMany(n = 100, details = mySmonoLTS_details, 
                     data = mySmonoLTS, effort = smEffort, times = times, 
                     target = "Stackhousia", Cm = Cm_Smono, 
                     Cw = Cw_Smono, w = 2)
SmonoDistTimeEnd <- Sys.time()

(SmonoDistProcessTime <- as.numeric(SmonoDistTimeEnd-SmonoDistTimeStart))

#make table for key results (density and se estimates)
mySmonoDistResults <- data.frame("method" = rep(c("LTS", "Opt", "GrB"), each = 100),
                            "idx" = rep(c(1:100), times = 3),
                            "estD" = numeric(300),
                            "seD" = numeric(300),
                            "cvD" = numeric(300),
                            "p" = numeric(300))

#populate results from resample lists
for(i in 1:100){
  if(length(try1[[1]][[i]][[1]][[4]]$object)>60){
    #results for LTS
    mySmonoDistResults$estD[mySmonoDistResults$method == "LTS" & 
                            mySmonoDistResults$idx == i] <- try1[[2]][[i]][[1]]$dht$individuals$D$Estimate
  mySmonoDistResults$seD[mySmonoDistResults$method == "LTS" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[1]]$dht$individuals$D$se
  mySmonoDistResults$cvD[mySmonoDistResults$method == "LTS" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[1]]$dht$individuals$D$cv
  mySmonoDistResults$p[mySmonoDistResults$method == "LTS" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[1]]$dht$individuals$average.p
  } else {
    mySmonoDistResults[mySmonoDistResults$method == "LTS" &
                         mySmonoDistResults$idx == i, c(3:5)] <- NA
  }
  #results for OPT
  if(length(try1[[1]][[i]][[2]][[4]]$object)>60){
    optCorrection <- nrow(try1[[1]][[i]][[2]][[4]])/length(try1[[1]][[i]][[2]][[5]])
  mySmonoDistResults$estD[mySmonoDistResults$method == "Opt" &
                            mySmonoDistResults$idx == i] <- try1[[2]][[i]][[2]]$dht$individuals$D$Estimate/optCorrection
  mySmonoDistResults$seD[mySmonoDistResults$method == "Opt" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[2]]$dht$individuals$D$se/optCorrection
  mySmonoDistResults$cvD[mySmonoDistResults$method == "Opt" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[2]]$dht$individuals$D$cv
  mySmonoDistResults$p[mySmonoDistResults$method == "Opt" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[2]]$dht$individuals$average.p
  } else {
    mySmonoDistResults[mySmonoDistResults$method == "Opt" &
                         mySmonoDistResults$idx == i, c(3:5)] <- NA
  }
  if(length(try1[[1]][[i]][[3]])>2){
   if(length(try1[[1]][[i]][[3]][[4]]$object)>60){
    mySmonoDistResults$estD[mySmonoDistResults$method == "GrB" &
                            mySmonoDistResults$idx == i] <- try1[[2]][[i]][[3]]$dht$individuals$D$Estimate
  mySmonoDistResults$seD[mySmonoDistResults$method == "GrB" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[3]]$dht$individuals$D$se
  mySmonoDistResults$cvD[mySmonoDistResults$method == "GrB" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[3]]$dht$individuals$D$cv
  mySmonoDistResults$p[mySmonoDistResults$method == "GrB" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[3]]$dht$individuals$average.p
  } 
  }
  else {
    mySmonoDistResults[mySmonoDistResults$method == "GrB" &
                         mySmonoDistResults$idx == i, c(3:5)] <- NA
  } 
}

seed1 <- .Random.seed
set.seed(seed1)

# Stackhousia plots ----

### 1m^2 ----

#distance analysis effort currently using 300 mins

#make table for key results (density and cv estimates)
Plotresults1m <- data.frame("round" = 1:100,
                          "dhat_doubleObs" = numeric(100),
                          "sed_double" = numeric(100),
                          "dhat_singleObs" = numeric(100),
                          "sed_single" = numeric(100),
                          "probOverall" = numeric(100),
                          "probObs1" = numeric(100),
                          "probObs2" = numeric(100),
                          "sed_double_mod" = numeric(100))#sed_double_mod is the model indiv estimate from the formula in Sutherland 2007 (or 2002, can't remember), use sed double


SmonoPlot1mTimeStart <- Sys.time()

# for loop resamples the plot data, giving single and double observer estimates for each resample.  Results table is populated in the loop (lines commented out write results to file)
for (foo in 1:100){
  myResample <- resamplePlots(plotDf = mySmono1m, timesDF = times, budget = smEffort, plotType = "plot1m")
  Plotresults1m[foo, c(2, 3)] <- myResample[c(6,7)]
  Plotresults1m[foo, 4] <- myResample[2]
  Plotresults1m[foo, 5] <- myResample[3]
  Plotresults1m[foo, 6] <- myResample[[5]][3]
  Plotresults1m[foo, 7] <- myResample[[5]][1]
  Plotresults1m[foo, 8] <- myResample[[5]][2]
  Plotresults1m[foo, 9] <- myResample[8]
  
  #saving the resample to the dataSim/resampleData1m folder
   write.table(myResample[[1]], paste0("dataSim/resampleSmono/resampleData1m/resample-", smEffort, "_", mySeed, "_single", foo, ".txt"))
   write.table(myResample[[4]], paste0("dataSim/resampleSmono/resampleData4m/resample-", smEffort, "_",mySeed, "_double", foo, ".txt"))
  print(foo)
}
SmonoPlot1mTimeEnd <- Sys.time()
(Smono1mProcessTime <- as.numeric(SmonoPlot1mTimeEnd-SmonoPlot1mTimeStart))

seed2 <- .Random.seed
set.seed(seed2)
### 4m^2 ----

# make table for key results
Plotresults4m <- data.frame("round" = 1:100,
                          "dhat_doubleObs" = numeric(100),
                          "sed_double" = numeric(100),
                          "dhat_singleObs" = numeric(100),
                          "sed_single" = numeric(100),
                          "probOverall" = numeric(100),
                          "probObs1" = numeric(100),
                          "probObs2" = numeric(100),
                          "sed_double_mod" = numeric(100))

SmonoPlot4mTimeStart <- Sys.time()

#same function as for 1m plots creates resamples, analyses and populates results table

for (boo in 1:100){
  myResample <- resamplePlots(plotDf = mySmono4m, timesDF = times, budget = smEffort, plotType = "plot4m")
  Plotresults4m[boo, c(2, 3)] <- myResample[c(6,7)]
  Plotresults4m[boo, 4] <- myResample[2]
  Plotresults4m[boo, 5] <- myResample[3]
  Plotresults4m[boo, 6] <- myResample[[5]][3]
  Plotresults4m[boo, 7] <- myResample[[5]][1]
  Plotresults4m[boo, 8] <- myResample[[5]][2]
  Plotresults1m[boo, 9] <- myResample[8]
  
   write.table(myResample[[1]], paste0("dataSim/resampleSmono/resampleData4m/resample-", smEffort, "_", mySeed, "_single", boo, ".txt"))
   write.table(myResample[[4]], paste0("dataSim/resampleSmono/resampleData4m/resample-", smEffort, "_", mySeed, "_double", boo, ".txt"))
  print(boo)
}
SmonoPlot4mTimeEnd <- Sys.time()

(Smono4mProcessTime <- as.numeric(SmonoPlot4mTimeEnd - SmonoPlot4mTimeStart))

#save results dfs

write.table(mySmonoDistResults, paste0("dataSim/resampleSmono/distResults-", smEffort, "_", mySeed, ".txt"))
write.table(Plotresults1m, paste0("dataSim/resampleSmono/plot1m-", smEffort, "_", mySeed, ".txt"))
write.table(Plotresults4m, paste0("dataSim/resampleSmono/plot4m-", smEffort, "_", mySeed, ".txt"))

# seed3 <- .Random.seed
# set.seed(seed3)
 set.seed(seed[,3])
# Senecio distance ----
 
sqEffort <- 275 

startSquad <- Sys.time()
#same function as for S mono resamples and analyses distance data for LTS and Opt methods
distSquad <- resampleMany_half(n = 100, details = mySquadLTS_details, 
                     data = mySquadLTS, effort = sqEffort, times = times, 
                     target = "Senecio", Cm = Cm_Squad, 
                     Cw = Cw_Squad, w = 10)
endSquad <- Sys.time()
(SquadDistProcessTime <- as.numeric(endSquad - startSquad))

#make table for key results
mySquadDistResults <- data.frame("method" = rep(c("LTS", "Opt"), each = 100),
                                 "idx" = rep(c(1:100), times = 2),
                                 "estD" = numeric(200),
                                 "seD" = numeric(200),
                                 "cvD" = numeric(200),
                                 "p" = numeric(200),
                                 "detfunc" = character(200))

#populate results table
for(i in 1:100){
  #results for LTS
  mySquadDistResults$estD[mySquadDistResults$method == "LTS" & 
                            mySquadDistResults$idx == i] <- distSquad[[2]][[i]][[1]]$dht$individuals$D$Estimate
  mySquadDistResults$seD[mySquadDistResults$method == "LTS" &
                           mySquadDistResults$idx == i] <- distSquad[[2]][[i]][[1]]$dht$individuals$D$se
  mySquadDistResults$cvD[mySquadDistResults$method == "LTS" &
                           mySquadDistResults$idx == i] <- distSquad[[2]][[i]][[1]]$dht$individuals$D$cv
  mySquadDistResults$p[mySquadDistResults$method == "LTS" &
                           mySquadDistResults$idx == i] <- distSquad[[2]][[i]][[1]]$dht$individuals$average.p
  mySquadDistResults$detfunc[mySquadDistResults$method == "LTS" &
                           mySquadDistResults$idx == i] <- distSquad[[2]][[i]][[1]]$ddf$name.message
  
  
  #results for OPT
  optCorrection <- nrow(distSquad[[1]][[i]][[2]][[4]])/length(distSquad[[1]][[i]][[2]][[5]])
  mySquadDistResults$estD[mySquadDistResults$method == "Opt" &
                            mySquadDistResults$idx == i] <- distSquad[[2]][[i]][[2]]$dht$individuals$D$Estimate/optCorrection
  mySquadDistResults$seD[mySquadDistResults$method == "Opt" &
                           mySquadDistResults$idx == i] <- distSquad[[2]][[i]][[2]]$dht$individuals$D$se/optCorrection
  mySquadDistResults$cvD[mySquadDistResults$method == "Opt" &
                           mySquadDistResults$idx == i] <- distSquad[[2]][[i]][[2]]$dht$individuals$D$cv
  mySquadDistResults$p[mySquadDistResults$method == "Opt" &
                           mySquadDistResults$idx == i] <- distSquad[[2]][[i]][[2]]$dht$individuals$average.p
  mySquadDistResults$detfunc[mySquadDistResults$method == "Opt" &
                           mySquadDistResults$idx == i] <- distSquad[[2]][[i]][[2]]$ddf$name.message  
}

# Senecio plots ----
seed4 <- .Random.seed
set.seed(seed4)
#create table to store key results
PlotresultsSQ <- data.frame("round" = 1:100,
                          "dhat_doubleObs" = numeric(100),
                          "sed_double" = numeric(100),
                          "dhat_singleObs" = numeric(100),
                          "sed_single)" = numeric(100),
                          "probOverall" = numeric(100),
                          "probObs1" = numeric(100),
                          "probObs2" = numeric(100),
                          "sed_double_mod" = numeric(100))

SquadPlotStart <- Sys.time()

#Same function as for S mono
for (zoo in 1:100){
  myResample <- resamplePlots_replacement(plotDf = mySquadPlots, timesDF = times, budget = sqEffort, plotType = "plot")
  PlotresultsSQ[zoo, c(2, 3)] <- myResample[c(6,7)]
  PlotresultsSQ[zoo, 4] <- myResample[2]
  PlotresultsSQ[zoo, 5] <- myResample[3]
  PlotresultsSQ[zoo, 6] <- myResample[[5]][3]
  PlotresultsSQ[zoo, 7] <- myResample[[5]][1]
  PlotresultsSQ[zoo, 8] <- myResample[[5]][2]
  PlotresultsSQ[zoo, 9] <- myResample[8]
  
   write.table(myResample[[1]], paste0("dataSim/resampleSquad/resamplePlot/resample-", sqEffort, "_",  mySeed, "_", zoo, ".txt"))
   write.table(myResample[[4]], paste0("dataSim/resampleSquad/resamplePlot/resample-", sqEffort, "_",  mySeed, "_", zoo, ".txt"))
  print(zoo)
}

SquadPlotEnd <- Sys.time()
(SquadPlotProcessTime <- as.numeric(SquadPlotEnd - SquadPlotStart))

# save df
write.table(mySquadDistResults, paste0("dataSim/resampleSquad/distResults-", sqEffort, "_", mySeed, ".txt"))
write.table(PlotresultsSQ, paste0("dataSim/resampleSquad/plot-", sqEffort, "_",  mySeed, ".txt"))

# Saved things ----

listOfSeeds <- cbind("seed1" = seed1, "seed2" = seed2, "seed3" = seed3, "seed4" = seed4)
#save list of seeds 
write.table(listOfSeeds, "dataSim/listOfSeeds.txt")

processTimes <- data.frame("Smono1m" = Smono1mProcessTime, "Smono4m" = Smono4mProcessTime, 
                  "SmonoDist" = SmonoDistProcessTime, "SquadDist" = SquadDistProcessTime, 
                  "SquadPlot" = SquadPlotProcessTime)

processTimes$total <- sum(processTimes)

write.table(processTimes, paste0("dataSim/processTimes_", mySeed, ".txt"))
