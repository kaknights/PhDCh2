# Analysis: ---- A comparison of the performance of distance- and plot-based methods for surveys of high density species: a wildflower case study

source("scripts/summaries.R")
#source("scripts/resampleFunction.R")
source("scripts/resampleDistFuncReplacement.R")

library(Distance); library(viridis)
mySeed <- 1 #used in naming files below
smEffort <- 600
sqEffort <- 600 
UnbandedReplEffort <- 600

# Stackhousia distance ----

# Resamples take a subset of LTS distance survey data, runs distance analysis using LTS, Opt and Grouped methods using the same data (Opt method uses additional datafrom the LTS dataset)
# resampleMany function takes LTS dataframes, creates tables required by ds functionin distance package (for LTS, Opt and GrB methods), runs distance models, stores results in a list.  n = number of resamples, effort = survey time in minutes
#Resamples are done without replacement for ds and plot methods
#All obs are treated as individuals: cluster sizes are either 1 or NA in the LTS data, obs where 2 indivs were close enough to be treated as a cluster were v rare (these were also treated as 1 indiv in the plot data, so any bias as a result is the same-ish)


set.seed(mySeed)
SmonoDistTimeStart <- Sys.time()
try1 <- resampleMany_Replacement(n = 10, details = mySmonoLTS_details, 
                     data = mySmonoLTS, effort = smEffort, times = times, 
                     target = "Stackhousia", Cm = Cm_Smono, 
                     Cw = Cw_Smono, w = 2)
SmonoDistTimeEnd <- Sys.time()

(SmonoDistProcessTime <- as.numeric(SmonoDistTimeEnd-SmonoDistTimeStart))
#on my laptop 10 rounds = 12.27 mins

#make table for key results (density and se estimates)
mySmonoDistResults <- data.frame("method" = rep(c("LTS", "Opt", "GrB"), each = 10),
                            "idx" = rep(c(1:10), times = 3),
                            "estD" = numeric(30),
                            "seD" = numeric(30),
                            "cvD" = numeric(30),
                            "p" = numeric(30),
                            "detfunc" = character(30))

#populate results from resample lists
for(i in 1:10){
  
    #results for LTS
if(length(try1[[2]][[i]][[1]])>0){
  mySmonoDistResults$estD[mySmonoDistResults$method == "LTS" & 
                            mySmonoDistResults$idx == i] <- try1[[2]][[i]][[1]]$dht$individuals$D$Estimate
  mySmonoDistResults$seD[mySmonoDistResults$method == "LTS" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[1]]$dht$individuals$D$se
  mySmonoDistResults$cvD[mySmonoDistResults$method == "LTS" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[1]]$dht$individuals$D$cv
  mySmonoDistResults$p[mySmonoDistResults$method == "LTS" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[1]]$dht$individuals$average.p
  mySmonoDistResults$detfunc[mySmonoDistResults$method == "LTS" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[1]]$ddf$name.message
} else {mySmonoDistResults[mySmonoDistResults$method == "LTS" &
                         mySmonoDistResults$idx == i, c(3:7)] <- NA
  
}
    #results for OPT
    if(length(try1[[2]][[i]][[2]])>0){
    optCorrection <- nrow(try1[[1]][[i]][[2]][[4]])/length(try1[[1]][[i]][[2]][[5]])
  mySmonoDistResults$estD[mySmonoDistResults$method == "Opt" &
                            mySmonoDistResults$idx == i] <- try1[[2]][[i]][[2]]$dht$individuals$D$Estimate/optCorrection
  mySmonoDistResults$seD[mySmonoDistResults$method == "Opt" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[2]]$dht$individuals$D$se/optCorrection
  mySmonoDistResults$cvD[mySmonoDistResults$method == "Opt" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[2]]$dht$individuals$D$cv
  mySmonoDistResults$p[mySmonoDistResults$method == "Opt" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[2]]$dht$individuals$average.p
  mySmonoDistResults$detfunc[mySmonoDistResults$method == "Opt" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[2]]$ddf$name.message  
    } else {mySmonoDistResults[mySmonoDistResults$method == "Opt" &
                         mySmonoDistResults$idx == i, c(3:7)] <- NA
    
  }
  
  #results for grouped
  if(length(try1[[1]][[i]][[3]])>2){
   
    mySmonoDistResults$estD[mySmonoDistResults$method == "GrB" &
                            mySmonoDistResults$idx == i] <- try1[[2]][[i]][[3]]$dht$individuals$D$Estimate
  mySmonoDistResults$seD[mySmonoDistResults$method == "GrB" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[3]]$dht$individuals$D$se
  mySmonoDistResults$cvD[mySmonoDistResults$method == "GrB" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[3]]$dht$individuals$D$cv
  mySmonoDistResults$p[mySmonoDistResults$method == "GrB" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[3]]$dht$individuals$average.p
  mySmonoDistResults$detfunc[mySmonoDistResults$method == "GrB" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[3]]$ddf$name.message
  } 
  else {
    mySmonoDistResults[mySmonoDistResults$method == "GrB" &
                         mySmonoDistResults$idx == i, c(3:7)] <- NA
  } 
}


# Stackhousia plots ----

### 1m^2 ----

#distance analysis effort currently using 300 mins

#make table for key results (density and cv estimates)
Plotresults1m <- data.frame("round" = 1:10,
                          "dhat_doubleObs" = numeric(10),
                          "sed_double" = numeric(10),
                          "dhat_singleObs" = numeric(10),
                          "sed_single" = numeric(10),
                          "probOverall" = numeric(10),
                          "probObs1" = numeric(10),
                          "probObs2" = numeric(10),
                          "sed_double_mod" = numeric(10))#sed_double_mod is the model indiv estimate from the formula in Sutherland 2007 (or 2002, can't remember), use sed double


# for loop resamples the plot data, giving single and double observer estimates for each resample.  Results table is populated in the loop (lines commented out write results to file)
set.seed(mySeed)

SmonoPlot1mTimeStart <- Sys.time()
for (foo in 1:10){
  myResample <- resamplePlots_replacement(plotDf = mySmono1m, timesDF = times, budget = smEffort, plotType = "plot1m")
  Plotresults1m[foo, c(2, 3)] <- myResample[c(6,7)]
  Plotresults1m[foo, 4] <- myResample[2]
  Plotresults1m[foo, 5] <- myResample[3]
  Plotresults1m[foo, 6] <- myResample[[5]][3]
  Plotresults1m[foo, 7] <- myResample[[5]][1]
  Plotresults1m[foo, 8] <- myResample[[5]][2]
  Plotresults1m[foo, 9] <- myResample[8]
  
  #saving the resample to the dataSim/resampleData1m folder
   write.table(myResample[[1]], paste0("dataSim/resampleSmono/resampleData1m/resample-", smEffort, "_", mySeed, "_single", foo, ".txt"))
   write.table(myResample[[4]], paste0("dataSim/resampleSmono/resampleData1m/resample-", smEffort, "_",mySeed, "_double", foo, ".txt"))
  print(foo)
}
SmonoPlot1mTimeEnd <- Sys.time()
(Smono1mProcessTime <- as.numeric(SmonoPlot1mTimeEnd-SmonoPlot1mTimeStart))

### 4m^2 ----

# make table for key results
Plotresults4m <- data.frame("round" = 1:10,
                          "dhat_doubleObs" = numeric(10),
                          "sed_double" = numeric(10),
                          "dhat_singleObs" = numeric(10),
                          "sed_single" = numeric(10),
                          "probOverall" = numeric(10),
                          "probObs1" = numeric(10),
                          "probObs2" = numeric(10),
                          "sed_double_mod" = numeric(10))

set.seed(mySeed)
SmonoPlot4mTimeStart <- Sys.time()

#same function as for 1m plots creates resamples, analyses and populates results table

for (boo in 1:10){
  myResample <- resamplePlots_replacement(plotDf = mySmono4m, timesDF = times, budget = smEffort, plotType = "plot4m")
  Plotresults4m[boo, c(2, 3)] <- myResample[c(6,7)]
  Plotresults4m[boo, 4] <- myResample[2]
  Plotresults4m[boo, 5] <- myResample[3]
  Plotresults4m[boo, 6] <- myResample[[5]][3]
  Plotresults4m[boo, 7] <- myResample[[5]][1]
  Plotresults4m[boo, 8] <- myResample[[5]][2]
  Plotresults4m[boo, 9] <- myResample[8]
  
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


# Senecio ----

## with replacement ----
# Uses an effort that allows an adequate measured subsample for ds for LTS and Opt (generally - there might be the odd one or two Opt samples that come out low)
# ds and plot sampling is done WITH REPLACEMENT, and samples are rounded to the nearest full transect
# cluster size is included in this analysis


### Senecio distance ----

set.seed(mySeed)
 
system.time(
  distSquad2 <- resampleMany_Replacement(n = 10, details = mySquadLTS_details, 
                     data = mySquadLTS, effort = sqEffort, times = times, 
                     target = "Senecio", Cm = Cm_Squad, 
                     Cw = Cw_Squad, w = 10)
)
#same function as for S mono resamples and analyses distance data for LTS and Opt methods

#make table for key results
mySquad2DistResults <- data.frame("method" = rep(c("LTS", "Opt"), each = 5),
                                 "idx" = rep(c(1:5), times = 2),
                                 "estD" = numeric(10),
                                 "seD" = numeric(10),
                                 "cvD" = numeric(10),
                                 "p" = numeric(10),
                                 "detfunc" = character(10))

#populate results table
for(i in 1:10){
  #results for LTS
  mySquad2DistResults$estD[mySquad2DistResults$method == "LTS" & 
                            mySquad2DistResults$idx == i] <- distSquad2[[2]][[i]][[1]]$dht$individuals$D$Estimate
  mySquad2DistResults$seD[mySquad2DistResults$method == "LTS" &
                           mySquad2DistResults$idx == i] <- distSquad2[[2]][[i]][[1]]$dht$individuals$D$se
  mySquad2DistResults$cvD[mySquad2DistResults$method == "LTS" &
                           mySquad2DistResults$idx == i] <- distSquad2[[2]][[i]][[1]]$dht$individuals$D$cv
  mySquad2DistResults$p[mySquad2DistResults$method == "LTS" &
                           mySquad2DistResults$idx == i] <- distSquad2[[2]][[i]][[1]]$dht$individuals$average.p
  mySquad2DistResults$detfunc[mySquad2DistResults$method == "LTS" &
                         mySquad2DistResults$idx == i] <- distSquad2[[2]][[i]][[1]]$ddf$name.message

 
  #results for OPT
  optCorrection <- nrow(distSquad2[[1]][[i]][[2]][[4]])/length(distSquad2[[1]][[i]][[2]][[5]])
  mySquad2DistResults$estD[mySquad2DistResults$method == "Opt" &
                            mySquad2DistResults$idx == i] <- distSquad2[[2]][[i]][[2]]$dht$individuals$D$Estimate/optCorrection
  mySquad2DistResults$seD[mySquad2DistResults$method == "Opt" &
                           mySquad2DistResults$idx == i] <- distSquad2[[2]][[i]][[2]]$dht$individuals$D$se/optCorrection
  mySquad2DistResults$cvD[mySquad2DistResults$method == "Opt" &
                           mySquad2DistResults$idx == i] <- distSquad2[[2]][[i]][[2]]$dht$individuals$D$cv
  mySquad2DistResults$p[mySquad2DistResults$method == "Opt" &
                           mySquad2DistResults$idx == i] <- distSquad2[[2]][[i]][[2]]$dht$individuals$average.p
  mySquad2DistResults$detfunc[mySquad2DistResults$method == "Opt" &
                           mySquad2DistResults$idx == i] <- distSquad2[[2]][[i]][[2]]$ddf$name.message  
}

### Senecio plots ----

set.seed(mySeed)
#create table to store key results
PlotresultsSQ2 <- data.frame("round" = 1:100,
                          "dhat_doubleObs" = numeric(100),
                          "sed_double" = numeric(100),
                          "dhat_singleObs" = numeric(100),
                          "sed_single)" = numeric(100),
                          "probOverall" = numeric(100),
                          "probObs1" = numeric(100),
                          "probObs2" = numeric(100),
                          "sed_double_mod" = numeric(100))

SquadPlotStart2 <- Sys.time()

#Same function as for S mono (no replacement)
for (zoo in 1:100){
  myResample2 <- resamplePlots_replacement(plotDf = mySquadPlots, timesDF = times, budget = sqEffort, plotType = "plot")
  PlotresultsSQ2[zoo, c(2, 3)] <- myResample2[c(6,7)]
  PlotresultsSQ2[zoo, 4] <- myResample2[2]
  PlotresultsSQ2[zoo, 5] <- myResample2[3]
  PlotresultsSQ2[zoo, 6] <- myResample2[[5]][3]
  PlotresultsSQ2[zoo, 7] <- myResample2[[5]][1]
  PlotresultsSQ2[zoo, 8] <- myResample2[[5]][2]
  PlotresultsSQ2[zoo, 9] <- myResample2[8]
  
   write.table(myResample2[[1]], paste0("dataSim/resampleSquad/resamplePlot/resample-", sqEffort, "_",  mySeed, "_single", zoo, "Repl.txt"))
   write.table(myResample2[[4]], paste0("dataSim/resampleSquad/resamplePlot/resample-", sqEffort, "_",  mySeed, "_double", zoo, "Repl.txt"))
  print(zoo)
}

SquadPlotEnd2 <- Sys.time()
(SquadPlotProcessTime2 <- as.numeric(SquadPlotEnd2 - SquadPlotStart2))

# save df
write.table(mySquad2DistResults, paste0("dataSim/resampleSquad/distResults-", sqEffort, "_", mySeed, "Repl.txt"))
write.table(PlotresultsSQ2, paste0("dataSim/resampleSquad/plot-", sqEffort, "_",  mySeed, "Repl.txt"))

set.seed(mySeed)

# Stackhousia Unbanded analysis ----

## With replacement

set.seed(mySeed)
SmonoUnbandedReplTimeStart <- Sys.time()
SmonoUnBandedRepl <- resampleManyWithUnbandedReplacement(n = 100, details = mySmonoLTS_details, 
                     data = mySmonoLTS, effort = UnbandedReplEffort, times = times, 
                     target = "Stackhousia", Cm = Cm_Smono, 
                     Cw = Cw_Smono, w = 2)
SmonoUnbandedReplTimeEnd <- Sys.time()

(SmonoUnbandedReplProcessTime <- as.numeric(SmonoUnbandedReplTimeEnd-SmonoUnbandedReplTimeStart))

#make table for key results (density and se estimates)
mySmonoUnbandedReplResults <- data.frame("method" = rep(c("Opt", "GrN"), each = 100),
                            "idx" = rep(c(1:100), times = 2),
                            "estD" = numeric(100),
                            "seD" = numeric(100),
                            "cvD" = numeric(100),
                            "p" = numeric(100),
                            "detfunc" = character(100))

#populate results from resample lists
for(i in 1:100){

  #results for OPT
  if(length(SmonoUnBandedRepl[[1]][[i]][[1]])>3){
  if(length(SmonoUnBandedRepl[[1]][[i]][[1]][[4]]$object)>60){
    optCorrection <- nrow(SmonoUnBandedRepl[[1]][[i]][[1]][[4]])/length(SmonoUnBandedRepl[[1]][[i]][[1]][[5]])
  mySmonoUnbandedReplResults$estD[mySmonoUnbandedReplResults$method == "Opt" &
                            mySmonoUnbandedReplResults$idx == i] <- SmonoUnBandedRepl[[2]][[i]][[1]]$dht$individuals$D$Estimate/optCorrection
  mySmonoUnbandedReplResults$seD[mySmonoUnbandedReplResults$method == "Opt" &
                           mySmonoUnbandedReplResults$idx == i] <- SmonoUnBandedRepl[[2]][[i]][[1]]$dht$individuals$D$se/optCorrection
  mySmonoUnbandedReplResults$cvD[mySmonoUnbandedReplResults$method == "Opt" &
                           mySmonoUnbandedReplResults$idx == i] <- SmonoUnBandedRepl[[2]][[i]][[1]]$dht$individuals$D$cv
  mySmonoUnbandedReplResults$p[mySmonoUnbandedReplResults$method == "Opt" &
                           mySmonoUnbandedReplResults$idx == i] <- SmonoUnBandedRepl[[2]][[i]][[1]]$dht$individuals$average.p
  mySmonoUnbandedReplResults$detfunc[mySmonoUnbandedReplResults$method == "Opt" &
                           mySmonoUnbandedReplResults$idx == i] <- SmonoUnBandedRepl[[2]][[i]][[1]]$ddf$name.message
  }
    }else {
    mySmonoUnbandedReplResults[mySmonoUnbandedReplResults$method == "Opt" &
                         mySmonoUnbandedReplResults$idx == i, c(3:7)] <- NA
  }
  if(length(SmonoUnBandedRepl[[1]][[i]][[2]])>2){
   if(length(SmonoUnBandedRepl[[1]][[i]][[2]][[4]]$object)>60){
    mySmonoUnbandedReplResults$estD[mySmonoUnbandedReplResults$method == "GrN" &
                            mySmonoUnbandedReplResults$idx == i] <- SmonoUnBandedRepl[[2]][[i]][[2]]$dht$individuals$D$Estimate
  mySmonoUnbandedReplResults$seD[mySmonoUnbandedReplResults$method == "GrN" &
                           mySmonoUnbandedReplResults$idx == i] <- SmonoUnBandedRepl[[2]][[i]][[2]]$dht$individuals$D$se
  mySmonoUnbandedReplResults$cvD[mySmonoUnbandedReplResults$method == "GrN" &
                           mySmonoUnbandedReplResults$idx == i] <- SmonoUnBandedRepl[[2]][[i]][[2]]$dht$individuals$D$cv
  mySmonoUnbandedReplResults$p[mySmonoUnbandedReplResults$method == "GrN" &
                           mySmonoUnbandedReplResults$idx == i] <- SmonoUnBandedRepl[[2]][[i]][[2]]$dht$individuals$average.p
  mySmonoUnbandedReplResults$detfunc[mySmonoUnbandedReplResults$method == "GrN" &
                           mySmonoUnbandedReplResults$idx == i] <- SmonoUnBandedRepl[[2]][[i]][[2]]$ddf$name.message
  } 
  }
  else {
    mySmonoUnbandedReplResults[mySmonoUnbandedReplResults$method == "GrB" &
                         mySmonoUnbandedReplResults$idx == i, c(3:7)] <- NA
  } 
}
write.table(mySmonoUnbandedReplResults, paste0("dataSim/resampleSmono/distResults-", UnbandedReplEffort, "_", mySeed, "UnbandedRepl.txt"))

# Saved things ----

# processTimes <- data.frame("Smono1m" = Smono1mProcessTime, 
#                            "Smono4m" = Smono4mProcessTime, 
#                           "SmonoDist" = SmonoDistProcessTime, 
#                           "SquadDist" = SquadDistProcessTime, 
#                           "SquadPlot" = SquadPlotProcessTime, 
#                           "SquadDistRepl" = Squad2DistProcessTime, 
#                           "SquadPlotRepl" = SquadPlotProcessTime2)
# 
# processTimes$total <- sum(processTimes)
# 
# write.table(processTimes, paste0("dataSim/processTimes_", mySeed, ".txt"))
