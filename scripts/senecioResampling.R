# Analysis: ---- A comparison of the performance of distance- and plot-based methods for surveys of high density species: a wildflower case study

# Resamples take a subset of LTS distance survey data, runs distance analysis using LTS and Opt methods using the same data (Opt method uses additional datafrom the LTS dataset)
# resampleMany function takes LTS dataframes, creates tables required by ds function in distance package (for LTS and Opt methods), runs distance models, stores results in a list.  n = number of resamples, effort = survey time in minutes
#Resamples are done with replacement for ds and plot methods
#Obs include clusters - when an observation was made and there were two individuals (originally seen as one), the measurement is to the centre of the 'group' and the cluster size is 2. 

# source("scripts/summaries.R")
# write.csv(mySquadLTS, "boabDataframes/mySquadLTS.csv")
# write.csv(mySquadLTS_details, "boabDataframes/mySquadLTS_details.csv")
# write.csv(mySquadPlots, "boabDataframes/mySquadPlots.csv")
# 
# sqObjects <- data.frame("Cm_Squad" = Cm_Squad, "Cw_Squad" = Cw_Squad)
# write.csv(sqObjects, "boabDataframes/sqObjects.csv")
library(Distance)
times <- read.csv("boabDataframes/times.csv")
mySquadLTS <- read.csv("boabDataframes/mySquadLTS.csv")
mySquadLTS_details <- read.csv("boabDataframes/mySquadLTS_details.csv")
mySquadPlots <- read.csv("boabDataframes/mySquadPlots.csv")
Cm_Squad <- read.csv("boabDataframes/sqObjects.csv")[,"Cm_Squad"]
Cw_Squad <- read.csv("boabDataframes/sqObjects.csv")[,"Cw_Squad"]

source("scripts/resampleDistFuncReplacement.R")
source("scripts/resamplePlots_replacement.R")

mySeed <- 1 #used in naming files below
sqEffort <- 600 
nRounds <- 1000

# Senecio ----

## with replacement ----
# Uses an effort that allows an adequate measured subsample for ds for LTS and Opt (generally - there might be the odd one or two Opt samples that come out low)
# ds and plot sampling is done WITH REPLACEMENT, and samples are rounded to the nearest full transect
# cluster size is included in this analysis


### Senecio distance ----

set.seed(mySeed)
 
sqDistStrt <- Sys.time()
distSquad2 <- resampleMany_Replacement(n = nRounds, details = mySquadLTS_details, 
                     data = mySquadLTS, effort = sqEffort, times = times, 
                     target = "Senecio", Cm = Cm_Squad, 
                     Cw = Cw_Squad, w = 10)
sqDistEnd <- Sys.time()
sqDistTime <- as.numeric(sqDistEnd-sqDistStrt)

# 10 rounds on my laptop = ~4 mins
# 100 rounds est time = 40 mins
# 1000 rounds est time = 400 mins

#same function as for S mono resamples and analyses distance data for LTS and Opt methods

#make table for key results
mySquad2DistResults <- data.frame("method" = rep(c("LTS", "Opt"), each = nRounds),
                                 "idx" = rep(c(1:nRounds), times = 2),
                                 "estD" = numeric(2*nRounds),
                                 "seD" = numeric(2*nRounds),
                                 "cvD" = numeric(2*nRounds),
                                 "p" = numeric(2*nRounds),
                                 "detfunc" = character(2*nRounds), stringsAsFactors = FALSE)

#populate results table
for(i in 1:nRounds){
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
PlotresultsSQ2 <- data.frame("round" = 1:nRounds,
                          "dhat_doubleObs" = numeric(nRounds),
                          "sed_double" = numeric(nRounds),
                          "dhat_singleObs" = numeric(nRounds),
                          "sed_single)" = numeric(nRounds),
                          "probOverall" = numeric(nRounds),
                          "probObs1" = numeric(nRounds),
                          "probObs2" = numeric(nRounds),
                          "sed_double_mod" = numeric(nRounds))

SquadPlotStart2 <- Sys.time()

#Same function as for S mono (no replacement)
for (zoo in 1:nRounds){
  myResample2 <- resamplePlots_replacement(plotDf = mySquadPlots, timesDF = times, budget = sqEffort, plotType = "plot")
  PlotresultsSQ2[zoo, c(2, 3)] <- myResample2[c(6,7)]
  PlotresultsSQ2[zoo, 4] <- myResample2[2]
  PlotresultsSQ2[zoo, 5] <- myResample2[3]
  PlotresultsSQ2[zoo, 6] <- myResample2[[5]][3]
  PlotresultsSQ2[zoo, 7] <- myResample2[[5]][1]
  PlotresultsSQ2[zoo, 8] <- myResample2[[5]][2]
  PlotresultsSQ2[zoo, 9] <- myResample2[8]
  
   write.table(myResample2[[1]], paste0("dataSim/resampleSquad/resamplePlot/resample-", sqEffort, "_",  mySeed, "_single", zoo, "Repl_", nRounds, ".txt"))
   write.table(myResample2[[4]], paste0("dataSim/resampleSquad/resamplePlot/resample-", sqEffort, "_",  mySeed, "_double", zoo, "Repl_", nRounds, ".txt"))
  print(zoo)
}

SquadPlotEnd2 <- Sys.time()
(SquadPlotProcessTime2 <- as.numeric(SquadPlotEnd2 - SquadPlotStart2))

# save df
write.table(mySquad2DistResults, paste0("dataSim/resampleSquad/distResults-", sqEffort, "_", mySeed, "Repl_", nRounds, ".txt"))
write.table(PlotresultsSQ2, paste0("dataSim/resampleSquad/plot-", sqEffort, "_",  mySeed, "Repl_", nRounds, ".txt"))

