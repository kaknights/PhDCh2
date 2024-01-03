# Simulations of plot and ds data to compare bias and precision
source("scripts/functions.R")
library(Distance)

# Pt 1: generating simulated data ----

# read.table("results/fieldEstimates.txt") #to read in saved results
budget <- 16*60 #minutes
myVars <- read.table("results/fieldEstimates.txt")
plotVars <- read.table("results/plotFieldEstimates.txt")

#species / survey characteristics
#_________________________________
# note: no heterogeneity in density over survey area

dm <- myVars$estD #same pop density per m^2
siteAreaM <- myVars$area #required for ds tables but doesn't affect results (site area in m)

### ds survey ----
#_________________
sigm <- myVars$sigma #in m
w <- myVars$W
mu <- sqrt(pi*(sigm^2)/2) #this expression comes from non-truncated data, 
E <- 2*mu*dm
Cl <- myVars$Cw*E # mins per m of transect just walking/counting
speed <- 1/Cl # m per min just walking/counting
unitL <- c(2,25)
nUnits <- round(budget/(myVars$nonSearchCost +(E*unitL*(myVars$Cm+myVars$Cw))))
# this calculated total time per unit is much higher than my recorded times

nSims <- 10 # 10 for testing, 10 000 will take several hours to run (or days, depending on computing power available)

### DS SIMULATIONS ########
#__________________________

myDsSimSM <- data.frame("simID" = numeric(length = nSims), "dhat" = numeric(nSims))

for (simID in 1:nSims){
  sim <- myDsSurvey(dm[1], nUnits[1], sigm[1], w[1], siteAreaM[1], unitL[1])
  myDsSimSM[simID,1] <- simID
  myDsSimSM[simID,"dhat"] <- sim$dht$individuals$D$Estimate
  print(simID) 
  }

myDsSimSM$error <- myDsSimSM$dhat-dm[1]
myDsSimSM$method <- "DS"

# EDIT FOLDER AND FILENAMES BEFORE SAVING
# write.table(myDsSimSM, "dataSim/prelimMethods/sm/myDsSimSM.txt")

myDsSimSQ <- data.frame("simID" = numeric(length = nSims), "dhat" = numeric(nSims))

for (simID in 1:nSims){
  try(sim <- myDsSurvey(dm[2], nUnits[2], sigm[2], w[2], siteAreaM[2], unitL[2]))
  myDsSimSQ[simID,1] <- simID
  myDsSimSQ[simID,"dhat"] <- sim$dht$individuals$D$Estimate
  print(simID) 
  }

myDsSimSQ$error <- myDsSimSQ$dhat-dm[2]
myDsSimSQ$method <- "DS"

# plot(myDsSimSQ$dhat, myDsSimSQ$error)
# abline(v = dm[2], lty = "dashed")
# abline(h = 0, lty = "dotted")
# 
# mean(myDsSimSQ$error)

# EDIT FOLDER AND FILENAMES BEFORE SAVING
# write.table(myDsSimSQ, "dataSim/prelimMethods/sq/myDsSimSQ.txt")

# same exercise for plot vars

### Plot survey: n plots for various p
#______________________
plotSize <- c(1, 4, 12.25) # m^2
expN <- c(dm[1]*plotSize[1], dm[1]*plotSize[2], dm[2]*plotSize[3])
p <- seq(0.8, 0.99, by = 0.01)
We <- 2*mu #assuming the same strip width as with ds?
plotCostSetUp <- plotVars$setUpCost#needs calculating
plotCostSearch <- data.frame("sm1m" = (log(1-p)*plotSize[1])/-We[1], "sm4m" = (log(1-p)*plotSize[2])/-We[1], "sq" = (log(1-p)*plotSize[3])/-We[2]) #ends up units of path length in m
t <- data.frame("sm1m" = plotCostSearch[,1]/speed[1], "sm4m" = plotCostSearch[,2]/speed[1], "sq" = plotCostSearch[,3]/speed[2]) #time searching each plot in mins

nPlots <- data.frame("sm1m" = round(budget/(plotCostSetUp[1]+t[,1])), "sm2m" = round(budget/(plotCostSetUp[2]+t[,2])), "sq" = round(budget/(plotCostSetUp[3]+t[,3])))

### Plot SIMULATIONS ########
#_______________________________#
nSims <- 10000

#EDIT indices and FOLDER/FILENAMES BEFORE RUNNING!

for (i in 1:length(p)){
  myPlotSim <- data.frame("simID" = numeric(nSims), "p" = numeric(nSims), "dhat" = numeric(nSims))
  
  for (simID in 1:nSims){
  nd <- rpois(ceiling(nPlots[i,3]), lambda = expN[3]*p[i]) #number detected in each plot
  myPlotSim[simID,1] <- simID
  myPlotSim[simID,"p"] <- p[i]
  myPlotSim[simID,"dhat"] <- mean(nd)/plotSize[3] #
  print(paste0("p = ", p[i], "; ", "simulation ", simID))
  }
  
  #commented out so it doesn't get overwritten accidentally
 # write.csv(myPlotSim, paste0("dataSim/prelimMethods/sm", "/sm1prelimResults_p", p[i], ".csv"), row.names = FALSE) 
 # write.csv(myPlotSim, paste0("dataSim/prelimMethods/sm", "/sm4prelimResults_p", p[i], ".csv"), row.names = FALSE) 
  write.csv(myPlotSim, paste0("dataSim/prelimMethods/sq", "/sqprelimResults_p", p[i], ".csv"), row.names = FALSE) 

}

#compile individual saved dataframes to a summary dataframe
myPlotData <- data.frame("simID" = integer(), "p" = numeric(), "dhat" = numeric())

#EDIT FOLDER/FILENAMES BEFORE RUNNING!

for(a in 1:length(p)){
  addData <- read.csv(paste0("dataSim/prelimMethods/sq","/sqprelimResults_p", p[a], ".csv"))
  myPlotData <- rbind(myPlotData, addData)
  print(paste0("round ", a))
}

#Edit dm index before running
myPlotData$error <- myPlotData$dhat-dm[2]
PrecisionPlot <- aggregate(myPlotData$dhat, by = list(myPlotData$p), FUN = sd)

BiasPlot <- aggregate(myPlotData$error, by = list(myPlotData$p), FUN = mean)

myPlotPlot <- merge(PrecisionPlot, BiasPlot, by = "Group.1")
names(myPlotPlot) <- c("p", "precision", "bias")

# write.table(myPlotPlot, paste0("dataSim/prelimMethods/sq", "/myPlotPlotsq.txt"))
