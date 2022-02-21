#### Graph of bias / precision for plot surveys over different q
#### Comparison with a distance sampling survey

library(Distance)
source("scripts/functions.R")

budget <- 8*60 #minutes

#species / survey characteristics
#_________________________________
dha <- 2000 #pop density per ha
dm <- dha/10000 #same pop density per m^2
siteArea <- 50 #ha
siteAreaM <- siteArea*1000

#ds survey 
#__________________________
sigm <- 2 #in m
w <- 5
mu <- sqrt(pi*(sigm^2)/2) #is this the same as the formula in Guru's paper for We?
E <- 2*mu*dm
pace <- 2/60 # mins per m of transect
dsCostT <- E*0.5+pace # minutes per m of transect (walking and measuring)
dsCost0 <- 15 # one-off costs (transect set-up)

#add ds survey as a point
dsData <- read.table("dataSim/prelimResults.txt")
dsData <- dsData[dsData$method == "DS", ]
#calculate precision and bias

dsP <- myCV(dsData$dhat)
dsB <- mean(dsData$error)

#Plot survey version 2: creating the function of q
#______________________

plotSize <- 100 # m^2
expN <- dm*plotSize
q <- seq(0.8, 0.99, by = 0.01)
We <- 2*mu #assuming the same strip width as with ds?
plotCostSetUp <- 10 
plotCostSearch <- (log(1-q)*plotSize)/-We #this can be path length
v <- 0.5 #average speed in m/s
t <- plotCostSearch/v #time searching each plot for q=0.99 in s
plotT <- t/60 
nPlots <- budget/(plotCostSetUp+plotT) 

myPlotData <- data.frame("simID" = integer(), "q" = numeric(), "dhat" = numeric())

for(a in 1:length(q)){
  addData <- read.csv(paste0("dataSim/prelimResults_q", q[a], ".csv"))
  myPlotData <- rbind(myPlotData, addData)
  print(paste0("round ", a))
}

myPlotData$error <- myPlotData$dhat-dm
PrecisionPlot <- aggregate(myPlotData$dhat, by = list(myPlotData$q), FUN = myCV)

BiasPlot <- aggregate(myPlotData$error, by = list(myPlotData$q), FUN = mean)

myPlotPlot <- merge(PrecisionPlot, BiasPlot, by = "Group.1")
names(myPlotPlot) <- c("q", "precision", "bias")

#png("../Thesis outline/ThirdYrReview/reviewGraph1.png")
#par(mar = c(4,5,4,2), xpd = TRUE)
#plot(myPlotPlot$bias, myPlotPlot$precision,
#     xlab = "bias (mean error)", ylab = "", type = "l", 
#     main = "q = 0.8-0.99", xlim = c(-0.045, 0.005), ylim = c(0.047, 0.057), las = 1)
#mtext("precision (cv(dhat))", side = 2, line = 3.5)
#points(x = dsB, y = dsP, pch = 8)
#legend(x = -0.04, y = 0.05, legend = "plot surveys", lty = 1, bty = "n")
#legend(x = -0.04, y = 0.049, legend = "ds survey", pch = 8, bty = "n")
#text(myPlotPlot$precision~myPlotPlot$bias, labels=myPlotPlot$q,cex=0.9, font=1, pos=4)
#dev.off()

png("../Thesis outline/ThirdYrReview/reviewGraph1.png")
par(mar = c(4,5,4,2), xpd = TRUE)
plot(myPlotPlot$bias, myPlotPlot$precision,
     xlab = "bias (mean error)", ylab = "", type = "l", 
     main = "", xlim = c(-0.045, 0.005), ylim = c(0.047, 0.057), las = 1)
mtext("precision (cv(dhat))", side = 2, line = 3.5)
points(x = dsB, y = dsP, pch = 8)
legend(x = -0.04, y = 0.05, legend = "plot surveys", lty = 1, bty = "n")
legend(x = -0.04, y = 0.049, legend = "ds survey", pch = 8, bty = "n")
text(x = -0.001, y= 0.0502, "q = 0.99",cex=0.9, font=1, pos=4)
text(x = -0.04, y= 0.056, "q = 0.80",cex=0.9, font=1, pos=4)
dev.off()