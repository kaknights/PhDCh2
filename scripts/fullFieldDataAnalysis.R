# Analysis of full field data sets ----

# needs functions from functions.R
source("scripts/functions.R")
library(Distance)

## Get pop parameters and estimates for timing variables from field data ----
source("scripts/summaries.R")

# make table

myVars <- data.frame("target" = c("Stackhousia", "Senecio"), "area" = numeric(2), "estD" = numeric(2), "W" = c(2,10), "sigma" = numeric(2), "unitCost" = numeric(2), "nonSearchCost" = numeric(2), "Cm" = c(Cm_Smono, Cm_Squad), "Cw" = c(Cw_Smono, Cw_Squad))

# get values, fill in table

# area

a1 <- sum(mySmonoLTS_details$length_m)*4 # 4 is full strip width
a2 <- sum(mySquadLTS_details$length_m)*20

# LTS models on full datasets to get est D and sigma

# stackhousia

sMonoTables <- distTables_OneOff(distDetails = mySmonoLTS_details, distData = mySmonoLTS, w = 2)

sMonoModelhn <- ds(data = sMonoTables[[4]][,1:2], key = "hn", region.table = sMonoTables[[1]], sample.table = sMonoTables[[2]], obs.table = sMonoTables[[3]], dht.group = TRUE)

sMonoModelhr <- ds(data = sMonoTables[[4]][,1:2], key = "hr", region.table = sMonoTables[[1]], sample.table = sMonoTables[[2]], obs.table = sMonoTables[[3]], dht.group = TRUE)

sMonoModelun <- ds(data = sMonoTables[[4]][,1:2], key = "unif", region.table = sMonoTables[[1]], sample.table = sMonoTables[[2]], obs.table = sMonoTables[[3]], dht.group = TRUE)

sMcompare <- as.data.frame(summarize_ds_models(sMonoModelhr, sMonoModelun, sMonoModelhn, output = "plain"))

sMonoResults <- summary(sMonoModelhn)

sMestD <- sMonoModelhn$dht$individuals$D$Estimate

sMsigm <- exp(sMonoResults$ds$coeff$key.scale$estimate)

myVars[1, c(2,3,5)] <- c(a1, sMestD, sMsigm)

# Opt model, only using measured data (only for detectability)
sMonoTablesOpt <- distTables_OneOff(distDetails = mySmonoOpt_details, distData = mySmonoOpt[!is.na(mySmonoOpt$perpDist_m),], w = 2)

sMonoModelhnOpt <- ds(data = sMonoTablesOpt[[4]][,1:2], key = "hn", region.table = sMonoTablesOpt[[1]], sample.table = sMonoTablesOpt[[2]], obs.table = sMonoTablesOpt[[3]], dht.group = TRUE)

sMonoModelhrOpt <- ds(data = sMonoTablesOpt[[4]][,1:2], key = "hr", region.table = sMonoTablesOpt[[1]], sample.table = sMonoTablesOpt[[2]], obs.table = sMonoTablesOpt[[3]], dht.group = TRUE)

sMonoModelunOpt <- ds(data = sMonoTablesOpt[[4]][,1:2], key = "unif", region.table = sMonoTablesOpt[[1]], sample.table = sMonoTablesOpt[[2]], obs.table = sMonoTablesOpt[[3]], dht.group = TRUE)

smCompareOpt <- as.data.frame(summarize_ds_models(sMonoModelhrOpt, sMonoModelunOpt, sMonoModelhnOpt, output = "plain"))



sMonoResultsOpt <- summary(sMonoModelunOpt)

# grouped model
#need to convert counts into a long-form table
grCount <- sum(mySmonoGrouped$count)
grData <- data.frame("transectID" = integer(grCount), 
                     "distbegin" = numeric(grCount),
                     "distend" = numeric(grCount),
                     "obsID" = 1:grCount)
bandVector <- numeric()
idVector <- integer()

for(i in 1:length(mySmonoGrouped$band_m)){
  addMe <- rep(mySmonoGrouped$band_m[i], each = mySmonoGrouped$count[i])
  idMe <- rep(mySmonoGrouped$transectID[i], each = mySmonoGrouped$count[i])
  bandVector <- c(bandVector, addMe)
  idVector <- c(idVector, idMe)
}

grData$distend <- bandVector
grData$transectID <- idVector

bandDF <- data.frame("distbegin" = c(0, 0.25, 0.5, 1, 1.5),
                     "distend" = c(0.25, 0.5, 1, 1.5, 2))

for(x in 1:length(grData$distbegin)){
  grData$distbegin[x] <- bandDF$distbegin[bandDF$distend==grData$distend[x]]
}

grTables <- distTables_OneOffGR(distDetails = mySmonoGrouped_details, distData = grData, w = 2)

grHn <- ds(data = grTables[[4]], key = "hn", region.table = grTables[[1]], sample.table = grTables[[2]], obs.table = grTables[[3]], dht.group = TRUE)
grHr <- ds(data = grTables[[4]], key = "hr", region.table = grTables[[1]], sample.table = grTables[[2]], obs.table = grTables[[3]], dht.group = TRUE)
grUnif <- ds(data = grTables[[4]], key = "unif", region.table = grTables[[1]], sample.table = grTables[[2]], obs.table = grTables[[3]], dht.group = TRUE)

grCompare <- as.data.frame(summarize_ds_models(grHn, grHr, grUnif, output = "plain"))


grResults <- summary(grUnif)

png("graphics/SM_DSplots.png", width = 1400)
par(pty = "s", mfrow = c(1,3), cex = 2, mar= c(4,3,3,1))
plot(sMonoModelhn, main = "LTS")

plot(sMonoModelunOpt, main = "Optimised DS")
plot(grUnif, main = "Grouped DS")

dev.off()

# senecio

#having a look at the data first as it's more finicky

hist(mySquadLTS$perpDist_m)

sQuadTables <- distTables_OneOff(distDetails = mySquadLTS_details, distData = mySquadLTS, w = 10)

sQuadModelhn <- ds(data = sQuadTables[[4]], key = "hn", region.table = sQuadTables[[1]], sample.table = sQuadTables[[2]], obs.table = sQuadTables[[3]])

sQuadModelhr <- ds(data = sQuadTables[[4]], key = "hr", region.table = sQuadTables[[1]], sample.table = sQuadTables[[2]], obs.table = sQuadTables[[3]])

sQuadModelun <- ds(data = sQuadTables[[4]], key = "unif", region.table = sQuadTables[[1]], sample.table = sQuadTables[[2]], obs.table = sQuadTables[[3]])

sQcompare <- summarize_ds_models(sQuadModelhn, sQuadModelhr, sQuadModelun, output = "plain")


sQuadResults <- summary(sQuadModelhn)

sQestD <- sQuadModelhn$dht$individuals$D$Estimate
sQsigm <- exp(sQuadResults$ds$coeff$key.scale$estimate)

# opt analysis for appendix ----

# S quad using clustering
hist(mySquadOpt$perpDist_m)
sQuadOpt <- distTables_OneOff(distDetails = mySquadOpt_details, distData = mySquadOpt, w = 10)

sQuadOptModelhn <- ds(data = sQuadOpt[[4]], key = "hn", region.table = sQuadOpt[[1]], sample.table = sQuadOpt[[2]], obs.table = sQuadOpt[[3]])

sQuadOptModelhr <- ds(data = sQuadOpt[[4]], key = "hr", region.table = sQuadOpt[[1]], sample.table = sQuadOpt[[2]], obs.table = sQuadOpt[[3]])

sQuadOptModelun <- ds(data = sQuadOpt[[4]], key = "unif", region.table = sQuadOpt[[1]], sample.table = sQuadOpt[[2]], obs.table = sQuadOpt[[3]])

sQOptCompare <- summarize_ds_models(sQuadOptModelhn, sQuadOptModelhr, sQuadOptModelun, output = "plain")

png("graphics/SQ_DSplots.png", width = 1400)
par(pty = "s", mfrow = c(1,3), cex = 2, mar= c(4,3,3,1))
plot(sQuadModelhn, main = "LTS")
plot(sQuadOptModelhn, main = "Optimised DS")
dev.off()

sQuadOptResults <- summary(sQuadOptModelhn)

sQestD_Opt <- sQuadOptModelhn$dht$individuals$D$Estimate
OptCorrection <- nrow(sQuadOpt[[4]])/nrow(mySquadOpt) #n1/nt
adj_sQestD_Opt <- sQestD_Opt/OptCorrection

sQsigm_Opt <- exp(sQuadOptResults$ds$coeff$key.scale$estimate)

myVars[2, c(2,3,5)] <- c(a2, sQestD, sQsigm)


#create pretty table for appendix (eventually)
## ds results on full dataset

#timing estimates

myVars[1, c(6,7)] <- times[times$target=="Stackhousia" & times$method == "LTS", c(3,5)]
myVars[2, c(6,7)] <- times[times$target=="Senecio" & times$method == "LTS", c(3,5)]

write.table(myVars, "results/fieldEstimates.txt")

#Same table for plot data: what do we need to know?

# costs of set up etc.

plotVars <- data.frame("type" = c("Smono1m", "Smono4m", "Squad"), "setUpCost" = numeric(3))
plotVars$setUpCost[plotVars$type=="Smono1m"] <- times$mean_other_time[times$method == "plot1m"]
plotVars$setUpCost[plotVars$type=="Smono4m"] <- times$mean_other_time[times$method == "plot4m"]
plotVars$setUpCost[plotVars$type=="Squad"] <- times$mean_other_time[times$method == "plot"]

write.table(plotVars, "results/plotFieldEstimates.txt")

# Expected benefit ----

#S mono:

expB <- function(Rc){
  (3/2)*((1+Rc)/(1+sqrt(Rc/2))^2)
  }

smonoExpB <- expB(Rc_Smono)
squadExpB <- expB(Rc_Squad)
