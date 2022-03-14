#Analysis: A comparison of the performance of distance- and plot-based methods 
# for surveys of high density species: a wildflower case study

source("scripts/summaries.R")
#source("scripts/resampleFunction.R")
source("scripts/resampleDistFunc.R")

library(Distance)

######################

#note, maybe remove 23964 from analysis (3 obs, NA on measuring time, 2 sticks were missed)
#search density equivalent effort? 

####################################
#                                  #
#    Resampling LTS survey data    #
#           all methods            #
#                                  #
####################################

start1 <- Sys.time()#2.38
try1 <- resampleMany(n = 100, details = mySmonoLTS_details, 
                     data = mySmonoLTS, effort = 300, times = times, 
                     target = "Stackhousia", Cm = Cm_Smono, 
                     Cw = Cw_Smono, w = 2)
end1 <- Sys.time()

start3 <- Sys.time()
try3 <- resampleMany(n = 100, details = mySquadLTS_details, 
                     data = mySquadLTS, effort = 360, times = times, 
                     target = "Senecio", Cm = Cm_Squad, 
                     Cw = Cw_Squad, w = 10)
end3 <- Sys.time()


mySmonoDistResults <- data.frame("method" = rep(c("LTS", "Opt", "GrB", "GrN"), each = 100),
                            "idx" = rep(c(1:100), times = 4),
                            "estD" = numeric(400),
                            "seD" = numeric(400),
                            "cvD" = numeric(400))


for(i in 1:100){
  #results for LTS
  mySmonoDistResults$estD[mySmonoDistResults$method == "LTS" & 
                            mySmonoDistResults$idx == i] <- try1[[2]][[i]][[1]]$dht$individuals$D$Estimate
  mySmonoDistResults$seD[mySmonoDistResults$method == "LTS" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[1]]$dht$individuals$D$se
  mySmonoDistResults$cvD[mySmonoDistResults$method == "LTS" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[1]]$dht$individuals$D$cv
  
  #results for OPT
  optCorrection <- nrow(try1[[1]][[i]][[2]][[4]])/length(try1[[1]][[i]][[2]][[5]])
  mySmonoDistResults$estD[mySmonoDistResults$method == "Opt" &
                            mySmonoDistResults$idx == i] <- try1[[2]][[i]][[2]]$dht$individuals$D$Estimate/optCorrection
  mySmonoDistResults$seD[mySmonoDistResults$method == "Opt" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[2]]$dht$individuals$D$se/optCorrection
  mySmonoDistResults$cvD[mySmonoDistResults$method == "Opt" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[2]]$dht$individuals$D$cv
  
  #results for grouped with bands
  mySmonoDistResults$estD[mySmonoDistResults$method == "GrB" &
                            mySmonoDistResults$idx == i] <- try1[[2]][[i]][[3]]$dht$individuals$D$Estimate
  mySmonoDistResults$seD[mySmonoDistResults$method == "GrB" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[3]]$dht$individuals$D$se
  mySmonoDistResults$cvD[mySmonoDistResults$method == "GrB" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[3]]$dht$individuals$D$cv
  
  #results for grouped without bands
  mySmonoDistResults$estD[mySmonoDistResults$method == "GrN" &
                            mySmonoDistResults$idx == i] <- try1[[2]][[i]][[4]]$dht$individuals$D$Estimate
  mySmonoDistResults$seD[mySmonoDistResults$method == "GrN" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[4]]$dht$individuals$D$se
  mySmonoDistResults$cvD[mySmonoDistResults$method == "GrN" &
                           mySmonoDistResults$idx == i] <- try1[[2]][[i]][[4]]$dht$individuals$D$cv
  
  }

plot(x = mySmonoDistResults$estD, y = mySmonoDistResults$seD, xlab = "D hat (indiv/m^2)", 
     ylab = "se (D hat)", pch = c(15:18)[as.factor(mySmonoDistResults$method)],
     col = c("red", "blue", "yellow", "black")[as.factor(mySmonoDistResults$method)], 
     main = "Comparison of distance methods (se): Stackhousia monogyna")

plot(x = mySmonoDistResults$estD, y = mySmonoDistResults$cvD, xlab = "D hat (indiv/m^2)", 
     ylab = "cv (D hat)", pch = c(15:18)[as.factor(mySmonoDistResults$method)],
     col = c("red", "blue", "yellow", "black")[as.factor(mySmonoDistResults$method)], 
     main = "Comparison of distance methods (cv): Stackhousia monogyna")
     
plot(x = mySmonoDistResults$estD[mySmonoDistResults$method == "Opt"], 
     y = mySmonoDistResults$seD[mySmonoDistResults$method == "Opt"], xlab = "D hat (indiv/m^2)", 
     ylab = "se (D hat)", xlim = c(0,10), ylim = c(0,4))

# plot se against estimate (for the 10 different values, use different symbols/colours for methods)
as.factor(mySmonoDistResults$method) # order is GrB, GrN, LTS, Opt

# do LTS Vs Opt Vs GrB as this analysis, try using cv also
# then do GrN vs the winner (Opt)

#stackhousia
effort <- 300
E  <-  nrow(mySmonoLTS)/sum(mySmonoLTS_details$length_m)

test <- resampleLTStheory(details = mySmonoLTS_details, data = mySmonoLTS,
                          effort = effort, times = times, 
                          method = "LTS", target = "Stackhousia", w =2,
                          E = nrow(mySmonoLTS)/sum(mySmonoLTS_details$length_m), 
                          Cm = Cm_Smono, Cw = Cw_Smono, l = 2)

test2 <- resampleLTSfield(details = mySmonoLTS_details, data = mySmonoLTS,
                          effort = effort, times = times, 
                          method = "LTS", target = "Stackhousia", w =2)

test3 <- resampleOptTheory(details = mySmonoLTS_details, data = mySmonoLTS, 
                           effort = effort, times = times, 
                           method = "Opt", target = "Stackhousia", 
                           E = nrow(mySmonoLTS)/sum(mySmonoLTS_details$length_m), 
                           Cm = 0.5805, Cw = 0.4813, l = 2, w = 2)

test4 <- resampleOptField(details = mySmonoLTS_details, data = mySmonoLTS, 
                          effort = effort, times = times, 
                          method = "Opt", target = "Stackhousia",  
                          Cm = 0.5805, Cw = 0.4813, w = 2)

test5 <- resampleGrouped(details = mySmonoLTS_details, data = mySmonoLTS,
                         effort = effort, times = times, 
                         method = "Grouped", target = "Stackhousia", w =2)

# Senecio

totalTimeSquadLTS <- times$mean_unit_time[9]*7
totalTimeSquadOpt <- times$mean_unit_time[8]*13

budget <- 500

trial <- resampleLTStheory(details = mySquadLTS_details, data = mySquadLTS,
                           effort = budget, times = times, 
                           method = "LTS", target = "Senecio", w =10,
                           E = nrow(mySquadLTS)/sum(mySquadLTS_details$length_m), 
                           Cm = Cm_Squad, Cw = Cw_Squad, l = 20)

# things to go over in meeting:
    # grouped no bands resampled with replacement - best solution?
    # discrepancy between estimated and actual measuring times for LTS (opt not so bad)

####################################
#                                  #
# Distance Analysis of each survey #
#        With survey costs         #
#                                  #
####################################

#all input tables should be identical in column names
# (except S mono Opt details table has adjusted finish time column - 
# only relevant for cost calculations)

SmonoOpt_distTables <- distTables(distDetails = mySmonoOpt_details,
                                  distData = mySmonoOpt,
                                  w = 2)

SmonoOpt_distModel <- ds(data = SmonoOpt_distTables[[4]], transect = "line",
                         formula = ~1, region.table = SmonoOpt_distTables[[1]],
                         sample.table = SmonoOpt_distTables[[2]],
                         obs.table = SmonoOpt_distTables[[3]])
summary(SmonoOpt_distModel)
plot(SmonoOpt_distModel)

#correct abundance, density for total count
Smono_alpha <- 0.644 #calculated from pilot data
measuredN <- length(mySmonoOpt$perpDist_m[!is.na(mySmonoOpt$perpDist_m)])
totalN <- length(mySmonoOpt$perpDist_m)
measuredN/totalN

SmonoOpt_D <- SmonoOpt_distModel$dht$individuals$D$Estimate*(totalN/measuredN)

area <- 79*4
SmonoOpt_Ncovd <- area*SmonoOpt_D
otherMethodNcovd <- totalN/SmonoOpt_distModel$dht$individuals$average.p

#precision estimates for detection function only

#do something for the cluster size column

SmonoLTS_distTables <- distTables(distDetails = mySmonoLTS_details,
                                  distData = mySmonoLTS,
                                  w = 2)
SmonoLTS_distModel <- ds(data = SmonoLTS_distTables[[4]], transect = "line",
                         formula = ~1, region.table = SmonoLTS_distTables[[1]],
                         sample.table = SmonoLTS_distTables[[2]],
                         obs.table = SmonoLTS_distTables[[3]])

summary(SmonoLTS_distModel)
plot(SmonoLTS_distModel)

##### Come back to this when all timings are tidied up ####
#function to calculate costs:
#totalSurveyCost <- 

#check unit costs for any bonkers start/end times, and notes for if 
#we stopped for anything

# focus on LTs, use for resampling/subsampling, use other actual surveys for effort estimation  

# resample LTS transects for opt

# what do I need? alpha and length, random select nearest (over?) n transects from 
# LTS data, turn every nth measurement into an NA and then make the distance tables

# assuming the lowest of the total costs between all the surveys will be the limiting factor

# 1m plot survey total cost:

#budget <- as_hms(sum(mySmono1m$unitTime)) #as of 15/02/22 this is 631 mins
#times for distance samples is much shorter!!!!!!!

B <- 300
aOpt <- 0.644
# adjust measure times to account for the quicker measuring due to second person (use pilot estimates)
# mu <- sqrt(pi*(sigm^2)/2)
# optL <-  B/(Ct + E*optA*Cm) or B/(E*(CW+optA*Cm))
unitCostLTS <- times$mean_unit_time[times$target == "Stackhousia" & times$method == "LTS"]
