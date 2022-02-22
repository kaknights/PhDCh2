#Analysis: A comparison of the performance of distance- and plot-based methods 
# for surveys of high density species: a wildflower case study

source("scripts/summaries.R")
source("scripts/optVars_pilot.R")
source("scripts/resampleFunction.R")

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

budget <- 500

trial <- resampleLTStheory(details = mySquadLTS_details, data = mySquadLTS,
                           effort = budget, times = times, 
                           method = "LTS", target = "Senecio", w =10,
                           E = nrow(mySquadLTS)/sum(mySquadLTS_details$length_m), 
                           Cm = Cm_Squad, Cw = Cw_Squad, l = 20)



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
