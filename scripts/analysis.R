#Analysis: A comparison of the performance of distance- and plot-based methods 
# for surveys of high density species: a wildflower case study

source("scripts/summaries.R")

library(Distance)

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
# optL <-  B/(Cw + E*optA*Cm)
unitCostLTS <- times$mean_unit_time[times$target == "Stackhousia" & times$method == "LTS"]

####################################
#                                  #
#    Resampling LTS survey data    #
#           all methods            #
#                                  #
####################################

# how many times to sample for each method (try 10 samples per method)
# thought: timing - use mean unit time to take sample, but depending on sample, the actual survey time would be different????

#LTS

#writing functions on 'resampleFunction.R'

#Opt

#Grouped

#Grouped without string

#search density equivalent effort? 

######################
# what is the cost ratio for measuring?
#note, maybe remove 23964 from analysis (3 obs, NA on measuring time, 2 sticks were missed)

# make sure you know which transects need different treatment for survey/measure (bad back days)
 #question for self: how different are the cost ratios calculated using the different options?

# S mono LTS
#problem: for S mono LTS, only have measuring from separate measure time (with someone else writing)
# I think this vastly underestimates measuring cost
#don't use survey cost? Only use pilot cost?
RcLTS <- as.numeric(sum(mySmonoLTS_details$timeMeasuring[!(mySmonoLTS_details$transectID %in% dntUseSurveyTime)], na.rm = TRUE))/
  as.numeric(sum(mySmonoLTS_details$surveyTime[!(mySmonoLTS_details$transectID %in% dntUseSurveyTime)]-mySmonoLTS_details$timeMeasuring[!(mySmonoLTS_details$transectID %in% dntUseSurveyTime)], na.rm = TRUE))

# S mono Opt
# 


#try the same method without the transects where someone else either did or timed the measuring


