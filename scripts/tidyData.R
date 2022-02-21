#######################
## Data Organisation ##
#_____________________#
library(readxl)
library(hms)
source("scripts/functions.R")
set.seed(1)

##########################################
#              Plot data                 #
##########################################

#read in data from excel files, format datetime as hms
myPlots <- read_excel("Fieldwork/Evans St/dataRaw.xlsx", sheet = "EStplot_doubleObs", na = "NA")
myPlots[ , "date"] <- as.Date(myPlots$date)
myPlots$travelTime <- as_hms(myPlots$travelTime)
myPlots$setUpTime <- as_hms(myPlots$setUpTime)
myPlots$timeSearch_m <- as_hms(myPlots$timeSearch_m)
myPlots$finishTime <- as_hms(myPlots$finishTime)

#remove calculated/converted columns created in excel or only used in pilots
drops <- c("surveyStartTime", "surveyEndTime", "TimeTotal", "totTimeMins", "TimeSearch1")
myPlots <- myPlots[ , !(names(myPlots) %in% drops)]

#rename columns to match in dist and plots
names(myPlots)[names(myPlots) == "travelTime"] <- "travelStartTime"

#add a column for total unit time
myPlots$unitTime <- as_hms(myPlots$finishTime-myPlots$travelStartTime)

#separate by species and plot size (and remove pilot plots)
mySmono1m <- myPlots[myPlots$target=="Stackhousia monogyna" & myPlots$area_m2==1, ]
mySmono4m <- myPlots[myPlots$target=="Stackhousia monogyna" & myPlots$area_m2==4 &
                       myPlots$plotID!=329, ]

mySquadPlots <- myPlots[myPlots$target=="Senecio quadridentatus" & myPlots$area_m2==12.25, ]

##########################################
#              Distance data             #
##########################################

#read in data from excel files, format date as date and datetime as hms

#distance survey observation data
myDistData <- read_excel("Fieldwork/Evans St/dataRaw.xlsx", sheet = "EStTrans_data", na = "NA")
myDistData$date <- as.Date(myDistData$date)

#distance survey observations for grouped surveys
myDistGrouped <- read_excel("Fieldwork/Evans St/dataRaw.xlsx", sheet = "EStTrans_dataGrouped", na = "NA")
myDistGrouped[ , "date"] <- as.Date(myDistGrouped$date)

#transect and timing details
myDistDetails <- read_excel("Fieldwork/Evans St/dataRaw.xlsx", sheet = "EStTrans_details", na = "NA")
myDistDetails[ , "date"] <- as.Date(myDistDetails$date)
myDistDetails$travelStartTime <- as_hms(myDistDetails$travelStartTime)
myDistDetails$setUpTime <- as_hms(myDistDetails$setUpTime)
myDistDetails$surveyTime <- as_hms(myDistDetails$surveyTime)
myDistDetails$halfwayTime <- as_hms(myDistDetails$halfwayTime)
myDistDetails$finishTime <- as_hms(myDistDetails$finishTime)
myDistDetails$timeMeasuring <- as_hms(myDistDetails$timeMeasuring)
myDistDetails$halfwayMeasure <- as_hms(myDistDetails$halfwayMeasure)

#remove calculated/converted columns created in excel or only used in pilots
remove <- c("setUpDec", "surveyTdec", "timeTot", "timeTotdec", "measureTdec")

myDistDetails <- myDistDetails[ , !(names(myDistDetails) %in% remove)]

##########################################
#              Correction                #
##########################################

#one transect was sampled twice (a transcription oopsie), 
#choose one at random to remove (random choice; remove earlier one)
myDistDetails <- myDistDetails[!(myDistDetails$transectID==29698 & myDistDetails$date=="2021-10-26"), ]

##########################################

#add a unit time column (same as above for plots)
myDistDetails$unitTime <- as_hms(myDistDetails$finishTime-myDistDetails$travelStartTime)

##########################################
#              Corrections               #
##########################################

#Adjusting times where field notes indicate the start/finish/measure/set up
#were interrupted or otherwise don't reflect the accurate time taken

check <- myDistDetails[myDistDetails$length_m==2 | myDistDetails$length_m==20, ]
check <- check[!is.na(check$length_m), ]

#survey time is NA for one survey - change length to NA to indicate not surveyed (no obs, no timing info)
myDistDetails$length_m[myDistDetails$transectID == 35042] <- NA

#transect ids where notes give time to adjust by

# 26830 (had to take a break for volunteer equipment failure)
#end time calculated by adding travel (2 mins, usually in these transects), 
#set up, survey, measure (from data)
#
endTime <- hms(0, 46, 16)
myDistDetails$unitTime[myDistDetails$transectID==26830] <- as_hms(endTime
                    - myDistDetails$travelStartTime[myDistDetails$transectID==26830])

# 48956
# lunch in middle of transect, 30 mins
myDistDetails$unitTime[myDistDetails$transectID==48956] <- 
  as_hms(myDistDetails$unitTime[myDistDetails$transectID==48956] - hms(0, 30, 0))

# unit times can be calculated even though one or more time variables are missing

# 2, 3 # start and end times approx or not recorded, but from start of 2 to 
# finish time of 3 makes sense for these two transects
start2end3 <- as_hms(myDistDetails$finishTime[myDistDetails$transectID==3]-
  myDistDetails$travelStartTime[myDistDetails$transectID==2])
myDistDetails$unitTime[myDistDetails$transectID==2] <- as_hms(start2end3 - 
  myDistDetails$unitTime[myDistDetails$transectID==3])

# start time not entered: 
# 22050 (no finish time, order of survey not clear, can't extrapolate) 
# 29696: looks like 30961 start time to end of 29696 makes sense for the total time of the two units

start30961end29696 <- as_hms(myDistDetails$finishTime[myDistDetails$transectID==29696] - 
                               myDistDetails$travelStartTime[myDistDetails$transectID==30961])
myDistDetails$unitTime[myDistDetails$transectID==30961 |myDistDetails$transectID==29696] <- as_hms(start30961end29696/2)

# finish time not entered
# 21418 and 24402: time to start of next transect is ~30 mins, survey only took <1 min with 2 min set up. Don't use for unit time.
# 33457: no start time for next transect so can't extrapolate. 
# 22050: no start or finish time, don't know what happened before or after.
# 45214: no start time for next transect (think this was the last one for the day), can't extrapolate
# 33180: extrapolating gives a unit time that doesn't make sense - given other unit times and no notes on why it 
# may have taken longer...? Don't use for unit time.

# 38842: no finish time but start time of next entry present (41062) 
myDistDetails$unitTime[myDistDetails$transectID == 38842] <- as_hms(myDistDetails$travelStartTime[myDistDetails$transectID == 41062] -
                                                              myDistDetails$travelStartTime[myDistDetails$transectID == 38842] - hms(0, 1, 0))
# 39794: no finish time but start time of next entry present (34407) 
myDistDetails$unitTime[myDistDetails$transectID == 39794] <- as_hms(myDistDetails$travelStartTime[myDistDetails$transectID == 34407] -
                                                              myDistDetails$travelStartTime[myDistDetails$transectID == 39794] - hms(0, 1, 0))
# 35121: no finish time but start time of next entry present (34163) 
myDistDetails$unitTime[myDistDetails$transectID == 35121] <- as_hms(myDistDetails$travelStartTime[myDistDetails$transectID == 34163] -
                                                              myDistDetails$travelStartTime[myDistDetails$transectID == 35121] - hms(0, 1, 0))
# 44249: no finish time but start time of next entry present (45214) 
myDistDetails$unitTime[myDistDetails$transectID == 44249] <- as_hms(myDistDetails$travelStartTime[myDistDetails$transectID == 45214] -
                                                                      myDistDetails$travelStartTime[myDistDetails$transectID == 44249] - hms(0, 1, 0))
# 35348: no finish time but start time of next entry present (35042) 
myDistDetails$unitTime[myDistDetails$transectID == 35348] <- as_hms(myDistDetails$travelStartTime[myDistDetails$transectID == 35042] -
                                                                      myDistDetails$travelStartTime[myDistDetails$transectID == 35348] - hms(0, 1, 0))
# 25873: no finish time but start time of next entry present (23042) 
myDistDetails$unitTime[myDistDetails$transectID == 25873] <- as_hms(myDistDetails$travelStartTime[myDistDetails$transectID == 23042] -
                                                                      myDistDetails$travelStartTime[myDistDetails$transectID == 25873] - hms(0, 1, 0))
# 10: interrupted by chat with people visiting site.  Can't extrapolate to start time of next transect.
# estimating unit time from adding set up, survey, est. travel and clear up time from other transects.
# ensure this comes before other adjustments so extrapolated times aren't used to estimate (base on recorded times) [2 and 3 above are ok]

noNaUnits <- myDistDetails$transectID[myDistDetails$transectID %in% 1:15 & !is.na(myDistDetails$unitTime)]

travelandClearUp <- as_hms((sum(myDistDetails$unitTime[myDistDetails$transectID %in% noNaUnits], na.rm = TRUE)- 
                      sum(myDistDetails$setUpTime[myDistDetails$transectID %in% noNaUnits], na.rm = TRUE) - 
                        sum(myDistDetails$surveyTime[myDistDetails$transectID %in% noNaUnits], na.rm = TRUE))/length(noNaUnits))
myDistDetails$unitTime[myDistDetails$transectID == 10] <- as_hms(round(travelandClearUp + myDistDetails$setUpTime[myDistDetails$transectID == 10] + 
                        myDistDetails$surveyTime[myDistDetails$transectID == 10]))

# 8: 

# ATTENTION: now thinking that this method (above for transect 10) might be better for extrapolating? 
# makes sure pee/water/snack breaks are not absorbed into the unit times.
# Also, need to track down and record the long tape set up times, average out and make sure they are
# accounted for, i.e. know when they occur inside the 'travel time' for a transect and when they do not.

#transect ids where notes suggest the transect shouldn't be used for timings 
#if only off by a min or two for start/end it's fine

# 36069, 29081, 23964, 26830
# Survey time is missing a bit (indicated by plus sign on data sheets)
# happened when the stopwatch didn't start at the beginning of the transect (usually not missing much)
# 23964, measuring time noted as not recorded (e.g. stopwatch mishap) (already NA)

dntUseSurveyTime <- c(36069, 29081, 23964, 26830)

# 50882, measuring time is missing time for first 7 detections S quad LTS method)
meanMeasureTime50882 <- as_hms(myDistDetails$timeMeasuring[myDistDetails$transectID==50882]/
                                 (nrow(myDistData[myDistData$transectID==50882,])-7))
myDistDetails$timeMeasuring[myDistDetails$transectID==50882] <- as_hms(round(myDistDetails$timeMeasuring[myDistDetails$transectID==50882]+
                                 (meanMeasureTime50882*7)))

# 10: -30s on survey time (plant people came over to chat, didn't stop the stopwatch straight away)

#check unit time makes sense given other times added together

check$timeCompare <- as_hms(check$setUpTime+check$surveyTime)

# 41386, 41391, 35042 #didn't set up bands S mono grouped
#also check plot data corrections
# 57802, 46367 # timed by second person (possibly more reliable time, S quad survey)
# 27155 (probably separate) timed by second person (S mono survey)

# where survey time and measure time are separate (rather than measure time is contained in survey time)
# survey time needs to be survey including measure (as for all other transects)
# but ensure that transect ids are noted

sepMeasTime <- c(37973, 36069, 29081, 23980, 23990, 26830, 26205)

##########################################

#separate by species and method (and remove pilot transects)

#Only transect details (no repeats), and Remove NA rows for the dfs with NAs in any of the conditional columns:
mySmonoOpt_details <- myDistDetails[myDistDetails$target=="Stackhousia monogyna" & myDistDetails$method=="Opt" 
                                    & !is.na(myDistDetails$length_m), ] 
mySmonoLTS_details <- myDistDetails[myDistDetails$target=="Stackhousia monogyna" & myDistDetails$method=="Standard"
                                    & myDistDetails$length_m==2, ]
mySmonoLTS_details <- mySmonoLTS_details[!is.na(mySmonoLTS_details$length_m), ]
mySmonoGrouped_details <- myDistDetails[myDistDetails$target=="Stackhousia monogyna" & myDistDetails$method=="Grouped"
                                        & myDistDetails$length_m==2, ]
mySmonoGrouped_details <- mySmonoGrouped_details[!is.na(mySmonoGrouped_details$length_m), ]
mySquadOpt_details <- myDistDetails[myDistDetails$target=="Senecio quadridentatus" & myDistDetails$method=="Opt"
                                    & !is.na(myDistDetails$length_m), ]
mySquadLTS_details <- myDistDetails[myDistDetails$target=="Senecio quadridentatus" & myDistDetails$method=="Standard"
                                    & myDistDetails$length_m==20 & !is.na(myDistDetails$length_m), ]


#All data (one row per observation, transect details are repeated, excludes transects with zero obs), and remove NA rows:
mySmonoOpt <- merge(x = mySmonoOpt_details[ , "transectID"], y = myDistData, by = "transectID")
#add a unique ID column for each obs in the survey (all distance surveys except grouped)
mySmonoOpt$obsID <- 1:nrow(mySmonoOpt)

##########################################
#              Correction                #
##########################################

#randomly exclude every nth measurement on the transects that were surveyed using the wrong alpha
#Just randomly delete one ninth of the measurements (adjust time calculated to account for fewer measurements). 
#You will end up with measurements to 8 ninths of the individuals (8/9 * 3/4 = 2/3).
#####!!!!!##### REMEMBER TO ADJUST TOTAL + MEASURING TIMES TO ACCOUNT FOR FEWER MEASUREMENTS TOO 
#(n obs adjusted for two of the Smono Opt transects)

propMeasured <- length(mySmonoOpt$perpDist_m[!is.na(mySmonoOpt$perpDist_m)])/length(mySmonoOpt$perpDist_m)

datesSelect <- as.Date(c("2021-10-26", "2021-10-25"))
mySubset <- mySmonoOpt[!is.na(mySmonoOpt$perpDist_m), ]
mySubset <- mySubset[mySubset$date %in% datesSelect, ]
exclude <- sample(mySubset$obsID[!is.na(mySubset$perpDist_m)], size = ceiling(1/9*nrow(mySubset)))
propMeasured_adjust <- (length(mySmonoOpt$perpDist_m[!is.na(mySmonoOpt$perpDist_m)])-length(exclude))/(length(mySmonoOpt$perpDist_m))

#which transects are the excluded records in
recsExcluded <- mySmonoOpt[mySmonoOpt$obsID %in% exclude, ]
#what is the mean time per measurement in each transect
timeAdjustTable <- aggregate.data.frame(recsExcluded$transectID, by = list(recsExcluded$transectID), FUN = length)
names(timeAdjustTable) <- c("transectID", "nObsExcluded")

obsPerTransect <- aggregate.data.frame(mySmonoOpt$perpDist_m[!is.na(mySmonoOpt$perpDist_m)], 
                                       by = list(mySmonoOpt$transectID[!is.na(mySmonoOpt$perpDist_m)]), FUN = length)
names(obsPerTransect) <- c("transectID", "nObs")
measTime <- aggregate.data.frame(as.numeric(mySmonoOpt_details$timeMeasuring), by = list(mySmonoOpt_details$transectID), FUN = unique) 
names(measTime) <- c("transectID", "measureTime")
obsPerTransect <- merge(obsPerTransect, measTime, by = "transectID")
obsPerTransect$timePerMeas <- round(obsPerTransect$measureTime/obsPerTransect$nObs)
timeAdjustTable <- merge(timeAdjustTable, obsPerTransect, by = "transectID")
timeAdjustTable$timeAdjust <- timeAdjustTable$nObsExcluded*timeAdjustTable$timePerMeas

#adjust measuring time in details table:
mySmonoOpt_details$timeMeasuring[mySmonoOpt_details$transectID %in% timeAdjustTable$transectID] <- as_hms(mySmonoOpt_details$timeMeasuring[mySmonoOpt_details$transectID %in% timeAdjustTable$transectID]-timeAdjustTable$timeAdjust)

mySmonoOpt_details$finishTimeAdjust <- mySmonoOpt_details$finishTime
mySmonoOpt_details$finishTimeAdjust[mySmonoOpt_details$transectID %in% timeAdjustTable$transectID] <- 
  as_hms(mySmonoOpt_details$finishTimeAdjust[mySmonoOpt_details$transectID %in% timeAdjustTable$transectID]-timeAdjustTable$timeAdjust)
###### TOTAL TIME FOR THESE TRANSECTS IS ADJUSTED IN THE 'timesDistFunc' function ####
# but the timesDistFunc function calculates the mean and creates a new df, 
# data in original data df remains unadjusted

#adjust unit time for these transects in mySmonoOpt_details
mySmonoOpt_details$unitTime[mySmonoOpt_details$transectID %in% timeAdjustTable$transectID] <- as_hms(mySmonoOpt_details$unitTime[mySmonoOpt_details$transectID %in% timeAdjustTable$transectID]-timeAdjustTable$timeAdjust)

mySmonoOpt$perpDist_m[mySmonoOpt$obsID %in% exclude] <- NA

##########################################

mySmonoLTS <- merge(x = mySmonoLTS_details[, "transectID"], y = myDistData, by = "transectID")
mySmonoLTS$obsID <- 1:nrow(mySmonoLTS)

mySmonoGrouped <- merge(x = mySmonoGrouped_details[, "transectID"],
                        y = myDistGrouped, by = "transectID")

mySquadOpt <- merge(x = mySquadOpt_details[,"transectID"], y = myDistData, by = "transectID")
mySquadOpt$obsID <- 1:nrow(mySquadOpt)

mySquadLTS <- merge(x = mySquadLTS_details["transectID"], y = myDistData, by = "transectID")
mySquadLTS$obsID <- 1:nrow(mySquadLTS)

rm(mySubset, check, datesSelect, exclude, remove, drops, recsExcluded, obsPerTransect, measTime, meanMeasureTime50882,
   endTime, noNaUnits, start2end3, start30961end29696, travelandClearUp)
