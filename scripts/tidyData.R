#######################
## Data Organisation ##
#_____________________#
library(readxl)
library(hms)
source("scripts/functions.R")
source("scripts/optVars_pilot.R")
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

#distance survey data
myDistData <- read_excel("Fieldwork/Evans St/dataRaw.xlsx", sheet = "EStTrans_data", na = "NA")
myDistData$date <- as.Date(myDistData$date)

#distance survey data for grouped surveys
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

#add a unit time column (same as above for plots)
myDistDetails$unitTime <- as_hms(myDistDetails$finishTime-myDistDetails$travelStartTime)

##########################################
#              Corrections               #
##########################################

#Adjusting times where field notes indicate the start/finish/measure/set up
#were interrupted or otherwise don't reflect the accurate time taken

### NOTE
# In all cases (except typos) the raw data remains unchanged, only calculated #
# columns are updated to reflect the adjustment                               #
                                                                            ###  

#summarising all transects excluding pilots 
check <- myDistDetails[myDistDetails$length_m==2 | myDistDetails$length_m==20, ]
check <- check[!is.na(check$length_m), ]

#survey time is NA for one survey - change length to NA to indicate not surveyed (no obs, no timing info)
myDistDetails$length_m[myDistDetails$transectID == 35042] <- NA #S mono grp

#transect ids where notes give time to adjust by
#______________________________________________________

# 26830 (had to take a break for volunteer equipment failure) (S mono LTS)
#end time calculated by adding travel (2 mins, usually in these transects), 
#set up, survey, measure (from data)
#
endTime <- hms(0, 46, 16)
myDistDetails$unitTime[myDistDetails$transectID==26830] <- as_hms(endTime
                    - myDistDetails$travelStartTime[myDistDetails$transectID==26830])

# 48956 (S quad LTS)
# lunch in middle of transect, 30 mins
myDistDetails$unitTime[myDistDetails$transectID==48956] <- 
  as_hms(myDistDetails$unitTime[myDistDetails$transectID==48956] - hms(0, 30, 0))

# unit times can be calculated even though one or more time variables are missing
#______________________________________________________

# transects 2, 3 (S quad Opt)
# start and end times approx or not recorded, but from start of 2 to 
# finish time of 3 makes sense for these two transects, make each half the total
start2end3 <- as_hms(myDistDetails$finishTime[myDistDetails$transectID==3]-
  myDistDetails$travelStartTime[myDistDetails$transectID==2])
myDistDetails$unitTime[myDistDetails$transectID==2] <- as_hms(start2end3 - 
  myDistDetails$unitTime[myDistDetails$transectID==3])

# start time not entered: 
# 22050 (no finish time, order of survey not clear, can't extrapolate) (S mono Opt) NOT CHANGED
# 29696: looks like 30961 start time to end of 29696 (S mono grouped) makes sense for the total time of the two units

start30961end29696 <- as_hms(myDistDetails$finishTime[myDistDetails$transectID==29696] - 
                               myDistDetails$travelStartTime[myDistDetails$transectID==30961])
myDistDetails$unitTime[myDistDetails$transectID==30961 |myDistDetails$transectID==29696] <- as_hms(start30961end29696/2)

# finish time not entered:
# 21418 and 24402: NOT CHANGED (Smono Opt) time to start of next transect is ~30 mins, survey only took <1 min with 2 min set up. Don't use for unit time.
# 33457: NOT CHANGED (S mono Opt) no start time for next transect so can't extrapolate NOT CHANGED. 
# 22050: NOT CHANGED (S mono Opt) no start or finish time, don't know what happened before or after.
# 45214: NOT CHANGED (S mono Opt) no start time for next transect (think this was the last one for the day), can't extrapolate
# 33180: NOT CHANGED (S mono Opt) extrapolating gives a unit time that doesn't make sense - given other unit times and no notes on why it 
# may have taken longer...? Don't use for unit time.
# 33457: (S mono Opt) no finish time, next transect has no start time NOT CHANGED
# 29698: (S mono Opt) no finish time, start time of next transect doesn't make sense (too long) NOT CHANGED

# 38842: (S mono Opt) no finish time but start time of next entry present (41062) 
myDistDetails$unitTime[myDistDetails$transectID == 38842] <- as_hms(myDistDetails$travelStartTime[myDistDetails$transectID == 41062] -
                                                              myDistDetails$travelStartTime[myDistDetails$transectID == 38842] )
# 39794: (S mono Opt) no finish time but start time of next entry present (34407) 
myDistDetails$unitTime[myDistDetails$transectID == 39794] <- as_hms(myDistDetails$travelStartTime[myDistDetails$transectID == 34407] -
                                                              myDistDetails$travelStartTime[myDistDetails$transectID == 39794])
# 35121: (S mono Opt) no finish time but start time of next entry present (34163) 
myDistDetails$unitTime[myDistDetails$transectID == 35121] <- as_hms(myDistDetails$travelStartTime[myDistDetails$transectID == 34163] -
                                                              myDistDetails$travelStartTime[myDistDetails$transectID == 35121])
# 44249: (S mono Opt) no finish time but start time of next entry present (45214) 
myDistDetails$unitTime[myDistDetails$transectID == 44249] <- as_hms(myDistDetails$travelStartTime[myDistDetails$transectID == 45214] -
                                                                      myDistDetails$travelStartTime[myDistDetails$transectID == 44249])
# 35348: (S mono grouped) no finish time but start time of next entry present (35042) 
myDistDetails$unitTime[myDistDetails$transectID == 35348] <- as_hms(myDistDetails$travelStartTime[myDistDetails$transectID == 35042] -
                                                                      myDistDetails$travelStartTime[myDistDetails$transectID == 35348])
# 25873: (S mono Opt) no finish time but start time of next entry present (23042) 
myDistDetails$unitTime[myDistDetails$transectID == 25873] <- as_hms(myDistDetails$travelStartTime[myDistDetails$transectID == 23042] -
                                                                      myDistDetails$travelStartTime[myDistDetails$transectID == 25873])

# 10: interrupted by chat with people visiting site.  Can't extrapolate to start time of next transect.
# estimating unit time from adding set up, survey, est. travel and clear up time from other transects.
# ensure this comes before other adjustments so extrapolated times aren't used to estimate (base on recorded times) [2 and 3 above are ok]

noNaUnits <- myDistDetails$transectID[myDistDetails$transectID %in% 1:15 & !is.na(myDistDetails$unitTime)]
# all non-NA unit times in transects 1-15 also have set up and survey time

travelandClearUp <- as_hms((sum(myDistDetails$unitTime[myDistDetails$transectID %in% noNaUnits], na.rm = TRUE)- 
                      sum(myDistDetails$setUpTime[myDistDetails$transectID %in% noNaUnits], na.rm = TRUE) - 
                        sum(myDistDetails$surveyTime[myDistDetails$transectID %in% noNaUnits], na.rm = TRUE))/length(noNaUnits))
myDistDetails$unitTime[myDistDetails$transectID == 10] <- as_hms(round(travelandClearUp + myDistDetails$setUpTime[myDistDetails$transectID == 10] + 
                        myDistDetails$surveyTime[myDistDetails$transectID == 10]))

# 8: S quad Opt, followed by 50196 (S quad LTS)

myDistDetails$unitTime[myDistDetails$transectID == 8] <- as_hms(round(travelandClearUp + myDistDetails$setUpTime[myDistDetails$transectID == 8] + 
                                                                         myDistDetails$surveyTime[myDistDetails$transectID == 8]))

# ATTENTION: now thinking that this method (above for transect 10) might be better for extrapolating? 
# makes sure pee/water/snack breaks are not absorbed into the unit times.

####
# SUMMARY of above: any NA unit times that can be estimated #
# reliably have been estimated. Remaining NA unit times are #
# 7 of the opt transects - all zero obs, no measuring     ###

# long tape set up and clear up 
#______________________________________________________

# S mono:

#Calculate set up/clear up times for each long transect (notes in excel sheet 'refTapeSetUp')

northSetUp <- hms(0, 3, 0) #recorded 2 out of 3 times but first is unlikely to be representative
#make sure the marking locations time is added to the optimised survey
northOptMark <- as_hms(hms(0, 23, 0) - hms(0, 3, 0))#time to set up and mark minus set up

northClearUp <- hms(0, 4, 0) #recorded 1 out of 3 times

southSetUp <- hms(0, 4, 0) #recorded 1 out of 5 times
#don't add set up for grouped survey south corner - is already included
southClearUp <- hms(0, 5.5, 0) #half of the interval between finishing at one corner and starting at the next

eastSetUp <- hms(0, 5, 0) #recorded 2 out of 3 times but second is unlikely to be representative (done with back pain)
eastClearUp <- hms(0, 1, 0) #recorded 1 out of 3 times

westSetUp <- hms(0, 4.25, 0)#recorded 2 out of 3 times, this is the average
westClearUp <- hms(0, 3, 0) #recorded 1 out of 3 times

#make sure each survey (opt, grouped, LTS) has time included for four set up/clear up events
# before averaging for unit time

addTime <- as_hms(northSetUp+northClearUp+southSetUp+southClearUp+eastSetUp+eastClearUp+westSetUp+westClearUp)
addTimeGrouped <- addTime-southSetUp
addTimeOpt <- as_hms(addTime + northOptMark)
  
# S quad:

#number of occasions relative to number of transects, so equivalent time can be added to the LTS transects
# one tape 'occasion' per 3 transects, set up took 10 mins in the recorded occasion
# check if the time is already included for Opt transects

check2 <- check[check$target == "Senecio quadridentatus" & check$method == "Opt", ]
SquadCompare <- data.frame("transectID" = check2$transectID, 
                           "timedParts" = as_hms(check2$setUpTime + check2$surveyTime),
                           "unitTime" = check2$unitTime)
SquadCompare$extra <- as_hms(SquadCompare$unitTime - SquadCompare$timedParts)
#looks like times include moving the long tape between rows in the grid, and clear up
# but not all long tape times are included. 
# Looks like I didn't add the first set up of the day (everything else is included)

# add one 10 minute occasion for opt survey
# Proportionally, same amount for LTS survey (2.33 x long tape occasions):
SquadAddTimeOpt <- hms(0, 10, 0)
SquadAddTimeLTS <- hms(20, 23, 0)

#transect ids where notes suggest the transect shouldn't be used for timings 
#______________________________________________________

# (if only off by a wee bit for start/end it's fine)

# 36069 (S mono LTS), 29081 (S mono LTS), 23964 (S mono LTS), 26830 (S mono LTS)
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
myDistDetails$surveyTime[myDistDetails$transectID == 10] <- as_hms(myDistDetails$surveyTime[myDistDetails$transectID == 10] - hms(30, 0, 0)) 
myDistDetails$unitTime[myDistDetails$transectID == 10] <- as_hms(myDistDetails$unitTime[myDistDetails$transectID == 10] - hms(30, 0, 0)) 

#check unit time makes sense given other times added together

#run 'check' lines again and add
check$timeCompare <- as_hms(check$setUpTime+check$surveyTime)

# 41386, 41391, 35042 #didn't set up bands S mono grouped
#also check plot data corrections
# 57802, 46367 # timed by second person (possibly more reliable time, S quad survey)
# 27155 (probably separate) timed by second person (S mono survey)

# where survey time and measure time were recorded separately 
# (rather than measure time is contained in survey time - orig data sheets only)
# survey time needs to be survey including measure (as for all other transects - changed in excel sheet)
# but ensure that transect ids are noted

sepMeasTime <- c(37973, 36069, 29081, 23980, 23990, 26830, 26205)

##########################################
#          End of Corrections            #
##########################################

#separate by species and method (and remove pilot transects)
#______________________________________________________

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

mySmonoLTS <- merge(x = mySmonoLTS_details[, "transectID"], y = myDistData, by = "transectID")
mySmonoLTS$obsID <- 1:nrow(mySmonoLTS)

mySmonoGrouped <- merge(x = mySmonoGrouped_details[, "transectID"],
                        y = myDistGrouped, by = "transectID")

mySquadOpt <- merge(x = mySquadOpt_details[,"transectID"], y = myDistData, by = "transectID")
mySquadOpt$obsID <- 1:nrow(mySquadOpt)

mySquadLTS <- merge(x = mySquadLTS_details["transectID"], y = myDistData, by = "transectID")
mySquadLTS$obsID <- 1:nrow(mySquadLTS)

##########################################
#            Corrections                 #
##########################################

#grouped data survey vs measure time doesn't match other methods
#______________________________________________________

#survey time is always the total
#move 'measure time' (time to mark individuals) to two other columns
#one 'mark' and the other 'count'

mySmonoGrouped_details$timeMark <- mySmonoGrouped_details$timeMeasuring
mySmonoGrouped_details$timeCount <- ifelse(mySmonoGrouped_details$timeMeasuring > 0, 
                                           as_hms(mySmonoGrouped_details$surveyTime - mySmonoGrouped_details$timeMark), hms(0, 0, 0))
mySmonoGrouped_details$timeCount <- as_hms(mySmonoGrouped_details$timeCount)

#grouped surveys where bands were not set up 
timeNoBands <- c(41386, 41391, 29696, 30961, 33827, 34140, 35413, 36364) 
dntUseTime <- c(22400) # grouped surveys that should not be used in timings

#randomly exclude every nth measurement on the transects that were surveyed using the wrong alpha
#______________________________________________________

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

###### TOTAL TIME FOR THESE TRANSECTS IS ADJUSTED IN THE 'timesDistFunc' function ####
# but the timesDistFunc function calculates the mean and creates a new df, 
# data in original data df remains unadjusted

#adjust unit time for these transects in mySmonoOpt_details
mySmonoOpt_details$unitTime[mySmonoOpt_details$transectID %in% timeAdjustTable$transectID] <- as_hms(mySmonoOpt_details$unitTime[mySmonoOpt_details$transectID %in% timeAdjustTable$transectID]-timeAdjustTable$timeAdjust)

mySmonoOpt$perpDist_m[mySmonoOpt$obsID %in% exclude] <- NA

#one transect was sampled twice (a transcription oopsie)
#______________________________________________________

#choose one at random to remove (random choice; remove earlier one)
mySmonoOpt_details <- mySmonoOpt_details[!(mySmonoOpt_details$transectID==29698 & mySmonoOpt_details$date=="2021-10-26"), ]
# this record has no obs in data (no need to remove)

# 37973 sampled twice - first sample was abandoned due to back pain, 
# marked 'ignore' in 'method' on excel sheet for removal (doesn't appear in details)
# remove corresponding records in data
mySmonoLTS <- mySmonoLTS[!(mySmonoLTS$transectID==37973 & mySmonoLTS$date=="2021-10-31"), ]

# LTS (and opt) measure time needs adjusting for some Stackhousia surveys
#______________________________________________________

# where measure time recorded in raw data is one person measuring, the other writing 
# (not ideal but injury required adjusted method)
# leave measure time and survey time as raw data, 
# adjust unit time to account for longer time measuring (it's a calculated var), 
# use Cm from pilot and n measurements to estimate time to add
# measure and survey time will be adjusted in the 'times' df on 'summaries.R'

# transect IDs that need adjusting
LTSadjust <- mySmonoLTS_details$transectID[mySmonoLTS_details$timeMeasuring>0 &
                                             !is.na(mySmonoLTS_details$timeMeasuring)]
LTSadjust <- sort(LTSadjust)
# n observations on each of those transects
nObsLTS <- aggregate(mySmonoLTS$obsID[mySmonoLTS$transectID %in% LTSadjust], 
                     by = list(mySmonoLTS$transectID[mySmonoLTS$transectID %in% LTSadjust]), 
                     FUN = length)

# estimate measure time using Cm from pilot data
nObsLTS$estMeasure <- as_hms(round(hms(minutes = Cm_Smono*nObsLTS$x)))
nObsLTS$estWalk <- as_hms(round(hms(minutes = Cw_Smono*nObsLTS$x)))
names(nObsLTS) <- c("transectID", "nObs", "estMeasure", "estWalk")

checkDifference <- data.frame("transectID" = mySmonoLTS_details$transectID[mySmonoLTS_details$transectID %in% LTSadjust], 
                  "actualMeasure" = as_hms(mySmonoLTS_details$timeMeasuring[mySmonoLTS_details$transectID %in% LTSadjust]),
                  "actualWalk" = as_hms(mySmonoLTS_details$surveyTime[mySmonoLTS_details$transectID %in% LTSadjust]-
                               mySmonoLTS_details$timeMeasuring[mySmonoLTS_details$transectID %in% LTSadjust]))
checkDifference <- merge(nObsLTS, checkDifference, by = "transectID")

#actual and estimated times are very different - but Cm (from pilot - one person measure/write) 
#seems reasonable so we'll go with it

checkDifference$adjustAmountSecs <- round((checkDifference$estMeasure - checkDifference$actualMeasure))
mySmonoLTS_details$timeMeasAdjust <- NA
mySmonoLTS_details$timeMeasAdjust[mySmonoLTS_details$transectID %in% LTSadjust] <- checkDifference$adjustAmountSecs

mySmonoLTS_details$unitTime[mySmonoLTS_details$transectID %in% LTSadjust] <- 
  as_hms(mySmonoLTS_details$unitTime[mySmonoLTS_details$transectID %in% LTSadjust] + 
  mySmonoLTS_details$timeMeasAdjust[mySmonoLTS_details$transectID %in% LTSadjust])

# Same process for opt data
Optadjust <- mySmonoOpt_details$transectID[mySmonoOpt_details$timeMeasuring>0 &
                                             !is.na(mySmonoOpt_details$timeMeasuring) &
                                             mySmonoOpt_details$date == "2021-11-04"]
nObsOpt <- aggregate(mySmonoOpt$obsID[mySmonoOpt$transectID %in% Optadjust & 
                                        !is.na(mySmonoOpt$perpDist_m)], 
                     by = list(mySmonoOpt$transectID[mySmonoOpt$transectID %in% Optadjust& 
                                                       !is.na(mySmonoOpt$perpDist_m)]), 
                     FUN = length) #using only measured obs
nObsOpt$estMeasure <- as_hms(round(hms(minutes = Cm_Smono*nObsOpt$x)))
nObsOpt$estWalk <- as_hms(round(hms(minutes = Cw_Smono*nObsOpt$x)))

names(nObsOpt) <- c("transectID", "nObs", "estMeasure", "estWalk")

checkDiffOpt <- data.frame("transectID" = mySmonoOpt_details$transectID[mySmonoOpt_details$transectID %in% Optadjust], 
                         "actualMeasure" = as_hms(mySmonoOpt_details$timeMeasuring[mySmonoOpt_details$transectID %in% Optadjust]),
                         "actualWalk" = as_hms(mySmonoOpt_details$surveyTime[mySmonoOpt_details$transectID %in% Optadjust]-
                                                     mySmonoOpt_details$timeMeasuring[mySmonoOpt_details$transectID %in% Optadjust]))
checkDiffOpt <- merge(nObsOpt, checkDiffOpt, by = "transectID")

checkDiffOpt$adjustAmountSecs <- round((checkDiffOpt$estMeasure - checkDiffOpt$actualMeasure))
mySmonoOpt_details$timeMeasAdjust <- NA
mySmonoOpt_details$timeMeasAdjust[mySmonoOpt_details$transectID %in% Optadjust] <- checkDiffOpt$adjustAmountSecs

mySmonoOpt_details$unitTime[mySmonoOpt_details$transectID %in% Optadjust] <- 
  as_hms(mySmonoOpt_details$unitTime[mySmonoOpt_details$transectID %in% Optadjust] + 
           mySmonoOpt_details$timeMeasAdjust[mySmonoOpt_details$transectID %in% Optadjust])

##########################################
#          End of Corrections            #
##########################################

#check all the observations are paired with a transect on the details list
allRecords <- merge(myDistDetails, myDistData, by = "transectID", all = TRUE)
obsOnly <- allRecords[is.na(allRecords$date.x) & allRecords$method!="Ignore", ]
#zero observed targets that are not linked to a transect in the details list
#any evidence of bias?

rm(mySubset, check, check2, datesSelect, exclude, remove, drops, recsExcluded, 
obsPerTransect, measTime, meanMeasureTime50882,endTime, noNaUnits, 
start2end3, start30961end29696, travelandClearUp, allRecords, obsOnly,
propMeasured, propMeasured_adjust, LTSadjust, nObsLTS, nObsOpt, SquadCompare,
eastClearUp, eastSetUp, northClearUp, northSetUp, southClearUp, southSetUp,
westClearUp, westSetUp)

############### What has been adjusted
############### Write notes here to print to console:

print("unit times for 12 transects have been adjusted/estimated using information from field notes, to correct for breaks or missing data in time fields")
print("S monogyna LTS transects (all) and Opt transects (4th Nov) adjusted (unit time) using Cm from pilot, due to injury forced method modification")
