#Analysis: A comparison of the performance of distance- and plot-based methods for surveys of high density species: a wildflower case study

source("scripts/tidyData.R")

#######_______________________________________________#######
#######   Data summary: sample sizes, distributions   #######
#######_______________________________________________#######

#total transect number, length, n observations

#make a table of transect number etc for each of the surveys
myDistSummary <- data.frame("target" = c("Stackhousia", "Stackhousia", "Stackhousia", "Senecio", "Senecio"), 
                            "method" = c("Optimised", "LTS", "Grouped", "Optimised", "LTS"), 
                            "N_transects" = numeric(5), "total_length_m" = numeric(5), "n_Obs" = numeric(5))

#S mono optimised
myDistSummary[1, 3:5] <- myDistSummaryFunc(details = mySmonoOpt_details, observations = mySmonoOpt)
#S mono LTS
myDistSummary[2, 3:5] <- myDistSummaryFunc(details = mySmonoLTS_details, observations = mySmonoLTS)
#S mono grouped (observations table is organised differently)
myDistSummary[myDistSummary$target=="Stackhousia" & myDistSummary$method=="Grouped", "N_transects"] <- nrow(mySmonoGrouped_details)
myDistSummary[myDistSummary$target=="Stackhousia" & myDistSummary$method=="Grouped", "total_length_m"] <- sum(mySmonoGrouped_details$length_m)
myDistSummary[myDistSummary$target=="Stackhousia" & myDistSummary$method=="Grouped", "n_Obs"] <- sum(mySmonoGrouped$count)
#S quad optimised
myDistSummary[4, 3:5] <- myDistSummaryFunc(details = mySquadOpt_details, observations = mySquadOpt)
#S quad LTS
myDistSummary[5, 3:5] <- myDistSummaryFunc(details = mySquadLTS_details, observations = mySquadLTS)

#distance histograms
# hist(mySmonoOpt$perpDist_m, xlab = "perp dist (m)", main = "S mono Optimised")
# hist(mySmonoLTS$perpDist_m, xlab = "perp dist (m)", main = "S mono LTS")
# hist(mySquadOpt$perpDist_m, xlab = "perp dist (m)", main = "S quad Optimised")
# hist(mySquadLTS$perpDist_m, xlab = "perp dist (m)", main = "S quad LTS")

myFreqGrouped <- aggregate.data.frame(x = mySmonoGrouped$count, by = list(mySmonoGrouped$band_m), FUN = sum)
names(myFreqGrouped) <- c("distBand", "count")

#n plots, mean and total counts
myPlotSummary <- data.frame("target" = c("Stackhousia", "Stackhousia", "Senecio"), "area_m2" = c(1, 4, 12.25), 
                            "nPlots" = integer(3),"count_primary" = numeric(3), "count_secondary" = numeric(3),
                            "probP1" = numeric(3), "probP2" = numeric(3), "probTot" = numeric(3), 
                            "dens_prim_m2" = numeric(3), "dens_all_m2" = numeric(3))

myPlotSummary[1, 3:10] <- plotSummaryFunc(dfname = mySmono1m)
myPlotSummary[2, 3:10] <- plotSummaryFunc(dfname = mySmono4m)
myPlotSummary[3, 3:10] <- plotSummaryFunc(dfname = mySquadPlots)

#######__________________________________#######
#######   Data summary: costs/times      #######
#######__________________________________#######

#times taken; set-up (incl. travel), sample
#TIMES IN MINS
times <- data.frame("target" = c(rep("Stackhousia", 6), rep("Senecio", 3)), 
                    "method" = c("plot1m", "plot4m", "Opt", "LTS", 
                                 "GroupedBands","GroupedNoBands",  "plot", "Opt", "LTS"), 
                    "mean_unit_time" = numeric(9), "mean_survey_time" = numeric(9),
                    "mean_other_time" = numeric(9), "n_complete_records" = numeric(9))

#mean unit time is the mean total time to sample one unit (a plot or a transect), including travel, setup and sampling time
#mean survey time is the mean time taken to complete the data collection (survey) of one sampling unit, NOT INCLUDING travel and set up time
#mean other time is the mean time per sample unit for everything EXCEPT the actual survey part

###Plot Surveys:
times[times$target == "Senecio" & times$method == "plot", 3:6] <- timesPlotFunction(df = mySquadPlots)
times[times$target == "Stackhousia" & times$method == "plot1m", c(4,6)] <- timesPlotFunction(df = mySmono1m)[-c(1,3)]
times[times$target == "Stackhousia" & times$method == "plot4m", c(4,6)] <- timesPlotFunction(df = mySmono4m)[-c(1,3)]

#Smono unit start and end include 1m and 4m plots.  
  #time for 1m and 4m plots at same location
SmonoPlots_unitTime <- as.numeric(sum(mySmono1m$finishTime-mySmono1m$travelStartTime)/nrow(mySmono1m))/60 

#estimating 1m:4m plots 40:60 set up time, subtract non-relevant times from total:

times[times$target == "Stackhousia" & times$method == "plot1m", "mean_unit_time"] <- round(SmonoPlots_unitTime - (0.6*(as.numeric(sum(mySmono4m$setUpTime))/nrow(mySmono4m))/60) - (as.numeric(sum(mySmono4m$timeSearch_m))/nrow(mySmono4m)/60),2)
times[times$target == "Stackhousia" & times$method == "plot4m", "mean_unit_time"] <- round(SmonoPlots_unitTime - (0.4*(as.numeric(sum(mySmono1m$setUpTime))/nrow(mySmono1m))/60) - (as.numeric(sum(mySmono1m$timeSearch_m))/nrow(mySmono1m)/60),2)
times[times$target == "Stackhousia" & times$method == "plot1m" | times$method == "plot4m", "mean_other_time"] <- round(times$mean_unit_time[times$target == "Stackhousia" & times$method == "plot1m" | times$method == "plot4m"] - times$mean_survey_time[times$target == "Stackhousia" & times$method == "plot1m" | times$method == "plot4m"],2)

#Distance Surveys: unit time and n complete records
#effort correct (n units total vs n units used in the calculations)
smonoOptUnitCorrect <- nrow(mySmonoOpt_details[!is.na(mySmonoOpt_details$unitTime), ])/nrow(mySmonoOpt_details)
smonoLTSUnitCorrect <- nrow(mySmonoLTS_details[!is.na(mySmonoLTS_details$unitTime), ])/nrow(mySmonoLTS_details)
#Note - LTS ratio =1, but am putting it in the code anyway, if further fixes change the ratio this is already done.

times[times$target == "Stackhousia" & times$method == "Opt", "mean_unit_time"] <- round(as.numeric(sum(mySmonoOpt_details$unitTime[!is.na(mySmonoOpt_details$unitTime)])+(addTimeOpt*smonoOptUnitCorrect)) / nrow(mySmonoOpt_details[!is.na(mySmonoOpt_details$unitTime), ])/60, 2)
times[times$target == "Stackhousia" & times$method == "Opt", "n_complete_records"] <- nrow(mySmonoOpt_details[!is.na(mySmonoOpt_details$travelStartTime) & !is.na(mySmonoOpt_details$finishTime) & !is.na(mySmonoOpt_details$surveyTime)  &!is.na(mySmonoOpt_details$timeMeasuring), ])
times[times$target == "Stackhousia" & times$method == "LTS", "mean_unit_time"] <- round(as.numeric(sum(mySmonoLTS_details$unitTime[!is.na(mySmonoLTS_details$unitTime)])+(addTime*smonoLTSUnitCorrect)) / nrow(mySmonoLTS_details[!is.na(mySmonoLTS_details$unitTime), ])/60, 2)
times[times$target == "Stackhousia" & times$method == "LTS", "n_complete_records"] <-  nrow(mySmonoLTS_details[!is.na(mySmonoLTS_details$travelStartTime) & !is.na(mySmonoLTS_details$finishTime) & !is.na(mySmonoLTS_details$surveyTime)  &!is.na(mySmonoLTS_details$timeMeasuring), ])

#stackhousia survey/measure times need to be adjusted (see notes in tidyData.R)
#using timeMeasAdjust column in details.  Unit time already adjusted in tidyData.R

#column 4 is survey time (both details have no NAs in survey time column)
times[times$target == "Stackhousia" & times$method == "Opt", 4] <- round(as.numeric(sum(mySmonoOpt_details$surveyTime) + sum(mySmonoOpt_details$timeMeasAdjust, na.rm = TRUE))/nrow(mySmonoOpt_details)/60, 2)
times[times$target == "Stackhousia" & times$method == "LTS", 4] <- round(as.numeric(sum(mySmonoLTS_details$surveyTime) + sum(mySmonoLTS_details$timeMeasAdjust, na.rm = TRUE))/nrow(mySmonoLTS_details)/60, 2)

#column 5 is other time ('add time' is already added in to unit time, so is accounted for)
times[c(3, 4), 5] <- times$mean_unit_time[c(3, 4)] - times$mean_survey_time[c(3, 4)]

#S quad 
#effort correct (n units total vs n units used in the calculations)
squadOptUnitCorrect <- nrow(mySquadOpt_details[!is.na(mySquadOpt_details$unitTime), ])/nrow(mySquadOpt_details)
squadLTSUnitCorrect <- nrow(mySquadLTS_details[!is.na(mySquadLTS_details$unitTime), ])/nrow(mySquadLTS_details)
#both ratios are 1.  No issues with n complete units in time columns (opt has missing finish times, but these have been accounted for)

times[times$target == "Senecio" & times$method == "Opt", "mean_unit_time"] <- round(as.numeric(sum(mySquadOpt_details$unitTime[!is.na(mySquadOpt_details$unitTime)])+SquadAddTimeOpt) / nrow(mySquadOpt_details[!is.na(mySquadOpt_details$unitTime), ])/60, 2)
times[times$target == "Senecio" & times$method == "Opt", "mean_survey_time"] <- round(as.numeric(sum(mySquadOpt_details$surveyTime, na.rm = TRUE))/length(mySquadOpt_details$surveyTime[!is.na(mySquadOpt_details$surveyTime)])/60,2)
times[times$target == "Senecio" & times$method == "Opt", "mean_other_time"] <- times$mean_unit_time[times$target == "Senecio" & times$method == "Opt"]-times$mean_survey_time[times$target == "Senecio" & times$method == "Opt"]
times[times$target == "Senecio" & times$method == "Opt", "n_complete_records"] <- nrow(mySquadOpt_details[!is.na(mySquadOpt_details$travelStartTime) & !is.na(mySquadOpt_details$finishTime) & !is.na(mySquadOpt_details$surveyTime)  &!is.na(mySquadOpt_details$timeMeasuring), ])

times[times$target == "Senecio" & times$method == "LTS", "mean_unit_time"] <- round(as.numeric(sum(mySquadLTS_details$unitTime[!is.na(mySquadLTS_details$unitTime)])+SquadAddTimeLTS) / nrow(mySquadLTS_details[!is.na(mySquadLTS_details$unitTime), ])/60, 2)
times[times$target == "Senecio" & times$method == "LTS", "mean_survey_time"] <- round(as.numeric(sum(mySquadLTS_details$surveyTime, na.rm = TRUE))/length(mySquadLTS_details$surveyTime[!is.na(mySquadLTS_details$surveyTime)])/60,2)
times[times$target == "Senecio" & times$method == "LTS", "mean_other_time"] <- times$mean_unit_time[times$target == "Senecio" & times$method == "LTS"]-times$mean_survey_time[times$target == "Senecio" & times$method == "LTS"]
times[times$target == "Senecio" & times$method == "LTS", "n_complete_records"] <- nrow(mySquadLTS_details[!is.na(mySquadLTS_details$travelStartTime) & !is.na(mySquadLTS_details$finishTime) & !is.na(mySquadLTS_details$surveyTime)  &!is.na(mySquadLTS_details$timeMeasuring), ])

#for grouped surveys adjustments need to be made to account for band set up
# vs no band set up time

# uses objects created in tidyData.R
#unit times
#unit corrections (n units total vs n units used in calculations)
banded <- mySmonoGrouped_details[!(mySmonoGrouped_details$transectID %in% timeNoBands) & 
                                   mySmonoGrouped_details$transectID != dntUseTime, ]
bandedUnitCorrect <- nrow(banded)/nrow(mySmonoGrouped_details)

times[times$method == "GroupedBands", "mean_unit_time"] <- round(as.numeric((sum(banded$unitTime)+ (addTimeGrouped*bandedUnitCorrect))/nrow(banded))/60, 2)
times[times$method == "GroupedBands", "mean_survey_time"] <- round(as.numeric(sum(mySmonoGrouped_details$surveyTime, na.rm = TRUE)/nrow(mySmonoGrouped_details)/60), 2)
#note - there are no NAs in survey time, but for unknown reasons this didn't run without na.rm = TRUE
times[times$method == "GroupedBands", "mean_other_time"] <- times$mean_unit_time[times$method == "GroupedBands"] -
                                                                 times$mean_survey_time[times$method == "GroupedBands"] 
times[times$method == "GroupedBands", "n_complete_records"] <-   
                                            nrow(banded[!is.na(banded$travelStartTime) & 
                                            !is.na(banded$finishTime) & 
                                            !is.na(banded$surveyTime)  &
                                            !is.na(banded$timeMeasuring), ])
notBanded <- mySmonoGrouped_details[mySmonoGrouped_details$transectID %in% timeNoBands &
                                      mySmonoGrouped_details$transectID != dntUseTime, ]
notbandedUnitCorrect <- nrow(notBanded)/nrow(mySmonoGrouped_details)

times[times$method == "GroupedNoBands", "mean_unit_time"] <- round(as.numeric((sum(notBanded$unitTime)+(addTimeGrouped*notbandedUnitCorrect))/nrow(notBanded))/60, 2)
times[times$method == "GroupedNoBands", "mean_survey_time"] <- round(as.numeric(sum(mySmonoGrouped_details$surveyTime, na.rm = TRUE)/nrow(mySmonoGrouped_details)/60), 2)
#note - there are no NAs in survey time, but for unknown reasons this didn't run without na.rm = TRUE
times[times$method == "GroupedNoBands", "mean_other_time"] <- times$mean_unit_time[times$method == "GroupedNoBands"] -
                                                                  times$mean_survey_time[times$method == "GroupedNoBands"] 
times[times$method == "GroupedNoBands", "n_complete_records"] <-   
                                            nrow(notBanded[!is.na(notBanded$travelStartTime) & 
                                            !is.na(notBanded$finishTime) & 
                                            !is.na(notBanded$surveyTime)  &
                                            !is.na(notBanded$timeMeasuring), ])

#add a column for total effort
times$totalEffort <- times$mean_unit_time*times$n_complete_records
times$totalEffort[3] <- times$mean_unit_time[3]*40
times$totalEffort[4] <- times$mean_unit_time[4]*32
times$totalEffort[5] <- times$mean_unit_time[5]*23
times$totalEffort[7] <- times$mean_unit_time[7]*46
times$totalEffort[8] <- times$mean_unit_time[8]*13

rm(SmonoPlots_unitTime, banded, notBanded, addTime, addTimeGrouped, addTimeOpt, SquadAddTimeLTS, SquadAddTimeOpt, bandedUnitCorrect, notbandedUnitCorrect,
   smonoLTSUnitCorrect, smonoOptUnitCorrect, squadLTSUnitCorrect, squadOptUnitCorrect)
