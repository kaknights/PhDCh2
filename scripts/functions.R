myDsSurvey <- function(dm, budget, dsCostT, dsCost0, sigm, w, siteAreaM){
  dsLength <- (budget-dsCost0)/dsCostT
  dsCovdArea <- 2*w*dsLength
  dsExpN <- dsCovdArea*dm
  dsRandN <- rpois(1, lambda = dsExpN) 
  distVect <- runif(dsExpN, min = 0, max = w)
  trial <- rbinom(length(distVect), 1, prob = exp(-distVect^2/(2*sigm^2)))
  detections <- distVect[trial==1]
  myData <- data.frame("object" = 1:length(detections), "distance" = detections)
  region.table <- data.frame("Region.Label" = "A", "Area" = siteAreaM)
  sample.table <- data.frame("Sample.Label" = "Test1", "Region.Label" = "A", "Effort" = dsLength)
  obs.table <- data.frame("object" = 1:length(detections), "Sample.Label" = "Test1", "Region.Label" = "A")
  model <- ds(data = myData, transect = "line", region.table = region.table, sample.table = sample.table, obs.table = obs.table)
  return(model)
}

myCV <- function(x){
  sd(x)/mean(x)
}

#function to calculate prob. detection for observer 1, observer 2 and overall
# 'p' stands for 'primary', 's' stands for 'secondary', numbers are person 1 and person 2
qFunc <- function(p1, s1, p2, s2){
  primCount <- p1*p2
  secCount <- s1*s2
  person2 <- p2*s2
  person1 <- p1*s1
  
  phatObs1 <- (primCount-secCount)/(primCount+person2)
  phatObs2 <- (primCount-secCount)/(primCount+person1)
  phat <- 1-(secCount/primCount)
  
  mydf <- data.frame("phatObs1" = phatObs1, "phatObs2" = phatObs2, "phat" = phat)
  
  return(mydf)
}

#optimal quadrat size
#s* = sqrt[c *d / (v * t)]

qSizeOpt <- function(c, t, counts, area){ #c is set up time, t is search time, area is quadrat size
  countPerM <- counts/area
  vCount <- var(countPerM)
  d <- sum(counts)/(length(counts)*area)
  
  sizeOpt <- sqrt(c*d/(vCount*t))
}

#function to calculate all the things in myPlotsSummary for one of the surveys (one row)
plotSummaryFunc <- function(dfname){
  area <- unique(dfname$area_m2)
  nPlots <- nrow(dfname)
  count_primary <- sum(dfname$kkPrimaryCount, na.rm = TRUE) + sum(dfname$SecObsPrimaryCount, na.rm = TRUE)
  count_secondary <- sum(dfname$kkSecondaryCount, na.rm = TRUE) + sum(dfname$SecObsSecondaryCount, na.rm = TRUE)
  probs <- qFunc(p1 = sum(dfname$kkPrimaryCount, na.rm = TRUE), s1 = sum(dfname$kkSecondaryCount, na.rm = TRUE),
                 p2 = sum(dfname$SecObsPrimaryCount, na.rm = TRUE), s2 = sum(dfname$SecObsSecondaryCount, na.rm = TRUE))
  dens_prim_m2 <- count_primary/(nPlots*area)
  dens_all_m2 <- (count_primary + sum(dfname$kkSecondaryCount, na.rm = TRUE) + sum(dfname$SecObsSecondaryCount, na.rm = TRUE)) /(nPlots*area)
  myThings <- data.frame(nPlots, count_primary, count_secondary, round(probs[1],2), 
                         round(probs[2],2), round(probs[3],2), round(dens_prim_m2,2), round(dens_all_m2,2))
  return(myThings)
}

#function to calculate myDistSummary for one survey (one row)
myDistSummaryFunc <- function(details, observations){
  N_transects <- nrow(details)
  total_length_m <- sum(details$length_m)
  n_Obs <- nrow(observations)
  mySummary <- data.frame(N_transects, total_length_m, n_Obs)
  return(mySummary)
}

#function to calculate the columns 3 to 6 in times summary table for plots 
#(some rows had to be calculated separately because the start and end times include two different surveys)
timesPlotFunction <- function(df){
  df1 <- df[!is.na(df$travelStartTime) & !is.na(df$finishTime), ]
  unitTime <- as.numeric(sum(df1$finishTime - df1$travelStartTime)/nrow(df1))/60
  surveyTime <- as.numeric(sum(df$timeSearch_m, na.rm = TRUE)/nrow(df[!is.na(df$timeSearch_m), ]))/60
  otherTime <- unitTime - surveyTime
  nRecs <- nrow(df[!is.na(df$travelStartTime) & !is.na(df$finishTime) & !is.na(df$timeSearch_m), ])
  
  return(c(round(unitTime,2), round(surveyTime,2), round(otherTime,2), nRecs))
}

#function to calculate columns 3 to 6 in times summary table for distance surveys
#x is the dataframe to be summarised
timesDistFunc <- function(x, timeAdjustTable){
  
  unitTime <- as.numeric(sum(x$unitTime[!is.na(x$unitTime)])) / nrow(x[!is.na(x$unitTime), ])/60
  surveyTime <- as.numeric(sum(x$surveyTime, na.rm = TRUE))/length(x$surveyTime[!is.na(x$surveyTime)])/60
  otherTime <- unitTime-surveyTime
  complete <-   nrow(x[!is.na(x$travelStartTime) & !is.na(x$finishTime) & !is.na(x$surveyTime)  &!is.na(x$timeMeasuring), ])
  results <- c(round(unitTime,2), round(surveyTime,2), round(otherTime,2), complete)
  
  return(results)
}






  
