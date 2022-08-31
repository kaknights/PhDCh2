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

# resamplePlots function takes the dataframe of plot data (plotDF), the dataframe of summarised timings (timesDF), the budget (in minutes), and the plot type (either "plot1m" or "plot4m").  Returns a list containing the resample data, estimates of density, SE, probabilities of detection.

resamplePlots <- function(plotDf, timesDF, budget, plotType){
  
  nUnits <- round(budget/timesDF$mean_unit_time[timesDF$method == plotType])
  samp <- sample(plotDf$plotID, size = nUnits)
  
  plotDf1 <- plotDf[plotDf$plotID %in% samp, ]#this is the sample for single observer
  #area1 <- nrow(plotDf1)*unique(plotDf1$area_m2)
  make1vect <- c(plotDf1$kkPrimaryCount[!is.na(plotDf1$kkPrimaryCount)], plotDf1$SecObsPrimaryCount[!is.na(plotDf1$SecObsPrimaryCount)])
  dhat1vect <- make1vect/unique(plotDf1$area_m2)
  dhat1 <- mean(dhat1vect)
  
  SEdhat1 <- sd(dhat1vect)/sqrt(length(dhat1vect))
  
  budgetA <- budget/2 #correcting for 2 people
  nUnitsA <- ceiling(budgetA/timesDF$mean_unit_time[timesDF$method == plotType])
  sampA <- sample(plotDf1$plotID, size = nUnitsA)
  
  plotDf2 <- plotDf1[plotDf1$plotID %in% sampA, ]#this is the double observer sample
  
  #make2vect is a vector of plot count totals
  make2vect <- c((plotDf2$kkPrimaryCount[!is.na(plotDf2$kkPrimaryCount)] +
                plotDf2$SecObsSecondaryCount[!is.na(plotDf2$SecObsSecondaryCount)]), 
             (plotDf2$SecObsPrimaryCount[!is.na(plotDf2$SecObsPrimaryCount)] +
                plotDf2$kkSecondaryCount[!is.na(plotDf2$kkSecondaryCount)]))
  
  X1.1 <- sum(plotDf2$kkPrimaryCount, na.rm = TRUE)#observer 1 primary count
  X1.2 <- sum(plotDf2$kkSecondaryCount, na.rm = TRUE)#observer 1 secondary count
  X2.1 <- sum(plotDf2$SecObsSecondaryCount, na.rm = TRUE)#observer 2 secondary count
  X2.2 <- sum(plotDf2$SecObsPrimaryCount, na.rm = TRUE)#observer 2 primary count
  
  #X. are the total number observed when each observer was primary
  X.1 <- X1.1 + X2.1  
  X.2 <- X1.2 + X2.2
  X.. <- X.1 + X.2  #total overall
  #betas are the X. as a proportion of the total n observed
  beta1 <- X.1/X..
  beta2 <- X.2/X..
  
  probs <- qFunc(p1 = X1.1, s1 = X1.2, p2 = X2.2, s2 = X2.1)
  
  area <- nrow(plotDf2)*unique(plotDf2$area_m2) #total area sampled
  
  if (probs[1,3]!="NaN"){
    #var N calculations are given, not var d, so need to convert
    
  N <- X../probs[1,3]
  dhat2 <- X../probs[1,3]/area
  
  varp <- probs[1,3]*(1-probs[1,3])^2*((1/(probs[1,1]*beta1) + 1/(probs[1,2]*beta2) + 1/(probs[1,2]*(1-probs[1,1])*beta1))
                                       + 1/(probs[1,1]*(1-probs[1,2])*beta2))/X..
  
  varN <- N*(X..*varp/probs[1,3]^3 + (1 - probs[1,3])/probs[1,3])
  
  vard <- varN/area^2
  
  SEdhat2 <- sqrt(vard)/sqrt(nrow(plotDf2)) #model based estimate of SE
  
  dhat2vect <- make2vect/probs[1,3]/unique(plotDf2$area_m2)
  SEdhat2vect <- sd(dhat2vect)/sqrt(length(dhat2vect)) #empirical between-plot estimate of SE 
  
  } else {
    dhat2 <- (X..)/area
    dhat2vect <- make2vect/unique(plotDf2$area_m2)
    SEdhat2vect <- sd(dhat2vect)/sqrt(length(dhat2vect))
    SEdhat2 <- NA
  }
  
  results <- list("resamp1" = as.data.frame(plotDf1), # single observer resample
                  "singleObsD" = dhat1,
                  "singleObsSeD" = SEdhat1,
                  "resamp2" = plotDf2, # double observer resample
                  "detectabilities" = probs, # prob det for observer 1, observer 2 and overall
                  "doubleObsD" = dhat2, 
                  "se(d)_emp" = SEdhat2vect,
                  "se(d)_mod" = SEdhat2)
  #se(d)_emp is cv of the plot-level densities 
  #se(d)_mod is based on the estimates of var D and var p on the point estimates for the sample (model based)
}

resamplePlots_replacement <- function(plotDf, timesDF, budget, plotType){
  
  nUnits <- round(budget/timesDF$mean_unit_time[timesDF$method == plotType])
  samp <- sample(plotDf$plotID, size = nUnits, replace = TRUE)
  samp <- as.data.frame(samp)
  plotDf1 <- merge(samp, plotDf, by.x = "samp", by.y = "plotID")#this is the sample for single observer
  
  make1vect <- c(plotDf1$kkPrimaryCount[!is.na(plotDf1$kkPrimaryCount)], plotDf1$SecObsPrimaryCount[!is.na(plotDf1$SecObsPrimaryCount)])
  dhat1vect <- make1vect/unique(plotDf1$area_m2)
  dhat1 <- mean(dhat1vect)
  
  SEdhat1 <- sd(dhat1vect)/sqrt(length(dhat1vect))
  
  budgetA <- budget/2 #correcting for 2 people
  nUnitsA <- ceiling(budgetA/timesDF$mean_unit_time[timesDF$method == plotType])
  myID <- 1:length(plotDf1$samp)
  sampA <- sample(myID, size = nUnitsA)
  
  plotDf2 <- plotDf1[sampA,] #this is the double observer sample
  
  #make2vect is a vector of plot count totals
  make2vect <- c((plotDf2$kkPrimaryCount[!is.na(plotDf2$kkPrimaryCount)] +
                plotDf2$SecObsSecondaryCount[!is.na(plotDf2$SecObsSecondaryCount)]), 
             (plotDf2$SecObsPrimaryCount[!is.na(plotDf2$SecObsPrimaryCount)] +
                plotDf2$kkSecondaryCount[!is.na(plotDf2$kkSecondaryCount)]))
  
  X1.1 <- sum(plotDf2$kkPrimaryCount, na.rm = TRUE)#observer 1 primary count
  X1.2 <- sum(plotDf2$kkSecondaryCount, na.rm = TRUE)#observer 1 secondary count
  X2.1 <- sum(plotDf2$SecObsSecondaryCount, na.rm = TRUE)#observer 2 secondary count
  X2.2 <- sum(plotDf2$SecObsPrimaryCount, na.rm = TRUE)#observer 2 primary count
  
  #X. are the total number observed when each observer was primary
  X.1 <- X1.1 + X2.1  
  X.2 <- X1.2 + X2.2
  X.. <- X.1 + X.2  #total overall
  #betas are the X. as a proportion of the total n observed
  beta1 <- X.1/X..
  beta2 <- X.2/X..
  
  probs <- qFunc(p1 = X1.1, s1 = X1.2, p2 = X2.2, s2 = X2.1)
  
  area <- nrow(plotDf2)*unique(plotDf2$area_m2) #total area sampled
  
  if (probs[1,3]!="NaN"){
    #var N calculations are given, not var d, so need to convert
    
  N <- X../probs[1,3]
  dhat2 <- X../probs[1,3]/area
  
  varp <- probs[1,3]*(1-probs[1,3])^2*((1/(probs[1,1]*beta1) + 1/(probs[1,2]*beta2) + 1/(probs[1,2]*(1-probs[1,1])*beta1))
                                       + 1/(probs[1,1]*(1-probs[1,2])*beta2))/X..
  
  varN <- N*(X..*varp/probs[1,3]^3 + (1 - probs[1,3])/probs[1,3])
  
  vard <- varN/area^2
  
  SEdhat2 <- sqrt(vard)/sqrt(nrow(plotDf2)) #model based estimate of SE
  
  dhat2vect <- make2vect/probs[1,3]/unique(plotDf2$area_m2)
  SEdhat2vect <- sd(dhat2vect)/sqrt(length(dhat2vect)) #empirical between-plot estimate of SE 
  
  } else {
    dhat2 <- (X..)/area
    dhat2vect <- make2vect/unique(plotDf2$area_m2)
    SEdhat2vect <- sd(dhat2vect)/sqrt(length(dhat2vect))
    SEdhat2 <- NA
  }
  
  results <- list("resamp1" = as.data.frame(plotDf1), # single observer resample
                  "singleObsD" = dhat1,
                  "singleObsSeD" = SEdhat1,
                  "resamp2" = plotDf2, # double observer resample
                  "detectabilities" = probs, # prob det for observer 1, observer 2 and overall
                  "doubleObsD" = dhat2, 
                  "se(d)_emp" = SEdhat2vect,
                  "se(d)_mod" = SEdhat2)
  #se(d)_emp is cv of the plot-level densities 
  #se(d)_mod is based on the estimates of var D and var p on the point estimates for the sample (model based)
}

# Test info ----
# plotDf <- mySquadPlots
# timesDF <- times
# budget <- 360
# plotType <- "plot"
