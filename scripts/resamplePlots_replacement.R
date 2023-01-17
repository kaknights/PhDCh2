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
  #se(d)_emp is SE of the plot-level densities 
  #se(d)_mod is based on the estimates of var D and var p on the point estimates for the sample (model based)
}
