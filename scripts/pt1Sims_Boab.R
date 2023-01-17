# bias/precision simulations from scratch

library(Distance)

# problem: distance sims over-estimating
#NOTE: length per individual transect, l (the sampling unit, such that sum of li = L, where L is the total length of transect), is arbitrarily chosen - we don't have an optimisation for this quantity.
#NOTE: the suffix _1 on distResults in Ch2/dataSim/prelimMethods/ds means d = 10, sigm = 2, w = 5, budget = 960, cl = 1, Cm = 0.5, C_0 = 10, l = 5 (starting pt) - hn with no adjustment terms. Corresponding plot sims are small (1m^2, set up cost 3 mins) and large (16m^2, set up cost 10 mins)
# suffix _2 is Senecio estimates from field data - hn with adjustment terms
# suffix _3 is Stackhousia estimates from field data - hn with adjustment terms

distEsts <- read.table("Ch2/results/fieldEstimates.txt")
plotEsts <- read.table("Ch2/results/plotFieldEstimates.txt")

#_____________________

# POPULATION PARS ---- 
#_____________________
set.seed(1)
d <- 10 #distEsts$estD[2] # individuals per m^2
sigm <- 2 #distEsts$sigma[2] 

#__________________

# SURVEY PARS ----
#__________________

budget <- 960 # 16 hours (2 days) = 960 mins
areaTot <- 100000 # m^2, area over which we want the abundance or average density estimate, not the covered area - format(1e+05, scientific = FALSE)

w <- 5 #distEsts$W[2] # m
mu <- sqrt(pi*sigm^2/2)
E <- 2*mu*d # untruncated (if no limit to distance, you would expect this rate of encounter)

Cl <- 1 #E*distEsts$Cw[2] # mins per m of transect just walking/searching, averaged over the whole survey
Cm <- 0.5 #distEsts$Cm[2] # half a minute per individual to measure

C_0 <- 10 #distEsts$nonSearchCost[2] # set up/travel between sites, in mins per sampling unit
C_ <- (E*Cm)+Cl # cost of surveying per unit length
l <- 5 # m; length of transect per sampling unit
C_1 <- C_*l # cost of surveying in mins per sampling unit
C_k <- C_0+C_1 # total cost per sampling unit

k <- budget/C_k #number of sampling units in the survey 

# recalc l so that k can be a round number (check with C_k div by budget)

#C_k <- budget/round(k)
#C_0 + C_1 <- budget/round(k)
#C_0 + C_ * l <- budget/round(k)
l <- (budget/round(k) - C_0)/C_
k <- round(k)

L <- k*l # total length of transect in the survey
covdA <- L*2*w # m^2; area covered by the survey
est_n <- L*E # estimated number of indivs detected in the survey
tot_N <- covdA*d # total number of indivs present in the covered area

#___________________________________________________

# SIMULATE SURVEY AND PERFORM DISTANCE ANALYSIS ----
#___________________________________________________

# comment out/in adjustment terms in the ds model
distanceSims <- function(d, l, w, sigm, areaTot){

N <- rpois(n = k, lambda = d*l*2*w) # random draw; number of individuals in area covered by one sampling unit
dists <- numeric(length = sum(N)) # vector to store distances
transName <- character(length = sum(N))
  
# random draw; Assign a distance to each individual
for (i in 1:length(N)){
  myDists <- runif(N[i], min = 0, max = w)
  strt <- if(i == 1){0} else {sum(N[1:i-1])+1} 
  end <- sum(N[1:i])
  dists[strt:end] <- myDists
  transName[strt:end] <- i
}

# bernoulli trial on probability of detection for each individual

trial <- rbinom(length(dists), size = 1, prob = exp(-dists^2/(2*sigm^2)))

#map obs to transects for distance tables
mapTab <- data.frame("distance" = dists, 
                     "Sample.Label" = transName,
                     "trial" = trial)

obs <- mapTab[mapTab$trial==TRUE, ]
obs$object <- 1:nrow(obs)

# create tables: region, sample, obs and data

region.table <- data.frame("Region.Label" = "Area_A", "Area" = 
                                 areaTot)
sample.table <- data.frame("Region.Label" = "Area_A", 
                           "Sample.Label" = 1:k,
                           "Effort" = l)
obs.table <- data.frame("Region.Label" = "Area_A", 
                        "Sample.Label" = obs$Sample.Label,
                        "object" = obs$object)
data <- obs[,-3]

# analyse: model with ds function, save key results
#NOTE: truncation argument - by default the function uses the largest distance as w unless w is supplied.  As simulated obs were made up to w specified by me in the sims, we should supply w.

myMod <- ds(data = data, truncation = w, transect = "line", 
            formula = ~1, key = "hn", #adjustment = NULL, 
            region.table = region.table, 
            sample.table = sample.table,
            obs.table = obs.table, quiet = TRUE)
myModSummary <- summary(myMod)

myResults <- c(nrow(data), myModSummary$dht$individuals$D$Estimate, exp(myModSummary$ds$coeff$key.scale$estimate), myModSummary$dht$individuals$average.p, myModSummary$dht$individuals$summary$ER, myModSummary$dht$individuals$summary$se.ER)

}

#__________________________________________

# REPEAT ANALYSIS AND COLLATE RESULTS ----
#__________________________________________

nSims <- 10000
mySims <- data.frame("n_obs" = integer(nSims), "est_D" = numeric(nSims), "est_sigm" = numeric(nSims), 
                     "est_p" = numeric(nSims), "ER" = numeric(nSims), "se.ER" = numeric(nSims))

system.time({
 for (i in 1:nSims){
 print(paste("sim number", i)) 
 try(sims <- distanceSims(d, l, w, sigm, areaTot))
 
  if(exists("mySims")){
    mySims[i,] <- sims
  }

}  
})
 
 mySims$error <- mySims$est_D-d

distBias <- mean(mySims$error)

boxplot(mySims$error, horizontal = TRUE, xlab = "error in estimated density (indiv/m^2)")

# write.csv(mySims, "Ch2/dataSim/prelimMethods/ds/distResults_3.csv")
# write.csv(mySims, "Ch2/dataSim/prelimMethods/ds/distResults_2.csv")
# write.csv(mySims, "Ch2/dataSim/prelimMethods/ds/distResults_1.csv")

#______________________

# PLOT SURVEY PARS ----
#______________________

# plot sizes and expected counts

plotSize <- 16 #m^2
expN <- d*plotSize

# search times (effort) for different detection probabilities
probDet <- seq(0.5, 0.99, by = 0.01)
We <- 2*mu #effective search width

# small plots
# setUp <- 3 #mins; effectively just travel time (consider locating the point then just put the frame down)
# searchPath <- (log(1-probDet)*plotSize)/-We #gives length of search path in m
# t <- searchPath/(1/Cl) # 1/Cl gives speed in m per min, t is time in mins

# large plots
setUp <- 10 # plotEsts$setUpCost[3] #mins
searchPath <- (log(1-probDet)*plotSize)/-We
t <- searchPath/(1/Cl)

# n plots in the sample
S <- budget/(setUp+t)
#S_16 <- budget/(setUp16+t_16)

#___________________________

# SIMULATE PLOT SURVEYS ----
#___________________________

# change folder names - either small/large or sm/sq (comment out as needed)
myPlotSims <- function(S, expN, probDet, nSims, plotSize){
  for (i in 1:length(probDet)){
    print(paste("prob. Det. ", probDet[i]))
    myPlotResults <- data.frame("simID" = 1:nSims,
                              "estD" = numeric(nSims),
                              "error" = numeric(nSims)
                              )
    for (ii in 1:nSims){
      nDraw <- rpois(n = S[i], lambda = expN*probDet[i]) 
      
      myPlotResults$simID[ii] <- ii
      myPlotResults$estD[ii] <- mean(nDraw)/plotSize
      myPlotResults$error[ii] <- myPlotResults$estD[ii]-d
    }
    # folder <- if (plotSize == 1){
    #   "sm1"
    # } else {
    #   if (plotSize == 4) {
    #     "sm4"
    #   } else {
    #     "sq"
    #   }
    # }
   #
    folder <- ifelse(plotSize == 1, "smallPlot", "largePlot")               
  write.table(myPlotResults, paste0("Ch2/dataSim/prelimMethods/", folder, "/simResults_P_", probDet[i],".txt"))
  }
  
}

system.time({
  
PlotSims <- myPlotSims(S = S, expN = expN, probDet = probDet, nSims = 10000, plotSize = 16)
})

# change folder names - either small/large or sm/sq (comment out as needed)
plotSimResults <- function(probDet, plotSize){
  myDF <- data.frame("prob.det" = probDet,
                     "mean_estD" = numeric(length(probDet)),
                     "mean_error" = numeric(length(probDet)),
                     "sd_D" = numeric(length(probDet)))
  for (i in 1:length(probDet)){
    # folder <- if (plotSize == 1){
    #   "sm1"
    # } else {
    #   if (plotSize == 4) {
    #     "sm4"
    #   } else {
    #     "sq"
    #   }
    # }
    folder <- ifelse(plotSize == 1, "smallPlot", "largePlot") 
    data <- read.table(paste0("Ch2/dataSim/prelimMethods/", folder, "/simResults_P_", probDet[i],".txt"))
    myDF$mean_estD[i] <- mean(data$estD)
    myDF$mean_error[i] <- mean(data$error)
    myDF$sd_D[i] <- sd(data$estD)
  }
  return(myDF)
}

plotresults <- plotSimResults(probDet = probDet, plotSize = 16)

# write.table(plotresults, "Ch2/dataSim/prelimMethods/smallPlot/collatedResults_1.txt")
# write.table(plotresults, "Ch2/dataSim/prelimMethods/largePlot/collatedResults_1.txt")
# write.table(plotresults, "Ch2/dataSim/prelimMethods/sm1/collatedResults_3.txt")
# write.table(plotresults, "Ch2/dataSim/prelimMethods/sm4/collatedResults_3.txt") 
# write.table(plotresults, "Ch2/dataSim/prelimMethods/sq/collatedResults_2.txt")
