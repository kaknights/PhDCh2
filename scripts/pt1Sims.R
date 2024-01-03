# bias/precision simulations from scratch

library(Distance)

#NOTE: the suffix _1 on distResults in Ch2/dataSim/prelimMethods/ds means d = 10, sigm = 2, w = 5, budget = 960, cl = 1, Cm = 0.5, C_0 = 10

# estimates from field data: 
distEsts <- read.table("results/fieldEstimates.txt")
plotEsts <- read.table("results/plotFieldEstimates.txt")

#_____________________

# POPULATION PARS ---- 
#_____________________

d <- distEsts$estD[2] # individuals per m^2
sigm <- distEsts$sigma[2] 

#__________________

# SURVEY PARS ----
#__________________

budget <- 960 # 16 hours (2 days) = 960 mins
areaTot <- 20000 # m^2, area over which we want the abundance or average density estimate, not the covered area - format(1e+05, scientific = FALSE)

w <- distEsts$W[2] # m
mu <- sqrt(pi*sigm^2/2)
E <- 2*mu*d # untruncated (if no limit to distance, you would expect this rate of encounter)

Cl <- E*distEsts$Cw[2] # mins per m of transect just walking/searching, averaged over the whole survey
Cm <- distEsts$Cm[2] # half a minute per individual to measure

C_0 <- distEsts$nonSearchCost[2] # set up/travel between sites, in mins per sampling unit
C_ <- (E*Cm)+Cl # cost of surveying per unit length
l <- 20 # m; length of transect per sampling unit
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
  transName[strt:end] <- LETTERS[i]
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
                           "Sample.Label" = LETTERS[1:k],
                           "Effort" = l)
obs.table <- data.frame("Region.Label" = "Area_A", 
                        "Sample.Label" = obs$Sample.Label,
                        "object" = obs$object)
data <- obs[,-3]

# analyse: model with ds function, save key results

myMod <- ds(data = data, truncation = w, transect = "line", 
            formula = ~1, key = "hn", adjustment = NULL, 
            region.table = region.table, 
            sample.table = sample.table,
            obs.table = obs.table, quiet = TRUE)
myModSummary <- summary(myMod)

myResults <- c(nrow(data), myModSummary$dht$individuals$D$Estimate, exp(myModSummary$ds$coeff$key.scale$estimate), myModSummary$dht$individuals$average.p, myModSummary$dht$individuals$summary$ER, myModSummary$dht$individuals$summary$se.ER)

}

#__________________________________________

# REPEAT ANALYSIS AND COLLATE RESULTS ----
#__________________________________________

nSims <- 1000
mySims <- data.frame("n_obs" = integer(nSims), "est_D" = numeric(nSims), "est_sigm" = numeric(nSims), 
                     "est_p" = numeric(nSims), "ER" = numeric(nSims), "se.ER" = numeric(nSims))

system.time({
for (i in 1:nSims){
 print(paste("sim number", i)) 
 sims <- distanceSims(d, l, w, sigm, areaTot)
 mySims[i,] <- sims
}
})

mySims$error <- mySims$est_D-d

distBias <- mean(mySims$error)

boxplot(mySims$error, horizontal = TRUE, xlab = "error in estimated density (indiv/m^2)")


#______________________

# PLOT SURVEY PARS ----
#______________________

# plot sizes and expected counts

plotSize16 <- 16 #m^2
expN16 <- d*plotSize16

plotSize1 <- 1
expN1 <- d*plotSize1

# search times (effort) for different detection probabilities
probDet <- seq(0.5, 0.99, by = 0.01)
We <- 2*mu #effective search width

# small plots
setUp1 <- 3 #mins; effectively just travel time (consider locating the point then just put the frame down)
searchPath1 <- (log(1-probDet)*plotSize1)/-We #gives length of search path in m
t_1 <- searchPath1/(1/Cl) # 1/Cl gives speed in m per min, t is time in mins

# large plots
setUp16 <- 10 #mins
searchPath16 <- (log(1-probDet)*plotSize16)/-We
t_16 <- searchPath16/(1/Cl)

# n plots in the sample
S_1 <- budget/(setUp1+t_1)
S_16 <- budget/(setUp16+t_16)

#___________________________

# SIMULATE PLOT SURVEYS ----
#___________________________

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
    folder <- ifelse(plotSize == 1, "smallPlot", "largePlot")
  write.table(myPlotResults, paste0("dataSim/prelimMethods/", folder, "/simResults_P_", probDet[i],".txt"))
  }
  
}

system.time({
  
smallPlotSims <- myPlotSims(S = S_1, expN = expN1, probDet = probDet, nSims = 100, plotSize = 1)
})
system.time({
  largePlotSims <- myPlotSims(S = S_16, expN = expN16, probDet = probDet, nSims = 10000, plotSize = 16)
})

plotSimResults <- function(probDet, plotSize){
  myDF <- data.frame("prob.det" = probDet,
                     "mean_estD" = numeric(length(probDet)),
                     "mean_error" = numeric(length(probDet)),
                     "sd_D" = numeric(length(probDet)))
  for (i in 1:length(probDet)){
    folder <- ifelse(plotSize == 1, "smallPlot", "largePlot")
    data <- read.table(paste0("dataSim/prelimMethods/", folder, "/simResults_P_", probDet[i],".txt"))
    myDF$mean_estD[i] <- mean(data$estD)
    myDF$mean_error[i] <- mean(data$error)
    myDF$sd_D[i] <- sd(data$estD)
  }
  return(myDF)
}

plot1results <- plotSimResults(probDet = probDet, plotSize = 1)
plot16results <- plotSimResults(probDet = probDet, plotSize = 16)

#write.table(plot1results, "dataSim/prelimMethods/smallPlot/collatedResults_1.txt")
#write.table(plot16results, "dataSim/prelimMethods/largePlot/collatedResults_1.txt")
