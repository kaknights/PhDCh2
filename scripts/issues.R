#NOTES: 
#maybe remove 23964 from analysis (3 obs, NA on measuring time, 2 sticks were missed)
#compare 'winner' (Opt for Smono) with grouped without bands
#precision of ds with differing numbers of bands? (appendix?)
# saved resamples and summary dataframes were run on 02.05.2022

# ISSUE 1 ----
# v difficult to see/distinguish the method types on graph - panels instead of overlay?
#do separate graphs for the appendix, summary graph for the main text
#distribution of density estimates is approx lognormal, CI estimates from Fewster paper???


# ISSUE 2 ---- 
# 4m plots seem to be clustering in 2 groups, not sure why
  # same issue in Senecio distance data
#Is there one very influential transect/plot?  The clusters are determined by whether or not that
#unit is in the sample?  Try to work this out
#simulate some samples with and without the highest density plots

#4m
plot(x = Plotresults4m$dhat_singleObs, y = Plotresults4m$cv_.d.single., pch = 15,
     xlab = "D hat (indiv/m^2)", main = "S mono 4m plots single observer",
     ylab = "cv (D hat)", xlim = c(0,22), ylim = c(0.3,1.1))
plot(x = Plotresults4m$dhat_doubleObs, y = Plotresults4m$cv.d.double., pch = 1,
     xlab = "D hat (indiv/m^2)", main = "S mono 4m plots double observer",
     ylab = "cv (D hat)", xlim = c(0,22), ylim = c(0.3,1.1))

#1m 
plot(x = Plotresults1m$dhat_singleObs, y = Plotresults1m$cv_.d.single., pch = 15,
     xlab = "D hat (indiv/m^2)", main = "S mono 1m plots single observer",
     ylab = "cv (D hat)", xlim = c(0,22), ylim = c(0.3,1.1))
plot(x = Plotresults1m$dhat_doubleObs, y = Plotresults1m$cv.d.double., pch = 1,
     xlab = "D hat (indiv/m^2)", main = "S mono 1m plots double observer",
     ylab = "cv (D hat)", xlim = c(0,22), ylim = c(0.3,1.1))

# ISSUE 3 ---- 
# effort for S mono LTS and 1m plots doesn't look right in times table
# work out effort properly for both species (max sample size, then resample with replacement)

#budget <- as_hms(sum(mySmono1m$unitTime)) #as of 15/02/22 this is 631 mins
#times for distance samples is much shorter!!!!!!!

#script (in markdown or source in markdown?) to run the plots

# things to go over in meeting:
# grouped no bands resampled with replacement - best solution?
# discrepancy between estimated and actual measuring times for LTS (opt not so bad)

# ISSUE 4 ----
#look at the mean of d hat and cv for each method, they should differ according to the detectability
#check detectability estimates for S mono plot resamples


# plotDdiscrep takes the results dataframe, calculates the mean single observer detectability, corrects the single observer raw density, returns the difference between corrected single and double observer mean density estimates.  +ve means double observer density is greater, -ve means corrected single observer density is greater.

plotDdiscrep <- function(results){
  myStuff1 <- sapply(FUN = mean, X = results)[c(4,5)] #mean density and cv single observer
myStuff2 <- sapply(FUN = mean, X = results[!is.na(results$probOverall),])[c(2,3)] #mean density and cv double observer

meanProbSingleObs <- mean((results$probObs1[!is.na(results$probOverall)] +
                            results$probObs2[!is.na(results$probOverall)])/2)

# mean single observer density / mean single observer detectability should be same as double obs density

result <- myStuff1[1]/meanProbSingleObs

percDiff <- 1-result/myStuff2[1]
return(percDiff)
}

# comparePlot creates plots of estimated density against cv for single, corrected single and double observer (two formats, panelled as three separate graphs, overlaid)
comparePlot <- function(results, pch, colour){
  mycolours <- viridis(4, option = colour)
 adj <- mean((results$probObs1[!is.na(results$probOverall)] +
                            results$probObs2[!is.na(results$probOverall)])/2) 
  
  par(mfrow = c(1, 3))
  plot(x = results$dhat_singleObs, y = results$cv_.d.single., pch = pch,
     xlab = "D hat (indiv/m^2)", main = "S mono 4m plots single observer",
     ylab = "cv (D hat)", xlim = c(0,22), ylim = c(0.3,1.1), col = mycolours[1])
  plot(x = results$dhat_singleObs/adj, y = (results$cv_.d.single./adj), pch = pch,
     xlab = "D hat (indiv/m^2)", main = "S mono 4m plots single observer \n adjusted for detectability",
     ylab = "cv (D hat)", xlim = c(0,22), ylim = c(0.3,1.1), col = mycolours[2])
  plot(x =results$dhat_doubleObs[!is.na(results$probOverall)], y = results$cv.d.double.[!is.na(results$probOverall)], 
     pch = pch,  xlab = "D hat (indiv/m^2)", main = "S mono 4m plots double observer",
     ylab = "cv (D hat)", xlim = c(0,22), ylim = c(0.3,1.1), col = mycolours[3])
  
  par(mfrow = c(1,1))
  plot(x = results$dhat_singleObs, y = results$cv_.d.single., pch = pch,
     xlab = "D hat (indiv/m^2)", 
     ylab = "cv (D hat)", xlim = c(0,22), ylim = c(0.3,1.1), col = mycolours[1])
  points(x = results$dhat_singleObs/adj, y = (results$cv_.d.single./adj), pch = pch,
     xlab = "D hat (indiv/m^2)", 
     ylab = "cv (D hat)", xlim = c(0,22), ylim = c(0.3,1.1), col = mycolours[2])
  points(x =results$dhat_doubleObs[!is.na(results$probOverall)], y = results$cv.d.double.[!is.na(results$probOverall)], 
     pch = pch, xlab = "D hat (indiv/m^2)", 
     ylab = "cv (D hat)", xlim = c(0,22), ylim = c(0.3,1.1), col = mycolours[3])
  legend(x = 12.5, y = 1, legend = c("single observer", "single observer adjusted", "double observer"),
     pch = pch, col = mycolours[1:3], bty = "n")
}

# these are sumamry dataframes of results using different random seeds immediately before the first plot resample (1:5, indicated by the last digit in file name)
# 4m plots
results1 <- read.table("dataSim/resampleSmono/plot4m-300_1.txt")
results2 <- read.table("dataSim/resampleSmono/plot4m-300_2.txt")
results3 <- read.table("dataSim/resampleSmono/plot4m-300_3.txt")
results4 <- read.table("dataSim/resampleSmono/plot4m-300_4.txt")
results5 <- read.table("dataSim/resampleSmono/plot4m-300_5.txt")
# 1m plots
results6 <- read.table("dataSim/resampleSmono/plot1m-300_1.txt")
results7 <- read.table("dataSim/resampleSmono/plot1m-300_2.txt")
results8 <- read.table("dataSim/resampleSmono/plot1m-300_3.txt")
results9 <- read.table("dataSim/resampleSmono/plot1m-300_4.txt")
results10 <- read.table("dataSim/resampleSmono/plot1m-300_5.txt")
# S quad plots
results11 <- read.table("dataSim/resampleSquad/plot-360_1.txt")
results12 <- read.table("dataSim/resampleSquad/plot-360_2.txt")
results13 <- read.table("dataSim/resampleSquad/plot-360_3.txt")
results14 <- read.table("dataSim/resampleSquad/plot-360_4.txt")
results15 <- read.table("dataSim/resampleSquad/plot-360_5.txt")

(diff1 <- plotDdiscrep(results1))
doSomePlots <- comparePlot(results = results1, pch = 16, colour = "A")

(diff2 <- plotDdiscrep(results2))
(diff3 <- plotDdiscrep(results3))
doMorePlots <- comparePlot(results = results3, pch = 16, colour = "A")

(diff4 <- plotDdiscrep(results4))
(diff5 <- plotDdiscrep(results5))
(diff6 <- plotDdiscrep(results6))
doplots6 <- comparePlot(results = results6, pch = 16, colour = "A")

(diff7 <- plotDdiscrep(results7))
(diff8 <- plotDdiscrep(results8))
(diff9 <- plotDdiscrep(results9))
(diff10 <- plotDdiscrep(results10))
(diff11 <- plotDdiscrep(results11))
(diff12 <- plotDdiscrep(results12))
(diff13 <- plotDdiscrep(results13))
(diff14 <- plotDdiscrep(results14))
(diff15 <- plotDdiscrep(results15))


#for rows (plot resamples) where probabilities can't be calculated, the density estimates are not included - has this biased the double observer sample? the plot counts are not zero (raw density is not zero), only 1 out of 100 is zero.  See if the densities are lower on average.

#adjusted plot resample function to keep the plot count /area when probability can't be estimated: new summary results, same analysis as above

# function to calculate difference between the double observer density estimate where detectability could and could not be estimated.

plotD_pNoP <- function(results){
  meanD_p <- mean(results$dhat_doubleObs[!is.na(results$probOverall)])
meanD_noP <- mean(results$dhat_doubleObs[is.na(results$probOverall)])
meanP <- mean(results$probOverall, na.rm = TRUE)
meanD_noP_adj <- meanD_noP/meanP

difference <- meanD_p-meanD_noP_adj
print(paste0("mean est. d where detectability is estimated = ", meanD_p, " indiv/m^2"))
print(paste0("mean est. d where detectability could not be estimated = ", meanD_noP_adj, " indiv/m^2 (using mean probability of detection across all resamples where this was estimated"))
print(paste0("difference = ", difference, " indiv/m^2"))
return(difference)
}

#4m plots
diff1 <- plotD_pNoP(results=results1)
diff2 <- plotD_pNoP(results = results2)
diff3 <- plotD_pNoP(results = results3)

#1m plots
diff6 <- plotD_pNoP(results = results6)
diff7 <- plotD_pNoP(results = results7)
diff8 <- plotD_pNoP(results = results8)

#Senecio plots
diff11 <- plotD_pNoP(results = results11)
diff12 <- plotD_pNoP(results = results12)
diff13 <- plotD_pNoP(results = results13)

#Yes, estimates are lower, so the overall estimate of density would be lower, making it likely that the single and double observer estimates would be closer if these lower counts are included.

  #Option 1: use mean p over the other plots to adjust the counts
  #Option 2: explain the discrepancy as a shortfall of the double observer method - can't deal well with lots of zeros.  Consider keeping those raw estimates in (the average p is pretty close to 1 anyway)
  #Option 3: use max effort possible to increase sample sizes so the chances of zero subtotals is minimised.  

#now calculate means over the whole set of resamples (cv is easier as an empirical estimate) and plot the mean + ci for each method (panel plot each method separately for an appendix)

#sampling variance for the double observer method doesn't take into account the variance on the estimate of p


#ideas for things to do: double sampling for plot samples (how many double observer plots to get the best precision), optimising transect length

# ISSUE 5 ----

#standardising se as empirical across the board
#that wasn't it, what was I thinking about?????? RESAMPLING WITH OR WITHOUT REPLACEMENT

#with budget = 300 mins for Stackhousia, there's no need to resample with replacement. The sample of LTS transects is plenty large enough to resample the necessary Opt sample.  For Senecio, the sample of LTS transects is 7, with a lot of small counts and one or two zeros.  The cost ratio is also higher (1.2 for Smono, 3.4 for S quad), so we need a MUCH bigger sample for Opt than for LTS.  Options are to resample with replacement, or use a very small budget, knowing that probably most of the resamples will fail to fit a detection function for LTS. Do both, go over both with Mick and Guru.

#testing resample with smaller sample - no replacement.  Need to know what budget to use to only need max 6 transects for Opt

KOpt <- 6 #max number of transects for Opt resample
tOpt <- KOpt*times$mean_unit_time[times$method=="Opt" & times$target=="Senecio"] #total budget for KOpt transects
KLTS <- tOpt/times$mean_unit_time[times$method=="LTS" & times$target=="Senecio"] #n LTS transects for same budget

# KLTS is 3.5, so will need to modify the resampling to kick out half of one of the transects.  Round up, pick one, randomly pick first or second half, then analyse.
# have done resample with replacement: budget 360 as before, seems to have worked.
# need to resample plots with replacement? Probably.

#getting error with resamples without replacement:
#Error in model fitting, returning: half-normal key function
#Error: Error in detfct.fit.opt(ddfobj, optim.options, bounds, misc.options) : 
#  No convergence.

#need to look this up and find out what it mean, the function is still giving results, so this might be missed.
#I think this means that the half-normal key function model fit the data, but the next one that was tried (in this case hn + cosine(2)) didn't fit, so it ditched the attempt and went back to half-normal.  So the half-normal was the best fit, the results stand.