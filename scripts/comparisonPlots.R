## Source file for graphics ##

#NOTE: 03.08.2022 random seed 2 files have se rather than cv, and whatever I changed seems to have changed the results.  Not sure what went wrong, but calculating cv using se/D changed where the points lie.  Now checking all other seeds.  May need to re-run the resampling analysis.  Also getting some strange points at cv=1 with random seed 1, which I'm sure weren't there before.

#Have re-run random seed 1, not any of the others

#read in resampled data (first number in file is effort, second is the random seed)
#S mono data (all are resampled without replacement)
mySmonoDistResults <- read.table("dataSim/resampleSmono/distResults-300_1.txt")
Plotresults1m <- read.table("dataSim/resampleSmono/plot1m-300_1.txt")
Plotresults4m <- read.table("dataSim/resampleSmono/plot4m-300_1.txt")

#S quad data (without replacement - smaller sample)
mySquadDistResults1 <- read.table("dataSim/resampleSquad/distResults-275_1.txt")
PlotresultsSQ1 <- read.table("dataSim/resampleSquad/plot-275_1.txt")

#Squad data (with replacement)
mySquadDistResults2 <- read.table("dataSim/resampleSquad/distResults-360_1Repl.txt")
PlotresultsSQ2 <- read.table("dataSim/resampleSquad/plot-360_1Repl.txt")


#resampled without replacement

###### S mono plot ######
###### All methods overlaid
#graph results for S mono

## graph S mono SE ----
virColours <- viridis(8, option = "D")
#png("graphics/S_mono_se.png")
plot(x = mySmonoDistResults$estD, 
     y = mySmonoDistResults$seD, xlab = "D hat (indiv/m^2)", 
     ylab = "se (D hat)", xlim = c(0,18), ylim = c(0,8),
     pch = 2,
     col = c(virColours[1],virColours[2],virColours[3])[as.factor(mySmonoDistResults$method)],  
     main = "Comparison of se for distance and plot\n methods: Stackhousia monogyna")
points(x = Plotresults1m$dhat_doubleObs, y = Plotresults1m$sed_double, pch = 1, col = virColours[4])
points(x = Plotresults1m$dhat_singleObs, y = Plotresults1m$sed_single, pch = 1, col = virColours[5])
points(x = Plotresults4m$dhat_doubleObs, y = Plotresults4m$sed_double, pch = 1, col = virColours[6])
points(x = Plotresults4m$dhat_singleObs, y = Plotresults4m$sed_single, pch = 1, col = virColours[7])
legend(x = 13, y = 3.8, 
       legend = c(levels(as.factor(mySmonoDistResults$method)),
                                  "double obs 1m plot", "single obs 1m plot",
                                "double obs 4m plot", "single obs 4m plot"),
       pch = c(2, 2, 2, 1, 1, 1, 1), 
       col = virColours[1:7], bty = "n")
#dev.off()

## graph S mono CV ----

#png("graphics/S_mono_cv.png")
plot(x = mySmonoDistResults$estD, 
     y = mySmonoDistResults$cvD, xlab = "D hat (indiv/m^2)", 
     ylab = "cv (D hat)", xlim = c(0,22), ylim = c(0.3,1.1),
     pch = 2, 
     col = c(virColours[1],virColours[2],virColours[3])[as.factor(mySmonoDistResults$method)], 
     main = "Comparison of cv for distance and plot\n methods: Stackhousia monogyna")
points(x = Plotresults1m$dhat_doubleObs, y = Plotresults1m$sed_double/Plotresults1m$dhat_doubleObs, 
       pch = 1, col = virColours[4])
points(x = Plotresults1m$dhat_singleObs, y = Plotresults1m$sed_single/Plotresults1m$dhat_singleObs, 
       pch = 1, col = virColours[5])
points(x = Plotresults4m$dhat_doubleObs, y = Plotresults4m$sed_double/Plotresults4m$dhat_doubleObs, 
       pch = 1, col = virColours[6])
points(x = Plotresults4m$dhat_singleObs, y = Plotresults4m$sed_single/Plotresults4m$dhat_singleObs, 
       pch = 1, col = virColours[7])
legend(x = 12, y = 1, 
       legend = c(levels(as.factor(mySmonoDistResults$method)),
                                  "double obs 1m plot", "single obs 1m plot",
                                "double obs 4m plot", "single obs 4m plot"),
       pch = c(2, 2, 2, 1, 1, 1, 1), 
       col = virColours[1:7], bty = "n")
#dev.off()

#S mono boxplot ----

smBoxData <- data.frame("method" = character(700),
                        "estD" = numeric(700))
smBoxData$method <- rep(c("LTS", "Opt", "GrB", "plot1S", "plot1D", "plot4S", "plot4D"), each = 100)

smBoxData$estD[1:100] <- mySmonoDistResults$estD[mySmonoDistResults$method=="LTS"]
smBoxData$estD[101:200] <- mySmonoDistResults$estD[mySmonoDistResults$method=="Opt"]
smBoxData$estD[201:300] <- mySmonoDistResults$estD[mySmonoDistResults$method=="GrB"]
smBoxData$estD[301:400] <- Plotresults1m$dhat_singleObs
smBoxData$estD[401:500] <- Plotresults1m$dhat_doubleObs
smBoxData$estD[501:600] <- Plotresults4m$dhat_singleObs
smBoxData$estD[601:700] <- Plotresults4m$dhat_doubleObs

#png("graphics/S_mono_box.png")
boxplot(smBoxData$estD~smBoxData$method, xlab = "", ylab = "estimated D")
#dev.off()

sm_se <- data.frame("method" = c("LTS", "Opt", "Grouped", "plot1mS", "plot1mD", "plot4mS", "plot4mD"), 
                    "meanD" = numeric(7),
                    "seD" = numeric(7))

mySmSe <- aggregate(mySmonoDistResults$estD[!is.na(mySmonoDistResults$estD)], by = list(mySmonoDistResults$method[!is.na(mySmonoDistResults$estD)]), FUN = sd)
mySmD <- aggregate(mySmonoDistResults$estD[!is.na(mySmonoDistResults$estD)], by = list(mySmonoDistResults$method[!is.na(mySmonoDistResults$estD)]), FUN = mean)

sm_se$seD[sm_se$method=="LTS"] <- mySmSe$x[mySmSe$Group.1=="LTS"]
sm_se$meanD[sm_se$method=="LTS"] <- mySmD$x[mySmD$Group.1=="LTS"]

sm_se$seD[sm_se$method=="Opt"] <- mySmSe$x[mySmSe$Group.1=="Opt"]
sm_se$meanD[sm_se$method=="Opt"] <- mySmD$x[mySmD$Group.1=="Opt"]

sm_se$seD[sm_se$method=="Grouped"] <- mySmSe$x[mySmSe$Group.1=="GrB"]
sm_se$meanD[sm_se$method=="Grouped"] <- mySmD$x[mySmD$Group.1=="GrB"]

sm_se$seD[sm_se$method=="plot1mS"] <- sd(Plotresults1m$dhat_singleObs)
sm_se$meanD[sm_se$method=="plot1mS"] <- mean(Plotresults1m$dhat_singleObs)

sm_se$seD[sm_se$method=="plot1mD"] <- sd(Plotresults1m$dhat_doubleObs)
sm_se$meanD[sm_se$method=="plot1mD"] <- mean(Plotresults1m$dhat_doubleObs)

sm_se$seD[sm_se$method=="plot4mS"] <- sd(Plotresults4m$dhat_singleObs)
sm_se$meanD[sm_se$method=="plot4mS"] <- mean(Plotresults4m$dhat_singleObs)

sm_se$seD[sm_se$method=="plot4mD"] <- sd(Plotresults4m$dhat_doubleObs)
sm_se$meanD[sm_se$method=="plot4mD"] <- mean(Plotresults4m$dhat_doubleObs)

sm_se$cvD <- round(sm_se$seD/sm_se$meanD, 3)

write.csv(sm_se, "dataSim/smMeanDtable.csv")

###### S quad without replacement ######
# All methods overlaid

## graph Squad SE ----

#png("graphics/S_quad_se.png")
plot(x = mySquadDistResults1$estD, y = mySquadDistResults1$seD, xlab = "D hat (indiv/m^2)", 
     ylab = "se (D hat)", pch = c(2, 17)[as.factor(mySquadDistResults1$method)],
     xlim = c(0, 0.32), ylim = c(0, 0.11),
     col = c(virColours[1],virColours[2])[as.factor(mySquadDistResults1$method)],
     main = "Comparison of se for distance and plot \n methods: Senecio quadridentatus")
points(x = PlotresultsSQ1$dhat_doubleObs, y = PlotresultsSQ1$se.d.double., pch = 1, col = virColours[4])
points(x = PlotresultsSQ1$dhat_singleObs, y = PlotresultsSQ1$se_.d.single., pch = 16, col = virColours[7])
legend(x = 0.22, y = 0.04, bty = "n",
       legend = c(levels(as.factor(mySquadDistResults1$method)), "double obs plot", "single obs plot"),
       pch = c(2, 17, 1, 16), col = c(virColours[1],virColours[2],virColours[4],virColours[7]))
#dev.off()


## graph S quad CV ----

#png("graphics/S_quad_cv.png")
plot(x = mySquadDistResults1$estD, y = mySquadDistResults1$cvD, xlab = "D hat (indiv/m^2)", 
     ylab = "cv (D hat)", pch = c(2, 17)[as.factor(mySquadDistResults1$method)],
     xlim = c(0, 0.32), ylim = c(0, 0.9),
     col = c(virColours[1],virColours[2])[as.factor(mySquadDistResults1$method)],
     main = "Comparison of cv for distance and plot \n methods : Senecio quadridentatus")
points(x = PlotresultsSQ1$dhat_doubleObs, y = PlotresultsSQ1$se.d.double./PlotresultsSQ1$dhat_doubleObs, pch = 1, col = virColours[4])
points(x = PlotresultsSQ1$dhat_singleObs, y = PlotresultsSQ1$se_.d.single./PlotresultsSQ1$dhat_singleObs, pch = 16, col = virColours[7])
legend(x = 0.21, y = 0.3, bty = "n",
       legend = c(levels(as.factor(mySquadDistResults1$method)), "double obs plot", "single obs plot"),
       pch = c(2, 17, 1, 16), col = c(virColours[1],virColours[2],virColours[4],virColours[7]))
#dev.off()

#boxplot showing mean and se: arrange data, need a df of method and D for all seven methods

# Senecio boxplot without replacement ----
sqBoxData1 <- data.frame("method" = character(400),
                        "estD" = numeric(400))
sqBoxData1$method <- rep(c("LTS", "Opt", "plotS", "plotD"), each = 100)

sqBoxData1$estD[1:100] <- mySquadDistResults1$estD[mySquadDistResults1$method=="LTS"]
sqBoxData1$estD[101:200] <- mySquadDistResults1$estD[mySquadDistResults1$method=="Opt"]
sqBoxData1$estD[201:300] <- PlotresultsSQ1$dhat_singleObs
sqBoxData1$estD[301:400] <- PlotresultsSQ1$dhat_doubleObs

#png("graphics/S_quad_box1.png")
boxplot(sqBoxData1$estD~sqBoxData1$method, xlab = "", ylab = "estimated D", ylim = c(0, 0.4))
#dev.off()

sq_se <- data.frame("method" = c("LTS", "Opt", "plotS", "plotD", "LTSr", "Optr", "plotSr", "plotDr"),
                    "meanD" = numeric(8),
                    "seD" = numeric(8))

sq_se$seD[sq_se$method=="LTS"] <- sd(mySquadDistResults1$estD[mySquadDistResults1$method=="LTS"])
sq_se$meanD[sq_se$method=="LTS"] <- mean(mySquadDistResults1$estD[mySquadDistResults1$method=="LTS"])

sq_se$seD[sq_se$method=="Opt"] <- sd(mySquadDistResults1$estD[mySquadDistResults1$method=="Opt"])
sq_se$meanD[sq_se$method=="Opt"] <- mean(mySquadDistResults1$estD[mySquadDistResults1$method=="Opt"])

sq_se$seD[sq_se$method=="plotS"] <- sd(PlotresultsSQ1$dhat_singleObs)
sq_se$meanD[sq_se$method=="plotS"] <- mean(PlotresultsSQ1$dhat_singleObs)

sq_se$seD[sq_se$method=="plotD"] <- sd(PlotresultsSQ1$dhat_doubleObs)
sq_se$meanD[sq_se$method=="plotD"] <- mean(PlotresultsSQ1$dhat_doubleObs)

# S quad with replacement ----

## graph Squad SE ----

#png("graphics/S_quad_seRepl.png")
plot(x = mySquadDistResults2$estD, y = mySquadDistResults2$seD, xlab = "D hat (indiv/m^2)", 
     ylab = "se (D hat)", pch = c(2, 17)[as.factor(mySquadDistResults2$method)],
     xlim = c(0, 0.32), ylim = c(0, 0.11),
     col = c(virColours[1],virColours[2])[as.factor(mySquadDistResults2$method)],
     main = "Comparison of se for distance and plot \n methods: Senecio quadridentatus")
points(x = PlotresultsSQ2$dhat_doubleObs, y = PlotresultsSQ2$se.d.double., pch = 1, col = virColours[4])
points(x = PlotresultsSQ2$dhat_singleObs, y = PlotresultsSQ2$se_.d.single., pch = 16, col = virColours[7])
legend(x = 0.22, y = 0.04, bty = "n",
       legend = c(levels(as.factor(mySquadDistResults2$method)), "double obs plot", "single obs plot"),
       pch = c(2, 17, 1, 16), col = c(virColours[1],virColours[2],virColours[4],virColours[7]))
#dev.off()


## graph S quad CV ----

#png("graphics/S_quad_cvRepl.png")
plot(x = mySquadDistResults2$estD, y = mySquadDistResults2$cvD, xlab = "D hat (indiv/m^2)", 
     ylab = "cv (D hat)", pch = c(2, 17)[as.factor(mySquadDistResults2$method)],
     xlim = c(0, 0.32), ylim = c(0, 0.9),
     col = c(virColours[1],virColours[2])[as.factor(mySquadDistResults2$method)],
     main = "Comparison of cv for distance and plot \n methods : Senecio quadridentatus")
points(x = PlotresultsSQ2$dhat_doubleObs, y = PlotresultsSQ2$se.d.double./PlotresultsSQ2$dhat_doubleObs, pch = 1, col = virColours[4])
points(x = PlotresultsSQ2$dhat_singleObs, y = PlotresultsSQ2$se_.d.single./PlotresultsSQ2$dhat_singleObs, pch = 16, col = virColours[7])
legend(x = 0.21, y = 0.3, bty = "n",
       legend = c(levels(as.factor(mySquadDistResults2$method)), "double obs plot", "single obs plot"),
       pch = c(2, 17, 1, 16), col = c(virColours[1],virColours[2],virColours[4],virColours[7]))
#dev.off()

# Senecio boxplot with replacement ----
sqBoxData2 <- data.frame("method" = character(400),
                        "estD" = numeric(400))
sqBoxData2$method <- rep(c("LTS", "Opt", "plotS", "plotD"), each = 100)

sqBoxData2$estD[1:100] <- mySquadDistResults2$estD[mySquadDistResults2$method=="LTS"]
sqBoxData2$estD[101:200] <- mySquadDistResults2$estD[mySquadDistResults2$method=="Opt"]
sqBoxData2$estD[201:300] <- PlotresultsSQ2$dhat_singleObs
sqBoxData2$estD[301:400] <- PlotresultsSQ2$dhat_doubleObs

#png("graphics/S_quad_boxRepl.png")
boxplot(sqBoxData2$estD~sqBoxData2$method, xlab = "", ylab = "estimated D", ylim = c(0, 0.4))
#dev.off()

sq_se$seD[sq_se$method=="LTSr"] <- sd(mySquadDistResults2$estD[mySquadDistResults1$method=="LTS"])
sq_se$meanD[sq_se$method=="LTSr"] <- mean(mySquadDistResults2$estD[mySquadDistResults1$method=="LTS"])

sq_se$seD[sq_se$method=="Optr"] <- sd(mySquadDistResults2$estD[mySquadDistResults1$method=="Opt"])
sq_se$meanD[sq_se$method=="Optr"] <- mean(mySquadDistResults2$estD[mySquadDistResults1$method=="Opt"])

sq_se$seD[sq_se$method=="plotSr"] <- sd(PlotresultsSQ2$dhat_singleObs)
sq_se$meanD[sq_se$method=="plotSr"] <- mean(PlotresultsSQ2$dhat_singleObs)

sq_se$seD[sq_se$method=="plotDr"] <- sd(PlotresultsSQ2$dhat_doubleObs)
sq_se$meanD[sq_se$method=="plotDr"] <- mean(PlotresultsSQ2$dhat_doubleObs)

sq_se$cvD <- round(sq_se$seD/sq_se$meanD, 3)

write.csv(sq_se, "dataSim/sqMeanDtable.csv")

# S mono Unbanded boxplot

SmUnbandedBox <- data.frame("method" = rep(c("Opt", "GrN"), each = 100),
                            "estD" = numeric(200))
SmUnbandedBox$estD[1:100] <- mySmonoUnbandedResults$estD[mySmonoUnbandedResults$method == "Opt"]
SmUnbandedBox$estD[101:200] <- mySmonoUnbandedResults$estD[mySmonoUnbandedResults$method == "GrN"]

boxplot(SmUnbandedBox$estD~SmUnbandedBox$method , xlab = "", ylab = "estimated D", ylim = c(0, 10))
