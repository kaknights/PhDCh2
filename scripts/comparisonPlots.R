## Source file for graphics ##

#NOTE: 03.08.2022 random seed 2 files have se rather than cv, and whatever I changed seems to have changed the results.  Not sure what went wrong, but calculating cv using se/D changed where the points lie.  Now checking all other seeds.  May need to re-run the resampling analysis.  Also getting some strange points at cv=1 with random seed 1, which I'm sure weren't there before.

#Have re-run random seed 1, not any of the others

#read in resampled data (first number in file is effort, second is the random seed)
#S mono data
mySmonoDistResults <- read.table("dataSim/resampleSmono/distResults-300_1.txt")
Plotresults1m <- read.table("dataSim/resampleSmono/plot1m-300_1.txt")
Plotresults4m <- read.table("dataSim/resampleSmono/plot4m-300_1.txt")

#S quad data

mySquadDistResults <- read.table("dataSim/resampleSquad/distResults-360_1.txt")
PlotresultsSQ <- read.table("dataSim/resampleSquad/plot-360_1.txt")

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

#boxplot showing mean and se: arrange data, need a df of method and D for all seven methods


smBoxData <- data.frame("method" = character(700),
                        "estD" = numeric(700))
smBoxData$method <- rep(c("LTS", "Opt", "GrB", "plot1mSingle", "plot1mDouble", "plot4mSingle", "plot4mDouble"), each = 100)

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

###### S quad ######
###### All methods overlaid

### graph Squad SE ----

#png("graphics/S_quad_se.png")
plot(x = mySquadDistResults$estD, y = mySquadDistResults$seD, xlab = "D hat (indiv/m^2)", 
     ylab = "se (D hat)", pch = c(2, 17)[as.factor(mySquadDistResults$method)],
     xlim = c(0, 0.32), ylim = c(0, 0.11),
     col = c(virColours[1],virColours[2])[as.factor(mySquadDistResults$method)],
     main = "Comparison of se for distance and plot \n methods: Senecio quadridentatus")
points(x = PlotresultsSQ$dhat_doubleObs, y = PlotresultsSQ$se.d.double., pch = 1, col = virColours[4])
points(x = PlotresultsSQ$dhat_singleObs, y = PlotresultsSQ$se_.d.single., pch = 16, col = virColours[7])
legend(x = 0.22, y = 0.04, bty = "n",
       legend = c(levels(as.factor(mySquadDistResults$method)), "double obs plot", "single obs plot"),
       pch = c(2, 17, 1, 16), col = c(virColours[1],virColours[2],virColours[4],virColours[7]))
#dev.off()


### graph S quad CV ----

#png("graphics/S_quad_cv.png")
plot(x = mySquadDistResults$estD, y = mySquadDistResults$cvD, xlab = "D hat (indiv/m^2)", 
     ylab = "cv (D hat)", pch = c(2, 17)[as.factor(mySquadDistResults$method)],
     xlim = c(0, 0.32), ylim = c(0, 0.9),
     col = c(virColours[1],virColours[2])[as.factor(mySquadDistResults$method)],
     main = "Comparison of cv for distance and plot \n methods : Senecio quadridentatus")
points(x = PlotresultsSQ$dhat_doubleObs, y = PlotresultsSQ$se.d.double./PlotresultsSQ$dhat_doubleObs, pch = 1, col = virColours[4])
points(x = PlotresultsSQ$dhat_singleObs, y = PlotresultsSQ$se_.d.single./PlotresultsSQ$dhat_singleObs, pch = 16, col = virColours[7])
legend(x = 0.21, y = 0.3, bty = "n",
       legend = c(levels(as.factor(mySquadDistResults$method)), "double obs plot", "single obs plot"),
       pch = c(2, 17, 1, 16), col = c(virColours[1],virColours[2],virColours[4],virColours[7]))
#dev.off()

#boxplot showing mean and se: arrange data, need a df of method and D for all seven methods
sqBoxData <- data.frame("method" = character(400),
                        "estD" = numeric(400))
sqBoxData$method <- rep(c("LTS", "Opt", "plotSingle", "plotDouble"), each = 100)

sqBoxData$estD[1:100] <- mySquadDistResults$estD[mySquadDistResults$method=="LTS"]
sqBoxData$estD[101:200] <- mySquadDistResults$estD[mySquadDistResults$method=="Opt"]
sqBoxData$estD[201:300] <- PlotresultsSQ$dhat_singleObs
sqBoxData$estD[301:400] <- PlotresultsSQ$dhat_doubleObs

#png("graphics/S_quad_box.png")
boxplot(sqBoxData$estD~sqBoxData$method, xlab = "", ylab = "estimated D")
#dev.off()
