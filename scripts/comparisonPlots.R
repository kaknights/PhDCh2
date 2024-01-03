## Source file for graphics ##
#library(viridis)
# data ----
#read in resampled data (first number in file is effort, second is the random seed)
#S mono data (all are resampled with replacement)
mySmonoDistResults <- read.table("dataSim/resampleSmono/distResults-600_1_1000.txt")
Plotresults1m <- read.table("dataSim/resampleSmono/plot1m-600_1_1000.txt")
Plotresults4m <- read.table("dataSim/resampleSmono/plot4m-600_1_1000.txt")

#S mono data unbanded grouped vs opt
#myUnbandedResults1 <- read.table("dataSim/resampleSmono/distResults-140_1Unbanded.txt")
myUnbandedResults2 <- read.table("dataSim/resampleSmono/distResults-600_1UnbandedRepl_1000.txt")

#Squad data (with replacement)
mySquadDistResults2 <- read.table("dataSim/resampleSquad/distResults-600_1Repl_1000.txt")
PlotresultsSQ2 <- read.table("dataSim/resampleSquad/plot-600_1Repl_1000.txt")

#S mono boxplot ----

smBoxData <- data.frame("method" = character(5000),
                        "estD" = numeric(5000))
smBoxData$method <- rep(c("LTS", "Opt", "GrB", "plot 1m \n(single)", "plot 1m \n(double)"), each = 1000)

smBoxData$estD[1:1000] <- mySmonoDistResults$estD[mySmonoDistResults$method=="LTS"]
smBoxData$estD[1001:2000] <- mySmonoDistResults$estD[mySmonoDistResults$method=="Opt"]
smBoxData$estD[2001:3000] <- mySmonoDistResults$estD[mySmonoDistResults$method=="GrB"]
smBoxData$estD[3001:4000] <- Plotresults1m$dhat_singleObs
smBoxData$estD[4001:5000] <- Plotresults1m$dhat_doubleObs

#default boxplot plotting order is alphabetical (...!?), so labels are ordered as such:

thingy <- expression(m^2)

#png("graphics/S_mono_box_Repl.png")#, width = 650
par(mar=c(4,5,2,2))
boxplot(smBoxData$estD~smBoxData$method, xlab = "", ylab = "", mgp = c(3, 2, 0), xaxt = "n", xpd = TRUE)
axis(side = 1, at = 1:7, labels = FALSE)
mtext("Grouped", side = 1, line = 1, at = 1) 
mtext("LTS", side = 1, line = 1, at = 2)
mtext("Opt", side = 1, line = 1, at = 3)                  
mtext("double obs", side = 1, line = 1, at = 4)
mtext("single obs", side = 1, line = 1, at = 5)
mtext(text = expression(italic(hat(D))), side = 2, line = 2.8, at = 8, las = 2, cex = 1.4) 
      
#dev.off()

# Senecio boxplot with replacement ----
sqBoxData2 <- data.frame("method" = character(4000),
                        "estD" = numeric(4000))
sqBoxData2$method <- rep(c("LTS", "Opt", "plotS", "plotD"), each = 1000)

sqBoxData2$estD[1:1000] <- mySquadDistResults2$estD[mySquadDistResults2$method=="LTS"]
sqBoxData2$estD[1001:2000] <- mySquadDistResults2$estD[mySquadDistResults2$method=="Opt"]
sqBoxData2$estD[2001:3000] <- PlotresultsSQ2$dhat_singleObs
sqBoxData2$estD[3001:4000] <- PlotresultsSQ2$dhat_doubleObs

#png("graphics/S_quad_boxRepl.png")
par(mar = c(4,5,2,2))
boxplot(sqBoxData2$estD~sqBoxData2$method, xlab = "", ylab = "", ylim = c(0, 0.42), xaxt = "n", xpd = TRUE, mgp = c(3, 2, 0))
axis(side = 1, at = 1:4, labels = FALSE)
mtext("LTS", side = 1, line = 1, at = 1) 
mtext("Opt", side = 1, line = 1, at = 2)        
mtext("double obs", side = 1, line = 1, at = 3) 
mtext("single obs", side = 1, line = 1, at = 4)
mtext(text = expression(italic(hat(D))), side = 2, line = 3, at = 0.22, las = 2, cex = 1.4) 

#dev.off()

# S mono Unbanded boxplot ----

SmUnbandedBox <- data.frame("method" = rep(c("Opt", "Grouped"), each = 1000),
                            "estD" = numeric(2000))
SmUnbandedBox$estD[1:1000] <- myUnbandedResults2$estD[myUnbandedResults2$method == "Opt"]
SmUnbandedBox$estD[1001:2000] <- myUnbandedResults2$estD[myUnbandedResults2$method == "GrN"]

#png("graphics/SmonoUnbandedBox.png", width = 520)
par(mar = c(4,6,2,2))
boxplot(SmUnbandedBox$estD~SmUnbandedBox$method , xlab = "", ylab = "", ylim = c(0, 9), xaxt = "n", xpd = TRUE, mgp = c(3, 2, 0))
axis(side = 1, at = 1:2, labels = FALSE)
mtext("Grouped\n(unmarked)", side = 1, line = 2, at = 1) 
mtext("Opt", side = 1, line = 1, at = 2)                  
mtext(text = expression(italic(hat(D))), side = 2, line = 3, at = 4.5, las = 2, cex = 1.4) 

#dev.off()

# summaries ----
# varD_sMono_dist_opt <- var(mySmonoDistResults$estD[mySmonoDistResults$method=="Opt"])
# varD_sMono_dist_lts <- var(mySmonoDistResults$estD[mySmonoDistResults$method=="LTS"])
# 
# benefit_smono <- varD_sMono_dist_lts/varD_sMono_dist_opt
# #1.4 in variance
# sqrt(benefit_smono) 
# #1.185 sd
# 
# var_sQuad_dist_opt <- var(mySquadDistResults2$estD[mySquadDistResults2$method=="Opt"])
# var_sQuad_dist_lts <- var(mySquadDistResults2$estD[mySquadDistResults2$method=="LTS"])
# 
# benefit_squad <- var_sQuad_dist_lts/var_sQuad_dist_opt
# #1.37 in variance
# sqrt(benefit_squad)
#1.169 sd

#double vs single obs plot difference

#1m
# plot1mEstDsingle_mean <- mean(Plotresults1m$dhat_singleObs)#5.00
# plot1mEstDdouble_mean <- mean(Plotresults1m$dhat_doubleObs)#5.59
# 
# plot1mDiff <- (1-plot1mEstDsingle_mean/plot1mEstDdouble_mean) #0.895
# 
# meanPsingle1m <- (mean(Plotresults1m$probObs1, na.rm = TRUE)+mean(Plotresults1m$probObs2, na.rm = TRUE))/2
# #0.888
# 
# meanPdouble1m <- mean(Plotresults1m$probOverall, na.rm = TRUE)
# #0.99
# (diffP1m <- 1-meanPsingle1m/meanPdouble1m)
# 
# #4m
# plot4mEstDsingle_mean <- mean(Plotresults4m$dhat_singleObs)#6.49
# plot4mEstDdouble_mean <- mean(Plotresults4m$dhat_doubleObs)#7.20
# 
# plot4mDiff <- (1-plot4mEstDsingle_mean/plot4mEstDdouble_mean) #0.901
# 
# meanPsingle4m <- (mean(Plotresults4m$probObs1, na.rm = TRUE)+mean(Plotresults4m$probObs2, na.rm = TRUE))/2
# #0.880
# 
# meanPdouble4m <- mean(Plotresults4m$probOverall, na.rm = TRUE)
# #0.99
# (diffP4m <- 1-meanPsingle4m/meanPdouble4m)
# 
# #Senecio
# plotSQEstDsingle_mean <- mean(PlotresultsSQ2$dhat_singleObs)#0.15
# plotSQEstDdouble_mean <- mean(PlotresultsSQ2$dhat_doubleObs)#0.16
# 
# plotSQDiff <- (1 - plotSQEstDsingle_mean/plotSQEstDdouble_mean)*100 #6.47%
# 
# meanPsingleSQ <- (mean(PlotresultsSQ2$probObs1, na.rm = TRUE)+mean(PlotresultsSQ2$probObs2, na.rm = TRUE))/2
# #0.945
# 
# meanPdoubleSQ <- mean(PlotresultsSQ2$probOverall, na.rm = TRUE)
# #1.00
# 
# (diffPsq <- 1-meanPsingleSQ/meanPdoubleSQ)

###### S mono plot ######
###### All methods overlaid
#graph results for S mono

## graph S mono SE ----
# virColours <- viridis(8, option = "D")
# #png("graphics/S_mono_se.png")
# plot(x = mySmonoDistResults$estD, 
#      y = mySmonoDistResults$seD, xlab = "D hat (indiv/m^2)", 
#      ylab = "se (D hat)", xlim = c(0,18), ylim = c(0,8),
#      pch = 2,
#      col = c(virColours[1],virColours[2],virColours[3])[as.factor(mySmonoDistResults$method)],  
#      main = "Comparison of se for distance and plot\n methods: Stackhousia monogyna")
# points(x = Plotresults1m$dhat_doubleObs, y = Plotresults1m$sed_double, pch = 1, col = virColours[4])
# points(x = Plotresults1m$dhat_singleObs, y = Plotresults1m$sed_single, pch = 1, col = virColours[5])
# points(x = Plotresults4m$dhat_doubleObs, y = Plotresults4m$sed_double, pch = 1, col = virColours[6])
# points(x = Plotresults4m$dhat_singleObs, y = Plotresults4m$sed_single, pch = 1, col = virColours[7])
# legend(x = 13, y = 3.8, 
#        legend = c(levels(as.factor(mySmonoDistResults$method)),
#                                   "double obs 1m plot", "single obs 1m plot",
#                                 "double obs 4m plot", "single obs 4m plot"),
#        pch = c(2, 2, 2, 1, 1, 1, 1), 
#        col = virColours[1:7], bty = "n")
#dev.off()

## graph S mono CV ----

#png("graphics/S_mono_cv.png")
# plot(x = mySmonoDistResults$estD, 
#      y = mySmonoDistResults$cvD, xlab = "D hat (indiv/m^2)", 
#      ylab = "cv (D hat)", xlim = c(0,22), ylim = c(0.1,1.1),
#      pch = 2, 
#      col = c(virColours[1],virColours[2],virColours[3])[as.factor(mySmonoDistResults$method)], 
#      main = "Comparison of cv for distance and plot\n methods: Stackhousia monogyna")
# points(x = Plotresults1m$dhat_doubleObs, y = Plotresults1m$sed_double/Plotresults1m$dhat_doubleObs, 
#        pch = 1, col = virColours[4])
# points(x = Plotresults1m$dhat_singleObs, y = Plotresults1m$sed_single/Plotresults1m$dhat_singleObs, 
#        pch = 1, col = virColours[5])
# points(x = Plotresults4m$dhat_doubleObs, y = Plotresults4m$sed_double/Plotresults4m$dhat_doubleObs, 
#        pch = 1, col = virColours[6])
# points(x = Plotresults4m$dhat_singleObs, y = Plotresults4m$sed_single/Plotresults4m$dhat_singleObs, 
#        pch = 1, col = virColours[7])
# legend(x = 12, y = 1, 
#        legend = c(levels(as.factor(mySmonoDistResults$method)),
#                                   "double obs 1m plot", "single obs 1m plot",
#                                 "double obs 4m plot", "single obs 4m plot"),
#        pch = c(2, 2, 2, 1, 1, 1, 1), 
#        col = virColours[1:7], bty = "n")
#dev.off()




# sm_se <- data.frame("method" = c("LTS", "Opt", "Grouped", "plot1S", "plot1D", "plot4mS", "plot4mD"), 
#                     "meanD" = numeric(7),
#                     "seD" = numeric(7))
# 
# mySmSe <- aggregate(mySmonoDistResults$estD[!is.na(mySmonoDistResults$estD)], by = list(mySmonoDistResults$method[!is.na(mySmonoDistResults$estD)]), FUN = sd)
# mySmD <- aggregate(mySmonoDistResults$estD[!is.na(mySmonoDistResults$estD)], by = list(mySmonoDistResults$method[!is.na(mySmonoDistResults$estD)]), FUN = mean)
# 
# sm_se$seD[sm_se$method=="LTS"] <- mySmSe$x[mySmSe$Group.1=="LTS"]
# sm_se$meanD[sm_se$method=="LTS"] <- mySmD$x[mySmD$Group.1=="LTS"]
# 
# sm_se$seD[sm_se$method=="Opt"] <- mySmSe$x[mySmSe$Group.1=="Opt"]
# sm_se$meanD[sm_se$method=="Opt"] <- mySmD$x[mySmD$Group.1=="Opt"]
# 
# sm_se$seD[sm_se$method=="Grouped"] <- mySmSe$x[mySmSe$Group.1=="GrB"]
# sm_se$meanD[sm_se$method=="Grouped"] <- mySmD$x[mySmD$Group.1=="GrB"]
# 
# sm_se$seD[sm_se$method=="plot1S"] <- sd(Plotresults1m$dhat_singleObs)
# sm_se$meanD[sm_se$method=="plot1S"] <- mean(Plotresults1m$dhat_singleObs)
# 
# sm_se$seD[sm_se$method=="plot1D"] <- sd(Plotresults1m$dhat_doubleObs)
# sm_se$meanD[sm_se$method=="plot1D"] <- mean(Plotresults1m$dhat_doubleObs)
# 
# sm_se$seD[sm_se$method=="plot4mS"] <- sd(Plotresults4m$dhat_singleObs)
# sm_se$meanD[sm_se$method=="plot4mS"] <- mean(Plotresults4m$dhat_singleObs)
# 
# sm_se$seD[sm_se$method=="plot4mD"] <- sd(Plotresults4m$dhat_doubleObs)
# sm_se$meanD[sm_se$method=="plot4mD"] <- mean(Plotresults4m$dhat_doubleObs)
# 
# sm_se$cvD <- round(sm_se$seD/sm_se$meanD, 3)

#write.csv(sm_se, "results/smMeanDtable.csv")

###### S quad without replacement ######
# All methods overlaid

# S quad with replacement ----

## graph Squad SE ----

#png("graphics/S_quad_seRepl.png")
# plot(x = mySquadDistResults2$estD, y = mySquadDistResults2$seD, xlab = "D hat (indiv/m^2)", 
#      ylab = "se (D hat)", pch = c(2, 17)[as.factor(mySquadDistResults2$method)],
#      xlim = c(0, 0.32), ylim = c(0, 0.11),
#      col = c(virColours[1],virColours[2])[as.factor(mySquadDistResults2$method)],
#      main = "Comparison of se for distance and plot \n methods: Senecio quadridentatus")
# points(x = PlotresultsSQ2$dhat_doubleObs, y = PlotresultsSQ2$se.d.double., pch = 1, col = virColours[4])
# points(x = PlotresultsSQ2$dhat_singleObs, y = PlotresultsSQ2$se_.d.single., pch = 16, col = virColours[7])
# legend(x = 0.22, y = 0.04, bty = "n",
#        legend = c(levels(as.factor(mySquadDistResults2$method)), "double obs plot", "single obs plot"),
#        pch = c(2, 17, 1, 16), col = c(virColours[1],virColours[2],virColours[4],virColours[7]))
# #dev.off()
# 
# ## graph S quad CV ----
# 
# #png("graphics/S_quad_cvRepl.png")
# plot(x = mySquadDistResults2$estD, y = mySquadDistResults2$cvD, xlab = "D hat (indiv/m^2)", 
#      ylab = "cv (D hat)", pch = c(2, 17)[as.factor(mySquadDistResults2$method)],
#      xlim = c(0, 0.32), ylim = c(0, 0.9),
#      col = c(virColours[1],virColours[2])[as.factor(mySquadDistResults2$method)],
#      main = "Comparison of cv for distance and plot \n methods : Senecio quadridentatus")
# points(x = PlotresultsSQ2$dhat_doubleObs, y = PlotresultsSQ2$se.d.double./PlotresultsSQ2$dhat_doubleObs, pch = 1, col = virColours[4])
# points(x = PlotresultsSQ2$dhat_singleObs, y = PlotresultsSQ2$se_.d.single./PlotresultsSQ2$dhat_singleObs, pch = 16, col = virColours[7])
# legend(x = 0.21, y = 0.3, bty = "n",
#        legend = c(levels(as.factor(mySquadDistResults2$method)), "double obs plot", "single obs plot"),
#        pch = c(2, 17, 1, 16), col = c(virColours[1],virColours[2],virColours[4],virColours[7]))
#dev.off()

# sq_se <- data.frame("method" = c("LTS", "Opt", "plotS", "plotD"), 
#                     "meanD" = numeric(4),
#                     "seD" = numeric(4))
# 
# mySqSe <- aggregate(mySquadDistResults2$estD[!is.na(mySquadDistResults2$estD)], by = list(mySquadDistResults2$method[!is.na(mySquadDistResults2$estD)]), FUN = sd)
# mySqD <- aggregate(mySquadDistResults2$estD[!is.na(mySquadDistResults2$estD)], by = list(mySquadDistResults2$method[!is.na(mySquadDistResults2$estD)]), FUN = mean)
# 
# sq_se$seD[sq_se$method=="LTS"] <- sd(mySquadDistResults2$estD[mySquadDistResults2$method=="LTS"])
# sq_se$meanD[sq_se$method=="LTS"] <- mean(mySquadDistResults2$estD[mySquadDistResults2$method=="LTS"])
# 
# sq_se$seD[sq_se$method=="Opt"] <- sd(mySquadDistResults2$estD[mySquadDistResults2$method=="Opt"])
# sq_se$meanD[sq_se$method=="Opt"] <- mean(mySquadDistResults2$estD[mySquadDistResults2$method=="Opt"])
# 
# sq_se$seD[sq_se$method=="plotS"] <- sd(PlotresultsSQ2$dhat_singleObs)
# sq_se$meanD[sq_se$method=="plotS"] <- mean(PlotresultsSQ2$dhat_singleObs)
# 
# sq_se$seD[sq_se$method=="plotD"] <- sd(PlotresultsSQ2$dhat_doubleObs)
# sq_se$meanD[sq_se$method=="plotD"] <- mean(PlotresultsSQ2$dhat_doubleObs)
# 
# sq_se$cvD <- round(sq_se$seD/sq_se$meanD, 3)

#write.csv(sq_se, "dataSim/sqMeanDtable.csv")



#Unbanded coefficient of variation

(cvD_unbanded <- sd(myUnbandedResults2$estD[myUnbandedResults2$method=="GrN"])/mean(myUnbandedResults2$estD[myUnbandedResults2$method=="GrN"])) #[1] 0.2157939

(cvD_D_unB_opt <- sd(myUnbandedResults2$estD[myUnbandedResults2$method=="Opt"])/mean(myUnbandedResults2$estD[myUnbandedResults2$method=="Opt"])) #[1] 0.3537305
