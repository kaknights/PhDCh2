# Bias vs precision plot

source("scripts/functions.R")
library(scales); library(raster) #can't remember what scale is for...

## Pt 2: make plots ----
#(part 1 is generating the simulated data.  See biasPrecisionSims.R)

# read in pars for reference

myVars <- read.table("results/fieldEstimates.txt")
plotVars <- read.table("results/plotFieldEstimates.txt")

## Stackhousia ----

#need to re-create summary dataframes for plots to include squared error

pathSM <- "dataSim/prelimMethods/sm1/"

check <- read.table(paste0(pathSM, "simResults_P_0.5.txt"))

probDet <- seq(0.5, 0.99, by = 0.01)
plotSize <- 1
plotSimResults <- function(probDet, plotSize){
  myDF <- data.frame("prob.det" = probDet,
                     "mean_estD" = numeric(length(probDet)),
                     "mean_error" = numeric(length(probDet)),
                     "sd_D" = numeric(length(probDet)),
                     "mean_sq_error" = numeric(length(probDet)))
  for (i in 1:length(probDet)){
    folder <- if (plotSize == 1){
      "sm1"
    } else {
      if (plotSize == 4) {
        "sm4"
      } else {
        "sq"
      }
    }
    #folder <- ifelse(plotSize == 1, "smallPlot", "largePlot") 
    data <- read.table(paste0("dataSim/prelimMethods/", folder, "/simResults_P_", probDet[i],".txt"))
    data$error_sq <- data$error^2
    myDF$mean_estD[i] <- mean(data$estD)
    myDF$mean_error[i] <- mean(data$error)
    myDF$sd_D[i] <- sd(data$estD)
    myDF$mean_sq_error[i] <- mean(data$error_sq)
  }
  return(myDF)
}

myPlotPlotsm1 <- plotSimResults(probDet = probDet, plotSize = 1)

#plot sims 1m^2
#myPlotPlotsm1 <- read.table("dataSim/prelimMethods/sm1/collatedResults_3.txt")

#bias:
myPlotPlotsm1$biasPerC <- myPlotPlotsm1$mean_error/myVars$estD[1]*100
#mean_error is the mean of estD-D from 10000 estimates of D

#for shades of grey
myPlotPlotsm1$col <- seq(0.11, 1, by = 0.9/50) 

#precision:
min(myPlotPlotsm1$sd_D); max(myPlotPlotsm1$sd_D)
# [1] 0.1540882
# [1] 0.2819795
# this is empirical SE(D)- sd(D) from 10000 estimates of D

#plot sims 4m^2

myPlotPlotsm4 <- plotSimResults(probDet = probDet, plotSize = 4)
#myPlotPlotsm4 <- read.table("dataSim/prelimMethods/sm4/collatedResults_3.txt")
myPlotPlotsm4$biasPerC <- myPlotPlotsm1$mean_error/myVars$estD[1]*100

myPlotPlotsm4$col <- seq(0.11, 1, by = 0.9/50)
min(myPlotPlotsm4$sd_D); max(myPlotPlotsm4$sd_D)
# [1] 0.09303153
# [1] 0.2239931

#ds sims 2m transect length
dsDatasm <- read.csv("dataSim/prelimMethods/ds/distResults_3.csv")

# Calculate mean squared error
dsDatasm$error.sq <- dsDatasm$error^2

dsMSE_sm <- mean(dsDatasm$error.sq)
dsMeanP_SM <- mean(dsDatasm$est_p)

#calculate precision and bias
dsP1 <- sd(dsDatasm$est_D)

#bias needs to be % of D
dsDatasm$biasPerC <- dsDatasm$error/myVars$estD[1]*100
dsB1 <- mean(dsDatasm$biasPerC)

gradient_legend <- as.raster(matrix(myPlotPlotsm1$col), ncol = 1)

## SM Plot ----

#copy this onto 'useful code', include gradient legend
#par()              # view current settings
opar <- par()      # make a copy of current settings
#par(opar)          # restore original settings


#for resolution, there is a lot of fiddling (taken care of in ggplot but feck that - learning a whole new thing at this stage) 

png("graphics/biasPrecisionPlotSM.png", width = 550)
layout(matrix(c(1,1,1,0,2,0), nrow = 3), heights = c(1,2,1), widths = c(4,1))

par(mar = c(6,7,4,3), xpd = TRUE)
plot(NULL,
     xlab = "", ylab = "", 
     xlim = c(-50, 3), ylim = c(0.05, 0.3), 
     cex.axis = 1.8, las = 1)
points(myPlotPlotsm1$biasPerC, myPlotPlotsm1$sd_D,
       pch = 16, col = alpha("black", myPlotPlotsm1$col), cex = 1.5)
points(myPlotPlotsm4$biasPerC, myPlotPlotsm4$sd_D,
       pch = 17, col = alpha("black", myPlotPlotsm4$col), cex = 1.5)
mtext("precision (SE(D))", side = 2, line = 5, cex = 1.3)
mtext("bias (mean % error in D_hat)", side = 1, line = 4, cex = 1.3)
points(x = dsB1, y = dsP1, pch = 4, cex = 2, lwd = 2)
legend(x = -48, y = 0.3, legend = c("plot (1m^2)", "plot (4m^2)","LTS"), 
       pch = c(16, 17, 4), horiz = TRUE, bty = "n", cex = 1.8, pt.cex = 1.5)
abline(v=0, lty = "dashed", xpd = FALSE)

par(mar = c(2,4,2,2))
plot(c(0,1), c(0,2), type = 'n', axes = F, ylab = "", xlab = "", xpd = TRUE)
mtext("plot prob. det.", side = 3, las = 1, at = 0.1, cex = 1.2, line = 1)
mtext("0.5", side = 2, las = 1, at = 0, line = 2)
mtext("0.99", side = 2, las = 1, at = 2, line = 2)
rasterImage(gradient_legend,0,0,0.4,2)
dev.off()

## Senecio ----

#ds sims 25m transect length
dsDatasq <- read.csv("dataSim/prelimMethods/ds/distResults_2.csv")

dsDatasq$error_sq <- dsDatasq$error^2
dsDatasq$biasPerC <- dsDatasq$error/myVars$estD[2]*100

dsP2 <- sd(dsDatasq$est_D)
dsB2 <- mean(dsDatasq$biasPerC)
dsMSE_sq <- mean(dsDatasq$error_sq)
dsMeanP_SQ <- mean(dsDatasq$est_p)
dsSE_MeanP_SQ <- sd(dsDatasq$est_p)

## plot data (only one plot size)

plotDatasq <- plotSimResults(probDet = probDet, plotSize = 12.25) 
#plotDatasq <- read.table("dataSim/prelimMethods/sq/collatedResults_2.txt")
plotDatasq$biasPerC <- plotDatasq$mean_error/myVars$estD[2]*100 
plotDatasq$col <- seq(0.11, 1, by = 0.9/50)
min(plotDatasq$sd_D); max(plotDatasq$sd_D)
# [1] 0.006810926
# [1] 0.01098576

## SQ Plot ----
par(opar)          # restore original settings

png("graphics/biasPrecisionPlotSQ.png", width = 550)
layout(matrix(c(1,1,1,0,2,0), nrow = 3), heights = c(1,2,1), widths = c(4,1))

par(mar = c(6,7,4,3), xpd = TRUE)
plot(NULL,
     xlab = "", ylab = "", 
     xlim = c(-50, 3), ylim = c(0.005, 0.012), 
     cex.axis = 1.5, las = 1)
points(plotDatasq$biasPerC, plotDatasq$sd_D, 
       pch = 16, col = alpha("black", plotDatasq$col), cex = 1.5)
points(x = dsB2, y = dsP2, pch = 4, cex = 2, lwd = 2)
mtext("bias (mean error as % of true D)", side = 1, line = 4, cex = 1.3)
mtext("precision (SE(D))", side = 2, line = 5, cex = 1.3)
legend(x = -48, y = 0.012, legend = c("plot (12.25m^2)", "LTS"), 
       pch = c(16, 4), horiz = TRUE, bty = "n", cex = 1.8, pt.cex = 1.5)
abline(v=0, lty = "dashed", xpd = FALSE)

par(mar = c(2,4,2,2))
plot(c(0,1), c(0,2), type = 'n', axes = F, ylab = "", xlab = "", xpd = TRUE)
mtext("plot prob. det.", side = 3, las = 1, at = 0.1, cex = 1.2, line = 1)
mtext("0.5", side = 2, las = 1, at = 0, line = 2)
mtext("0.99", side = 2, las = 1, at = 2, line = 2)
rasterImage(gradient_legend,0,0,0.4,2)
dev.off()

## Combined Plot ----

par(opar)          # restore original settings

#new plot: using MSE instead of SE and bias
png("graphics/biasPrecision_RMSE.png", width = 850)
par(mfrow = c(1, 2), mar = c(5, 5, 3, 1), xpd = TRUE)
plot(x = myPlotPlotsm1$prob.det, y = sqrt(myPlotPlotsm1$mean_sq_error), ylab = "Root Mean Squared Error", xlab = "", ylim = c(0, 2.2), cex.lab = 1.5, cex.axis = 1.5)
points(x = myPlotPlotsm4$prob.det, y = sqrt(myPlotPlotsm4$mean_sq_error), pch = 16)
points(x = dsMeanP_SM, y = sqrt(dsMSE_sm), pch = 4, cex = 2, lwd = 2)
legend(x = 0.8, y = 2, legend = c(expression(1~m^2), expression(4~m^2), "LTS"), pch=c(1,16,4), cex = 1.5, bty = 'n')
mtext("probability of detection", line = 3, side = 1, cex = 1.5, at = 1.08)
mtext("A", line = 1, side = 3, at = 0.75, cex = 1.5)

plot(x = plotDatasq$prob.det, y = sqrt(plotDatasq$mean_sq_error), ylab = "", xlab = "", ylim = c(0,0.075), cex.lab = 1.5, cex.axis = 1.5, pch = 16)
points(x = dsMeanP_SQ, y = sqrt(dsMSE_sq), pch = 4, cex = 2, lwd = 2)
legend(x = 0.8, y = 0.07, bty = 'n', legend = c("plot", "LTS"), pch = c(16, 4), cex = 1.5)
mtext("B", line = 1, side = 3, at = 0.75, cex = 1.5)
dev.off()



png("graphics/biasPrecisionPlot_sidebyside.png", width = 1000)

layout(matrix(c(1,1,1,2,2,2,0,3,0), nrow = 3), heights = c(1,2,1), widths = c(4, 4, 1))
layout.show(3)

#plot 1: stackhousia
par(mar = c(6,7,4,1), xpd = TRUE)
plot(NULL,
     xlab = "", ylab = "", 
     xlim = c(-50, 3), ylim = c(0.05, 0.3), 
     cex.axis = 1.8, las = 1)
points(myPlotPlotsm1$biasPerC, myPlotPlotsm1$sd_D,
       pch = 16, col = alpha("black", myPlotPlotsm1$col), cex = 1.6)
points(myPlotPlotsm4$biasPerC, myPlotPlotsm4$sd_D,
       pch = 17, col = alpha("black", myPlotPlotsm4$col), cex = 1.6)
mtext("precision (SE(D))", side = 2, line = 5, cex = 1.4)
mtext("A", side = 3, line = 2, cex = 1.4, at = -25)
mtext("bias (mean error as % of true D)", side = 1, line = 4, cex = 1.4, at = 10)
legend(x = -48, y = 0.3, legend = c("plot (1m^2)", "plot (4m^2)","LTS"), 
       pch = c(16, 17, 4), horiz = FALSE, bty = "n", cex = 1.9, pt.cex = 1.6)
points(x = dsB1, y = dsP1, pch = 4, cex = 2, lwd = 2)
abline(v=0, lty = "dashed", xpd = FALSE)

#plot 2: senecio
par(mar = c(6,7,4,3), xpd = TRUE)
plot(NULL,
     xlab = "", ylab = "", 
     xlim = c(-50, 3), ylim = c(0.005, 0.012), 
     cex.axis = 1.8, las = 1)
points(plotDatasq$biasPerC, plotDatasq$sd_D, 
       pch = 16, col = alpha("black", plotDatasq$col), cex = 1.6)
points(x = dsB2, y = dsP2, pch = 4, cex = 2, lwd = 2)
#mtext("bias (mean error as % of true D)", side = 1, line = 4, cex = 1.3)
#mtext("precision (SE(D))", side = 2, line = 5, cex = 1.3)
mtext("B", side = 3, line = 2, cex = 1.4, at = -25)
legend(x = -48, y = 0.012, legend = c("plot (12.25m^2)", "LTS"), 
       pch = c(16, 4), horiz = FALSE, bty = "n", cex = 1.9, pt.cex = 1.6)
abline(v=0, lty = "dashed", xpd = FALSE)

#plot 3: gradient legend
par(mar = c(2,4,2,2))
plot(c(0,1), c(0,2), type = 'n', axes = F, ylab = "", xlab = "", xpd = TRUE)
mtext("plot prob. det.", side = 3, las = 1, at = 0.1, cex = 1.4, line = 1)
mtext("0.5", side = 2, las = 1, at = 0, line = 2)
mtext("0.99", side = 2, las = 1, at = 2, line = 2)
rasterImage(gradient_legend,0,0,0.4,2)

dev.off()

# summary stats

# Senecio SE is very low - is this because the actual density is a smaller number?

# calculate CV

SQcvD <- myCVemp(dsDatasq$est_D)
SMcvD <- myCVemp(dsDatasm$est_D)

# looks fine - cv of the two species is fairly similar

# table of values used in sims; DISTANCE estimated D, fitted model par, unit cost (mins), non searching cost (mins), Cm, Cw, PLOTS set up cost

ch2FieldEsts <- data.frame("target" = c("Stackhousia", "Senecio"), 
                           "estD" = myVars$estD, 
                           "fitted model" = character(2), 
                           "ds unit cost (mins)" = myVars$unitCost, 
                           "ds non search cost (mins)" = myVars$nonSearchCost,
                           "Cm" = myVars$Cm, "Cw" = myVars$Cw,
                           "plot non search cost (mins)" = plotVars$setUpCost[c(1,3)],
                           "plot sm 4m set up cost" = c(plotVars$setUpCost[2], NA))
write.csv(ch2FieldEsts, "results/ch2fieldests.csv")

