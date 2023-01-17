# mean and CI over the resamples ----

## distance surveys

# read in dataframes
SmonoDist <- read.table("dataSim/resampleSmono/distResults-300_2.txt")
SquadDist <- read.table("dataSim/resampleSquad/distResults-360_2.txt")
Smono_plot1m <- read.table("dataSim/resampleSmono/plot1m-300_2.txt")
Smono_plot4m <- read.table("dataSim/resampleSmono/plot4m-300_2.txt")
Squad_plot <- read.table("dataSim/resampleSquad/plot-360_2.txt")

SM_sum_results <- do.call(data.frame, aggregate(x = SmonoDist$estD[!is.na(SmonoDist$estD)], 
                           by = list(SmonoDist$method[!is.na(SmonoDist$estD)]), 
                           FUN = function(x) c(mn = mean(x), sd = sd(x), n = length(x))))

names(SM_sum_results) <- c("method", "meanDhat", "sd(dhat)", "nResamples")
SQ_sum_results <- do.call(data.frame, aggregate(x = SquadDist$estD[!is.na(SquadDist$estD)], 
                           by = list(SquadDist$method[!is.na(SquadDist$estD)]), 
                           FUN = function(x) c(mn = mean(x), sd = sd(x), n = length(x))))
names(SQ_sum_results) <- c("method", "meanDhat", "sd(dhat)", "nResamples")

plotSummarise <- function(df){
  mean1 <- mean(df$dhat_singleObs)
  sd1 <- sd(df$dhat_singleObs)
  n1 <- length(df$dhat_singleObs)
    mydf <- data.frame("method" = "plotSingle",
                      "meanDhat" = mean1,
                      "sd(dhat)" = sd1,
                      "nResamples" = n1)
  mean2 <- mean(df$dhat_doubleObs)
  sd2 <- sd(df$dhat_doubleObs)
  n2 <- length(df$dhat_doubleObs)
  mydf[2,1] <- "plotDouble"
  mydf[2,2:4] <- c(mean2, sd2, n2)
  return(mydf)
}

SM_sum_results[4:5,1:4] <- plotSummarise(df = Smono_plot1m)
SM_sum_results[4:5,"method"] <- c("plotSingle1m", "plotDouble1m")
SM_sum_results[6:7,1:4] <- plotSummarise(df = Smono_plot4m)
SM_sum_results[6:7,"method"] <- c("plotSingle4m", "plotDouble4m")
SQ_sum_results[3:4,1:4] <- plotSummarise(df = Squad_plot)

#add se from modelled estimates

distSEDmod_SM <- aggregate.data.frame(SmonoDist$seD[!is.na(SmonoDist$seD) & SmonoDist$estD != 0], by = list(SmonoDist$method[!is.na(SmonoDist$seD) & SmonoDist$estD != 0]), FUN = mean)
names(distSEDmod_SM) <- c("method", "seD_mod")
SM_sum_results <- merge(SM_sum_results, distSEDmod_SM, by = "method")

distSEDmod_SQ <- aggregate.data.frame(SquadDist$seD[!is.na(SquadDist$seD) & SquadDist$estD != 0], by = list(SquadDist$method[!is.na(SquadDist$seD) & SquadDist$estD != 0]), FUN = mean)
names(distSEDmod_SQ) <- c("method", "seD_mod")
SQ_sum_results <- merge(SQ_sum_results, distSEDmod_SQ, by = "method")

#do CIs need to be asymmetrical?

myCI <- function(df){
  
lower <- quantile(df$dhat_doubleObs, probs = 0.025, na.rm = TRUE)
upper <- quantile(df$dhat_doubleObs, probs = 0.975, na.rm = TRUE)
  
CI <- c(lower, upper)
return(CI)
}

SM_sum_results$CI95_lower <- numeric(nrow(SM_sum_results))
SM_sum_results$CI95_upper <- numeric(nrow(SM_sum_results))

SM_sum_results[1, 5:6] <- myCI(df = SmonoDist[SmonoDist$method == "GrB", ])
SM_sum_results[2, 5:6] <- myCI(df = SmonoDist[SmonoDist$method == "LTS", ])
SM_sum_results[3, 5:6] <- myCI(df = SmonoDist[SmonoDist$method == "Opt", ])
SM_sum_results[4, 5:6] <- myCI(df = Smono_plot1m)
SM_sum_results[6, 5:6] <- myCI(df = Smono_plot4m)
SM_sum_results[5, 5:6] <- myCI(df = Smono_plot1m)
SM_sum_results[7, 5:6] <- myCI(df = Smono_plot4m)

#histograms

hist(SmonoDist$estD[SmonoDist$method == "LTS"], xlab = "Dhat LTS", main = "")
hist(SmonoDist$estD[SmonoDist$method == "GrB"], xlab = "Dhat GrB", main = "")
hist(SmonoDist$estD[SmonoDist$method == "Opt"], xlab = "Dhat Opt", main = "")
hist(Smono_plot1m$dhat_singleObs, xlab = "Dhat plot1m single", main = "")
hist(Smono_plot1m$dhat_doubleObs, xlab = "Dhat plot1m double", main = "")
hist(Smono_plot4m$dhat_singleObs, xlab = "Dhat plot4m single", main = "")
hist(Smono_plot4m$dhat_doubleObs, xlab = "Dhat plot4m double", main = "")