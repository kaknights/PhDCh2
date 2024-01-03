library(readxl)
library(Distance)

#Stackhousia surveys

#optimal ratios and measuring frequencies:

#if ratio is 0.2, then you measure 1 in 5
1/0.2 #would be measure every 5th
#so 1/alphOpt is measure every nth
1/(1-0.9)

alphOpt <- seq(0.01:1, by = 0.01)
#how do I work this out??
#ratio of 1 in 2
1/0.42
1/0.51
1/0.66

ratio <- data.frame("alphOpt" = alphOpt,
                    "measNth" = nth)

#don't remember which 2m grid this is
my2mGrid <- read.csv("Fieldwork/Evans St/2mGridSurveyPoints.csv")
myGridSample <- sample(my2mGrid$id, 100)

my2mSampleList <- my2mGrid[my2mGrid$id%in%myGridSample, ]

write.csv(my2mSampleList, "Fieldwork/Evans St/samplePlots.csv")
#1m Grid for transect starting points
my1mGrid <- read.csv("Fieldwork/Evans St/1mGridSampleSpace.csv")
my1mGridSample <- sample(my1mGrid$id, 50)

my1mSampleList <- my1mGrid[my1mGrid$id%in%my1mGridSample, ]

write.csv(my1mSampleList, "Fieldwork/Evans St/sampleTrans.csv")

#random sample of the random sample

myTransPoints <- read.csv("Fieldwork/Evans St/sampleTrans.csv")

myFirst20 <- sample(myTransPoints$id, size = 20, replace = FALSE)

my20 <- c(10421,15475,17038,17981,22081,24047,25549,28500,
          29437,38857,40967,46727,50778,51117,51514,52112,
          52475,53012,53035,61585)
newOrder20 <- sample(my20)  
#[1] 53035 53012 51514 46727 17981 15475 29437 52475 28500 17038
#[11] 22081 51117 24047 52112 40967 50778 10421 61585 25549 38857

myNext30 <- c(8159,9125,9128,10386,12905,18979,24037,28023,
              28514,30027,30322,31889,35684,36213,37894,37942,
              39780,39785,41981,42198,44403,45529,51147,51756,
              52158,56228,65395,68521,70439,71059) 
newOrderT30 <- sample(myNext30)
# 56228 71059 12905 37894 52158 41981 35684  8159 39785 18979
# [11] 42198 68521 31889 51756 45529 44403 30027 30322 70439  9128
# [21] 39780 37942 65395 24037 51147 28023  9125 10386 36213 28514


myPlots <- read.csv("Fieldwork/Evans St/samplePlots.csv")
myFirst35 <- sample(myPlots$id, size = 35, replace = FALSE)

newOrder35 <- c(1288,3107,3932,4273,4891,4898,5268,6069,6209,7530,7249,7088,
                7745,8511,8192,9451,9951,9834,9850,10197,10768,11087,11668,
                11176,12599,12896,13929,14362,14869,14708,14722,15215,15717,
                15895,15582)
new <- sample(newOrder35)

#9834 15895  4891  1288  7530 15582  3107  8192  6209  8511 12896 14362 13929 15215 11176  9951  4273 11668  3932  5268 12599  9850
#[23] 14708 10197 10768  6069  9451 15717  7249  7745 14722 11087  4898  7088 14869

#optimal plot size for the stackhousia

#s* = sqrt[c *d / (v * t)]

#planning for surveys 10.10.2021
#have reduced survey area to a part of the field (S mono)
#have grid of plot points
#Don't know yet how many plots are equivalent to distance survey
#need more pilot data
#but also don't have much time to do the surveys! So need to plan
#for pilot data and full survey data at same time!

#pick 50 plots and randomise order, so any excess can be excluded
#without biasing

mySmonoPlots <- read.csv("Fieldwork/Evans St/fullSurveyPlotPointsSmonoREDUCED.csv")
doneSampled <- read.csv("Fieldwork/Evans St/SmonoPlotPoints.csv")

mySmonoPlots <- mySmonoPlots[mySmonoPlots$id%in%doneSampled$name==FALSE, ]
mySamp <- sample(mySmonoPlots$id, 15)

Smono2mPlotSamp <- mySmonoPlots[mySmonoPlots$id%in%mySamp,]

order <- 1:15

OrderedSamp <- cbind.data.frame("id" = mySamp, order)

newDF <- merge(OrderedSamp, Smono2mPlotSamp, by = "id")

#write.csv(newDF, "Fieldwork/Evans St/SmonoPlotPoints2.csv")

#Few more random transects for pilot data

transPoints <- read.csv("Fieldwork/Evans St/SmonoDistTransGridCentrePoints.csv")

transSamp <- sample(transPoints$id, 10)

transPoints <- transPoints[transPoints$id%in%transSamp, ]

idOrder <- 1:10

myNewDf <- cbind.data.frame("id" = transSamp, idOrder)

transPoints <- merge(myNewDf, transPoints, by = "id")
transPoints$newX <- transPoints$x-5

write.csv(transPoints, "Fieldwork/Evans St/SmonoTransPoints.csv")

#Stackhousia survey:
#calculate q: q=1-e^{-\frac{W_{e}vT}{A_{q}}}

#read in plot data for all species
plotData <- read_excel(path = "Fieldwork/Evans St/dataRaw.xlsx", 
                       sheet = "EStplot_doubleObs", na = "NA")

#subset stackhousia data in 1m quadrats
plot1m <- plotData[plotData$target=="Stackhousia monogyna" & plotData$area_m2==1, ]

totalCounts <- apply(cbind(plot1m$kkPrimaryCount,
                           plot1m$kkSecondaryCount,
                           plot1m$SecObsPrimaryCount,
                           plot1m$SecObsSecondaryCount),1,sum,
                     na.rm = TRUE)

#recalculating optimal plot size
SmonoOptPlotSize <- qSizeOpt(c = sum(plot1m$totTimeMins-plot1m$TimeSearch1)/nPlots1m, 
                             t = sum(plot1m$TimeSearch1)/nPlots1m, 
                             counts = totalCounts, area = 1)

#calculate prob. det
my1mP <- qFunc(p1 = sum(plot1m$kkPrimaryCount, na.rm = TRUE), s1 = sum(plot1m$kkSecondaryCount, na.rm = TRUE), 
               p2 = sum(plot1m$SecObsPrimaryCount, na.rm = TRUE), s2 = sum(plot1m$SecObsSecondaryCount, na.rm = TRUE))

#subset stackhousia data in larger quadrat (sum the whole thing, = 1m plus 3m = 4m^2)
plot4m <- plotData[plotData$target=="Stackhousia monogyna" & plotData$plotID !=329 & plotData$plotID !=425, ]
my4mP <- qFunc(p1 = sum(plot4m$kkPrimaryCount, na.rm = TRUE), s1 = sum(plot4m$kkSecondaryCount, na.rm = TRUE), 
               p2 = sum(plot4m$SecObsPrimaryCount, na.rm = TRUE), s2 = sum(plot4m$SecObsSecondaryCount, na.rm = TRUE))

#calculate effort for the plot surveys - total time spent surveying, do separately for 1m and 4m plots
nPlots1m <- length(plot1m$plotID)
nPlots4m <- length(plot4m$plotID)

time1m <- sum(plot1m$totTimeMins, na.rm = TRUE)-sum(plot4m$TimeSearch1[plot4m$area_m2==4], na.rm = TRUE)
time4m <- sum(plot4m$totTimeMins, na.rm = TRUE)/2 #because the start/end times are the same for the 1m and 4m plots, so they appear twice

#estimate detection function (sigma)

#make histogram for visual evaluation
smDist <-read_excel(path = "Fieldwork/Evans St/dataRaw.xlsx", 
                    sheet = "EStTrans_data", na = "NA")
#subset by date for transects that I did alone
smDist$date <- as.Date(smDist$date)
#subset by all s mono transects within the survey area where max dist was 2m
withinArea <- c(25549, 30027, 30322, 31889, 35684, 37894,
                37942, 38857, 45529, 101, 141, 283, 128)#think some of these weren't actually surveyed...

mySubset <- smDist[smDist$transectID%in%withinArea, ]

hist(mySubset$perpDist_m)

#as of 24.10.2021, can't get a detection function yet - still obvious pattern from patches in relation to the orientation of the transect

#optimising the survey

#calculate alpha opt and distance opt 
length(unique(mySubset$transectID))
nDet <- length(na.omit(mySubset$perpDist_m))

#need transect data sheet for times
smTrans <- read_excel(path = "Fieldwork/Evans St/dataRaw.xlsx", 
                      sheet = "EStTrans_details", na = "NA")

include <- unique(mySubset$transectID)

transSubset <- smTrans[smTrans$transectID%in%include, ]
Cm <- sum(transSubset$measureTdec)/nDet #0.5805

timeSearching <- sum(transSubset$surveyTdec)-sum(transSubset$measureTdec)
Cw <- timeSearching/nDet #0.4813

Rc <- Cm/Cw #1.2

alphaOpt <- sqrt(Cw/(2*Cm))
# = 0.644

totallength <- sum(transSubset$length_m)
speed <- totallength/timeSearching

#optimal length (uses objects calculated below); doesn't account for travel and set up costs per transect
optL1mEquiv <- time1m/(E*(Cw+(alphaOpt*Cm)))
optL4mEquiv <- time4m/(E*(Cw+(alphaOpt*Cm)))

#costs for distance transects (one off costs vs survey time for e.g. 2m)

E <- nDet/totallength
Cp <- E*(Cm+Cw) #total cost per m given rate of detection
CpOpt <- E*(Cw+(alphaOpt*Cm))
C2m <- 2*Cp #cost estimate for a 2m transect
C2mOpt <- 2*CpOpt

#travel and setup costs per transect
timeNonSearch <- sum(transSubset$timeTotdec, na.rm = TRUE)*60-sum(transSubset$surveyTdec)
perTrans <- timeNonSearch/length(unique(transSubset$transectID))

total2mTransEstCost <- perTrans+C2m
#equivalent effort for distance transects

nTrans1mEquiv <- time1m/total2mTransEstCost
nTrans4mEquiv <- time4m/total2mTransEstCost

#what total length of transect can the budget allow? if transects are all 2m?
NtransOpt1mEquiv <- time1m/(C2mOpt+perTrans)
NtransOpt4mEquiv <- time4m/(C2mOpt+perTrans)

#locations for Stackhousia optimised distance sampling week of 25/10/21

points <- read.csv("Fieldwork/Evans St/1mGridSmonoPossiblePoints.csv")

my45 <- sample(points$id, 45)

mySet <- points[points$id%in%my45,]

write.csv(mySet, "Fieldwork/Evans St/SmonoOptTransects.csv")

#locations for Stackhousia grouped distance sampling week of 25/10/21
#how many?

setUp <- 10
travel <- 2
clearUp <- 1

search <- 10

estTcost <- setUp+travel+clearUp+search

nTgrouped <- time1m/estTcost
nTgrouped4 <- time4m/estTcost

pointsSmGrouped <- read.csv("Fieldwork/Evans St/1mGridSmonoPossiblePoints.csv")

my28 <- sample(pointsSmGrouped$id, 28)

mySet <- pointsSmGrouped[pointsSmGrouped$id%in%my28,]

write.csv(mySet, "Fieldwork/Evans St/SmonoGroupedTransects.csv")
add2 <- sample(pointsSmGrouped$id, 2)
myNewSet <- pointsSmGrouped[pointsSmGrouped$id%in%add2,]

#Stackhousia standard distance sampling locations and number of transects

#first, how many transects?
#already done 4, what points are covered by the 4?
#removed from points layer in QGIS
newTransPointsSM <- read.csv("Fieldwork/Evans St/1mGridSmonoPossiblePoints2.csv")
#Equivalent to plot effort by time
CDSeffort1mplot <- time1m/total2mTransEstCost
CDSeffort4mplot <- time4m/total2mTransEstCost

my36 <- sample(newTransPointsSM$id, 36)

mySet36 <- newTransPointsSM[newTransPointsSM$id%in%my36,]

write.csv(mySet36, "Fieldwork/Evans St/SmonoCDSTrans_Most.csv")

#need more points because some of them are too close together (tried to build a function to choose points but is taking forever)
#I have 36, 2 might be in the ditch, and 4 are too close to others, have 4 already done, so need 2 more (plus a few just in case?)
#put the just in case ones on a different sheet, and only add them at the end if needed.
add2more <- sample(newTransPointsSM$id[-my36], 2)
myNew2 <- newTransPointsSM[newTransPointsSM$id%in%add2more, ]

mySet38 <- rbind(mySet36, myNew2)
write.csv(mySet38, "Fieldwork/Evans St/SmonoCDSTrans_Most.csv")

#Addendum: field methods appendix, December 2023

# can't find the info in this script so recreating:
# n targets counted in pilot plots
# stackhousia:

myNsm <- sum(myPlots[myPlots$area_m2==4 & myPlots$date=="2021-09-27", 12:15], na.rm = TRUE)

#opt areas: #c is set up time (mean per unit), t is search time (per unit area), area is quadrat size (mean), counts needs to be a vector

# set-up time not recorded, mean time for other similar sized plots is 11 mins (may include counting sticks????). Estimating 20 mins for all 5 plots
# search time per unit area
myTsm <- c(8, 16, 9, 12, 17.5)
myTsm <- sum(myTsm)/(4*5)

smPilot <- myPlots[myPlots$area_m2==4 & myPlots$date=="2021-09-27", ]
smPilot$count <- rowSums(smPilot[,12:15], na.rm = TRUE)

optSMplot <- qSizeOpt(c = 4, t = myTsm, counts = smPilot$count, area = 4)

