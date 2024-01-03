library(readxl)

#optimal plot size for the senecio

#s* = sqrt[c *d / (v * t)]

#total time 2x 10x 10m plots is not recorded
#v rough 20 mins total -10 mins search time

c <- 4 #set up time
d <- (3+8)/200 #density
t <- 20/200
counts <- c(3,8)
countPerM <- counts/25 #ADDENDUM 2023: I think this is wrong!!! unless the pilot plots were 5 x 5 instead of 10 x 10???
vCount <- var(countPerM)
sdCount <- sd(countPerM)

sizeOpt <- sqrt(c*d/(vCount*t))

#optimal size given only 2 pilot quadrats is 12.8m
#ADDENDUM, using exactly the same numbers in 2023 gives 10.5
sides <- sqrt(sizeOpt)

#ADDENDUM 2023: using the correct count per m if the plots were 100m^2
countPerM <- counts/100
vCount <- var(countPerM)

sizeOpt <- sqrt(c*d/(vCount*t)) #42
sides <- sqrt(sizeOpt) #6.5

##ADDENDUM 2023: using the correct count per m if the plots were 25m^2
d <- (3+8)/50 #density
t <- 20/50
countPerM <- counts/25
vCount <- var(countPerM)

sizeOpt <- sqrt(c*d/(vCount*t))
sides <- sqrt(sizeOpt)

#

#just back of enveloping: if density is as above, and 
#sigma is 10
sigm <- 10
mu <- sqrt(pi*(sigm^2)/2)
E <- 2*mu*d

predL <- 200/E

#so roughly 150m to get 200 detections

#if I estimate 12 hours of surveying, and they take 20 mins each

nSenqPlots <- 12*3

#import grid of points over survey area

SquadGrid <- read.csv("Fieldwork/Evans St/SquadTotalAreaGridPoints.csv")

SQsample <- sample(SquadGrid$id, 36)

SQplotPoints <- SquadGrid[SquadGrid$id%in%SQsample, ]

write.csv(SQplotPoints, "Fieldwork/Evans St/SquadPlotPointsTotal.csv")

#3 points turned out to be in the ditch area, so add 3 more points in
# case the daisy isn't growing past the ditch

#add 10 more points in case removing close ones becomes necessary.
#randomise order.

SQextraSample <- sample(SquadGrid$id, 10)
SQextraOrder <- 1:10
mySQdf <- data.frame("id" = SQextraSample, "order" = SQextraOrder)

SQextraPlotPoints <- SquadGrid[SquadGrid$id%in%SQextraSample, ]

SQextraPlotPoints <- merge(mySQdf, SQextraPlotPoints, by = "id")

write.csv(SQextraPlotPoints, "Fieldwork/Evans St/SquadPlotPointsExtra.csv")

#add 10 more points in case removing close ones becomes necessary.
#randomise order.

already <- read.csv("Fieldwork/Evans St/SquadPlotPointsAll46.csv")

SQextra2Sample <- sample(SquadGrid$id, 20)

SQextra2Order <- 1:10
mySQdf2 <- data.frame("id" = SQextra2Sample, "order" = SQextra2Order)

SQextraPlotPoints2 <- SquadGrid[SquadGrid$id%in%SQextra2Sample, ]

SQextraPlotPoints2 <- merge(mySQdf2, SQextraPlotPoints2, by = "id")

write.csv(SQextraPlotPoints2, "Fieldwork/Evans St/SquadPlotPointsExtra2.csv")

#planning transects, based on how much plot data I have
myPlotData <- read_excel("Fieldwork/Evans St/dataRaw.xlsx", 
                         sheet = "EStplot_doubleObs", na = "NA")
myPlotData <- myPlotData[myPlotData$target=="Senecio quadridentatus" & 
                           myPlotData$area_m2==12.25, ]
myPlotData$beginT <- strftime(myPlotData$travelTime, format="%H:%M:%S")
myPlotData$endT <- strftime(myPlotData$finishTime, format="%H:%M:%S")

myEffort <- na.omit(myPlotData$totTimeMins)

totalEffort <- sum(myEffort)

#if a transect takes an hour
nTransects <- totalEffort/60
#6.67

#locations of all points in the survey area (1m grid)
SquadGridTrans <- read.csv("Fieldwork/Evans St/Squad1mGridPoints.csv")
SQtransPoints <- sample(SquadGridTrans$id, 10)
#[1] 50882 45191 42277 39094 48956 45132 61289 50258 43836 38181

#Have a list of 10 starting points, remove any that are overlapping in covered area
points <- SquadGridTrans[SquadGridTrans$id%in%SQtransPoints, ]
write.csv(points, "Fieldwork/Evans St/SquadTransPossPoints.csv")

#new list of possible starting point locations, with all non-available points removed
SquadGridTrans1 <- read.csv("Fieldwork/Evans St/SquadTransPossPointsFirstRemove.csv")
SQtransPoints1 <- sample(SquadGridTrans1$id, 12)
# [1] 57802y 47098n(adjacent) 45511n 40416n 
#32149n 49523n 48686n 56886n 46367y 42965y 45469
#[12] 34670

points1 <- SquadGridTrans1[SquadGridTrans1$id%in%SQtransPoints1, ]
write.csv(points1, "Fieldwork/Evans St/SquadTransPossPoints1.csv")

#have seven points, make a new list that can be used as a shapefile
#and gpx file

#have data from 2 transects, worked out effort, need more points
SQtransPoints2 <- sample(SquadGridTrans1$id, 25)
SQgridPoints2 <- SquadGridTrans1[SquadGridTrans1$id%in%SQtransPoints2, ]
write.csv(SQgridPoints2, "Fieldwork/Evans St/SquadTransPossPoints2.csv")
#none of these are in the available areas, so will use for OPT transects
#have isolated rough available areas, using that file 
SQcdsAvail <- read.csv("Fieldwork/Evans St/SquadCDSavailablePlaces.csv")
SQcdsPoints <- sample(SQcdsAvail$id, 10)
SQcdsTransLastPoints <- SQcdsAvail[SQcdsAvail$id%in%SQcdsPoints, ]

write.csv(SQcdsTransLastPoints, "Fieldwork/Evans St/SquadCDSpossPoints3.csv")

#Optimisation calculations based on first lot of transect data (CDS)
#using only 2 x 20m transects when Linda was timing.

myTransData <- read_excel("Fieldwork/Evans St/dataRaw.xlsx", 
                         sheet = "EStTrans_data", na = "NA")
myTransData <- myTransData[myTransData$species=="Senecio quadridentatus" &
                             myTransData$transectID!=802, ]

hist(myTransData$perpDist_m)

myTimeData <- read_excel("Fieldwork/Evans St/dataRaw.xlsx", 
                         sheet = "EStTrans_details", na = "NA")
myTimeData <- myTimeData[myTimeData$target=="Senecio quadridentatus" &
                           myTimeData$length_m==20, ]
timeTot <- sum(myTimeData$surveyTdec) #total time in minutes 
timeMeas <- sum(myTimeData$measureTdec) #total time spent measuring in minutes
timeSearch <- timeTot-timeMeas

Rc <- timeMeas/timeSearch

alphaOpt <- sqrt(1/(2*Rc))

nth <- 1/alphaOpt
#measure 2 in 5: 1,0,1,0,0

#For effort: how long for total length of transect and how long for 
#optimised transect?

B <- 400
setUp <- sum((myTimeData$timeTotdec*60)-myTimeData$surveyTdec)/nrow(myTimeData)

perTrans <- sum(myTimeData$timeTotdec*60)/nrow(myTimeData)

#if transects are 20m
nTrans <- B/perTrans
#8.7 - so 9 means 2 more than those already located.

Cw <- timeSearch/nrow(myTransData)
Cm <- timeMeas/nrow(myTransData)

E <- nrow(myTransData)/sum(myTimeData$length_m)

B1 <- B-setUp #if only 1 transect is used
Opt_L <-  B1/(Cw + E*alphaOpt*Cm) #transect length in m
#not accounting for set up cost
B5 <- B-(5*setUp)
Opt_L_5 <-  B5/(Cw + E*alphaOpt*Cm) 

B15 <- B-(15*setUp)
Opt_L_15 <-  B15/(Cw + E*alphaOpt*Cm) 

#not sure how to work this out
#have notes on paper:
#k = n transects of length 20m

opt20mTime <- setUp+(E*20*(Cw+(alphaOpt*Cm)))
n20mOptTrans <- B/opt20mTime
#at time of writing this is 13.45, or 13.5

#so I need 9 x CDS transects, and 13.5 optimised transects
#or call it 14 and remember to record first 10m and second 10m
#selecting one of 15 gridpoints to drop from the OPT set

myDrop <- 1:15
set.seed(1)
sample(myDrop, 1)
#9

#now just curious about detectability with the plots

myDet <- qFunc(p1=sum(myPlotData$kkPrimaryCount, na.rm = TRUE),
               s1=sum(myPlotData$kkSecondaryCount,na.rm = TRUE),
               p2=sum(myPlotData$SecObsPrimaryCount, na.rm = TRUE),
               s2=sum(myPlotData$SecObsSecondaryCount, na.rm = TRUE))

# senecio - looks like I didn't use the two 10x10m plots as pilots (the optimal plot size is coming out super different)
myNsq <- sum(myPlots[myPlots$target=="Senecio quadridentatus" & myPlots$date=="2021-09-27", 12:15], na.rm = TRUE)
