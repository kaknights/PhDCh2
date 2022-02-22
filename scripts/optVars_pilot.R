                    ####################################
                    #                                  #
                    #    Optimisation variables from   #
                    #            pilot data            #
                    #                                  #
                    ####################################

library(readxl)

########### Stackhousia ###########

smDist <-read_excel(path = "Fieldwork/Evans St/dataRaw.xlsx", 
                    sheet = "EStTrans_data", na = "NA")
#subset by date for transects that I did alone
smDist$date <- as.Date(smDist$date)
#subset by all s mono transects within the survey area where max dist was 2m
withinArea <- c(25549, 30027, 30322, 31889, 35684, 37894,
                37942, 38857, 45529, 101, 141, 283, 128)
#think some of these weren't actually surveyed...

mySubset <- smDist[smDist$transectID%in%withinArea, ]
#calculate alpha opt and distance opt 
length(unique(mySubset$transectID))
nDet <- length(na.omit(mySubset$perpDist_m))

#need transect data sheet for times
smTrans <- read_excel(path = "Fieldwork/Evans St/dataRaw.xlsx", 
                      sheet = "EStTrans_details", na = "NA")

include <- unique(mySubset$transectID)

transSubset <- smTrans[smTrans$transectID%in%include, ]
Cm_Smono <- sum(transSubset$measureTdec)/nDet #0.5805

timeSearching <- sum(transSubset$surveyTdec)-sum(transSubset$measureTdec)
Cw_Smono <- timeSearching/nDet #0.4813

Rc_Smono <- Cm_Smono/Cw_Smono #1.2

alphaOpt_Smono <- sqrt(Cw_Smono/(2*Cm_Smono))
# = 0.644

totallength <- sum(transSubset$length_m)
speed <- totallength/timeSearching

#_______________________________________________________________________________

########### Senecio ###########



myTimeData <- read_excel("Fieldwork/Evans St/dataRaw.xlsx", 
                         sheet = "EStTrans_details", na = "NA")
myTimeData <- myTimeData[myTimeData$transectID==46367 |
                           myTimeData$transectID==57802, ]
myTimeData <- myTimeData[!is.na(myTimeData$transectID), ]
myTransData <- read_excel("Fieldwork/Evans St/dataRaw.xlsx", 
                          sheet = "EStTrans_data", na = "NA")
myTransData <- myTransData[myTransData$transectID %in% myTimeData$transectID, ]

timeTot <- sum(myTimeData$surveyTdec) #total time in minutes 
timeMeas <- sum(myTimeData$measureTdec) #total time spent measuring in minutes
timeSearch <- timeTot-timeMeas
Cw_Squad <- timeSearch/nrow(myTransData)
Cm_Squad <- timeMeas/nrow(myTransData)

Rc_Squad <- timeMeas/timeSearch

alphaOpt_Squad <- sqrt(1/(2*Rc_Squad))

rm(smDist, withinArea, mySubset, nDet, smTrans, include, transSubset, 
   totallength, speed, timeSearching, myTimeData, myTransData, timeTot,
   timeMeas, timeSearch)
