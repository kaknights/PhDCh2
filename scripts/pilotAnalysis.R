#read in pilot data

library(readxl)
pilot <- read_excel(path = "Fieldwork/Evans St/dataRaw.xlsx", 
                        sheet = "EStTrans_data")
#subset by date and transects inside the (newly defined) survey area
pilot$date <- as.Date(pilot$date)
withinArea <- c(22081, 25549, 30027, 30322, 31889, 35684, 37894,
                37942, 38857, 45529)
mySubset <- pilot[pilot$date>="2021-10-04" &
                    pilot$date<="2021-10-05" & 
                    pilot$transectID%in%withinArea, ]

#perpDist column being read as character because of NAs, convert
mySubset$perpDist_m <- as.numeric(mySubset$perpDist_m)
#not sure why but some entries have extra decimal places (not present
# on the excel file), e.g. 1.72 is being read as 1.719999999995?!
mySubset$perpDist_m <- round(mySubset$perpDist_m, 2)

#subset to only where max width is 2m
mySubset <- mySubset[mySubset$perpDist_m<=2, ]

#make histogram of distances
hist(mySubset$perpDist_m)

#effort 

#to make equivalent between transects and plots:
  #how long transects to get enough data (c. 200 obs)
  #how many transects for grouped and optimised?

#how many transects so far
#read in transect data (not distance data)

pTrans <- read_excel(path = "Fieldwork/Evans St/dataRaw.xlsx",
                     sheet = "EStTrans_details", na = "NA")
pTrans$date <- as.Date(pTrans$date)
pTransSubset <- pTrans[pTrans$transectID%in%withinArea, ]
  
meanTperPlot <- mean(pTransSubset$timeTotdec)
meanSetUpPerPlot <- mean(pTransSubset$timeTotdec-(pTransSubset$surveyTdec)/60)
meanSurveyTperPlot <- mean(pTransSubset$surveyTdec/60)


#equivalent effort makes how many plots?


