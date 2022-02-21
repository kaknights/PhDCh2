#Fieldwork methods

plotGrid <- read.csv("Fieldwork/RoyalPark/gridInfo.csv")

plotGrid <- plotGrid[plotGrid$area>=100, ]

mySample <- sample(plotGrid$id, 20)

myPlotSample <- plotGrid[plotGrid$id%in%mySample, ]

write.csv(myPlotSample, "Fieldwork/RoyalPark/pilotPlots.csv")

#choose 10 random gridsquares

#how many plots and what sizes?
#how many plots do I need to show some form of trend, or informative information to plan the full survey?
  #using 10m x 10m plots in the pilot seems like a reasonable plan (they used that in the abundance paper)
  #check that paper for plot counts

#should I save the different size plots for the wildflowers?

#double observer methods

#distance sampling: what do we need to know?
  #Cm - record time start, time end, and cumulative time for each measurement
  #Cw - by subtraction
  #how many transects to use in the pilot?

#Other information to record
  #flowering status
  #cluster size

#datasheet design
#cue up stuff for printing
#email the SERCO dude

#Search time????

#Evans St plot grid sample plots [think this was before I'd decided to 
#make the areas smaller and didn't know which species I was going to survey yet]

myGrid <- read.csv("Fieldwork/Evans St/EStGrid.csv")
myGridSample <- sample(myGrid$id, 25)

myGridPlotSample <- myGrid[myGrid$id%in%myGridSample, ]

write.csv(myGridPlotSample, "Fieldwork/Evans St/pilotPlots.csv")

#ensure enough space in all directions (if it's the middle of the plot)
# 
# myFunc <- function(xydf){ 
#   randPoint <- sample(xydf$id, 1)
#   pointID, pointX, pointY,
#   xmin <- pointX-5
#   xmax <- pointX+5
#   ymin <- pointY-2
#   ymax <- pointY+2
#   myDf <- data.frame("x"= c(xmin, xmax, xmin, xmax),
#                      "y"= c(ymax, ymax, ymin, ymin))
#   result <- merge(myDf, xydf, by = c("x", "y"))
# }

#optimal plot size 
#attempting to make a function to choose points that aren't within a certain distance of other points
#is not working yet...
pointsDF <- newTransPointsSM[,c(1, 6, 7)]
pointsDF$id <- as.numeric(pointsDF$id)
sepDistX <- 2
sepDistY <- 4
nPoints <- 36

pointSepFunc <- function(pointsDF, sepDistX, sepDistY, nPoints){
  myDFout <- data.frame("name" = numeric(length = nPoints), 
                        "x" = numeric(length = nPoints),
                        "y" = numeric(length = nPoints))
  startingPoint <- sample(pointsDF$id, 1)
  
  myDFout[1,] <- pointsDF[pointsDF$id==startingPoint,]
  excludeX <- c(-sepDistX:-1, 1:sepDistX)
  excludeY <- c(-sepDistY:-1, 1:sepDistY)
  
  unavailable <- expand.grid("x" = myDFout$x+excludeX,
                             "y" = myDFout$y[myDFout$name])
  
  for(a in 1:(nPoints-1)){
    repeat {
      #pick a random point id from pointsDF
      nextPoint <- sample(pointsDF$id, 1)
      if(nextPoint%in%myDFout$name) {next} else 
        if((myDFout$x[myDFout$name==nextPoint]+sepDistX) &
           myDFout$x[myDFout$name==nextPoint]<=any(myDFout$x+sepDistX))} 
    
  }}
}



