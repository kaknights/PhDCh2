#############  Resampling LTS survey data ##################

# Test info ----
#stackhousia

# effort <- 300
# details <- mySmonoLTS_details
# data <- mySmonoLTS
# w <- 2
# n <- 1
# times <- times
# target <- "Stackhousia"
# Cm <- Cm_Smono
# Cw <- Cw_Smono
# i <- 1
# ii <- 1

#senecio

# effort <- 600
# details <- mySquadLTS_details
# data <- mySquadLTS
# w <- 10
# n <- 1
# times <- times
# target <- "Senecio"
# Cm <- Cm_Squad
# Cw <- Cw_Squad
# i <- 1
# ii <- 1

# Distance tables ----

#function to make Distance tables (using (mostly) identically formatted distance survey data):
distTables <- function(distDetails, distData, w){
  region.table <- data.frame("Region.Label" = "EvansSt", "Area" = 
                                 sum(distDetails$length_m)* (2*w))
    sample.table <- data.frame("Sample.Label" = distDetails$transectID,
                               "Region.Label" = "EvansSt",
                               "Effort" = distDetails$length_m)
    if(length(distData$obsID)>0){
    obs.table <- data.frame("object" = distData$obsID[!is.na(distData$perpDist_m)],
                            "Region.Label" = "EvansSt",
                            "Sample.Label" = distData$transectID[!is.na(distData$perpDist_m)])
    data <- data.frame("object" = distData$obsID[!is.na(distData$perpDist_m)],
                       "distance" = distData$perpDist_m[!is.na(distData$perpDist_m)])
    tablesList <- list(region.table, sample.table, obs.table, data)
    } else {
      tablesList <- list(region.table, sample.table)
    }
  return(tablesList)
}

#function to produce distance tables using LTS data as input, getting grouped
#as output
#replace single 'perpDist_m' column with two columns, distbegin and distend
distTablesGrouped <- function(distDetails, distData, w){
  region.table <- data.frame("Region.Label" = "EvansSt", "Area" = 
                               sum(distDetails$length_m)* (2*w))
  sample.table <- data.frame("Sample.Label" = distDetails$transectID,
                             "Region.Label" = "EvansSt",
                             "Effort" = distDetails$length_m)
  
  if(length(distData$obsID)>0){
  obs.table <- data.frame("object" = distData$obsID[!is.na(distData$perpDist_m)],
                          "Region.Label" = "EvansSt",
                          "Sample.Label" = distData$transectID[!is.na(distData$perpDist_m)])
  data <- data.frame("object" = distData$obsID[!is.na(distData$perpDist_m)],
                     "distance" = distData$perpDist_m[!is.na(distData$perpDist_m)])
  data$distbegin <- ifelse(data$distance <= 0.25, 0, 
                           ifelse(data$distance> 0.25 & data$distance <= 0.5, 0.25,
                                  ifelse(data$distance> 0.5 & data$distance <= 1, 0.5, 
                                         ifelse(data$distance >1 & data$distance <= 1.5, 1,
                                                1.5)))) 
  
  data$distend <- ifelse(data$distbegin == 0 | data$distbegin == 0.25, data$distbegin+0.25,
                         data$distbegin+0.5) 
  
  data <- data[,!(colnames(data)== "distance")]
  
  tablesList <- list(region.table, sample.table, obs.table, data)    
  } else {
    tablesList <- list(region.table, sample.table)
  }

  return(tablesList)
}


# Resample ----

# resampleCreateTables takes LTS data and survey variables (effort, unit times,
# costs of walking/measuring, w, distance bands for grouped survey) and gives
# tables ready for distance analysis for LTS, Opt and Grouped methods, where the 
# same transects selected for the LTS survey are used as the basis of the 
# survey data for the other methods. Resampling is done without replacement.

resampleCreateTables <- function(details, data, effort, times, 
                                 target, w, Cm, Cw, ii){
  #resample LTS transects
  tLTS <- times$mean_unit_time[times$target == target & times$method == "LTS"]
  KLTS <- round(effort/tLTS, 1)
  sampLTS <- sample(details$transectID, KLTS)
  mydetailsLTS <- details[details$transectID %in% sampLTS, ]
  mydataLTS <- data[data$transectID %in% mydetailsLTS$transectID, ]
  
  #create distance tables for LTS survey
  myDistTablesLTS <- distTables(distDetails = mydetailsLTS,
                                distData = mydataLTS,
                                w = w)
  
  #take LTS resample data and extend transect length to optimal length,
  #add additional data and transect details to LTS survey data
  
  tOpt <- times$mean_unit_time[times$target == target & times$method == "Opt"]
  KOpt <- round(effort/tOpt)
  diffOpt <- KOpt-KLTS
  
  sampOpt <- sample(details$transectID[!(details$transectID %in% sampLTS)],
                    diffOpt, replace = FALSE)
  mydetailsOpt <- rbind(mydetailsLTS, details[details$transectID %in% sampOpt, ])
  mydataOpt <- rbind(mydataLTS, data[data$transectID %in% sampOpt, ])
  
  #remove distance measurements according to alpha*
  
  aOpt <- sqrt(Cw/(2*Cm))
  my_idxOpt <- seq(1, nrow(mydataOpt), round(1/aOpt, 1))
  mydataOpt$perpDist_m[-my_idxOpt] <- NA
  
  #create distance tables for optimised survey
  
  myDistTablesOpt <- distTables(distDetails = mydetailsOpt,
                                distData = mydataOpt[!is.na(mydataOpt$perpDist_m), ],
                                w = w)
  myDistTablesOpt[[length(myDistTablesOpt)+1]] <- mydataOpt$obsID
  
  #need to add an 'ifelse' so Senecio doesn't throw an error here
  
  if(target == "Senecio"){
    myStuff <- list(myDistTablesLTS, myDistTablesOpt)
    folder <- "resampleSquad/"
        write.table(mydataLTS, paste0("dataSim/", folder, "resampleLTS/resampData", ii, ".txt"))
        write.table(mydataOpt, paste0("dataSim/", folder, "resampleOpt/resampData", ii, ".txt"))
        write.table(mydetailsLTS, paste0("dataSim/", folder, "resampleLTS/resampDetails", ii, ".txt"))
        write.table(mydetailsOpt, paste0("dataSim/", folder, "resampleOpt/resampDetails", ii, ".txt"))
        
  } else {
  #use LTS resample for grouped survey
      tGrB <- times$mean_unit_time[times$method == "GroupedBands"]
      KGrB <- round(effort/tGrB)
      diffGrB <- KGrB - KLTS #why is this here? is not used...
      sampGrB <- sample(mydetailsLTS$transectID, KGrB)
      mydetailsGrB <- mydetailsLTS[mydetailsLTS$transectID %in% sampGrB, ]
      mydataGrB <- mydataLTS[mydataLTS$transectID %in% sampGrB, ]

        myDistTablesGrB <- distTablesGrouped(distDetails = mydetailsGrB,
                                        distData = mydataGrB[!is.na(mydataGrB$perpDist_m), ],
                                        w = w)
 
        myStuff <- list(myDistTablesLTS, myDistTablesOpt, myDistTablesGrB)
        folder <- "resampleSmono/"
      write.table(mydataLTS, paste0("dataSim/", folder, "resampleLTS/resampData", ii, ".txt"))
      write.table(mydataOpt, paste0("dataSim/", folder, "resampleOpt/resampData", ii, ".txt"))
      write.table(mydataGrB, paste0("dataSim/", folder, "resampleGrB/resampData", ii, ".txt"))
      write.table(mydetailsLTS, paste0("dataSim/", folder, "resampleLTS/resampDetails", ii, ".txt"))
      write.table(mydetailsOpt, paste0("dataSim/", folder, "resampleOpt/resampDetails", ii, ".txt"))
      write.table(mydetailsGrB, paste0("dataSim/", folder, "resampleGrB/resampDetails", ii, ".txt"))
        
  }
  return(myStuff)
}

#version with suffix 'half' allows LTS sample to contain a half-length transect.  Some caveats apply: Senecio only, not all transects recorded first and second half.
resampleCreateTables_half <- function(details, data, effort, times, 
                                 target, w, Cm, Cw, ii){
  #resample LTS transects
  tLTS <- times$mean_unit_time[times$target == target & times$method == "LTS"]
  
  #rounds to the nearest half
  KLTS <- round(effort/tLTS*2)/2
  
  #if the number of transects needs to include a half-length, choose one at random and choose either first or second half.
  if (KLTS %% 1!=0){
    sampLTS <- sample(details$transectID, ceiling(KLTS))
 
  mydetailsLTS <- details[details$transectID %in% sampLTS, ]
  half <- sample(sampLTS[-c(57802,46367)], 1)
  mydetailsLTS[mydetailsLTS$transectID==half, "length_m"] <- 10
  mydataLTS <- data[data$transectID %in% mydetailsLTS$transectID, ]
  remove <- sample(c(1,2), 1)
  delete <- mydataLTS[mydataLTS$transectID==half & mydataLTS$section==remove,]
  mydataLTS <- mydataLTS[!mydataLTS$obsID %in% delete$obsID, ]
  
  }else{
    sampLTS <- sample(details$transectID, KLTS)
  mydetailsLTS <- details[details$transectID %in% sampLTS, ]
  mydataLTS <- data[data$transectID %in% mydetailsLTS$transectID, ]
  
  }

  
  #create distance tables for LTS survey
  myDistTablesLTS <- distTables(distDetails = mydetailsLTS,
                                distData = mydataLTS,
                                w = w)
  
  #take LTS resample data and extend transect length to optimal length,
  #add additional data and transect details to LTS survey data
  
  tOpt <- times$mean_unit_time[times$target == target & times$method == "Opt"]
  KOpt <- round(effort/tOpt)
  diffOpt <- KOpt-KLTS
  
  sampOpt <- sample(details$transectID[!(details$transectID %in% sampLTS)],
                    floor(diffOpt), replace = FALSE)
  sampOpt <- c(sampLTS, sampOpt)
  mydetailsOpt <- details[details$transectID %in% sampOpt, ]
  mydataOpt <- data[data$transectID %in% sampOpt, ]
  
  #remove distance measurements according to alpha*
  
  aOpt <- sqrt(Cw/(2*Cm))
  my_idxOpt <- seq(1, nrow(mydataOpt), round(1/aOpt, 1))
  mydataOpt$perpDist_m[-my_idxOpt] <- NA
  
  #create distance tables for optimised survey
  
  myDistTablesOpt <- distTables(distDetails = mydetailsOpt,
                                distData = mydataOpt[!is.na(mydataOpt$perpDist_m), ],
                                w = w)
  myDistTablesOpt[[length(myDistTablesOpt)+1]] <- mydataOpt$obsID
  
  #need to add an 'ifelse' so Senecio doesn't throw an error here
  
  if(target == "Senecio"){
    myStuff <- list(myDistTablesLTS, myDistTablesOpt)
    folder <- "resampleSquad/"
    write.table(mydataLTS, paste0("dataSim/", folder, "resampleLTS/resampData", ii, "half.txt"))
    write.table(mydataOpt, paste0("dataSim/", folder, "resampleOpt/resampData", ii, "half.txt"))
    write.table(mydetailsLTS, paste0("dataSim/", folder, "resampleLTS/resampDetails", ii, "half.txt"))
    write.table(mydetailsOpt, paste0("dataSim/", folder, "resampleOpt/resampDetails", ii, "half.txt"))
    
  } else {
  #use LTS resample for grouped survey
      tGrB <- times$mean_unit_time[times$method == "GroupedBands"]
      KGrB <- round(effort/tGrB)
      diffGrB <- KGrB - KLTS
      sampGrB <- sample(mydetailsLTS$transectID, KGrB)
      mydetailsGrB <- mydetailsLTS[mydetailsLTS$transectID %in% sampGrB, ]
      mydataGrB <- mydataLTS[mydataLTS$transectID %in% sampGrB, ]

        myDistTablesGrB <- distTablesGrouped(distDetails = mydetailsGrB,
                                        distData = mydataGrB[!is.na(mydataGrB$perpDist_m), ],
                                        w = w)
 
        myStuff <- list(myDistTablesLTS, myDistTablesOpt, myDistTablesGrB)
        folder <- "resampleSmono/"
      write.table(mydataLTS, paste0("dataSim/", folder, "resampleLTS/resampData", ii, "half.txt"))
      write.table(mydataOpt, paste0("dataSim/", folder, "resampleOpt/resampData", ii, "half.txt"))
      write.table(mydataGrB, paste0("dataSim/", folder, "resampleGrB/resampData", ii, "half.txt"))
      write.table(mydetailsLTS, paste0("dataSim/", folder, "resampleLTS/resampDetails", ii, "half.txt"))
      write.table(mydetailsOpt, paste0("dataSim/", folder, "resampleOpt/resampDetails", ii, "half.txt"))
      write.table(mydetailsGrB, paste0("dataSim/", folder, "resampleGrB/resampDetails", ii, "half.txt"))
        
  }
  return(myStuff)
  }



# Loop resamples ----

#resampleMany uses the resampleCreateTables function repeated many times
#and returns a list of lists of results of distance analysis on each data set

resampleMany <- function(n, details, data, effort, times, target, 
                         Cm, Cw, w){
  
  myDataList <- vector(mode="list", n)
  myModelList <- vector(mode = "list", n)
  
  for (i in 1:n){ 
    myDataList[[i]] <- vector(mode = "list", 0)
    myModelList[[i]] <- vector(mode = "list", 0)
  }
  
  for (ii in 1:n) {
    resamp <- resampleCreateTables(details, data, effort, times,  
                                   target, w, Cm, Cw, ii)
    resamp$idx <- ii
    myDataList[[ii]] <- resamp

    if(length(resamp[[1]][[4]]$object)>60){
      modelLTS <- ds(data = resamp[[1]][[4]], transect = "line",
                formula = ~1, region.table = resamp[[1]][[1]],
                sample.table = resamp[[1]][[2]],
                obs.table = resamp[[1]][[3]])
      myModelList[[ii]][[1]] <- modelLTS
      
      if(length(resamp[[2]][[4]]$object)>60)
        modelOpt <- ds(data = resamp[[2]][[4]], transect = "line",
                       formula = ~1, region.table = resamp[[2]][[1]],
                       sample.table = resamp[[2]][[2]],
                       obs.table = resamp[[2]][[3]])
      myModelList[[ii]][[2]] <- modelOpt
      
      myModelList[[ii]]$idx <- ii
      
      if(target == "Stackhousia"){
        if(length(resamp[[3]])==4)  {
          modelGrB <- ds(data = resamp[[3]][[4]], transect = "line",
                         formula = ~1, region.table = resamp[[3]][[1]],
                         sample.table = resamp[[3]][[2]],
                         obs.table = resamp[[3]][[3]], cutpoints = c(0, 0.25, 0.5, 1, 
                                                                     1.5, 2))
          myModelList[[ii]][[3]] <- modelGrB     
        } 
      
      }
      } else {
        if(length(resamp[[2]][[4]]$object)>60)
      modelOpt <- ds(data = resamp[[2]][[4]], transect = "line",
                   formula = ~1, region.table = resamp[[2]][[1]],
                   sample.table = resamp[[2]][[2]],
                   obs.table = resamp[[2]][[3]])
      myModelList[[ii]][[2]] <- modelOpt
      
      myModelList[[ii]]$idx <- ii
      
       if(target == "Stackhousia"){
        if(length(resamp[[3]])==4)  {
      modelGrB <- ds(data = resamp[[3]][[4]], transect = "line",
                  formula = ~1, region.table = resamp[[3]][[1]],
                  sample.table = resamp[[3]][[2]],
                  obs.table = resamp[[3]][[3]], cutpoints = c(0, 0.25, 0.5, 1, 
                                                            1.5, 2))
        myModelList[[ii]][[3]] <- modelGrB     
      } 
       }
      }     
         print(paste0("round ", ii))    
      }
print(paste0("finished at ", strftime(Sys.time(), format="%H:%M:%S")))
  return(list(myDataList, myModelList))
  
  }
  
#to be used with the create tables function that allows for half a transect to be used.
resampleMany_half <- function(n, details, data, effort, times, target, 
                         Cm, Cw, w){
  
  myDataList <- vector(mode="list", n)
  myModelList <- vector(mode = "list", n)
  
  for (i in 1:n){ 
    myDataList[[i]] <- vector(mode = "list", 0)
    myModelList[[i]] <- vector(mode = "list", 0)
  }
  
  for (ii in 1:n) {
    resamp <- resampleCreateTables_half(details, data, effort, times,  
                                   target, w, Cm, Cw, ii)
    resamp$idx <- ii
    myDataList[[ii]] <- resamp
    
    if(length(resamp[[1]][[4]]$object)>60){
      modelLTS <- ds(data = resamp[[1]][[4]], transect = "line",
                formula = ~1, region.table = resamp[[1]][[1]],
                sample.table = resamp[[1]][[2]],
                obs.table = resamp[[1]][[3]])
      myModelList[[ii]][[1]] <- modelLTS
      
      if(length(resamp[[2]][[4]]$object)>60)
        modelOpt <- ds(data = resamp[[2]][[4]], transect = "line",
                       formula = ~1, region.table = resamp[[2]][[1]],
                       sample.table = resamp[[2]][[2]],
                       obs.table = resamp[[2]][[3]])
      myModelList[[ii]][[2]] <- modelOpt
      
      myModelList[[ii]]$idx <- ii
      
      if(target == "Stackhousia"){
        if(length(resamp[[3]])==4)  {
          modelGrB <- ds(data = resamp[[3]][[4]], transect = "line",
                         formula = ~1, region.table = resamp[[3]][[1]],
                         sample.table = resamp[[3]][[2]],
                         obs.table = resamp[[3]][[3]], cutpoints = c(0, 0.25, 0.5, 1, 
                                                                     1.5, 2))
          myModelList[[ii]][[3]] <- modelGrB     
        } 
      
      }
      } else {
        if(length(resamp[[2]][[4]]$object)>60)
      modelOpt <- ds(data = resamp[[2]][[4]], transect = "line",
                   formula = ~1, region.table = resamp[[2]][[1]],
                   sample.table = resamp[[2]][[2]],
                   obs.table = resamp[[2]][[3]])
      myModelList[[ii]][[2]] <- modelOpt
      
      myModelList[[ii]]$idx <- ii
      
       if(target == "Stackhousia"){
        if(length(resamp[[3]])==4)  {
      modelGrB <- ds(data = resamp[[3]][[4]], transect = "line",
                  formula = ~1, region.table = resamp[[3]][[1]],
                  sample.table = resamp[[3]][[2]],
                  obs.table = resamp[[3]][[3]], cutpoints = c(0, 0.25, 0.5, 1, 
                                                            1.5, 2))
        myModelList[[ii]][[3]] <- modelGrB     
      } 
       }
      }     
         print(paste0("round ", ii))    
      }
print(paste0("finished at ", strftime(Sys.time(), format="%H:%M:%S")))
  return(list(myDataList, myModelList))
  
  }



#version of function that includes unbanded grouped ds 
resampleCreateTablesWithUnbanded <- function(details, data, effort, times, 
                                             target, w, Cm, Cw){
  #resample LTS transects
  tLTS <- times$mean_unit_time[times$target == target & times$method == "LTS"]
  KLTS <- round(effort/tLTS)
  sampLTS <- sample(details$transectID, KLTS)
  mydetailsLTS <- details[details$transectID %in% sampLTS, ]
  mydataLTS <- data[data$transectID %in% mydetailsLTS$transectID, ]
  
  #create distance tables for LTS survey
  myDistTablesLTS <- distTables(distDetails = mydetailsLTS,
                                distData = mydataLTS,
                                w = w)
  
  #take LTS resample data and extend transect length to optimal length,
  #add additional data and transect details to LTS survey data
  
  tOpt <- times$mean_unit_time[times$target == target & times$method == "Opt"]
  KOpt <- round(effort/tOpt)
  diffOpt <- KOpt-KLTS
  
  sampOpt <- sample(details$transectID[!(details$transectID %in% sampLTS)],
                    diffOpt, replace = TRUE)  
  mydetailsOpt <- rbind(mydetailsLTS, details[details$transectID %in% sampOpt, ])
  mydataOpt <- rbind(mydataLTS, data[data$transectID %in% sampOpt, ])
  
  #remove distance measurements according to alpha*
  
  aOpt <- sqrt(Cw/(2*Cm))
  my_idxOpt <- seq(1, nrow(mydataOpt), round(1/aOpt, 1))
  mydataOpt$perpDist_m[-my_idxOpt] <- NA
  
  #create distance tables for optimised survey
  
  myDistTablesOpt <- distTables(distDetails = mydetailsOpt,
                                distData = mydataOpt[!is.na(mydataOpt$perpDist_m), ],
                                w = w)
  myDistTablesOpt[[5]] <- mydataOpt$obsID
  
  #need to add an 'ifelse' so Senecio doesn't throw an error here
  
  if(target == "Senecio"){
    myStuff <- list(myDistTablesLTS, myDistTablesOpt)
    
  } else {
    #use LTS resample for grouped survey
    tGrB <- times$mean_unit_time[times$method == "GroupedBands"]
    KGrB <- round(effort/tGrB)
    diffGrB <- KGrB - KLTS
    sampGrB <- sample(mydetailsLTS$transectID, KGrB)
    mydetailsGrB <- mydetailsLTS[mydetailsLTS$transectID %in% sampGrB, ]
    mydataGrB <- mydataLTS[mydataLTS$transectID %in% sampGrB, ]
    
    if(nrow(mydataGrB)>0){
      myDistTablesGrB <- distTablesGrouped(distDetails = mydetailsGrB,
                                           distData = mydataGrB[!is.na(mydataGrB$perpDist_m), ],
                                           w = w)
      
      tGrN <- times$mean_unit_time[times$method == "GroupedNoBands"]
      KGrN <- round(effort/tGrN)
      diffGrN <- KGrN - KLTS
      
      #not enough data: this needs to resample more than the total n LTS transects
      ########## Still need to fix this: use the full sample then resample from the pot again?
      sampGrN <- sample(details$transectID[!(details$transectID %in% sampLTS)], diffGrN, replace = TRUE)
      mydetailsGrN <- rbind(mydetailsLTS, details[details$transectID %in% sampGrN, ])
      mydataGrN <- rbind(mydataLTS, data[data$transectID %in% sampGrN, ])
      
      myDistTablesGrN <- distTablesGrouped(distDetails = mydetailsGrN,
                                           distData = mydataGrN[!is.na(mydataGrN$perpDist_m), ],
                                           w = w)
      
      myStuff <- list(myDistTablesLTS, myDistTablesOpt, myDistTablesGrB, myDistTablesGrN)
    } else {
      tGrN <- times$mean_unit_time[times$method == "GroupedNoBands"]
      KGrN <- round(effort/tGrN)
      diffGrN <- KGrN - KLTS
      #not enough data: this needs to resample more than the total n LTS transects
      ########## Still need to fix this: use the full sample then resample from the pot again?
      sampGrN <- sample(details$transectID[!(details$transectID %in% sampLTS)], diffGrN, replace = TRUE)
      mydetailsGrN <- rbind(mydetailsLTS, details[details$transectID %in% sampGrN, ])
      mydataGrN <- rbind(mydataLTS, data[data$transectID %in% sampGrN, ])
      
      myDistTablesGrN <- distTablesGrouped(distDetails = mydetailsGrN,
                                           distData = mydataGrN[!is.na(mydataGrN$perpDist_m), ],
                                           w = w)
      myStuff <- list(myDistTablesLTS, myDistTablesOpt, mydetailsGrB, myDistTablesGrN)
      
    }
    
  }
  
  return(myStuff)
}

## not sure how this is different to the one below.  Check before using.

# Binning without bands ----

#versions of functions needed to run comparison of binning method, where marking out bands does not add to the time cost.  Comparing Opt with binning without bands for Stackhousia.

## Distance tables ----

# Dist tables grouped should work, as long as all 5 bands are used.  If analysis of number of bands is needed, a new function (or wrapper set up) will be needed.  Resample without replacement (requires small sample)

#max number of transects in the LTS data are 32, so need max grouped sample to be e.g. 26; ensure an acutal 'resample'.

# KGrN <- 26
# effort <- times$mean_unit_time[times$method == "GroupedNoBands"]*KGrN

#KOpt <- effort/times$mean_unit_time[times$target == target & times$method == "Opt"]

## Resample ----

resampleCreateTables_Unbanded <- function(details, data, effort, times, 
                                 target, w, Cm, Cw, ii){

  #take LTS resample data and extend transect length to optimal length,
  #add additional data and transect details to LTS survey data
  
  tOpt <- times$mean_unit_time[times$target == target & times$method == "Opt"]
  KOpt <- round(effort/tOpt)
  
  sampOpt <- sample(details$transectID, KOpt, replace = FALSE)
  mydetailsOpt <- details[details$transectID %in% sampOpt, ]
  mydataOpt <- data[data$transectID %in% sampOpt, ]
  #Add a clause to continue without making tables if there isn't any data?  
  if(nrow(mydataOpt)==0){
    myDistTablesOpt <- distTables(distDetails = mydetailsOpt,
                                distData = mydataOpt[!is.na(mydataOpt$perpDist_m), ],
                                w = w)
  myDistTablesOpt[[length(myDistTablesOpt)+1]] <- 0
  } else {
    #remove distance measurements according to alpha*
  
  aOpt <- sqrt(Cw/(2*Cm))
  my_idxOpt <- seq(1, nrow(mydataOpt), round(1/aOpt, 1))
  mydataOpt$perpDist_m[-my_idxOpt] <- NA
  
  #create distance tables for optimised survey
  
  myDistTablesOpt <- distTables(distDetails = mydetailsOpt,
                                distData = mydataOpt[!is.na(mydataOpt$perpDist_m), ],
                                w = w)
  myDistTablesOpt[[length(myDistTablesOpt)+1]] <- mydataOpt$obsID
  }
  
  #use Opt resample for grouped survey
      tGrN <- times$mean_unit_time[times$method == "GroupedNoBands"]
      KGrN <- round(effort/tGrN)
      diffGrN <- KGrN - KOpt
      sampGrN <- sample(details$transectID[!(details$transectID %in% sampOpt)], diffGrN, replace = FALSE)
      sampGrN <- c(sampOpt, sampGrN)
      mydetailsGrN <- details[details$transectID %in% sampGrN, ]
      mydataGrN <- data[data$transectID %in% sampGrN, ]

        myDistTablesGrN <- distTablesGrouped(distDetails = mydetailsGrN,
                                        distData = mydataGrN[!is.na(mydataGrN$perpDist_m), ],
                                        w = w)
 
        myStuff <- list(myDistTablesOpt, myDistTablesGrN)
        folder <- "resampleSmono/"
     
      write.table(mydataOpt, paste0("dataSim/", folder, "resampleOpt/resampData", ii, "UnBanded.txt"))
      write.table(mydataGrN, paste0("dataSim/", folder, "resampleGrN/resampData", ii, "UnBanded.txt"))
      
      write.table(mydetailsOpt, paste0("dataSim/", folder, "resampleOpt/resampDetails", ii, "UnBanded.txt"))
      write.table(mydetailsGrN, paste0("dataSim/", folder, "resampleGrN/resampDetails", ii, "UnBanded.txt"))
        
  
  return(myStuff)
}

## Loop resamples ----
resampleManyWithUnbanded <- function(n, details, data, effort, times, target, 
                         Cm, Cw, w){
  
  myDataList <- vector(mode="list", n)
  myModelList <- vector(mode = "list", n)
  
  for (i in 1:n){ 
    myDataList[[i]] <- vector(mode = "list", 2)
    myModelList[[i]] <- vector(mode = "list", 2)
  }  for (ii in 1:n) {
    resamp <- resampleCreateTables_Unbanded(details, data, effort, times,  
                                   target, w, Cm, Cw, ii)
    resamp$idx <- ii
    myDataList[[ii]] <- resamp
    
    # Added 'if' for occasions when opt resample has no obs
    if(length(resamp[[1]])==5){
      modelOpt <- ds(data = resamp[[1]][[4]], transect = "line",
                   formula = ~1, region.table = resamp[[1]][[1]],
                   sample.table = resamp[[1]][[2]],
                   obs.table = resamp[[1]][[3]])
      myModelList[[ii]][[1]] <- modelOpt
      modelGrN <- ds(data = resamp[[2]][[4]], transect = "line",
                       formula = ~1, region.table = resamp[[2]][[1]],
                       sample.table = resamp[[2]][[2]],
                       obs.table = resamp[[2]][[3]], cutpoints = c(0, 0.25, 0.5, 1, 
                                                                   1.5, 2))
        
        myModelList[[ii]][[2]] <- modelGrN
    } else {
      modelGrN <- ds(data = resamp[[2]][[4]], transect = "line",
                       formula = ~1, region.table = resamp[[2]][[1]],
                       sample.table = resamp[[2]][[2]],
                       obs.table = resamp[[2]][[3]], cutpoints = c(0, 0.25, 0.5, 1, 
                                                                   1.5, 2))
        
        myModelList[[ii]][[2]] <- modelGrN
    }
    
        myModelList[[ii]]$idx <- ii
        
    print(paste0("round ", ii))
  }
  print(paste0("finished at ", strftime(Sys.time(), format="%H:%M:%S")))
  return(list(myDataList, myModelList))
  
}

