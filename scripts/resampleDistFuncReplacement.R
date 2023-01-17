# Resampling functions with replacement 

##Resampling LTS survey data ----

# Test info ----
#stackhousia

# effort <- 600
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

# function to create distance tables for one-off analysis

distTables_OneOff <- function(distDetails, distData, w){
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

#function to make Distance tables from resampled data where the ID columns for transect and observation may be duplicated (so a different unique ID column was added to the wrapper function that resamples the LTS data:
distTables_Replacement <- function(distDetails, distData, w){
  region.table <- data.frame("Region.Label" = "EvansSt", "Area" = 
                                 sum(distDetails$length_m)* (2*w))
    sample.table <- data.frame("Sample.Label" = distDetails$uniqueIDtrans,
                               "Region.Label" = "EvansSt",
                               "Effort" = distDetails$length_m)
    if(length(distData$obsID)>0){
    obs.table <- data.frame("object" = distData$uniqueIDobs[!is.na(distData$perpDist_m)],
                            "Region.Label" = "EvansSt",
                            "Sample.Label" = distData$uniqueIDtrans[!is.na(distData$perpDist_m)])
    data <- data.frame("object" = distData$uniqueIDobs[!is.na(distData$perpDist_m)],
                       "distance" = distData$perpDist_m[!is.na(distData$perpDist_m)])
    tablesList <- list(region.table, sample.table, obs.table, data)
    } else {
      tablesList <- list(region.table, sample.table)
    }
  return(tablesList)
}

# Senecio analysis needs a column for cluster size
distTables_ReplClust <- function(distDetails, distData, w){
  region.table <- data.frame("Region.Label" = "EvansSt", "Area" = 
                                 sum(distDetails$length_m)* (2*w))
    sample.table <- data.frame("Sample.Label" = distDetails$uniqueIDtrans,
                               "Region.Label" = "EvansSt",
                               "Effort" = distDetails$length_m)
    if(length(distData$obsID)>0){
    obs.table <- data.frame("object" = distData$uniqueIDobs[!is.na(distData$perpDist_m)],
                            "Region.Label" = "EvansSt",
                            "Sample.Label" = distData$uniqueIDtrans[!is.na(distData$perpDist_m)])
    data <- data.frame("object" = distData$uniqueIDobs[!is.na(distData$perpDist_m)],
                       "distance" = distData$perpDist_m[!is.na(distData$perpDist_m)],
                       "size" = distData$clusterSize[!is.na(distData$perpDist_m)])
    tablesList <- list(region.table, sample.table, obs.table, data)
    } else {
      tablesList <- list(region.table, sample.table)
    }
  return(tablesList)
}


distTablesGroupedRepl <- function(distDetails, distData, w){
  region.table <- data.frame("Region.Label" = "EvansSt", "Area" = 
                               sum(distDetails$length_m)* (2*w))
  sample.table <- data.frame("Sample.Label" = distDetails$uniqueIDtrans,
                             "Region.Label" = "EvansSt",
                             "Effort" = distDetails$length_m)
  
  if(length(distData$obsID)>0){
  obs.table <- data.frame("object" = distData$uniqueIDobs[!is.na(distData$perpDist_m)],
                          "Region.Label" = "EvansSt",
                          "Sample.Label" = distData$uniqueIDtrans[!is.na(distData$perpDist_m)])
  data <- data.frame("object" = distData$uniqueIDobs[!is.na(distData$perpDist_m)],
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

# resampleCreateTables takes LTS data and survey variables (effort, unit times, costs of walking/measuring, w, distance bands for grouped survey) and gives tables ready for distance analysis for LTS, Opt and Grouped methods, where the same transects selected for the LTS survey are used as the basis of the survey data for the other methods. 

#version with suffix 'replacement' or 'Repl' resamples with replacement.
resampleCreateTables_Replacement <- function(details, data, effort, times, 
                                 target, w, Cm, Cw, ii){
  #resample LTS transects
  tLTS <- times$mean_unit_time[times$target == target & times$method == "LTS"]
  KLTS <- round(effort/tLTS)
  remainder <- KLTS-(effort/tLTS)
  signLTS <- sign(remainder)
  remainder <- abs(remainder)
  # N transects sampled needs to be consistent with the budget - if there is a partial transect, remove/add some % of the values and make sure the shorter length (or additional transect) is taken account of in the tables
  # +ve remainder means K is rounded UP, so some obs need to be removed and transect length shortened
  # -ve remainder means K is rounded DOWN, so we need to add another transect for the extra obs
  sampLTS <- sample(details$transectID, KLTS, replace = TRUE)
  sampLTS <- as.data.frame(sampLTS)
  mydetailsLTS <- merge(sampLTS, details, by.x = "sampLTS", by.y = "transectID")
  colnames(mydetailsLTS)[colnames(mydetailsLTS)=="sampLTS"] <- "transectID"
  mydetailsLTS$uniqueIDtrans <- 1:nrow(mydetailsLTS)
  
  mydataLTS <- merge(mydetailsLTS, data, by= "transectID")
  mydataLTS$uniqueIDobs <- 1:nrow(mydataLTS)
  
  #corrections for partial transect
  if(signLTS == 1){
    
  #rm extra bit of length from transect selected at random
    transEdit <- sample(mydetailsLTS$uniqueIDtrans, 1)
    mydetailsLTS$length_m[mydetailsLTS$uniqueIDtrans==transEdit] <- mydetailsLTS$length_m[mydetailsLTS$uniqueIDtrans==transEdit]*(1-remainder) 
    transEditID <- mydetailsLTS$transectID[mydetailsLTS$uniqueIDtrans==transEdit]
    #select at random obs to remove
    selectFromThese <- mydataLTS$uniqueIDobs[mydataLTS$transectID==transEditID]
    
    if(length(selectFromThese)>0){
      removeThese <- sample(selectFromThese, size = round(length(selectFromThese)*remainder))
      mydataLTS <- mydataLTS[!(mydataLTS$uniqueIDobs %in% removeThese),]
    }
  
    
  } else {
    #select at random another transect
    newTransect <- sample(details$transectID, 1, replace = TRUE) 
    newDetails <- details[details$transectID==newTransect, ]
   
    #adjust length
    newDetails$length_m <- abs(remainder)*newDetails$length_m
    newDetails$uniqueIDtrans <- max(mydetailsLTS$uniqueIDtrans)+1
   
    #add to details
    mydetailsLTS <- rbind(mydetailsLTS, newDetails)
   
    #select new obs
    newObs <- data[data$transectID==newTransect,]
    if(nrow(newObs)!=0) {
      addThese <- sample(newObs$obsID, size = round(abs(remainder)*length(newObs$obsID)))
      newObs <- newObs[newObs$obsID %in% addThese, ]
      newObs$uniqueIDobs <- (nrow(mydataLTS)+1):(nrow(mydataLTS)+nrow(newObs))
   
      #add to data
      newData <- merge(newDetails, newObs, by = "transectID")  
      mydataLTS <- rbind(mydataLTS, newData)
   
    }
  
  } 
  
  #create distance tables for LTS survey
  if (target == "Stackhousia"){
    myDistTablesLTS <- distTables_Replacement(distDetails = mydetailsLTS,
                                distData = mydataLTS,
                                w = w)
  } else {
    myDistTablesLTS <- distTables_ReplClust(distDetails = mydetailsLTS,
                                distData = mydataLTS,
                                w = w)
  }
  
  #take LTS resample data and extend transect length to optimal length,
  #add additional data and transect details to LTS survey data
  
  tOpt <- times$mean_unit_time[times$target == target & times$method == "Opt"]
  KOpt <- round(effort/tOpt)
  remainderOpt <- KOpt-(effort/tOpt)
  mysign <- sign(remainderOpt)
  remainderOpt <- abs(remainderOpt)
  diffOpt <- KOpt-KLTS #KLTS is the round number, not corrected for a partial transect
  sampOpt <- sample(details$transectID,
                    diffOpt, replace = TRUE)

  sampOpt <- c(sampLTS$sampLTS, sampOpt)
  sampOpt <- as.data.frame(sampOpt)
  mydetailsOpt <- merge(sampOpt, details, by.x = "sampOpt", by.y = "transectID")
  colnames(mydetailsOpt)[colnames(mydetailsOpt)=="sampOpt"] <- "transectID"
  mydetailsOpt$uniqueIDtrans <- 1:nrow(mydetailsOpt)
  mydataOpt <- merge(mydetailsOpt, data, by= "transectID")
  mydataOpt$uniqueIDobs <- 1:nrow(mydataOpt)
  
  #add corrections for partial transect for Opt sample
    if(mysign == 1){
      #the remainder is positive - remove some obs and shorten transect
      #rm extra bit of last transect on length
      transEditOpt <- sample(mydetailsOpt$uniqueIDtrans, 1)
      mydetailsOpt$length_m[mydetailsOpt$uniqueIDtrans==transEditOpt] <- mydetailsOpt$length_m[mydetailsOpt$uniqueIDtrans==transEditOpt]*(1-remainderOpt) 
  
      #select at random obs to remove
      transEditIdOpt <- mydetailsOpt$transectID[mydetailsOpt$uniqueIDtrans==transEditOpt]
      chooseFromThese <- mydataOpt$uniqueIDobs[mydataOpt$transectID==transEditIdOpt]
      if(length(chooseFromThese)>0){
        rejectThese <- sample(chooseFromThese, size = round(length(chooseFromThese)*remainderOpt))
        mydataOpt <- mydataOpt[!(mydataOpt$uniqueIDobs %in% rejectThese),]
      }
   
    } else {
      #select at random another transect
      newTransectOpt <- sample(details$transectID, 1, replace = TRUE) 
      newDetailsOpt <- details[details$transectID==newTransectOpt, ]
   
      #adjust length
      newDetailsOpt$length_m <- abs(remainderOpt)*newDetailsOpt$length_m
      newDetailsOpt$uniqueIDtrans <- max(mydetailsOpt$uniqueIDtrans)+1
   
      #add to details
      mydetailsOpt <- rbind(mydetailsOpt, newDetailsOpt)
   
      #select new obs
      newObsOpt <- data[data$transectID==newTransectOpt,]
      
      if(nrow(newObsOpt)!=0) {
        addTheseOpt <- sample(newObsOpt$obsID, size = round(abs(remainderOpt)*length(newObsOpt$obsID)))
        newObsOpt <- newObsOpt[newObsOpt$obsID %in% addTheseOpt, ]
        newObsOpt$uniqueIDobs <- (nrow(mydataOpt)+1):(nrow(mydataOpt)+nrow(newObsOpt))
   
        #add to data
        newDataOpt <- merge(newDetailsOpt, newObsOpt, by = "transectID")  
        mydataOpt <- rbind(mydataOpt, newDataOpt)
      }
   
  
  } 
  
  #remove distance measurements according to alpha*
  
  aOpt <- sqrt(Cw/(2*Cm))
  my_idxOpt <- seq(1, nrow(mydataOpt), round(1/aOpt, 1))
  mydataOpt$perpDist_m[-my_idxOpt] <- NA
  
  #create distance tables for optimised survey
  
  if (target == "Senecio"){
    myDistTablesOpt <- distTables_ReplClust(distDetails = mydetailsOpt,
                                distData = mydataOpt[!is.na(mydataOpt$perpDist_m), ],
                                w = w)
  } else {
    myDistTablesOpt <- distTables_Replacement(distDetails = mydetailsOpt,
                                distData = mydataOpt[!is.na(mydataOpt$perpDist_m), ],
                                w = w)
  }
  
  myDistTablesOpt[[length(myDistTablesOpt)+1]] <- mydataOpt$obsID
  
  #add an 'ifelse' because we don't do grouped analysis for Senecio 
      
  if(target == "Senecio"){
    myStuff <- list(myDistTablesLTS, myDistTablesOpt)
    folder <- "resampleSquad/"
    write.table(mydataLTS, paste0("dataSim/", folder, "resampleLTS/resampData", ii, "Repl.txt"))
    write.table(mydataOpt, paste0("dataSim/", folder, "resampleOpt/resampData", ii, "Repl.txt"))
    write.table(mydetailsLTS, paste0("dataSim/", folder, "resampleLTS/resampDetails", ii, "Repl.txt"))
    write.table(mydetailsOpt, paste0("dataSim/", folder, "resampleOpt/resampDetails", ii, "Repl.txt"))
    
              
  } else {
  #use LTS resample for grouped survey
      tGrB <- times$mean_unit_time[times$method == "GroupedBands"]
      KGrB <- round(effort/tGrB)
      remainderGrB <- KGrB-(effort/tGrB)
      mysignGrB <- sign(remainderGrB)
      
      #diffGrB <- KGrB - KLTS #only need this line if KLTS sample is smaller than KGrB (put 'diffGrB' in next line)
      sampGrB <- sample(mydetailsLTS$transectID, KGrB, replace = FALSE)
      #sampGrB <- c(sampLTS$sampLTS, sampGrB) #line only needed if KLTS is smaller than KGrB
      sampGrB <- as.data.frame(sampGrB)
      mydetailsGrB <- merge(sampGrB, details, by.x = "sampGrB", by.y = "transectID")
      colnames(mydetailsGrB)[colnames(mydetailsGrB)=="sampGrB"] <- "transectID"
      mydetailsGrB$uniqueIDtrans <- 1:nrow(mydetailsGrB)
      mydataGrB <- merge(mydetailsGrB, data, by = "transectID")
      mydataGrB$uniqueIDobs <- 1:nrow(mydataGrB)
      
      #corrections for partial transect
      if(mysign == 1){
        #the remainder is positive - remove some obs and shorten transect
        #rm extra bit of last transect on length
        transEditGrB <- sample(mydetailsGrB$uniqueIDtrans, 1)
        mydetailsGrB$length_m[mydetailsGrB$uniqueIDtrans==transEditGrB] <- mydetailsGrB$length_m[mydetailsGrB$uniqueIDtrans==transEditGrB]*(1-remainderGrB) 
  
        #select at random obs to remove
        transEditIdGrB <- mydetailsGrB$transectID[mydetailsGrB$uniqueIDtrans==transEditGrB]
        FromThese <- mydataGrB$uniqueIDobs[mydataGrB$uniqueIDtrans==transEditIdGrB]
          if(length(FromThese)>0){
            reject <- sample(FromThese, size = round(length(FromThese)*remainderGrB))
            mydataGrB <- mydataGrB[!(mydataGrB$uniqueIDobs %in% reject),]
          }
   
    } else {
      #select at random another transect
      newTransectGrB <- sample(details$transectID, 1, replace = TRUE) 
      newDetailsGrB <- details[details$transectID==newTransectGrB, ]
   
      #adjust length
      newDetailsGrB$length_m <- abs(remainderGrB)*newDetailsGrB$length_m
      newDetailsGrB$uniqueIDtrans <- max(mydetailsGrB$uniqueIDtrans)+1
   
      #add to details
      mydetailsGrB <- rbind(mydetailsGrB, newDetailsGrB)
   
      #select new obs
      newObsGrB <- data[data$transectID==newTransectGrB,]
      
      if(nrow(newObsGrB!=0)) {
        addTheseGrB <- sample(newObsGrB$obsID, size = round(abs(remainderGrB)*length(newObsGrB$obsID)))
        newObsGrB <- newObsGrB[newObsGrB$obsID %in% addTheseGrB, ]
        newObsGrB$uniqueIDobs <- (nrow(mydataGrB)+1):(nrow(mydataGrB)+nrow(newObsGrB))
   
        #add to data
        newDataGrB <- merge(newDetailsGrB, newObsGrB, by = "transectID")  
        mydataGrB <- rbind(mydataGrB, newDataGrB)
      }
   
  
  } 
      myDistTablesGrB <- distTablesGroupedRepl(distDetails = mydetailsGrB,
                                        distData = mydataGrB[!is.na(mydataGrB$perpDist_m), ],
                                        w = w)
      folder <- "resampleSmono/"
      write.table(mydataLTS, paste0("dataSim/", folder, "resampleLTS/resampData", ii, "Repl.txt"))
      write.table(mydataOpt, paste0("dataSim/", folder, "resampleOpt/resampData", ii, "Repl.txt"))
      write.table(mydataGrB, paste0("dataSim/", folder, "resampleGrB/resampData", ii, "Repl.txt"))
      write.table(mydetailsLTS, paste0("dataSim/", folder, "resampleLTS/resampDetails", ii, "Repl.txt"))
      write.table(mydetailsOpt, paste0("dataSim/", folder, "resampleOpt/resampDetails", ii, "Repl.txt"))
      write.table(mydetailsGrB, paste0("dataSim/", folder, "resampleGrB/resampDetails", ii, "Repl.txt"))
        
      myStuff <- list(myDistTablesLTS, myDistTablesOpt, myDistTablesGrB)
  }
  return(myStuff)
}

#testing for Senecio - one round at a time
# set.seed(mySeed)
# test <- resampleCreateTables_Replacement(details = details, data = data, effort = effort, times = times, target = "Senecio", Cm = Cm, Cw = Cw, w = w, ii =  1)
# #failTest <- test
# length(test[[1]][[3]][[3]])
# hist(test[[1]][[4]]$distance)
# for(x in 1:n){
#   try(modelLTShn <- ds(data = test[[1]][[4]], transect = "line",
#                 formula = ~1, key = "hn", region.table = test[[1]][[1]],
#                 sample.table = test[[1]][[2]],
#                 obs.table = test[[1]][[3]]))
#   try(modelLTShr <- ds(data = test[[1]][[4]], transect = "line",
#                 formula = ~1, key = "hr", region.table = test[[1]][[1]],
#                 sample.table = test[[1]][[2]],
#                 obs.table = test[[1]][[3]]))
#   try(modelLTSun <- ds(data = test[[1]][[4]], transect = "line",
#                 formula = ~1, key = "unif", region.table = test[[1]][[1]],
#                 sample.table = test[[1]][[2]],
#                 obs.table = test[[1]][[3]]))
#   
#   myLog <- c(exists("modelLTShn"), exists("modelLTShr"), exists("modelLTSun"))
#   myLogTrue <- c("modelLTShn", "modelLTShr", "modelLTSun")[which(myLog)]
#     if(length(myLogTrue) == 1){myModelList[[ii]][[1]] <- get(myLogTrue)}
#   
#     if(length(myLogTrue) == 3){myCompare <- summarize_ds_models(modelLTShn, modelLTShr, modelLTSun)
#                              myModelList[[ii]][[1]] <- get(eval(parse(text = (substr(myCompare[1,1], 10, 19)))))}
#   
#     if(length(myLogTrue) == 2){myCompare <- summarize_ds_models(get(myLogTrue[1]), get(myLogTrue[2]))
#                                myModelList[[ii]][[1]] <- get(eval(parse(text = (substr(myCompare[1,1], 13, 24)))))}
#     
# }

#write.csv(myCompare, "dataSim/exampleModelCompare.csv")

# Loop resamples ----

#resampleMany uses the resampleCreateTables function repeated many times
#and returns a list of lists of results of distance analysis on each data set

resampleMany_Replacement <- function(n, details, data, effort, times, target, 
                         Cm, Cw, w){
  
  myDataList <- vector(mode="list", n)
  myModelList <- vector(mode = "list", n)
  
  for (i in 1:n){ 
    myDataList[[i]] <- vector(mode = "list", 0)
    myModelList[[i]] <- vector(mode = "list", 0)
  }
  
    for (ii in 1:n) {
      resamp <- resampleCreateTables_Replacement(details, data, effort, times,  
                                   target, w, Cm, Cw, ii)
      resamp$idx <- ii
      myDataList[[ii]] <- resamp

      #had to change indexing for resamp from one analysis to another - sometimes it should be resamp[[1]][[4]] etc, others just resamp[[4]]
        # Fit models with each of the key functions (LTS)
      try(modelLTShn <- ds(data = resamp[[1]][[4]], transect = "line",
                formula = ~1, key = "hn", region.table = resamp[[1]][[1]],
                sample.table = resamp[[1]][[2]],
                obs.table = resamp[[1]][[3]]))
        
      try(modelLTShr <- ds(data = resamp[[1]][[4]], transect = "line",
                formula = ~1, key = "hr", region.table = resamp[[1]][[1]],
                sample.table = resamp[[1]][[2]],
                obs.table = resamp[[1]][[3]]))
        
      try(modelLTSun <- ds(data = resamp[[1]][[4]], transect = "line",
                formula = ~1, key = "unif", region.table = resamp[[1]][[1]],
                sample.table = resamp[[1]][[2]],
                obs.table = resamp[[1]][[3]]))
      
        # Compare models and Pass selected model into output
      myLog <- c(exists("modelLTShn"), exists("modelLTShr"), exists("modelLTSun"))
      if(sum(myLog)==0){myModelList[[ii]][[1]] <- "No LTS models fit"}
        
      myLogTrue <- c("modelLTShn", "modelLTShr", "modelLTSun")[which(myLog)]
      
      if(length(myLogTrue) == 1){myModelList[[ii]][[1]] <- get(myLogTrue)}
  
      if(length(myLogTrue) == 3){myCompare <- summarize_ds_models(modelLTShn, modelLTShr, modelLTSun)
                                  myModelList[[ii]][[1]] <- get((substr(myCompare[1,1], 9, 18)))}
  
      if(length(myLogTrue) == 2){myCompare <- summarize_ds_models(get(myLogTrue[1]), get(myLogTrue[2]))
                               myModelList[[ii]][[1]] <- get(eval(parse(text = (substr(myCompare[1,1], 13, 24)))))}
    
        # Fit models with each of the key functions (Opt)
      try(modelOpthn <- ds(data = resamp[[2]][[4]], transect = "line",
                       formula = ~1, key = "hn", region.table = resamp[[2]][[1]],
                       sample.table = resamp[[2]][[2]],
                       obs.table = resamp[[2]][[3]]))
      try(modelOpthr <- ds(data = resamp[[2]][[4]], transect = "line",
                       formula = ~1, key = "hr", region.table = resamp[[2]][[1]],
                       sample.table = resamp[[2]][[2]],
                       obs.table = resamp[[2]][[3]]))
      try(modelOptun <- ds(data = resamp[[2]][[4]], transect = "line",
                       formula = ~1, key = "un", region.table = resamp[[2]][[1]],
                       sample.table = resamp[[2]][[2]],
                       obs.table = resamp[[2]][[3]]))
      # Compare models and Pass selected model into output
      myLog1 <- c(exists("modelOpthn"), exists("modelOpthr"), exists("modelOptun"))
      if(sum(myLog1)==0){myModelList[[ii]][[2]] <- "No Opt models fit"}
        
      myLogTru1 <- c("modelOpthn", "modelOpthr", "modelOptun")[which(myLog1)]
      
      if(length(myLogTru1) == 1){myModelList[[ii]][[2]] <- get(myLogTru1)}
  
      if(length(myLogTru1) == 3){myCompar1 <- summarize_ds_models(modelOpthn, modelOpthr, modelOptun)
                                  myModelList[[ii]][[2]] <- get(substr(myCompar1[1,1], 9, 18))}
  
      if(length(myLogTru1) == 2){myCompar1 <- summarize_ds_models(get(myLogTru1[1]), get(myLogTru1[2]))
                                  myModelList[[ii]][[2]] <- get(eval(parse(text = (substr(myCompar1[1,1], 13, 24)))))}
      
         
      
          if(target == "Stackhousia"){
            
              try(modelGrBhn <- ds(data = resamp[[3]][[4]], transect = "line",
                             key = "hn", region.table = resamp[[3]][[1]],
                             sample.table = resamp[[3]][[2]],
                             obs.table = resamp[[3]][[3]], cutpoints = c(0, 0.25, 0.5, 1, 
                                                                         1.5, 2)))
              try(modelGrBhr <- ds(data = resamp[[3]][[4]], transect = "line",
                             key = "hr", region.table = resamp[[3]][[1]],
                             sample.table = resamp[[3]][[2]],
                             obs.table = resamp[[3]][[3]], cutpoints = c(0, 0.25, 0.5, 1, 
                                                                         1.5, 2)))
              try(modelGrBun <- ds(data = resamp[[3]][[4]], transect = "line",
                             key = "un", region.table = resamp[[3]][[1]],
                             sample.table = resamp[[3]][[2]],
                             obs.table = resamp[[3]][[3]], cutpoints = c(0, 0.25, 0.5, 1, 
                                                                         1.5, 2)))
               # Compare models and Pass selected model into output
              myLog2 <- c(exists("modelGrBhn"), exists("modelGrBhr"), exists("modelGrBun"))
              if(sum(myLog2)==0){myModelList[[ii]][[3]] <- "No GrB models fit"}
                
              myLogTru2 <- c("modelGrBhn", "modelGrBhr", "modelGrBun")[which(myLog2)]
              
              if(length(myLogTru2) == 1){myModelList[[ii]][[3]] <- get(myLogTru2)}
          
              if(length(myLogTru2) == 3){myCompar2 <- summarize_ds_models(modelGrBhn, modelGrBhr, modelGrBun)
                                          myModelList[[ii]][[3]] <- get(substr(myCompar2[1,1], 9, 18))}
          
              if(length(myLogTru2) == 2){myCompar2 <- summarize_ds_models(get(myLogTru2[1]), get(myLogTru2[2]))
                                          myModelList[[ii]][[3]] <- get(eval(parse(text = (substr(myCompar2[1,1], 13, 24)))))}
              
          
          } #end of stackhousia 'if' statement
      
                  myModelList[[ii]]$idx <- ii
                  print(paste0("round ", ii)) 
                  rm(modelLTShn, modelLTShr, modelLTSun, modelOpthn, modelOpthr, modelOptun)
                if(target == "Stackhousia"){rm(modelGrBhn, modelGrBhr, modelGrBun)}
    }
print(paste0("finished at ", strftime(Sys.time(), format="%H:%M:%S")))
  return(list(myDataList, myModelList))
  
}

# stTime <- Sys.time()
# testing <- resampleMany_Replacement(n, details, data, effort, times, target, 
#                          Cm, Cw, w)
# totTime <- as.numeric(Sys.time()-stTime)


# Binning without bands ----

#versions of functions needed to run comparison of binning method, where marking out bands does not add to the time cost.  Comparing Opt with binning without bands for Stackhousia.

resampleCreateTables_UnbandedReplacement <- function(details, data, effort, times, 
                                 target, w, Cm, Cw, ii){

  #take LTS resample data and extend transect length to optimal length,
  #add additional data and transect details to LTS survey data
  
  tOpt <- times$mean_unit_time[times$target == target & times$method == "Opt"]
  KOpt <- round(effort/tOpt)
  remainderOpt <- KOpt-(effort/tOpt)
  signOpt <- sign(remainderOpt)
  remainderOpt <- abs(remainderOpt)
  sampOpt <- sample(details$transectID, KOpt, replace = TRUE)
   
   sampOpt <- as.data.frame(sampOpt)
   mydetailsOpt <- merge(sampOpt, details, by.x = "sampOpt", by.y = "transectID")
   colnames(mydetailsOpt)[colnames(mydetailsOpt)=="sampOpt"] <- "transectID"
   mydetailsOpt$uniqueIDtrans <- 1:nrow(mydetailsOpt)
   
   mydataOpt <- merge(mydetailsOpt, data, by.x = "transectID", by.y = "transectID")
   mydataOpt$uniqueIDobs <- 1:nrow(mydataOpt)
  
  #add corrections for partial transect for Opt sample
    if(signOpt == 1){
      #the remainder is positive - remove some obs and shorten transect
      #rm extra bit of last transect on length
      transEditOpt <- sample(mydetailsOpt$uniqueIDtrans, 1)
      mydetailsOpt$length_m[mydetailsOpt$uniqueIDtrans==transEditOpt] <- mydetailsOpt$length_m[mydetailsOpt$uniqueIDtrans==transEditOpt]*(1-remainderOpt) 
  
      #select at random obs to remove
      transEditIdOpt <- mydetailsOpt$transectID[mydetailsOpt$uniqueIDtrans==transEditOpt]
      chooseFromThese <- mydataOpt$uniqueIDobs[mydataOpt$transectID==transEditIdOpt]
      if(length(chooseFromThese)>0){
        rejectThese <- sample(chooseFromThese, size = round(length(chooseFromThese)*remainderOpt))
        mydataOpt <- mydataOpt[!(mydataOpt$uniqueIDobs %in% rejectThese),]
      }
   
    } else {
      #select at random another transect
      newTransectOpt <- sample(details$transectID, 1, replace = TRUE) 
      newDetailsOpt <- details[details$transectID==newTransectOpt, ]
   
      #adjust length
      newDetailsOpt$length_m <- abs(remainderOpt)*newDetailsOpt$length_m
      newDetailsOpt$uniqueIDtrans <- max(mydetailsOpt$uniqueIDtrans)+1
   
      #add to details
      mydetailsOpt <- rbind(mydetailsOpt, newDetailsOpt)
   
      #select new obs
      newObsOpt <- data[data$transectID==newTransectOpt,]
      
      if(nrow(newObsOpt)!=0) {
        addTheseOpt <- sample(newObsOpt$obsID, size = round(abs(remainderOpt)*length(newObsOpt$obsID)))
        newObsOpt <- newObsOpt[newObsOpt$obsID %in% addTheseOpt, ]
        newObsOpt$uniqueIDobs <- (nrow(mydataOpt)+1):(nrow(mydataOpt)+nrow(newObsOpt))
   
        #add to data
        newDataOpt <- merge(newDetailsOpt, newObsOpt, by = "transectID")  
        mydataOpt <- rbind(mydataOpt, newDataOpt)
      }
    }  
   
  #Add a clause to continue without making tables if there isn't any data?  
  if(nrow(mydataOpt)==0){
    myDistTablesOpt <- distTables_Replacement(distDetails = mydetailsOpt,
                                distData = mydataOpt[!is.na(mydataOpt$perpDist_m), ],
                                w = w)
  myDistTablesOpt[[length(myDistTablesOpt)+1]] <- 0
  } else {
    #remove distance measurements according to alpha*
  
  aOpt <- sqrt(Cw/(2*Cm))
  my_idxOpt <- seq(1, nrow(mydataOpt), round(1/aOpt, 1))
  mydataOpt$perpDist_m[-my_idxOpt] <- NA
  
  #create distance tables for optimised survey
  
  myDistTablesOpt <- distTables_Replacement(distDetails = mydetailsOpt,
                                distData = mydataOpt[!is.na(mydataOpt$perpDist_m), ],
                                w = w)
  myDistTablesOpt[[length(myDistTablesOpt)+1]] <- mydataOpt$obsID
  }
  
  #use Opt resample for grouped survey
      tGrN <- times$mean_unit_time[times$method == "GroupedNoBands"]
      KGrN <- round(effort/tGrN)
      remainGrN <- KGrN - (effort/tGrN)
      signGrN <- sign(remainGrN)
      remainGrN <- abs(remainGrN)
      
      diffGrN <- KGrN - KOpt
      sampGrN <- sample(details$transectID, diffGrN, replace = TRUE)
      sampGrN <- c(sampOpt$sampOpt, sampGrN)
      sampGrN <- as.data.frame(sampGrN)
      mydetailsGrN <- merge(sampGrN, details, by.x = "sampGrN", by.y = "transectID")
      colnames(mydetailsGrN)[colnames(mydetailsGrN)=="sampGrN"] <- "transectID"
      mydetailsGrN$uniqueIDtrans <- 1:nrow(mydetailsGrN)
      mydataGrN <- merge(mydetailsGrN, data, by.x = "transectID", by.y = "transectID")
      mydataGrN$uniqueIDobs <- 1:nrow(mydataGrN)
      
      #add corrections for partial transect for grouped sample
      if(signGrN == 1){
      #the remainder is positive - remove some obs and shorten transect
      #rm extra bit of transect length
      transEditGrN <- sample(mydetailsGrN$uniqueIDtrans, 1)
      mydetailsGrN$length_m[mydetailsGrN$uniqueIDtrans==transEditGrN] <- mydetailsGrN$length_m[mydetailsGrN$uniqueIDtrans==transEditGrN]*(1-remainGrN) 
  
      #select at random obs to remove
      transEditIdGrN <- mydetailsGrN$transectID[mydetailsGrN$uniqueIDtrans==transEditGrN]
      chooseFromThese <- mydataGrN$uniqueIDobs[mydataGrN$transectID==transEditIdGrN]
      if(length(chooseFromThese)>0){
        rejectThese <- sample(chooseFromThese, size = round(length(chooseFromThese)*remainGrN))
        mydataGrN <- mydataGrN[!(mydataGrN$uniqueIDobs %in% rejectThese),]
      }
   
    } else {
      #select at random another transect
      newTransectGrN <- sample(details$transectID, 1, replace = TRUE) 
      newDetailsGrN <- details[details$transectID==newTransectGrN, ]
   
      #adjust length
      newDetailsGrN$length_m <- abs(remainGrN)*newDetailsGrN$length_m
      newDetailsGrN$uniqueIDtrans <- max(mydetailsGrN$uniqueIDtrans)+1
   
      #add to details
      mydetailsGrN <- rbind(mydetailsGrN, newDetailsGrN)
   
      #select new obs
      newObsGrN <- data[data$transectID==newTransectGrN,]
      
      if(nrow(newObsGrN)!=0) {
        addTheseGrN <- sample(newObsGrN$obsID, size = round(abs(remainGrN)*length(newObsGrN$obsID)))
        newObsGrN <- newObsGrN[newObsGrN$obsID %in% addTheseGrN, ]
        newObsGrN$uniqueIDobs <- (nrow(mydataGrN)+1):(nrow(mydataGrN)+nrow(newObsGrN))
   
        #add to data
        newDataGrN <- merge(newDetailsGrN, newObsGrN, by = "transectID")  
        mydataGrN <- rbind(mydataGrN, newDataGrN)
      }
    } 

        myDistTablesGrN <- distTablesGroupedRepl(distDetails = mydetailsGrN,
                                        distData = mydataGrN[!is.na(mydataGrN$perpDist_m), ],
                                        w = w)
 
        myStuff <- list(myDistTablesOpt, myDistTablesGrN)
        folder <- "resampleSmono/"
     
      write.table(mydataOpt, paste0("dataSim/", folder, "resampleOpt/resampData", ii, "UnBandedRepl.txt"))
      write.table(mydataGrN, paste0("dataSim/", folder, "resampleGrN/resampData", ii, "UnBandedRepl.txt"))
      
      write.table(mydetailsOpt, paste0("dataSim/", folder, "resampleOpt/resampDetails", ii, "UnBandedRepl.txt"))
      write.table(mydetailsGrN, paste0("dataSim/", folder, "resampleGrN/resampDetails", ii, "UnBandedRepl.txt"))
        
  
  return(myStuff)
}

## Loop resamples ----

resampleManyWithUnbandedReplacement <- function(n, details, data, effort, times, target, 
                         Cm, Cw, w){
  
  myDataList <- vector(mode="list", n)
  myModelList <- vector(mode = "list", n)
  
  for (i in 1:n){ 
    myDataList[[i]] <- vector(mode = "list", 2)
    myModelList[[i]] <- vector(mode = "list", 2)
  }
  
  for (ii in 1:n) {
    resamp <- resampleCreateTables_UnbandedReplacement(details, data, effort, times,  
                                   target, w, Cm, Cw, ii)
    resamp$idx <- ii
    myDataList[[ii]] <- resamp
    
    # Added 'if' for occasions when opt resample has no obs

      try(modelOpthn <- ds(data = resamp[[1]][[4]], transect = "line",
                           formula = ~1, key = "hn", region.table = resamp[[1]][[1]],
                           sample.table = resamp[[1]][[2]],
                           obs.table = resamp[[1]][[3]]))
      try(modelOpthr <- ds(data = resamp[[1]][[4]], transect = "line",
                           formula = ~1, key = "hr", region.table = resamp[[1]][[1]],
                           sample.table = resamp[[1]][[2]],
                           obs.table = resamp[[1]][[3]]))
      try(modelOptun <- ds(data = resamp[[1]][[4]], transect = "line",
                           formula = ~1, key = "un", region.table = resamp[[1]][[1]],
                           sample.table = resamp[[1]][[2]],
                           obs.table = resamp[[1]][[3]]))
    
      # Compare models and Pass selected model into output
      myLog1 <- c(exists("modelOpthn"), exists("modelOpthr"), exists("modelOptun"))
      if(sum(myLog1)==0){myModelList[[ii]][[2]] <- "No Opt models fit"}
        
      myLogTru1 <- c("modelOpthn", "modelOpthr", "modelOptun")[which(myLog1)]
      
      if(length(myLogTru1) == 1){myModelList[[ii]][[1]] <- get(myLogTru1)}
  
      if(length(myLogTru1) == 3){myCompar1 <- summarize_ds_models(modelOpthn, modelOpthr, modelOptun)
                                  myModelList[[ii]][[1]] <- get(substr(myCompar1[1,1], 9, 18))}
  
      if(length(myLogTru1) == 2){myCompar1 <- summarize_ds_models(get(myLogTru1[1]), get(myLogTru1[2]))
                                  myModelList[[ii]][[1]] <- get(eval(parse(text = (substr(myCompar1[1,1], 13, 24)))))}
    
      try(modelGrNhn <- ds(data = resamp[[2]][[4]], transect = "line",
                       key = "hn", region.table = resamp[[2]][[1]],
                       sample.table = resamp[[2]][[2]],
                       obs.table = resamp[[2]][[3]], cutpoints = c(0, 0.25, 0.5, 1, 
                                                                   1.5, 2)))
      try(modelGrNhr <- ds(data = resamp[[2]][[4]], transect = "line",
                       key = "hr", region.table = resamp[[2]][[1]],
                       sample.table = resamp[[2]][[2]],
                       obs.table = resamp[[2]][[3]], cutpoints = c(0, 0.25, 0.5, 1, 
                                                                   1.5, 2)))
      try(modelGrNun <- ds(data = resamp[[2]][[4]], transect = "line",
                       key = "un", region.table = resamp[[2]][[1]],
                       sample.table = resamp[[2]][[2]],
                       obs.table = resamp[[2]][[3]], cutpoints = c(0, 0.25, 0.5, 1, 
                                                                   1.5, 2)))
       # Compare models and Pass selected model into output
      myLog2 <- c(exists("modelGrNhn"), exists("modelGrNhr"), exists("modelGrNun"))
      if(sum(myLog2)==0){myModelList[[ii]][[3]] <- "No GrN models fit"}
        
      myLogTru2 <- c("modelGrNhn", "modelGrNhr", "modelGrNun")[which(myLog2)]
      
      if(length(myLogTru2) == 1){myModelList[[ii]][[2]] <- get(myLogTru2)}
  
      if(length(myLogTru2) == 3){myCompar2 <- summarize_ds_models(modelGrNhn, modelGrNhr, modelGrNun)
                                  myModelList[[ii]][[2]] <- get(substr(myCompar2[1,1], 9, 18))}
  
      if(length(myLogTru2) == 2){myCompar2 <- summarize_ds_models(get(myLogTru2[1]), get(myLogTru2[2]))
                                  myModelList[[ii]][[2]] <- get(eval(parse(text = (substr(myCompar2[1,1], 13, 24)))))}

      myModelList[[ii]]$idx <- ii

      print(paste0("round ", ii))
      rm(modelOpthn, modelOpthr, modelOptun, modelGrNhn, modelGrNhr, modelGrNun)
      
              }
    
  print(paste0("finished at ", strftime(Sys.time(), format="%H:%M:%S")))
  return(list(myDataList, myModelList))
  
}

# testing2 <- resampleManyWithUnbandedReplacement(n, details, data, effort, times, target, 
#                          Cm, Cw, w)
