####################################
#                                  #
#    Resampling LTS survey data    #
#           all methods            #
#                                  #
####################################

#function to make Distance tables (using identically formatted distance survey data):
distTables <- function(distDetails, distData, w){
  region.table <- data.frame("Region.Label" = "EvansSt", "Area" = 
                               sum(distDetails$length_m)* (2*w))
  sample.table <- data.frame("Sample.Label" = distDetails$transectID,
                             "Region.Label" = "EvansSt",
                             "Effort" = distDetails$length_m)
  obs.table <- data.frame("object" = distData$obsID[!is.na(distData$perpDist_m)],
                          "Region.Label" = "EvansSt",
                          "Sample.Label" = distData$transectID[!is.na(distData$perpDist_m)])
  data <- data.frame("object" = distData$obsID[!is.na(distData$perpDist_m)],
                     "distance" = distData$perpDist_m[!is.na(distData$perpDist_m)])
  tablesList <- list(region.table, sample.table, obs.table, data)
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
  return(tablesList)
}

# resampleCreateTables takes LTS data and survey variables (effort, unit times,
# costs of walking/measuring, w, distance bands for grouped survey) and gives
# tables ready for distance analysis for LTS, Opt and Grouped methods, where the 
# same transects selected for the LTS survey are used as the basis of the 
# survey data for the other methods

resampleCreateTables <- function(details, data, effort, times, 
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


###############################################################################

#resampleMany uses the resampleCreateTables function repeated many times
#and returns a list of lists of results of distance analysis on each data set

resampleMany <- function(n, details, data, effort, times, target, 
                         Cm, Cw, w){
  
  myDataList <- vector(mode="list", n)
  myModelList <- vector(mode = "list", n)
  
  for (i in 1:n){ #Tidy this up so there aren't empty elements in Senecio lists
    myDataList[[i]] <- vector(mode = "list", 4)
    myModelList[[i]] <- vector(mode = "list", 4)
  }
  
  for (ii in 1:n) {
    resamp <- resampleCreateTables(details, data, effort, times, 
                                   target, w, Cm, Cw)
    resamp$idx <- ii
    myDataList[[ii]] <- resamp
    
    modelLTS <- ds(data = resamp[[1]][[4]], transect = "line",
                formula = ~1, region.table = resamp[[1]][[1]],
                sample.table = resamp[[1]][[2]],
                obs.table = resamp[[1]][[3]])
    modelOpt <- ds(data = resamp[[2]][[4]], transect = "line",
                   formula = ~1, region.table = resamp[[2]][[1]],
                   sample.table = resamp[[2]][[2]],
                   obs.table = resamp[[2]][[3]])
    if(target == "Senecio"){
      myModelList[[ii]][[1]] <- modelLTS
      myModelList[[ii]][[2]] <- modelOpt
      myModelList[[ii]]$idx <- ii
    } else {
      if(class(resamp[[3]][1]) == "list"){
        modelGrB <- ds(data = resamp[[3]][[4]], transect = "line",
                  formula = ~1, region.table = resamp[[3]][[1]],
                  sample.table = resamp[[3]][[2]],
                  obs.table = resamp[[3]][[3]], cutpoints = c(0, 0.25, 0.5, 1, 
                                                            1.5, 2))
        modelGrN <- ds(data = resamp[[4]][[4]], transect = "line",
                  formula = ~1, region.table = resamp[[4]][[1]],
                  sample.table = resamp[[4]][[2]],
                  obs.table = resamp[[4]][[3]], cutpoints = c(0, 0.25, 0.5, 1, 
                                                              1.5, 2))
        myModelList[[ii]][[1]] <- modelLTS
        myModelList[[ii]][[2]] <- modelOpt
        myModelList[[ii]][[3]] <- modelGrB
        myModelList[[ii]][[4]] <- modelGrN
        myModelList[[ii]]$idx <- ii
        
        
      } else {
        modelGrN <- ds(data = resamp[[4]][[4]], transect = "line",
                       formula = ~1, region.table = resamp[[4]][[1]],
                       sample.table = resamp[[4]][[2]],
                       obs.table = resamp[[4]][[3]], cutpoints = c(0, 0.25, 0.5, 1, 
                                                                   1.5, 2))
        myModelList[[ii]][[1]] <- modelLTS
        myModelList[[ii]][[2]] <- modelOpt
        myModelList[[ii]][[3]] <- modelGrN
        myModelList[[ii]]$idx <- ii
      }
      
    }

    print(paste0("round ", ii))
  }
  print(paste0("finished at ", strftime(Sys.time(), format="%H:%M:%S")))
  return(list(myDataList, myModelList))
  
  #take results and print them to file every 10 rounds?
}


