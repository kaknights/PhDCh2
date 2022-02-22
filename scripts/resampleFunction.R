# function to resample field data from LTS transects

# details = df of transect level info
# data = df of detections and distances
# effort = number of minutes available for sampling
# times = df of mean unit times
# w = max distance of observations (2 for S mono, 10 for S quad)

# function gives distance tables ready for analysis with ds function
# dist tables are a list (region, sample, observation, data)
# details table for the resample is added to the list (item 5)


# resample using LTS method - n transects calculated using distance theory and
# pilot data for costs of measuring and walking per detection

resampleLTStheory <- function(details, data, effort, times, 
                          method, target, E, Cm, Cw, l, w){
  # exclude <- numeric(0)
  # a <- 0
  # repeat can't remember the control flow for this
  # samp <- sample(details$transectID[!(details$transectID %in% exclude)], 1)
  # exclude[a] <- samp
  # a <- a + 1
  ltot <- effort/(E*(Cw+Cm))
  K <- round(effort/ltot)
  samp <- sample(details$transectID, K)
  mydetails <- details[details$transectID %in% samp, ]
  mydata <- data[data$transectID %in% mydetails$transectID, ]
  myDistTables <- distTables(distDetails = mydetails,
                             distData = mydata,
                             w = w)
  myDistTables[[5]] <- mydetails
  myDistTables[[6]] <- mydata
  return(myDistTables)
}

# resample using LTS method - n transects calculated using full survey mean unit time

resampleLTSfield <- function(details, data, effort, times, 
                                method, target, w){
  t <- times$mean_unit_time[times$target == target & times$method == method]
  K <- round(effort/t)
  samp <- sample(details$transectID, K)
  mydetails <- details[details$transectID %in% samp, ]
  mydata <- data[data$transectID %in% mydetails$transectID, ]
  myDistTables <- distTables(distDetails = mydetails,
                             distData = mydata,
                             w = w)
  myDistTables[[5]] <- mydetails
  myDistTables[[6]] <- mydata
  return(myDistTables)
}


# resample LTS data using Opt method - n transects calculated using distance theory and
# pilot data for costs of measuring and walking per detection

resampleOptTheory <- function(details, data, effort, times, 
                          method, target, E, Cm, Cw, l, w){
 # t <- times$mean_unit_time[times$target == target & times$method == method]
  aOpt <- sqrt(Cw/(2*Cm))
  ltot <- effort/(E*(Cw+aOpt*Cm))
  # this isn't going to work...
  # or use mean unit time
  K <- round(ltot/l)
  samp <- sample(details$transectID, K)
  mydetails <- details[details$transectID %in% samp, ]
  mydata <- data[data$transectID %in% mydetails$transectID, ]
  my_idx <- seq(1, nrow(mydata), round(1/aOpt, 1))
  mydata$perpDist_m[-my_idx] <- NA
  myDistTables <- distTables(distDetails = mydetails,
                             distData = mydata[!is.na(mydata$perpDist_m), ],
                             w = w)
  myDistTables[[5]] <- mydetails
  myDistTables[[6]] <- mydata
  return(myDistTables)
}

resampleOptField <- function(details, data, effort, times, 
                                method, target, Cm, Cw, w){
  aOpt <- sqrt(Cw/(2*Cm))
  t <- times$mean_unit_time[times$target == target & times$method == method]
  K <- round(effort/t)
  samp <- sample(details$transectID, K)
  mydetails <- details[details$transectID %in% samp, ]
  mydata <- data[data$transectID %in% mydetails$transectID, ]
  my_idx <- seq(1, nrow(mydata), round(1/aOpt, 1))
  mydata$perpDist_m[-my_idx] <- NA
  myDistTables <- distTables(distDetails = mydetails,
                             distData = mydata[!is.na(mydata$perpDist_m), ],
                             w = w)
  myDistTables[[5]] <- mydetails
  myDistTables[[6]] <- mydata
  return(myDistTables)
}

resampleGrouped <- function(details, data, effort, times, 
                            method, target, w){
  t <- times$mean_unit_time[times$target == target & times$method == method]
  K <- round(effort/t)
  samp <- sample(details$transectID, K)
  mydetails <- details[details$transectID %in% samp, ]
  mydata <- data[data$transectID %in% mydetails$transectID, ]
  myDistTables <- distTablesGrouped(distDetails = mydetails,
                             distData = mydata[!is.na(mydata$perpDist_m), ],
                             w = w)
  myDistTables[[5]] <- mydetails
  myDistTables[[6]] <- mydata
  return(myDistTables)
}

resamplePlots <- function(x){
  
}
