######################################################################

## Distance optimisation simulations: Optimisation functions        ##
##                        for calculations                          ##

######################################################################

#dsOptCalcR takes R, sigm, Cm, Cw and Budget as arguments and returns 
# a dataframe containing all the survey characteristics and optimisation calculations
# R = encounter rate (n detections per meters)
# sigm = sigma, scale parameter of the detection function
# Cm = cost of measuring one detected target (in minutes)
# Cw = cost of walking to next detection (in minutes)
# Budget = survey time (total walking plus total measuring) in minutes
dsOptCalcR <- function(R, sigm, Cm, Cw, Budget){
  w <- 2*sigm
  mu <- sqrt(pi*(sigm^2)/2)
  D <- R/(2*mu)
  C1 <- R*Cw
  Opt_Alpha <-  ifelse(sqrt(C1/(2*R*Cm))< 1, sqrt(C1/(2*R*Cm)), 1)
  Opt_L <-  Budget/(C1 + R*Opt_Alpha*Cm) #transect length in m
  optArea <- Opt_L*2*w # in sq m
  fullLength <- Budget/(C1 + (R*Cm)) # in m
  fullArea  <-  fullLength*2*w  #in sq m
  Rc <- Cm/Cw  
  
  #how many encounters expected 
  expNfull <- (mu/w)*(D*fullArea) 
  sampSizeFull <- if(expNfull>80) TRUE
  else if(expNfull>60) "POSS"
  else FALSE
  expNopt <- (mu/w)*(D*optArea)
  measuredNopt <- expNopt*Opt_Alpha
  sampSizeOpt <- if(expNopt*Opt_Alpha>80) TRUE
  else if(expNopt*Opt_Alpha>60) "POSS"
  else FALSE
  
  optCalcR <- data.frame(w = w, mu = mu, D = D, C1 = C1, Rc = Rc, 
                    Opt_Alpha = Opt_Alpha, Opt_L = Opt_L, optArea = optArea, 
                    fullLength = fullLength, fullArea = fullArea, 
                    expNfull = expNfull, sampSizeFull = sampSizeFull, 
                    expNopt = expNopt, measuredNopt = measuredNopt,
                    sampSizeOpt = sampSizeOpt)
}

# dsOptCalcD is the same as dsOptCalcR except it takes D as an argument instead of R
# D = density, targets per m2
dsOptCalcD <- function(D, sigm, Cm, Cw, Budget){
  w <- 2*sigm
  mu <- sqrt(pi*(sigm^2)/2)
  R <- 2*mu*D
  C1 <- R*Cw
  Opt_Alpha <-  ifelse(sqrt(C1/(2*R*Cm))< 1, sqrt(C1/(2*R*Cm)), 1)
  Opt_L <-  Budget/(C1 + R*Opt_Alpha*Cm) #transect length in m
  optArea <- Opt_L*2*w # in sq m
  fullLength <- Budget/(C1 + (R*Cm)) # in m
  fullArea  <-  fullLength*2*w  #in sq m
  
  Ct <- Cm*R
  Rc <- Cm/Cw  
  
  #how many encounters expected 
  expNfull <- (mu/w)*(D*fullArea) 
  sampSizeFull <- if(expNfull>80) TRUE
  else if(expNfull>60) "POSS"
  else FALSE
  expNopt <- (mu/w)*(D*optArea)
  measuredNopt <- expNopt*Opt_Alpha
  sampSizeOpt <- if(expNopt*Opt_Alpha>80) TRUE
  else if(expNopt*Opt_Alpha>60) "POSS"
  else FALSE
  
  optCalcD <- data.frame(w = w, mu = mu, D = D, C1 = C1, Rc = Rc, 
                    Opt_Alpha = Opt_Alpha, Opt_L = Opt_L, optArea = optArea, 
                    fullLength = fullLength, fullArea = fullArea, 
                    expNfull = expNfull, sampSizeFull = sampSizeFull, 
                    expNopt = expNopt, measuredNopt = measuredNopt,
                    sampSizeOpt = sampSizeOpt)
}
#predBenOpt takes Rc as an arugument and calculates the 'adjusted' predicted 
#benefit of optimisation

predBenOptAdj <- function(Rc){
  predben <- 3/2*((1+Rc)/(1+sqrt(Rc/2))^2) #benefit as ratio of variances
  adjPredBen <- 0.79*predben + 0.19/predben 
  return(adjPredBen)
}

#Opt alpha from Cm and Cw

OptA <- function(Cm, Cw){
  result <- sqrt(Cw/(2*Cm))
}

#use CDS effort to convert through budget to optimised sampling effort

CDStoOptEffort <- function(tlengthCDS, Cm, Cw, R){
  B <- tlengthCDS*R*(Cw+Cm)
  OptAlph <- sqrt(Cw/(2*Cm))
  OptLength <-  B/(R*(Cw+OptAlph*Cm))
  return(OptLength)
}

#function to ensure min n measured detections for opt sample
#need to ensure that nOpt is >80, starting with density, ending up with length of transect needed

minLopt <- function(dens, sigm, Opt_Alpha, minN = 80){
  mu<-sqrt(pi*(sigm^2)/2)
  R <- 2*mu*dens
  minL <- (minN/Opt_Alpha)/R  #length required for minimum detections
}

predBen <- function(Rc){
  3/2*((1+Rc)/(1+sqrt(Rc/2))^2)
}
