##Distance sampling optimisation functions

#function to calculate the optimal proportion to measure 
#when surveying 2 species, species A and species B
 #arguments are:
# Cl = cost per unit length of transect (walking, not measuring)
# Cm = cost to measure one detected target's distance
# sigmA = sigma for species A (in same units as w, strip half width)
# sigmB = sigma for species B
# Da = density of species A
# Db = density of species B

optAoptB <- function(Cl, Cm, sigmA, sigmB, Da, Db){
 
  muA <- sqrt(pi*(sigmA^2)/2)
  muB <- sqrt(pi*(sigmB^2)/2)

  alphA <- sqrt(Cl/Cm*((2*muB)/(2*muA)/((2*muA*Db)+(2*muB*Da))))/sqrt(2)
  alphB <- sqrt(Cl/Cm*((2*muA)/(2*muB)/((2*muA*Db)+(2*muB*Da))))/sqrt(2)
  myOptProps <- data.frame(alphA, alphB)
  return(myOptProps)
}

#myTest <- optAoptB(Cl = 0.05, Cm = 0.5, sigmA = 10, sigmB = 8, 
#Da = 0.5, Db = 0.1)
altoptAoptB <- function(costMult, muA, muB, Da, Db){
  
  alphA <- sqrt(costMult*((muB)/(muA)/((2*muA*Db)+(2*muB*Da))))/sqrt(2)
  alphB <- sqrt(costMult*((muA)/(muB)/((2*muA*Db)+(2*muB*Da))))/sqrt(2)
  myOptProps <- data.frame(alphA, alphB)
  return(myOptProps)
}

#version 1 of this function:
# altoptAoptB <- function(costMult, muA, muB, Da, Db){
#   
#   alphA <- sqrt(costMult*((muB)/(muA)/((muA*Db)+(muB*Da))))/2
#   alphB <- sqrt(costMult*((muA)/(muB)/((muA*Db)+(muB*Da))))/2
#   myOptProps <- data.frame(alphA, alphB)
#   return(myOptProps)
# }


######################################################################

## Distance optimisation simulations: Optimisation functions        ##
##                        for calculations                          ##

######################################################################

#dsOptCalcE takes E, sigm, Cm, Cw and Budget as arguments and returns 
# a dataframe containing all the survey characteristics and optimisation calculations
# E = encounter rate (n detections per meters)
# sigm = sigma, scale parameter of the detection function
# Cm = cost of measuring one detected target (in minutes)
# Cw = cost of walking to next detection (in minutes)
# Budget = survey time (total walking plus total measuring) in minutes
dsOptACalcE <- function(E, sigm, Cm, Cw, Budget){
  w <- 2*sigm
  mu <- sqrt(pi*(sigm^2)/2)
  D <- E/(2*mu)
  C1 <- E*Cw
  Opt_Alpha <-  ifelse(sqrt(C1/(2*E*Cm))< 1, sqrt(C1/(2*E*Cm)), 1)
  Opt_L <-  Budget/(C1 + E*Opt_Alpha*Cm) #transect length in m
  optArea <- Opt_L*2*w # in sq m
  fullLength <- Budget/(C1 + (E*Cm)) # in m
  fullArea  <-  fullLength*2*w  #in sq m
  Rc <- Cm/Cw  
  
  #how many encounters expected 
  expNfull <- (mu/w)*(D*fullArea) 
  sampSizeFull <- if(expNfull>80) TRUE
  else if(expNfull>60) "BDRLN"
  else FALSE
  expNopt <- (mu/w)*(D*optArea)
  measuredNopt <- expNopt*Opt_Alpha
  sampSizeOpt <- if(expNopt*Opt_Alpha>80) TRUE
  else if(expNopt*Opt_Alpha>60) "BDRLN"
  else FALSE
  
  optCalcE <- data.frame(w = w, mu = mu, D = D, C1 = C1, Rc = Rc, 
                         Opt_Alpha = Opt_Alpha, Opt_L = Opt_L, optArea = optArea, 
                         fullLength = fullLength, fullArea = fullArea, 
                         expNfull = expNfull, sampSizeFull = sampSizeFull, 
                         expNopt = expNopt, measuredNopt = measuredNopt,
                         sampSizeOpt = sampSizeOpt)
}

# dsOptCalcD is the same as dsOptCalcE except it takes D as an argument instead of E
# D = density, targets per m2
dsOptACalcD <- function(D, sigm, Cm, Cw, Budget){
  w <- 2*sigm
  mu <- sqrt(pi*(sigm^2)/2)
  E <- 2*mu*D
  C1 <- E*Cw
  Opt_Alpha <-  ifelse(sqrt(C1/(2*E*Cm))< 1, sqrt(C1/(2*E*Cm)), 1)
  Opt_L <-  Budget/(C1 + E*Opt_Alpha*Cm) #transect length in m
  optArea <- Opt_L*2*w # in sq m
  fullLength <- Budget/(C1 + (E*Cm)) # in m
  fullArea  <-  fullLength*2*w  #in sq m
  
  Ct <- Cm*E
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

CDStoOptEffort <- function(tlengthCDS, Cm, Cw, E){
  B <- tlengthCDS*E*(Cw+Cm)
  OptAlph <- sqrt(Cw/(2*Cm))
  OptLength <-  B/(E*(Cw+OptAlph*Cm))
  return(OptLength)
}

#function to ensure min n measured detections for opt sample
#need to ensure that nOpt is >80, starting with density, ending up with length of transect needed

minLopt <- function(dens, sigm, Opt_Alpha, minN = 80){
  mu<-sqrt(pi*(sigm^2)/2)
  E <- 2*mu*dens
  minL <- (minN/Opt_Alpha)/E  #length required for minimum detections
}

predBen <- function(Rc){
  3/2*((1+Rc)/(1+sqrt(Rc/2))^2)
}