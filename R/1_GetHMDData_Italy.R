####### Program for getting most recent data from HMD
############# Written by JMA, thanks to Tim Riffe
############# 20/07/2019
library(HMDHFDplus)
library(data.table)

# Set working directory

# get all countries in HMD
XYZ <- getHMDcountries()
# set your username for HMD
us <- "jmaburto@colmex.mx"
# set your password
pw <- "kolmogorov"

# get all the lifetables available from HMD
HMDL <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  Males        <- readHMDweb(x,"mltper_1x1",username=us,password=pw)
  Females      <- readHMDweb(x,"fltper_1x1",username=us,password=pw)
  Males$Sex    <- "m"
  Females$Sex  <- "f"
  CTRY         <- rbind(Females, Males)
  CTRY$PopName <- x
  CTRY    
}, us = us, pw = pw))

# convert to data.table
HMDL <- data.table(HMDL)

# save the data
save(HMDL,file="R/Data/HMD_Data.RData")


# get 1-1 lifetables for Italian females
LT.ITA.1 <- do.call(rbind,lapply(XYZ[26], function(x, us, pw){
  cat(x,"\n")
  Females         <- readHMDweb(x,"fltper_1x1",username=us,password=pw)
  Females$Sex     <- "f"
  Females$PopName <- x
  Females 
}, us = us, pw = pw))
LT.ITA.1  <-  data.table(LT.ITA.1)

# get 1-10 lifetables for Swedish females
LT.ITA.10 <- do.call(rbind,lapply(XYZ[26], function(x, us, pw){
  cat(x,"\n")
  Females          <- readHMDweb(x,"fltper_1x10",username=us,password=pw)
  Females$Sex      <- "f"
  Females$PopName  <-  x
  Females
}, us = us, pw = pw))
LT.ITA.10  <-  data.table(LT.ITA.10)

LT.ITA.1[,6:9]  <- LT.ITA.1[,6:9]/100000
LT.ITA.10[,6:9] <- LT.ITA.10[,6:9]/100000

save(LT.ITA.1,LT.ITA.10,file="R/Data/Italy_HMD.RData")


