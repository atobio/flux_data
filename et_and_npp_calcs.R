## script to get transpiration,
## evaporation from ET, gpp and respiration
## from npp with flux data
##
## after passing flux data through 
## intital_raw_flux_calc.R
##
##Ana Tobio

rm(list=ls())

require(xlsx)

#read in data
setwd('/Users/Ana/Documents/siberia_data/dg_fluxes/')
fluxdata<- read.xlsx("final_fluxes.xlsx", sheetIndex=1, startRow=1, header=T)
#spectra <- read.xlsx("spectra_final.xlsx", sheetIndex =1, startRow=1, header=T)

#create new data frame
newdata <- data.frame(plot = character(36),
                      et = numeric(36),
                      transpiration = numeric(36),
                      evaporation = numeric(36),
                      npp = numeric(36),
                      gpp = numeric(36),
                      respiration = numeric(36),
                      par=numeric(36),
                      temp.c = numeric(36),
                      rh = numeric(36),
                      stringsAsFactors = FALSE)

#for loop to calcuate respir, photo, evap, trans 
#insert values into dataframe  
j<-1
for(i in 1:nrow(fluxdata))
{
  if(substr(fluxdata$plot[i], 3,3) == "D")
  {
    #transpiration = light h2o flux - dark h2o flux 
    newdata$transpiration[j] <- (fluxdata$umol.h2o.m2.sec.1[i-1]) - (fluxdata$umol.h2o.m2.sec.1[i])
    #evaporation = dark h2o flux
    newdata$evaporation[j] <- fluxdata$umol.h2o.m2.sec.1[i]
    #et = transpiration + evaporation (light h2o flux)
    newdata$et[j] <- fluxdata$umol.h2o.m2.sec.1[i-1]
    
    #gpp = dark co2 flux - light co2 flux
    newdata$gpp[j] <- (fluxdata$umol.co2.m2.sec.1[i]) - (fluxdata$umol.co2.m2.sec.1[i-1])
    #respiration = dark co2 flux
    newdata$respiration[j] <- fluxdata$umol.co2.m2.sec.1[i]
    #npp = gpp - respiration (light co2 flux)
    newdata$npp[j] <- fluxdata$umol.co2.m2.sec.1[i-1]
    
    newdata$plot[j] <- as.character(fluxdata$plot[i])
    newdata$par[j] <- fluxdata$par[i-1]
    newdata$rh[j] <- mean(c(fluxdata$rh[i], fluxdata$rh[i-1]))
    newdata$temp.c[j] <- mean(c(fluxdata$temp.c[i], fluxdata$temp.c[i-1]))
    
    j <- j +1
  }
 }


write.csv(newdata,file=paste("et_npp","csv",sep="."),row.names=F)
