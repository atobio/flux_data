#
# script to clena uo 
# raw fluxes and calculate
# co2 and h2o flux
# 



rm(list=ls())


##enter folder you're reading from 
setwd("/Users/Ana/Documents/siberia_data/dg_fluxes//all_fluxes")


##make sure to create "plots' folder in that folder
in.files <- list.files(pattern=".txt", full.names=T)
plot.files <- paste("plots/",list.files(pattern=".txt"),".pdf",sep="")
flux.rates <- as.data.frame(in.files)
flux.rates$CO2 <- 9999
flux.rates$H2O <- 9999
# set the initial sample window for slope calc #
flux.rates$CO2.start <- 1
flux.rates$CO2.finish <- 60
flux.rates$H2O.start <- 1
flux.rates$H2O.finish <- 60
flux.rates$CO2.std <- 0
flux.rates$H2O.std <- 0


##read all the fluxes and calculate
##c02 and h20 flux, std dev
##make plot for each raw flux 
for(i in 1:length(in.files))
{
  dat <- read.table(file=in.files[i],header=T,skip=1)
  rC <- c(flux.rates$CO2.start[i]:flux.rates$CO2.finish[i])
  rH <- c(flux.rates$H2O.start[i]:flux.rates$H2O.finish[i])
  c.reg <- lm(dat$CO2.ppm.[rC]~rC)
  h.reg <- lm(dat$H2O.ppt.[rH]~rH)
  
  #plot
  pdf(file=plot.files[i],5,10)
  par(mfcol=c(2,1),mar=c(4,3,2,2))
  plot(dat$CO2.ppm.,xlab="Time (seconds)",
       ylab= "[CO2] (ppm)",
       main=substr(in.files[i],1,24))
  points(rC,dat$CO2.ppm.[rC],pch=16,col="green")
  lines(rC,predict(c.reg),lwd=2,col="red")
  
  
  plot(dat$H2O.ppt.,xlab="Time (seconds)",
       ylab= "[H2O] (ppt)")
  points(rH,dat$H2O.ppt.[rH],pch=16,col="green")
  lines(rH,predict(h.reg),lwd=2,col="blue")
  dev.off()
  
  
  flux.rates$CO2[i] <- coefficients(c.reg)[2]
  flux.rates$H2O[i] <- coefficients(h.reg)[2]
  
  #std dev
  flux.rates$CO2.std[i] <- coef(summary(c.reg))[, 2]
  flux.rates$H2O.std[i] <- coef(summary(h.reg))[, 2]
  
  flux.rates$date[i] <- as.character(dat$Date.Y.M.D.[1])
  flux.rates$time[i] <- as.character(dat$Time.H.M.S.[1])
}


##write flux to excel file 
write.csv(flux.rates,file=paste("flux.rates",Sys.Date(),"csv",sep="."),row.names=F)











### now go through and check the fluxes to be sure the flux is not messy
### make any necessary changes to time, read the file back in,

flux.rates <- read.csv(file.choose())

for(i in 1:length(flux.rates$in.files))
{
  dat <- read.table(file=flux.rates$in.files[i],header=T,skip=1)
  y <- c(flux.rates$start,flux.rates$finish)
  c.reg <- lm(dat$CO2.ppm.[y]~y)
  h.reg <- lm(dat$H2O.ppt.[y]~y)
  
  flux.rates$CO2[i] <- coefficients(c.reg)[2]
  flux.rates$H2O[i] <- coefficients(h.reg)[2]
  #std dev
  flux.rates$CO2.std[i] <- coef(summary(c.reg))[, 2]
  flux.rates$H2O.std[i] <- coef(summary(h.reg))[, 2]
}

write.csv(flux.rates,file=paste("flux.rates.final",Sys.Date(),"csv",sep="."))
