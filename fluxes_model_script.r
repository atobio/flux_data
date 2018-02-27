#####################################################################
###########Script created by A Tobio on 2/14/18. ####################
###########The script runs a Bayesian model      ####################
###########describing ET across stands in        ####################
###########in response to environmental drivers  ####################
#####################################################################

#load libraries
library(plyr)
library(rjags)
library(coda)
library(mcmcplots)
library(lubridate)
#library(plot3D)
library(rgl)
#library(scatterplot3d)
#set directory
#setwd("/Users/Ana/Documents/siberia_data/dg_fluxes")
#read in data
fluxes <- read.csv("z:\\student_research\\tobio\\all_fluxes_2015_16resave.csv")
#canopy rh and temperature for 2016 data
datRH <- read.csv("z:\\student_research\\tobio\\viperSensor\\decagon\\met\\RH.VP4.csv")
datTC <- read.csv("z:\\student_research\\tobio\\viperSensor\\decagon\\met\\TempC.VP4.csv")
#read in precip data
datAirP <- read.csv("z:\\student_research\\tobio\\viperSensor\\airport\\airport.csv")

#met under story for 2015
datDAV <- read.csv("z:\\student_research\\tobio\\viperSensor\\met\\DAV.csv")
datHDF1 <- read.csv("z:\\student_research\\tobio\\viperSensor\\met\\HDF1.csv")
datLBR <- read.csv("z:\\student_research\\tobio\\viperSensor\\met\\LBR.csv")
datLDF2 <- read.csv("z:\\student_research\\tobio\\viperSensor\\met\\LDF2.csv")
datMDF1 <- read.csv("z:\\student_research\\tobio\\viperSensor\\met\\MDF1.csv")
datMDF2 <- read.csv("z:\\student_research\\tobio\\viperSensor\\met\\MDF2.csv")

#############################################
#####organize fluxes data      ##############
#############################################

#check how many fluxes are in each stand
f.count <- aggregate(fluxes$umol.h2o.m2.sec.1, by=list(fluxes$stand, fluxes$density), FUN="length")
colnames(f.count) <- c("stand","density", "count")
f.count$densityID <- ifelse(f.count$density == "h",1,
                            ifelse(f.count$density == "m",2,
                                   3))
f.count$standID <- seq(1,dim(f.count)[1])
#add stand id to the fluxes data
fluxes2 <- join(fluxes, f.count, by=c("stand","density"), type="left")

#############################################
#####organize met data         ##############
#############################################
#subset and match
datLRHmet <- data.frame(datRH[datRH$site=="ld",1:3], RH=datRH$RH.VP4[datRH$site=="ld"])
datLTCmet <- data.frame(datTC[datTC$site=="ld",1:3], Temp=datTC$TempC.VP4[datTC$site=="ld"])

datHRHmet <- data.frame(datRH[datRH$site=="hd",1:3], RH=datRH$RH.VP4[datRH$site=="hd"])
datHTCmet <- data.frame(datTC[datTC$site=="hd",1:3], Temp=datTC$TempC.VP4[datTC$site=="hd"])

datLmet <- join(datLRHmet, datLTCmet, by=c("doy","year","hour"),type="inner")
datHmet <- join(datHRHmet, datHTCmet, by=c("doy","year","hour"),type="inner")

datLmet$standID <- rep(5, dim(datLmet)[1])
datHmet$standID <- rep(1, dim(datHmet)[1])
# organize RH and temp data for 2015
#first convert dates
dateDAV <- as.Date(datDAV[,1], "%m/%d/%Y %H:%M") 
datDAVmet <- data.frame(doy=yday(dateDAV), year=year(dateDAV), hour=datDAV$hour, RH=datDAV$VP.3.Humidity.Temp,Temp=datDAV$VP.3.Humidity.Temp.1, standID=rep(1, dim(datDAV)[1]))
dateHDF1 <- as.Date(datHDF1[,1], "%m/%d/%Y %H:%M") 
datHDF1met <- data.frame(doy=yday(dateHDF1), year=year(dateHDF1), hour=datHDF1$hour, RH=datHDF1$VP.3.Humidity.Temp,Temp=datHDF1$VP.3.Humidity.Temp.1, standID=rep(2, dim(datHDF1)[1]))
##### gap filling end of august with HDF1
dav.gap <- datHDF1met[datHDF1met$doy >= 235 & datHDF1met$year == 2015, 1:5]
dav.gap$standID <- rep(1,dim(dav.gap)[1])
datDAVmet <- rbind(datDAVmet, dav.gap)


dateLDF2 <- as.Date(datLDF2[,1], "%m/%d/%Y %H:%M") 
datLDF2met <- data.frame(doy=yday(dateLDF2), year=year(dateLDF2), hour=datLDF2$hour, RH=datLDF2$VP.3.Humidity.Temp,Temp=datLDF2$VP.3.Humidity.Temp.1, standID=rep(5, dim(datLDF2)[1]))
dateLBR<- as.Date(datLBR[,1], "%m/%d/%Y %H:%M") 
datLBRmet <- data.frame(doy=yday(dateLBR), year=year(dateLBR), hour=datLBR$hour, RH=datLBR$VP.3.Humidity.Temp,Temp=datLBR$VP.3.Humidity.Temp.1, standID=rep(4, dim(datLBR)[1]))

#### gap filling september with LDF2
lbr.gap <- datLDF2met[datLDF2met$hour == 17 & datLDF2met$doy ==246 & datLDF2met$year == 2015, 1:5]
lbr.gap$standID <- rep(4, 1)
datLBRmet <- rbind(datLBRmet, lbr.gap)


dateMDF1 <- as.Date(datMDF1[,1], "%m/%d/%Y %H:%M") 
datMDF1met <- data.frame(doy=yday(dateMDF1), year=year(dateMDF1), hour=datMDF1$hour, RH=datMDF1$VP.3.Humidity.Temp,Temp=datMDF1$VP.3.Humidity.Temp.1, standID=rep(7, dim(datMDF1)[1]))
dateMDF2 <- as.Date(datMDF2[,1], "%m/%d/%Y %H:%M") 
datMDF2met <- data.frame(doy=yday(dateMDF2), year=year(dateMDF2), hour=datMDF2$hour, RH=datMDF2$VP.3.Humidity.Temp,Temp=datMDF2$VP.3.Humidity.Temp.1, standID=rep(8, dim(datMDF2)[1]))

datHDRmet <- data.frame(datDAVmet[,1:5], standID=rep(3, dim(datDAVmet)[1]))
datLDF3met <- data.frame(datLDF2met[,1:5], standID=rep(6, dim(datLDF2met)[1]))
datMDF4met <- data.frame(datMDF1met[,1:5], standID=rep(9, dim(datMDF1)[1]))


datMet<- rbind(datLmet,datHmet, datDAVmet,datHDF1met,datLBRmet,datLDF2met,datMDF1met,datMDF2met,datHDRmet,datLDF3met,datMDF4met)

datMet$e.sat <- 0.611*exp((17.502*datMet$Temp)/(datMet$Temp+240.97))
datMetRHfix<-ifelse(datMet$RH>=1,.999,datMet$RH)
datMet$VPD <- (datMet$e.sat-(datMetRHfix*datMet$e.sat))


#################################################
##############precipitation######################
#################################################


pre.v <- numeric(0)
for(i in 5:dim(datAirP)[1]){
  pre.v[i] <- sum(datAirP$Pr.mm[(i-1):(i-5)])
}

datAirP$Pr.ave <- pre.v



##################################################
##################################################
#####combine met   ###############################
##################################################

head(fluxes2)


fluxes2$hourR <- round_any(fluxes2$hour, 0.5)

colnames(datMet)[3] <- "hourR"


fluxes3 <- join(fluxes2, datMet, by=c("doy","year","hourR","standID"), type="left")

fluxes4 <- join(fluxes3, datAirP, by=c("doy","year"), type="left")

fluxes5 <- fluxes4[fluxes4$umol.h2o.m2.sec.1 <= quantile(fluxes4$umol.h2o.m2.sec.1, .95),]

standDay <- unique(data.frame(doy=fluxes5$doy, year=fluxes5$year, standID =fluxes5$standID, densityID = fluxes5$densityID))
standDay$standDay <- seq(1,dim(standDay)[1])

fluxes6 <- join(fluxes5, standDay, by =c("doy", "year", "standID", "densityID"), type = "left")
vpd.sd <- aggregate(fluxes6$VPD, by = list(fluxes6$standDay), FUN="mean")
par.sd <- aggregate(fluxes6$par, by = list(fluxes6$standDay), FUN="mean")


standDay$vpd <- vpd.sd$x
standDay$par <- par.sd$x

standDay <- join(standDay, datAirP, by = c("doy", "year"), type = "left")

flux.sd <- aggregate(fluxes6$umol.h2o.m2.sec.1, by = list(fluxes6$standDay), FUN="mean")
flux.std <- aggregate(fluxes6$umol.h2o.m2.sec.1, by = list(fluxes6$standDay), FUN="sd")

standDay$flux <- flux.sd$x
standDay$flux.std <- flux.std$x

#################################################
#################plots!!#########################
#################################################

cp <- ifelse(standDay$densityID == 1, "darkgreen",
             ifelse(standDay$densityID == 2, "tomato3",
                    "royalblue3"))

plot((standDay$vpd), standDay$flux, col = cp, pch=19)
hf <- lm(standDay$flux[standDay$densityID == 1] ~ standDay$vpd[standDay$densityID == 1])
summary(hf)
mf <- lm(standDay$flux[standDay$densityID == 2] ~ standDay$vpd[standDay$densityID == 2])
summary(mf)
lf <- lm(standDay$flux[standDay$densityID == 3] ~ standDay$vpd[standDay$densityID == 3])
summary(lf)


plot(standDay$par, standDay$flux, pch=19, col=cp)


scatter3D(log(standDay$vpd), standDay$Pr.ave, standDay$flux)


plot(log(standDay$vpd), standDay$par)
cor(log(standDay$vpd), standDay$par)

plot(standDay$par, standDay$flux, col=cp, pch=19)


#############################################
#####set up model run          ##############
#############################################


datalist <- list(Nobs=dim(standDay)[1], w.flux = standDay$flux, densityID = standDay$densityID, Ndensity=3, 
                 vpd = standDay$vpd)

parms <- c("b.0", "b.1", "sig.flux","w.rep")

flux.mod <- jags.model(file = "C:\\Users\\Ana\\Documents\\GitHub\\flux_data\\fluxes_model_code.r", 
                       data = datalist, n.adapt=10000, n.chains = 3)


flux.coda <- coda.samples(flux.mod, variable.names = parms, n.iter=10000, thin =1)

mcmcplot(flux.coda, parms = c("b.0", "b.1", "sig.flux", "mu.flux"), 
         dir = "C:\\Users\\Ana\\Documents\\siberia_data\\model_output\\run6\\history" )

mod.out <- summary(flux.coda)
write.table(mod.out$statistics, "C:\\Users\\Ana\\Documents\\siberia_data\\model_output\\run6\\mod_stats.csv",
            sep=",")
write.table(mod.out$quantiles, "C:\\Users\\Ana\\Documents\\siberia_data\\model_output\\run6\\mod_quantile.csv",
            sep=",")


gexp <- "\\[*[[:digit:]]*\\]"
rep.dat <- data.frame(mod.out$statistics[gsub(gexp,"", rownames(mod.out$statistics)) == "w.rep",])


plot(standDay$flux, rep.dat$Mean, xlim=c(0,2600), ylim=c(0,2600))
fit1 <- lm(rep.dat$Mean ~ standDay$flux)
summary(fit1)
abline(0,1, lwd=2, col="red")
abline(fit1, lwd=2, lty=3)

fluxMean <- aggregate(fluxes6$umol.h2o.m2.sec.1, by = list(fluxes6$standDay), FUN = "mean")

plot(fluxMean$x, data.matrix(mod.out$statistics[13:(83+12),1]))


## par relationship?
## compare et to pet - to see if values over 6000 are real?!
## plots of et to pet - against vpd 

