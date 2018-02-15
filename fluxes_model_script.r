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
#set directory
#setwd("/Users/Ana/Documents/siberia_data/dg_fluxes")
#read in data
fluxes <- read.csv("z:\\student_research\\tobio\\all_fluxes_2015_16.csv")
#canopy rh and temperature
datRH <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\RH.VP4.csv")
datTC <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\TempC.VP4.csv")
#read in precip data
datAirP <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\airport\\airport.csv")

#############################################
#####organize fluxes data      ##############
#############################################

#check how many fluxes are in each stand
f.count <- aggregate(fluxes$umol.h2o.m2.sec.1, by=list(fluxes$stand, fluxes$density), FUN="length")
colnames(f.count) <- c("stand","density", "count")
f.count$standID <- seq(1,dim(f.count)[1])
#add stand id to the fluxes data
fluxes2 <- join(fluxes, f.count, by=c("stand","density"), type="left")

#############################################
#####organize met data         ##############
#############################################
#subset and match
datLRHmet <- data.frame(datRH[datRH$site=="ld",1:3], RHL=datRH$RH.VP4[datRH$site=="ld"])
datLTCmet <- data.frame(datTC[datTC$site=="ld",1:3], TempL=datTC$TempC.VP4[datTC$site=="ld"])

datHRHmet <- data.frame(datRH[datRH$site=="hd",1:3], RHH=datRH$RH.VP4[datRH$site=="hd"])
datHTCmet <- data.frame(datTC[datTC$site=="hd",1:3], TempH=datTC$TempC.VP4[datTC$site=="hd"])
#join each data type together to average across stands
RHall <- join(datLRHmet,datHRHmet, by=c("doy","year", "hour"), type="full")
#take the average of both stands. If one is missing just use the stand with data
RHave <- ifelse(RHall$


