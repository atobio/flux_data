library(plyr)
library(rjags)
library(coda)
library(mcmcplots)

setwd("/Users/Ana/Documents/siberia_data/dg_fluxes")

fluxes <- read.csv("all_fluxes_2015_16.csv")

f.count <- aggregate(fluxes$umol.h2o.m2.sec.1, by=list(fluxes$stand, fluxes$density), FUN="length")

colnames(f.count) <- c("stand","density", "count")
f.count$standID <- seq(1,dim(f.count)[1])

fluxes2 <- join(fluxes, f.count, by=c("stand","density"), type="left")


