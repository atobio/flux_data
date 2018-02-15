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

datalist <- list(Nobs=dim(fluxes2)[1], w.flux = fluxes2$umol.h2o.m2.sec.1, vpd = fluxes2$vpd, 
                 standID = fluxes2$standID, Nstand=dim(f.count)[1])

parms <- c("b.0", "b.1", "sig.flux","w.rep")

flux.mod <- jags.model(file = "C:\\Users\\Ana\\Documents\\GitHub\\flux_data\\fluxes_model_code.r", 
                       data = datalist, n.adapt=10000, n.chains = 3)


flux.coda <- coda.samples(flux.mod, variable.names = parms, n.iter=10000, thin =1)

mcmcplot(flux.coda, parms = c("b.0", "b.1", "sig.flux"), 
         dir = "C:\\Users\\Ana\\Documents\\siberia_data\\model_output\\run1\\history" )

mod.out <- summary(flux.coda)
write.table(mod.out$statistics, "C:\\Users\\Ana\\Documents\\siberia_data\\model_output\\run1\\mod_stats.csv",
            sep=",")
write.table(mod.out$quantiles, "C:\\Users\\Ana\\Documents\\siberia_data\\model_output\\run1\\mod_quantile.csv",
            sep=",")

