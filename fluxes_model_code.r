model{


#likelihood 
for(i in 1:Nobs){
  w.flux[i] ~ dnorm(mu.flux[i], tau.flux)
  mu.flux[i] <- b.0[densityID[i]] + b.1[densityID[i]] * log(VPD[i]) + b.2[densityID[i]] * pr.ave[i] + b.3[densityID[i]] * par[i] 
  w.rep[i] ~ dnorm(mu.flux[i], tau.flux)
}

#assign priors
for(j in 1:Ndensity){
  b.0[j] ~ dnorm(0,0.001)
  b.1[j] ~ dnorm(0,0.001)
  b.2[j] ~ dnorm(0,0.001) 
  b.3[j] ~ dnorm(0,0.001) 
}

tau.flux <- pow(sig.flux,-2)

sig.flux ~ dunif(0,50000)

}