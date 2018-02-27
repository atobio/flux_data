model{


#likelihood 
for(i in 1:Nobs){
  
  # calculate mean for standDay
  w.flux[i] ~ dnorm(mu.flux[i], tau.flux)
  mu.flux[i] <- b.0[densityID[i]] + b.1[densityID[i]] * vpd[i]
  
  w.rep[i] ~ dnorm(mu.flux[i], tau.flux)
  }
  
  
  
for(j in 1:Ndensity){
  b.0[j] ~ dnorm(0,0.00000001)

  b.1[j] ~ dnorm(0,0.00000001)

  
}
  
  tau.flux <- pow(sig.flux, -2)
  sig.flux ~ dunif(0,50000)


}