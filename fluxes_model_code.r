model{


#likelihood 
for(i in 1:Nobs){
  w.flux[i] ~ dnorm(mu.flux[i], tau.flux)
  mu.flux[i] <- b.0[standID[i]] + b.1[standID[i]] * vpd[i]
  w.rep[i] ~ dnorm(mu.flux[i], tau.flux)
}

#assign priors
for(j in 1:Nstand){
  b.0[j] ~ dnorm(0,0.001)
  b.1[j] ~ dnorm(0,0.001)
}

tau.flux <- pow(sig.flux,-2)

sig.flux ~ dunif(0,50000)

}