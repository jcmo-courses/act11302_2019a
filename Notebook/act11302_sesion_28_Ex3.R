model{
  #Likelihood
  for (i in 1:n){
    y[i]~dpois(mu[i])
    log(mu[i])<-b[1]+b[2]*step(time[i]-a)
  }
  #Priors
  a <- c+1999
  c ~ dcat(p[])
  for (j in 1:12){p[j]<-1/12}
  for (j in 1:2){b[j]~dnorm(0,0.001)}
}
