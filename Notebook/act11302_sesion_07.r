Binomial.Plot <-function(n, p, low=0, high=n,scale = F, a=NA,b=NA,calcProb=!all(is.na(c(a,b))),quantile=NA,calcQuant=!is.na(quantile)){
  # Binomial
  sd = sqrt(n * p * (1 - p))
  if(scale && (n > 10)) {
    low = max(0, round(n * p - 4 * sd))
    high = min(n, round(n * p + 4 * sd))
  }
  values = low:high
  probs = dbinom(values, n, p)
  plot(c(low,high), c(0,max(probs)), type = "n", 
       xlab = "Numero de casos",
       ylab = "Masa de probabilidad",
       main = "")
  lines(values, probs, type = "h", col = 2)
  abline(h=0,col=3)
  if(calcProb) {
    if(is.na(a))
      a = 0
    if(is.na(b))
      b = n
    if(a > b) {
      d = a
      a = b
      b = d
    }
    a = round(a)
    b = round(b)
    prob = pbinom(b,n,p) - pbinom(a-1,n,p)
    title(paste("P(",a," <= N <= ",b,") = ",round(prob,6),sep=""),line=0,col.main=4)
    u = seq(max(c(a,low)),min(c(b,high)),by=1)
    v = dbinom(u,n,p)
    lines(u,v,type="h",col=4)
  }
  else if(calcQuant==T) {
    if(quantile < 0 || quantile > 1)
      stop("El cuantil debe estar entre 0 y 1")
    x = qbinom(quantile,n,p)
    title(paste(" ",quantile," quantile = ",x,sep=""),line=0,col.main=4)
    u = 0:x
    v = dbinom(u,n,p)
    lines(u,v,type="h",col=4)
  }
  return(invisible())
}

Binomial.Plot(100,.3)

Binomial.Plot(100,0.3,a=27,b=33,scale=T)


# -------------------

Poisson.Plot <- function(mu, a=NA,b=NA,calcProb=(!is.na(a) | !is.na(b)),quantile=NA,calcQuant=!is.na(quantile)){
  # Poisson
  sd = sqrt(mu)
  low = max(0, round(mu - 3 * sd))
  high = round(mu + 5 * sd)
  values = low:high
  probs = dpois(values, mu)
  plot(c(low,high), c(0,max(probs)), type = "n", 
       xlab = "Numero de casos",
       ylab = "Masas de probabilidad",
       main = "")
  lines(values, probs, type = "h", col = 2)
  abline(h=0,col=3)
  if(calcProb) {
    if(is.na(a)){ a = 0 }
    if(is.na(b)){
      a = round(a)
      prob = 1-ppois(a-1,mu)
      title(paste("P(",a," <= Y ) = ",round(prob,6),sep=""),line=0,col.main=4)
      u = seq(max(c(a,low)),high,by=1)
    }
    else {
      if(a > b) {d = a; a = b; b = d;}
      a = round(a); b = round(b)
      prob = ppois(b,mu) - ppois(a-1,mu)
      title(paste("P(",a," <= N <= ",b,") = ",round(prob,6),sep=""),line=0,col.main=4)
      u = seq(max(c(a,low)),min(c(b,high)),by=1)
    }
    v = dpois(u,mu)
    lines(u,v,type="h",col=4)
  }
  else if(calcQuant==T) {
    if(quantile < 0 || quantile > 1)
      stop("El cuantil debe estar entre 0 y 1")
    x = qpois(quantile,mu)
    title(paste("",quantile," quantile = ",x,sep=""),line=0,col.main=4)
    u = 0:x
    v = dpois(u,mu)
    lines(u,v,type="h",col=4)
  }
  return(invisible())
}

# --------------


ab0class <- function(alpha, beta, pO, K){
  ab0dist <- array(NA, dim=c(K+1,3))
  ab0dist[,1] <- seq(0:K)-1
  ab0dist[1,2] <- p0
  k <- 1
  for(k in 1:K){
    ab0dist[(k+1),2] <- as.numeric(ab0dist[k,2] %*% (alpha + beta/k)) 
  }
  ab0dist[,3] <- cumsum(ab0dist[,2])
  return(ab0dist)
}

if(!require("ggplot2")){install.packages("ggplot2")}
suppressMessages(library("ggplot2"))
ggplot(data.frame(x=c(0:20)), aes(x)) +
  geom_point(aes(y=dpois(x, 10)), colour="red", 
             ylab = "masa de probabildad", xlab="N") +
  geom_point(aes(y=dbinom(x, 100, 0.1)), colour="blue") +
  geom_point(aes(y=dnbinom(x, 90, 0.9)), colour="green")

K <- 100
alpha <- 0
beta <- 30
p0 <- exp(-beta)
ab0dist <- ab0class(alpha, beta, pO, K)

plot(ab0dist[,1],ab0dist[,2],type="h", ylab="P(N=n)", col=2, xlab="n", main="");abline(h=0:1,col=3)
points(ab0dist[,1],ab0dist[,2],col=2);abline(h=0,col=3)
