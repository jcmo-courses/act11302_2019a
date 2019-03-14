#### MICHELLE GARCIA  ####
#### BINOMIAL NEGATIVA ####
#### 143751 #### 


BinNeg.Plot <- function(mu, n , p, a=NA,b=NA,calcProb=(!is.na(a) | !is.na(b)),quantile=NA,calcQuant=!is.na(quantile))
{
  # Poisson
  sd = sqrt(mu)
  low = max(0, round(mu - 3 * sd))
  high = round(mu + 5 * sd)
  values = low:high
  probs = dnbinom(values, n, p, mu, log= FALSE)
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
      prob = 1-pnbinom(a-1,n,p,mu, lower.trail = TRUE, log.p = FALSE)
      title(paste("P(",a," <= Y ) = ",round(prob,6),sep=""),line=0,col.main=4)
      u = seq(max(c(a,low)),high,by=1)
    }
    else {
      if(a > b) {d = a; a = b; b = d;}
      a = round(a); b = round(b)
      prob = pnbinom(b,n,mu, lower.tail = TRUE, log.p = FALSE) - pnbinom(a-1,n,mu,lower.tail = TRUE, log.p = FALSE )
      title(paste("P(",a," <= N <= ",b,") = ",round(prob,6),sep=""),line=0,col.main=4)
      u = seq(max(c(a,low)),min(c(b,high)),by=1)
    }
    v = dnbinom(u,n,p, mu ,log = FALSE)
    lines(u,v,type="h",col=4)
  }
  else if(calcQuant==T) {
    if(quantile < 0 || quantile > 1)
      stop("El cuantil debe estar entre 0 y 1")
    x = qnbinom(quantile,n,p,mu, lower.tail = TRUE, log.p = FALSE )
    title(paste("",quantile," quantile = ",x,sep=""),line=0,col.main=4)
    u = 0:x
    v = dnbinom(u,n,p,mu, log = FALSE)
    lines(u,v,type="h",col=4)
  }
  return(invisible())
}

BinNeg.Plot(10,0.5)



