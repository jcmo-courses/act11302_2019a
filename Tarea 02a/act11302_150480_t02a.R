BinomialNegativa.Plot <- function(n,p, a=NA,b=NA,calcProb=(!is.na(a) | !is.na(b)),quantile=NA,calcQuant=!is.na(quantile)){
  media = n*(1-p)/p
  sd = sqrt(n*(1-p)/p^2)
  low = max(0, round(media - 3 * sd))
  high = round(media + 5 * sd)
  values = low:high
  probs = dnbinom(values,size=50,mu=media)
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
      prob = 1-pnbinom(a-1,size=50,mu=media)
      title(paste("P(",a," <= Y ) = ",round(prob,6),sep=""),line=0,col.main=4)
      u = seq(max(c(a,low)),high,by=1)
    }
    else {
      if(a > b) {d = a; a = b; b = d;}
      a = round(a); b = round(b)
      prob = pnbinom(b,size=50,mu=media) - pnbinom(a-1,size=50,mu=media)
      title(paste("P(",a," <= N <= ",b,") = ",round(prob,6),sep=""),line=0,col.main=4)
      u = seq(max(c(a,low)),min(c(b,high)),by=1)
    }
    v = dnbinom(u,size=50,mu=media)
    lines(u,v,type="h",col=4)
  }
  else if(calcQuant==T) {
    if(quantile < 0 || quantile > 1)
      stop("El cuantil debe estar entre 0 y 1")
    x = qnbinom(quantile,mu=media)
    title(paste("",quantile," quantile = ",x,sep=""),line=0,col.main=4)
    u = 0:x
    v = dnbinom(u,mu=media)
    lines(u,v,type="h",col=4)
  }
  return(invisible())
}

BinomialNegativa.Plot(100,.7,a=35,b=48)