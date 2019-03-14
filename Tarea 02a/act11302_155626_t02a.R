BinNeg.Plot <-function(r, p,low=0,high=r,scale=F,a=NA,b=NA,calcProb=!all(is.na(c(a,b))),quantile=NA,calcQuant=!is.na(quantile)){
  # Binomial Negativa
  sd = sqrt(r*(1-p)/p^2)
  if(scale && (r > 10)){
    low = max(0, round(r*(1-p)/p-3*sd))
    high = min(r, round(r*(1-p)/p+3*sd))
  }
  values = low:high
  probs = dnbinom(values,r,p)
  plot(c(low,high), c(0,max(probs)), type = "n", 
       xlab = "Numero de casos",
       ylab = "Masa de probabilidad",
       main = "Binomial Negativa")
  lines(values, probs, type = "h", col = 2)
  abline(h=0,col=3)
  if(calcProb) {
    if(is.na(a))
      a = 0
    if(is.na(b))
      b = r
    if(a > b) {
      d = a
      a = b
      b = d
    }
    a = round(a)
    b = round(b)
    prob = pnbinom(b,r,p) - pnbinom(a-1,r,p)
    title(paste("P(",a," <= X <= ",b,") = ",round(prob,6),sep=""),line=0,col.main=4)
    u = seq(max(c(a,low)),min(c(b,high)), by=1)
    v = dnbinom(u,r,p)
    lines(u,v,type="h",col=4)
  }
  else if(calcQuant==T) {
    if(quantile < 0 || quantile > 1)
      stop("El cuantil debe estar entre 0 y 1")
    x = qnbinom(quantile,r,p)
    title(paste(" ",quantile," quantile = ",x,sep=""),line=0,col.main=4)
    u = 0:x
    v = dnbinom(u,r,p)
    lines(u,v,type="h",col=2)
  }
  return(invisible())
}

BinNeg.Plot(100,0.9)

BinNeg.Plot(100,0.7,a=35,b=50,scale=T)
