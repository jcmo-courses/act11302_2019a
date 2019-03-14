#---
#title: "Tarea1"
#author: "C??sar P??rez"
#date: "6/2/2019"
#output: pdf_document
#---
  
#  ```{r setup, include=FALSE}
#knitr::opts_chunk$set(echo = TRUE)
#```

if(!require("repmis")){install.packages("repmis")}
library("repmis")

data <- source_data("https://github.com/JCMO-ITAM/Data4Analysis/blob/master/d4a_allstateclaim_data.csv?raw=true")

data <- as.data.frame(data)

J <- nrow(data)
n0 <- nrow(as.matrix(which(data$Claim_Amount==0)))
J;n0

#binomial
N = 1
theta0_sb <- n0/(J*N)
theta0_sb

theta0_b <- seq(.0000, .9999, .001)
lik_theta0_b <- dbeta(theta0_b, 1 + n0, 1 + (J*N-n0) )

plot(theta0_b, lik_theta0_b, xlim=c(-0.1,1), ylim=c(0, 1.25 * max(lik_theta0_b,1.6)),
     type = "l", ylab= "Verosimilitud", lty = 3,
     xlab= "theta_0", las=1, main="",lwd=2,
     cex.lab=1.5, cex.main=1.5, col = "darkorange", axes=FALSE)
title("Binomial")
axis(1, at = seq(0,1,.2)) #adds custom x axis
axis(2, las=1) # custom y axis

#geom??trica 

theta0_sg = (J-n0)/n0
theta0_sg

theta0_g <- seq(.000, 1.2, .001)
lik_theta0_g <- dbeta(theta0_g, 1 + J-n0, 1 +J )

plot(theta0_g, lik_theta0_g, xlim=c(-0.1,1), ylim=c(0, 1.25 * max(lik_theta0_g,1.6)),
     type = "l", ylab= "Verosimilitud", lty = 3,
     xlab= "theta_0", las=1, main="",lwd=2,
     cex.lab=1.5, cex.main=1.5, col = "darkorange", axes=FALSE)
title("Geom??trica")
axis(1, at = seq(0,1.2,.2)) #adds custom x axis
axis(2, las=1) # custom y axis

#poisson

theta0_sp <- n0/(J*N)
theta0_sp

theta0_p <- seq(.0000, .9999, .001)
lik_theta0_p <- dgamma(theta0_p, 1 + n0, J )

plot(theta0_p, lik_theta0_p, xlim=c(-0.1,1), ylim=c(0, 1.25 * max(lik_theta0_p,1.6)),
     type = "l", ylab= "Verosimilitud", lty = 3,
     xlab= "theta_0", las=1, main="",lwd=2,
     cex.lab=1.5, cex.main=1.5, col = "darkorange", axes=FALSE)
title("Poisson")
axis(1, at = seq(0,1,.2)) #adds custom x axis
axis(2, las=1) # custom y axis