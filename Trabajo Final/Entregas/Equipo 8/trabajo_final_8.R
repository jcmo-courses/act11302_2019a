install.packages("readr")
install.packages("fitdistrplus")
install.packages("actuar")
install.packages("ismev")
install.packages("evir")
library(readr)
library(fitdistrplus)
library(actuar)
library(ismev)
library(evir)
danish <- read_csv("~/Desktop/ITAM/Act 3/Proyecto Final/DanishInsurance_MultivarData_Full.csv")

fecha<-as.Date(danish$date,"%m/%d/%Y")
edificio<-danish$building
contenido<-danish$contents
utilidades<-danish$profits
total<-danish$total

entrenamiento<-danish[1:1040,]
prueba<-danish[1715:2167,]

par(mfrow=c(1,2))
hist(danish$total, main="Monto Total Danios", xlab="Monto Total de Danios", 
     col = "red")
boxplot(danish$total, main="Monto Total Danios")
par(mfrow=c(1,1))


#codigo para obtener la frecuencia de siniestros en su respectiva fecha
aux<-as.data.frame(table(fecha))
contador<-aux$Freq

paso<-data.frame(fecha[!duplicated(fecha)],contador)

fechaTotal<-as.Date(seq(from=as.Date("1980-1-1"), to=as.Date("1990-12-31"), by="days"),"%m/%d/%Y")
frecuencia<-c(replicate(length(fechaTotal),0))
df<-data.frame(fechaTotal,frecuencia)

for(i in 1:length(fechaTotal)){
  for(j in 1:1645){
    if(df[i,1]==paso[j,1]){
      df[i,2]<-paso[j,2]
      break
    }
  }
  j<-1
}

#vector con frecuencia de siniestros de 1980-1990
frec<-df[,2]
dias<-2192
frecentrenamiento<-frec[c(1:2192)]

#agrupacion por semanas y meses
semanas<-c()
mes<-c()
sem<-floor(dias/7)
j<-1
for(i in 1:sem){
  k<-7*i
  semanas[i]<-sum(frecentrenamiento[j:k])
  j<-k
}
hist(semanas,breaks=20,xlim=c(0,20))

mes<-floor(dias/12)
meses<-c()
j<-1
for(i in 1:mes){
  k<-12*i
  meses[i]<-sum(frecentrenamiento[j:k])
  j<-k
}
hist(meses,breaks=10, xlim=c(0,20))


#ajustando los siniestros por diferentes distribuciones
#poisson
poisson1985<-fitdistr(semanas,densfun="poisson",method="mle")
summary(poisson1985)

hist(semanas,breaks=20,freq=FALSE,ylab="",ylim=c(0,.20),xlim=c(0,15),xlab="")
par(new=TRUE)
plot(dpois(x=seq(-1:15),lambda=poisson1985$estimate[[1]]),type="l",
     col="red",ylab=" ",ylim=c(0,.20),xlim=c(0,15),xlab="")

#geometrica
geometrica1985<-fitdistr(semanas,densfun="geometric",method="mle")
summary(geometrica1985)

hist(semanas,breaks=20,freq=FALSE,ylab="",ylim=c(0,.20),xlim=c(0,15),xlab="")
par(new=TRUE)
plot(dgeom(x=seq(-1:15),prob=geometrica1985$estimate[[1]]),type="l",
     col="red",ylab=" ",ylim=c(0,.20),xlim=c(0,15),xlab="")

#binomial negativa
nbinom1985<-fitdistr(semanas,densfun="negative binomial")
summary(nbinom1985)

hist(semanas,breaks=20,freq=FALSE,ylab="",ylim=c(0,.20),xlim=c(0,15),xlab="")
par(new=TRUE)
plot(dnbinom(x=seq(-1:15),size=nbinom1985$estimate[[1]],mu=nbinom1985$estimate[[2]]),type="l",
     col="red",ylab=" ",ylim=c(0,.20),xlim=c(0,15),xlab="")

#comparacion de los tres modelos
hist(semanas,breaks=20,freq=FALSE,ylab="",ylim=c(0,.20),xlim=c(0,15),xlab="")
par(new=TRUE)
plot(dpois(x=seq(-1:15),lambda=poisson1985$estimate[[1]]),type="l",
     col="red",ylab=" ",ylim=c(0,.20),xlim=c(0,15),xlab="")
par(new=TRUE)
plot(dgeom(x=seq(-1:15),prob=geometrica1985$estimate[[1]]),type="l",
     col="blue",ylab=" ",ylim=c(0,.20),xlim=c(0,15),xlab="")
par(new=TRUE)
plot(dnbinom(x=seq(-1:15),size=nbinom1985$estimate[[1]],
             mu=nbinom1985$estimate[[2]]),type="l",
     col="green",ylab=" ",ylim=c(0,.20),xlim=c(0,15),xlab="")

##################################
totalentrenamiento<-danish$total[1:1040]
hist(totalentrenamiento,breaks=100,freq=FALSE)

#ajustamos a modelos de probabilidad
#gamma
gamma1985<-fitdist(totalentrenamiento,"gamma",method="mle")
hist(totalentrenamiento,breaks=100,freq=FALSE,xlim=c(0,100),ylim=c(0,.35))
par(new=TRUE)
plot(dgamma(seq(from=0,to=50,by=.1),shape=gamma1985$estimate[[1]],
            rate=gamma1985$estimate[[2]]),xlim=c(0,100),ylim=c(0,.5),type="l")

#weibull
weibull1985<-fitdist(totalentrenamiento,"weibull",method="mle")
hist(totalentrenamiento,breaks=100,freq=FALSE,xlim=c(0,100),ylim=c(0,.35))
par(new=TRUE)
plot(dweibull(seq(from=0,to=50,by=.1),shape=weibull1985$estimate[[1]],
            scale=weibull1985$estimate[[2]]),xlim=c(0,100),ylim=c(0,.35),type="l")

#pareto
pareto1985<-fitdist(totalentrenamiento,"pareto",method="mle")
hist(totalentrenamiento,breaks=200,freq=FALSE,xlim=c(0,100),ylim=c(0,.35))
par(new=TRUE)
plot(dpareto(seq(from=0,to=50,by=.1),shape=pareto1985$estimate[[1]],
             scale=pareto1985$estimate[[2]]),type="l",xlim=c(0,100),ylim=c(0,.35))
######################
#Agregacion de Riesgos
k <- 10000
S <- c(rep(0, k))

for (i in 1:k){
  N <- rpois(30, 52*poisson1985$estimate[[1]])
  N_barra <- mean(N)
  N_barra_techo <- as.integer(N_barra) 
  
  weibull_sim<- rweibull(N_barra_techo, shape = weibull1985$estimate[[1]], scale = weibull1985$estimate[[2]])
  S[i] <- sum(weibull_sim)
}

S_gorro <- mean(S)
secuencia_agre<-c(min(S):max(S))
agregacion<-fitdistr(S,"normal")
hist(S,xlim = c(min(S), max(S)),ylim = c(0,.0125),freq=FALSE,
     xlab="",ylab="",main="Agregacion de Siniestros")
par(new = TRUE)
plot(x = secuencia_agre, y =dnorm(secuencia_agre, mean = agregacion$estimate[[1]],
                                  sd = agregacion$estimate[[2]]),    type = "l", 
     col = "red", xlim = c(min(S), max(S)), ylim = c(0,.0125),xlab="",ylab="")

######################
#calcular primas
#a)
prima_base<-agregacion$estimate[[1]]
#b) TVaR alpha = .999
alfa<-.999
prima_tvar<-qnorm(alfa,mean=agregacion$estimate[[1]],sd=agregacion$estimate[[2]])
#c)
riesgo<-.05
prima_varianza <- agregacion$estimate[[1]]+riesgo*agregacion$estimate[[2]]^2




