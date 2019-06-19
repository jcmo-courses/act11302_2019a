#======================================== PROYECTO FINAL (EQUIPO 4) ================================================#
#========================================    CALCULO ACTUARIAL 3    ================================================#

# INTEGRANTES:

# PAULINA GOMEZ ZU?IGA
# FERNANDO MARTIN ZAMORA
# FABRIZZIO PEREZ ACEITUNO

#===================================================================================================================#
#====================================== CAPTURA DE DATOS Y LIBRERIAS ===============================================#

datos <- read.csv("/Downloads/DanishInsurance_MultivarData_Full.csv",header = T)
attach(datos)
library(MASS)
if(!require("fitdistrplus")){install.packages("fitdistrplus")}
library('fitdistrplus')
if(!require("plyr")){install.packages("plyr")}
library('plyr')
if(!require("actuar")){install.packages("actuar")}
library('actuar')
if(!require("fBasics")){install.packages("fBasics")}
library('fBasics')
if(!require("stats")){install.packages("stats")}
library('stats')
if(!require("forecast")){install.packages("forecast")}
library('forecast')
if(!require("ggplot2")){install.packages("ggplot2")}
library('ggplot2')


datos[,'anio'] <-as.numeric(substr(date,7,11))
datos[,'mes'] <-as.numeric(substr(date,1,2))
for(i in 1:nrow(datos)){
  if(datos[i,7]<=6){datos[i,"semestre"] <- 1}
  else{datos[i,"semestre"] <- 2}
}

#=========================================== DIVISION DE DATOS =====================================================#

datosent <- datos[which(datos[,6]<=1985),]
datosprueba <- datos[which((datos[,6]>=1986&datos[,6]<=1987)),]
datos.b.ent <- datosent[,c(6,2)]
datos.c.ent <- datosent[,c(6,3)]
datos.p.ent <- datosent[,c(6,4)]
datos.t.ent <- datosent[,c(6,5)]
freqan <- table(datosent$anio,datosent$mes)
frecan <- as.numeric(as.matrix(freqan))
frecsem <- table(datosent$anio,datosent$semestre)
frecsem <- as.numeric(as.matrix(frecsem))

# SUMMARY
summary(datos.b.ent$building)
summary(datos.c.ent$contents)
summary(datos.p.ent$profits)
summary(datos.t.ent$total)

# HISTOGRAMAS
hist(datos.t.ent[,2],breaks=200,main="Pérdidas Totales",xlab="Monto de la Pérdida",ylab="Frecuencia",xlim=c(0,20),col="dark orange",border = "white")
par(new=T)
plot(density(datos.t.ent[,2]),xlim=c(0,20),main="",axes=F,ylab="",xlab="",col="dark blue",lwd=3)
hist(datos.b.ent[,2],breaks=50)
hist(datos.c.ent[,2],breaks=50)
hist(datos.p.ent[,2],breaks=50)

#=============================== MODELOS PARA LA FRECUENCIA DE SINIESTROS =========================================#

# BINOMIAL
fit.binom <- fitdist(frecan,"binom",fix.arg=list(size=nrow(datosent)),start=list(prob=0.5))
plot(fit.binom)

# POISSON
fit.pois <- fitdist(frecan,"pois")
plot(fit.pois)

# BINOMIAL NEGATIVA
fit.nbinom <- fitdist(frecan,"nbinom")
plot(fit.binom)
gofstat(list(fit.binom,fit.nbinom,fit.pois), fitnames = c("Binomial","Binomial Negativa", "Poisson"),discrete = T)

fit.pois$estimate*12

#============================ MODELOS PARA LAS SEVERIDADES INDIVIDUALES ===========================================#

descdist(datos.t.ent[,2],boot = 1000)
yti2fit <- datos.t.ent$total

# GAMMA
yti.gam <- fitdist(yti2fit,"gamma")
plot(yti.gam)
summary(yti.gam)

# EXPONENCIAL
yti.exp <- fitdist(yti2fit,"exp")
plot(yti.exp)
summary(yti.exp)

# LOGNORMAL
yti.logn <- fitdist(yti2fit,"lnorm")
plot(yti.logn)
summary(yti.logn)
yti.logn$estimate

# WEIBULL
yti.wei <- fitdist(yti2fit,"weibull")
plot(yti.wei)
summary(yti.wei)

# BURR
yti.burr <- fitdist(yti2fit,"burr",start = list(shape1=1,shape2=1,rate=1))
plot(yti.burr)
summary(yti.burr)

# PARETO
yti.par <- fitdist(yti2fit,"pareto")
plot(yti.par)
summary(yti.par)

# PARETO GENERALIZADA
yti.parg <- fitdist(yti2fit,"genpareto",start = list(shape1=1,shape2=1,rate=1))
plot(yti.parg)
summary(yti.parg)

# GUMBEL
yti.gumb <- fitdist(yti2fit,"gumbel",start = list(alpha=1,scale=1))
plot(yti.gumb)
summary(yti.gumb)

fit.dist <- list(yti.gam,yti.exp,yti.logn,yti.wei,yti.burr,yti.par,yti.parg,yti.gumb)
dist <- c("Gamma","Exponencial","LogNormal","Weibull","Burr","Pareto","Pareto Generalizada","Gumbel")
pbas <- gofstat(fit.dist, fitnames = dist)

leg1 <- c("Gamma","Exponencial","LogNormal")
denscomp(list(yti.gam,yti.exp,yti.logn), legendtext = leg1,fitcol = c("red","blue","orange"),fitlty = 19,fittype = "o",main="Histograma y Densidades Teóricas",ylab="Densidad",xlab="Datos")
cdfcomp (list(yti.gam,yti.exp,yti.logn), legendtext = leg1,fitcol = c("red","blue","orange"),fitlty = 19,main="FDAs Empíricas y Teóricas",ylab="FDA",xlab="Datos",lwd=3)
qqcomp  (list(yti.gam,yti.exp,yti.logn), legendtext = leg1,fitcol = c("red","light blue","orange"),fitpch=19,xlab="Cuantiles Teóricos",ylab="Cuantiles Empíricos",main="Gráfica Cuantil-Cuantil")
ppcomp  (list(yti.gam,yti.exp,yti.logn), legendtext = leg1,fitcol = c("red","light blue","orange"),fitpch=19,xlab="Cuantiles Teóricos",ylab="Cuantiles Empíricos",main="Gráfica P-P")


#=============================== PROPUESTAS PARA LA AGREGACION DE RIESGOS =========================================#

modelfreq <- expression(data=rpois(fit.pois$estimate*12))
modelsev <- expression(data=rlnorm(yti.logn$estimate[1],yti.logn$estimate[2]))
media.x <- mlnorm(1,yti.logn$estimate[1],yti.logn$estimate[2])
var.x <- mlnorm(2,yti.logn$estimate[1],yti.logn$estimate[2])-media.x^2
asim.x <- (mlnorm(3,yti.logn$estimate[1],yti.logn$estimate[2])-3*media.x*var.x-media.x^3)/var.x^(3/2)
media.n <- var.n <- fit.pois$estimate*12
asim.n <- 1/sqrt(fit.pois$estimate*12)
media.s <- media.x*media.n
var.s <- var.n*(var.x+media.x^2)
asim.s <- (asim.n*var.n^(3/2)*media.x^3+3*var.n*media.x*var.x+media.n*asim.x*var.x^(3/2))/var.s^(3/2)


# SIMULACION
Fssim <- aggregateDist("simulation",nb.simul = 10000,model.freq = modelfreq,model.sev = modelsev)
plot(Fssim)
summary(Fssim)

# NORMAL
Fsnorm <-aggregateDist("normal",moments=c(media.s,var.s))
plot(Fsnorm)

# NORMAL MODIFICADA CON SESGO
Fsnsesg<-aggregateDist("npower",moments=c(media.s,var.s,asim.s))
plot(Fsnsesg)


plot(Fssim,col="red",lwd=6,xlim=c(200,1000),sub="",main="Distribuci?n del Monto Agregado de Reclamos")
par(new=T)
plot(Fsnorm,col="dark orange",lwd=4,xlim=c(200,1000),xlab="",ylab="",main="",axes=F,sub="")
par(new=T)
plot(Fsnsesg,col="dark blue",lwd=3,xlim=c(200,1000),xlab="",ylab="",main="",axes=F,sub="")
legend(x=600,y=0.4,legend=c("Simulaci?n","Normal","Normal Modificada con Sesgo"),fill=c("red","dark orange","dark blue"))

# HISTOGRAMA DEL MONTO AGREGADO DE RECLAMOS POR SIMULACION
hist(knots(Fssim),breaks=50,freq = F)
Fs <- density(knots(Fssim))
plot(Fs)
datos.agreg <- knots(Fssim)
descdist(datos.agreg,boot=1000)

# AJUSTE PARA EL MONTO AGREGADO DE SINIESTROS

# NORMAL
fit.ag.norm <- fitdist(datos.agreg,"norm")
plot(fit.ag.norm)
summary(fit.ag.norm)

# GAMMA GENERALIZADA
fit.ag.ggen <- fitdist(datos.agreg,"trgamma",start=list(shape1=1,shape2=1,rate=1))
plot(fit.ag.ggen)
summary(fit.ag.ggen)

# LOGNORMAL
fit.ag.lnrom <- fitdist(datos.agreg,"lnorm")
plot(fit.ag.lnrom)
summary(fit.ag.lnrom)
fit.ag.lnrom$estimate
# GAMMA
fit.ag.gam <- fitdist(datos.agreg,"gamma")
plot(fit.ag.gam)
summary(fit.ag.gam)

# WEIBULL
fit.ag.wei <- fitdist(datos.agreg,"weibull")
plot(fit.ag.wei)
summary(fit.ag.wei)

distag <- c("Normal","Gamma Generalizada","LogNormal","Weibull")
gofstat(list(fit.ag.norm,fit.ag.ggen,fit.ag.lnrom,fit.ag.wei),fitnames=distag)

denscomp(list(fit.ag.norm,fit.ag.ggen,fit.ag.lnrom,fit.ag.wei), legendtext = distag,fitcol = c("red","dark blue","dark orange","dark green"),fitlty = 19,fittype = "o",main="Histograma y Densidades Teóricas",ylab="Densidad",xlab="Datos",lwd=3)
cdfcomp (list(fit.ag.norm,fit.ag.ggen,fit.ag.lnrom,fit.ag.wei), legendtext = distag,fitcol = c("red","dark blue","dark orange","dark green"),fitlty = 19,main="FDAs Empíricas y Teóricas",ylab="FDA",xlab="Datos",lwd=3)
qqcomp  (list(fit.ag.norm,fit.ag.ggen,fit.ag.lnrom,fit.ag.wei), legendtext = distag,fitcol = c("red","dark blue","dark orange","dark green"),fitpch=19,xlab="Cuantiles Teóricos",ylab="Cuantiles Empíricos",main="Gráfica Cuantil-Cuantil")
ppcomp  (list(fit.ag.norm,fit.ag.ggen,fit.ag.lnrom,fit.ag.wei), legendtext = distag,fitcol = c("red","dark blue","dark orange","dark green"),fitpch=19,xlab="Cuantiles Teóricos",ylab="Cuantiles Empíricos",main="Gráfica P-P")

#========================================== DATOS DE PRUEBA =======================================================#

datos.b.p <- datosprueba[,c(6,2)]
datos.c.p <- datosprueba[,c(6,3)]
datos.p.p <- datosprueba[,c(6,4)]
datos.t.p <- datosprueba[,c(6,5)]
freqanp <- table(datosprueba$anio,datosprueba$mes)
frecanp <- as.numeric(as.matrix(freqanp))
frecsemp <- table(datosprueba$anio,datosprueba$semestre)
frecsemp <- as.numeric(as.matrix(frecsemp))

#=============================== MODELOS PARA LA FRECUENCIA DE SINIESTROS =========================================#

# BINOMIAL
fit.p.bin <- fitdist(frecanp,"binom",fix.arg=list(size=nrow(datosprueba)),start=list(prob=0.5))
plot(fit.p.bin)
summary(fit.p.bin)

# BINOMIAL NEGATIVA
fit.p.nbin <- fitdist(frecanp,"nbinom")
plot(fit.p.nbin)
summary(fit.p.nbin)

# POISSON
fit.p.pois <- fitdist(frecanp,"pois")
plot(fit.p.pois)
summary(fit.p.pois)

gofstat(list(fit.p.bin,fit.p.nbin,fit.p.pois),fitnames = c("Binomial","Binomial Negativa", "Poisson"))


#============================ MODELOS PARA LAS SEVERIDADES INDIVIDUALES ===========================================#

yprueba <- datosprueba$total

# GAMMA
yp.gam <- fitdist(yprueba,"gamma")
plot(yp.gam)
summary(yp.gam)

# EXPONENCIAL
yp.exp <- fitdist(yprueba,"exp")
plot(yp.exp)
summary(yp.exp)

# LOGNORMAL
yp.ln <- fitdist(yprueba,"lnorm")
plot(yp.ln)
summary(yp.ln)

# WEIBULL
yp.wei <- fitdist(yprueba,"weibull")
plot(yp.wei)
summary(yp.wei)

# PARETO
yp.par <- fitdist(yprueba,"pareto")
plot(yp.par)
summary(yp.par)

fit.dist.p <- list(yp.gam,yp.exp,yp.ln,yp.wei,yp.par)
dist.p <- c("Gamma","Exponencial","LogNormal","Weibull","Pareto")
pbas.p <- gofstat(fit.dist.p, fitnames = dist.p)

# SIMULACION MONTOS AGREGADOS DE SINIESTROS (DATOS DE PRUEBA)
modelfreqprue <- expression(data=rpois(fit.p.pois$estimate*12))
modelsevprue <- expression(data=rlnorm(yp.ln$estimate[1],yp.ln$estimate[2]))
Fsprueba <- aggregateDist("simulation",nb.simul = 10000,model.freq = modelfreqprue,model.sev = modelsevprue)
plot(Fsprueba)
summary(Fsprueba)
datosagregprueba <- knots(Fsprueba)

# NORMAL
normal <- fitdist(datosagregprueba,"norm")
plot(normal)
summary(normal)
normal$estimate
# LOGNORMAL
lognormal <- fitdist(datosagregprueba,"lnorm")
plot(lognormal)
summary(lognormal)

gofstat(list(normal,lognormal),fitnames = c("Normal","LogNormal"))

#================================================ PREDICCION =====================================================#

# SIMULACION DATOS DE ENTRENAMIENTO
K <- 10000
simagreg86 <- NULL
simagreg87 <- NULL
simind86 <- NULL
simind87 <- NULL
datosprueba86 <- datosprueba[which(datosprueba$anio==1986),5]
datosprueba87 <- datosprueba[which(datosprueba$anio==1987),5]
for(i in 1:K){
    simagreg86[i] <- sum(rlnorm(rpois(1,fit.p.pois$estimate*12),yp.ln$estimate[1],yp.ln$estimate[2]))
    simagreg87[i] <- sum(rlnorm(rpois(1,fit.p.pois$estimate*12),yp.ln$estimate[1],yp.ln$estimate[2]))
    simind86[i] <- rlnorm(1,yp.ln$estimate[1],yp.ln$estimate[2])
    simind87[i] <- rlnorm(1,yp.ln$estimate[1],yp.ln$estimate[2])
  }

hist(simagreg86,breaks=50,freq=F)
hist(simagreg87,breaks=50,freq=F)
hist(simind86,breaks=50,freq=F)
hist(simind87,breaks=50,freq=F)

# PREDICCION INDIVIDUAL
plot(ecdf(datosprueba86),xlim=c(0,30),col="dark blue",lwd=3,main="Distribuci?n Emp?rica (Datos Prueba vs. Predicci?n)",sub="1986")
par(new=T)
plot(ecdf(simind86),xlim=c(0,30),col="dark orange",lwd=3,xlab="",ylab="",main="",axes=F)
legend(x=25,y=0.4,legend=c("Ajuste","Datos"),fill=c("dark orange","dark blue"))
plot(ecdf(datosprueba87),xlim=c(0,30),col="dark blue",lwd=3,main="Distribuci?n Emp?rica (Datos Prueba vs. Predicci?n)",sub="1987")
par(new=T)
plot(ecdf(simind87),xlim=c(0,30),col="dark orange",lwd=3,xlab="",ylab="",main="",axes=F)
legend(x=25,y=0.4,legend=c("Ajuste","Datos"),fill=c("dark orange","dark blue"))

# PREDICCION AGREGADO
modelo.arima1 <- auto.arima(simagreg86)
prediccion1 <- forecast(simagreg86,model=modelo.arima1,h=1,level=95)
prediccion1

modelo.arima2 <- auto.arima(simagreg87)
prediccion2 <- forecast(simagreg87,model=modelo.arima2,h=1,level=95)
prediccion2


#========================================= CALCULO DE PRIMAS =====================================================#

# SIMULACION DATOS DE PRUEBA
simagregprueba <- NULL
for(i in 1:K){simagregprueba[i] <- sum(rlnorm(rpois(1,fit.p.pois$estimate*12),yp.ln$estimate[1],yp.ln$estimate[2]))}
hist(simagregprueba,breaks=30)

denscomp(list(normal,lognormal), legendtext = c("Normal","LogNormal"),fitcol = c("dark blue","dark orange"),fitlty = 19,fittype = "o",main="Histograma y Densidades Te?ricas",ylab="Densidad",xlab="Datos",lwd=3)
cdfcomp (list(normal,lognormal), legendtext = c("Normal","LogNormal"),fitcol = c("dark blue","dark orange"),fitlty = 19,main="FDAs Emp?ricas y Te?ricas",ylab="FDA",xlab="Datos",lwd=3)
qqcomp  (list(normal,lognormal), legendtext = c("Normal","LogNormal"),fitcol = c("dark blue","dark orange"),fitpch=19,xlab="Cuantiles Te?ricos",ylab="Cuantiles Emp?ricos",main="Gr?fica Cuantil-Cuantil")
ppcomp  (list(normal,lognormal), legendtext = c("Normal","LogNormal"),fitcol = c("dark blue","dark orange"),fitpch=19,xlab="Cuantiles Te?ricos",ylab="Cuantiles Emp?ricos",main="Gr?fica P-P")


# PRIMA BASE
Pbase <- mean(simagregprueba)

# PRIMA TVaR (ALPHA=0.999)
alpha <- 0.999
g <- floor((length(simagregprueba)+1)*alpha)
h <- (length(simagregprueba)+1)*alpha-g
ordenados <- sort(simagregprueba)
Cuantil999 <- (1-h)*ordenados[g]+h*ordenados[g+1]
Cte999 <- (1/(length(simagregprueba)-g))*sum(ordenados[9991:10000])
Cuantil999

# PRIMA PRINCIPIO DE UTILIDAD CERO
W <- 10000
beta <- 0.05
ux <- function(x){1-exp(-beta*x)}     
fgmnorm<-function(t,miu,sigma){exp((miu*t)+(sigma*sigma*t*t)*(1/2))}
pu0 <- (1/beta)*log(fgmnorm(beta,normal$estimate[1],normal$estimate[2]))


#============================================ MODIFICACIONES =====================================================#

y2mod <- datosprueba$total
ymod <- NULL
N <- length(y2mod)
d <- 0.1
c <- 0.95
M <- 23
for(i in 1:N){
  if(y2mod[i]<y2mod[i]*d){ymod[i] <- 0}
  else if(y2mod[i]*d<=y2mod[i] && y2mod[i]<=M){ymod[i] <- c*(y2mod[i]*(1-d))}
  else {ymod[i] <- c*(M-y2mod[i]*d)}
}

#================================== CALCULO DE PRIMAS CON MODIFICACIONES ==========================================#

# GAMMA
ymod.gam <- fitdist(ymod,"gamma")
plot(ymod.gam)
summary(ymod.gam)

# EXPONENCIAL
ymod.exp <- fitdist(ymod,"exp")
plot(ymod.exp)
summary(ymod.exp)

# LOGNORMAL
ymod.ln <- fitdist(ymod,"lnorm")
plot(ymod.ln)
summary(ymod.ln)

# WEIBULL
ymod.wei <- fitdist(ymod,"weibull")
plot(ymod.wei)
summary(ymod.wei)

# PARETO
ymod.par <- fitdist(ymod,"pareto")
plot(ymod.par)
summary(ymod.par)

# PARETO GENERALIZADA
ymod.gpar <- fitdist(ymod,"genpareto",start=list(shape1=1,shape2=1))
plot(ymod.gpar)
summary(ymod.gpar)

fit.mod <- list(ymod.gam,ymod.exp,ymod.ln,ymod.wei,ymod.par,ymod.gpar)
dist.mod <- c("Gamma","Exponencial","LogNormal","Weibull","Pareto","Pareto Generalizada")
pbas.mod <- gofstat(fit.mod, fitnames = dist.mod)

# SIMULACIONES PARA EL MONTO AGREGADO DE RECLAMOS
smod<- NULL
for(i in 1:K){smod[i] <- sum(rgenpareto(rpois(1,fit.p.pois$estimate*12),ymod.gpar$estimate[1],ymod.gpar$estimate[2]))}
hist(smod,breaks=30,main="Monto Agregado de Reclamos con Modificaciones",xlab="Monto de la Pérdida",ylab="Frecuencia",col="orange",border = "white")
par(new=T)
plot(density(smod),main="",axes=F,ylab="",xlab="",col="dark blue",lwd=3)
# NORMAL
normalmod <- fitdist(smod,"norm")
plot(normalmod)
summary(normalmod)

# PRIMA BASE
pbasemod <- mean(smod)

# PRIMA TVaR (ALPHA=0.999)
gmod <- floor((length(smod+1)*alpha))
hmod <- (length(smod)+1)*alpha-g
ordmod <- sort(smod)
Cuantil999mod <- (1-hmod)*ordmod[g]+h*ordmod[g+1]
Cte999mod <- (1/(length(smod)-g))*sum(ordmod[9991:10000])
Cuantil999mod
var.n*(var.x+media.x^2)
# PRIMA PRINCIPIO DE UTILIDAD CERO
pu0mod <- (1/beta)*log(fgmnorm(beta,normalmod$estimate[1],normalmod$estimate[2]))
