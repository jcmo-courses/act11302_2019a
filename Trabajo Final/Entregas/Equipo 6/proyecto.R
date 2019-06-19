## Despacho Actuarial Moreno
## ITAM - Cálculo Actuarial III
## Proyecto Final 
## 01 - Junio - 2019

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## Librerías
library("tidyr")
library("dplyr")
library("ggplot2")
library("EnvStats")
library("VGAM")
library("MASS")
library("GB2")
library("evir")
library("stringr")
library("stats")
library("base")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## Datos
completo <- read.csv('DanishInsurance_MultivarData_Full.csv')
completo[contains('1980',vars=completo$date),'year'] <- '1980'
completo[contains('1981',vars=completo$date),'year'] <- '1981'
completo[contains('1982',vars=completo$date),'year'] <- '1982'
completo[contains('1983',vars=completo$date),'year'] <- '1983'
completo[contains('1984',vars=completo$date),'year'] <- '1984'
completo[contains('1985',vars=completo$date),'year'] <- '1985'
completo[contains('1986',vars=completo$date),'year'] <- '1986'
completo[contains('1987',vars=completo$date),'year'] <- '1987'
completo[contains('1988',vars=completo$date),'year'] <- '1988'
# Lectura de datos de entrenamiento
entrenamiento <- completo %>% slice(which(completo$year<=1988))

completo[contains('1989',vars=completo$date),'year'] <- '1989'
completo[contains('1990',vars=completo$date),'year'] <- '1990'
# Lectura de datos de prueba
prueba <- completo %>% slice(which(completo$year>1988))


# Siniestralidad por categoría para entrenamiento 
ggplot(entrenamiento, aes(x=year,y=log(building))) + geom_boxplot() + labs(x='',title='Siniestralidad para edificio')

ggplot(entrenamiento, aes(x=year,y=log(contents))) + geom_boxplot() + labs(x='',title='Siniestralidad para contenidos')

ggplot(entrenamiento, aes(x=year,y=log(profits))) + geom_boxplot() + labs(x='',title='Siniestralidad para ventas')

# Siniestralidad por categoría para prueba

ggplot(prueba, aes(x=year,y=log(building))) + geom_boxplot() + labs(x='',title='Siniestralidad para edificio')

ggplot(prueba, aes(x=year,y=log(contents))) + geom_boxplot() + labs(x='',title='Siniestralidad para contenidos')

ggplot(prueba, aes(x=year,y=log(profits))) + geom_boxplot() + labs(x='',title='Siniestralidad para ventas')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# Frecuencia de siniestros Poisson

# Para building anual
n_building <- entrenamiento %>% filter(building>0) %>% group_by(year) %>% count()
# Gráfica para número de siniestros de building por año
ggplot(n_building) + geom_col(aes(x=year,y=n,fill=year)) + labs(x='',title='n_building por año')
lambda_building <- mean(n_building$n)

# Para contents anual
n_contents <- entrenamiento %>% filter(contents>0) %>% group_by(year) %>% count()
# Gráfica para número de siniestros de contents por año
ggplot(n_contents) + geom_col(aes(x=year,y=n,fill=year)) + labs(x='',title='n_contents por año')
lambda_contents <- mean(n_contents$n)

# Para profits anual
n_profits <- entrenamiento %>% filter(profits>0) %>% group_by(year) %>% count()
# Gráfica para número de siniestros de profits por año
ggplot(n_profits) + geom_col(aes(x=year,y=n,fill=year)) + labs(x='',title='n_profits por año')
lambda_profits <- mean(n_profits$n)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Frecuencia de siniestros Binomial 

# Simulación de cartera
Severidad <- runif(1,min=0,max=.2)
Promedio.Frecuencia = (((entrenamiento %>% filter(building>0) %>% count()+entrenamiento %>% filter(contents>0) %>% count()+entrenamiento %>% filter(profits>0) %>% count())/3)/9)
Cartera <- Promedio.Frecuencia/Severidad
CarteraReal <- ceiling(Cartera*(1+runif(1,min=0,max=.2)))
CarteraRealAjustada <-CarteraReal-entrenamiento%>%filter(building>0)%>%filter(year>='1988')%>%count()
CarteraRealAjustada
# Obtenemos probabilidades de Siniestro
Pb <- entrenamiento%>%filter(building>0)%>%filter(year>='1988')%>%count()/CarteraRealAjustada
Pc <- entrenamiento%>%filter(contents>0)%>%filter(year>='1988')%>%count()/CarteraRealAjustada
Pp <- entrenamiento%>%filter(profits>0)%>%filter(year>='1988')%>%count()/CarteraRealAjustada

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Frecuencia de siniestros Binomial Negativa

# Se usa la simulación de cartera del modelo binomial
# Para modelar la frecuencia se utilizara la binomial negativa con su respectivo probabilidades de sinietro. 
# Puesto que la binomial negativa cuantos exitos necesito hasta que aparezcan r siniestros

BinNeg.Plot(CarteraRealAjustada[1,1],Pb[1,1])
BinNeg.Plot(CarteraRealAjustada[1,1],Pc[1,1])
BinNeg.Plot(CarteraRealAjustada[1,1],Pp[1,1])

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## Severidad de siniestros

# Gráficas de siniestros por año
ggplot(entrenamiento,aes(x=building,group=year,fill=year)) + geom_histogram(binwidth=1)
ggplot(entrenamiento,aes(x=contents,group=year,fill=year)) + geom_histogram(binwidth=1)
ggplot(entrenamiento,aes(x=profits,group=year,fill=year)) + geom_histogram(binwidth=1)
 
# Gráficas de arriba pero focalizadas
entrenamiento %>% filter(building<50) %>% ggplot(aes(x=building,group=year,fill=year)) + geom_histogram(binwidth=1)
entrenamiento %>% filter(contents<40) %>% ggplot(aes(x=contents,group=year,fill=year)) + geom_histogram(binwidth=1)
entrenamiento %>% filter(profits<30) %>% ggplot(aes(x=profits,group=year,fill=year)) + geom_histogram(binwidth=1)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Modelo de distribución empírica 

# Distribución empírica para building
qu_b <- qemp(p = seq(0, 1, len = 10000), entrenamiento$building,discrete = TRUE) 
f.edf_b <- demp(qu_b, entrenamiento$building) 
f.edf_b <- f.edf_b/sum(f.edf_b)
epdfPlot(entrenamiento$building,xlim=c(0,100))
plot(x=qu_b,y=f.edf_b,type='l',main='Distribución empírica para building')

# Distribución empírica para contents
qu_c <- qemp(p = seq(0, 1, len = 10000), entrenamiento$contents,discrete = TRUE) 
f.edf_c <- demp(qu_c, entrenamiento$contents) 
f.edf_c <- f.edf_c/sum(f.edf_c)
epdfPlot(entrenamiento$contents,xlim=c(0,100))
plot(x=qu_c,y=f.edf_c,type='l',main='Distribución empírica para contents')

# Distribución empírica para profits
qu_p <- qemp(p = seq(0, 1, len = 10000), entrenamiento$profits,discrete = TRUE) 
f.edf_p <- demp(qu_b, entrenamiento$profits) 
f.edf_p <- f.edf_p/sum(f.edf_p)
epdfPlot(entrenamiento$profits,xlim=c(0,100))
plot(x=qu_p,y=f.edf_p,type='l',main='Distribución empírica para profits')

# Distribución empírica para total 
qu_t <- qemp(p = seq(0, 1, len = 1648), entrenamiento$total,discrete = TRUE) 
f.edf_t <- demp(qu_t, entrenamiento$total) 
f.edf_t <- f.edf_t/sum(f.edf_t)
epdfPlot(entrenamiento$total,xlim=c(0,100))
plot(x=qu_t,y=f.edf_t,type='l',main='Distribución empírica para total')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# Modelo de distribución lognormal

# Modelo para total
Par.Lognormal_t <- fitdistr(entrenamiento$total,"lognormal")
Par.Lognormal_t
mean_log_t <- as.double(Par.Lognormal_t$estimate['meanlog'])
sd_log_t <- as.double(Par.Lognormal_t$estimate['sdlog'])
Completo.Unique <- sort(unique(entrenamiento$total))
dlognormal <- dlnorm(Completo.Unique,mean_log_t,sd_log_t)
dlognormal <- dlognormal/sum(dlognormal)
ggplot(data=NULL,aes(x=Completo.Unique,y=dlognormal))+geom_path()+labs(title='Distribución Lognormal para total',x='Monto de reclamo',y='f(t)')

# Modelo para building
Par.Lognormal_b <- fitdistr(entrenamiento$building[which(entrenamiento$building>0)],"lognormal")
Par.Lognormal_b
mean_log_b <- as.double(Par.Lognormal_b$estimate['meanlog'])
sd_log_b <- as.double(Par.Lognormal_b$estimate['sdlog'])

# Modelo para contents
Par.Lognormal_c <- fitdistr(entrenamiento$contents[which(entrenamiento$contents>0)],"lognormal")
Par.Lognormal_c
mean_log_c <- as.double(Par.Lognormal_c$estimate['meanlog'])
sd_log_c <- as.double(Par.Lognormal_c$estimate['sdlog'])
ent.building <- sort(unique(entrenamiento$building))
dlognormal_b <- dlnorm(ent.building,mean_log_b,sd_log_b)
dlognormal_b <- dlognormal_b/sum(dlognormal_b)
plot(x=ent.building,y=dlognormal_b,type='l') 

# Modelo para profits
Par.Lognormal_p <- fitdistr(entrenamiento$profits[which(entrenamiento$profits>0)],"lognormal")
Par.Lognormal_p
mean_log_p <- as.double(Par.Lognormal_p$estimate['meanlog'])
sd_log_p <- as.double(Par.Lognormal_p$estimate['sdlog'])


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# Predicción para datos de prueba con Empírica

# Frecuencia
num_buil <- prueba %>% filter(profits>0) %>% group_by(year) %>% count()
num_cont <- prueba %>% filter(contents>0) %>% group_by(year) %>% count()
num_prof <- prueba %>% filter(profits>0) %>% group_by(year) %>% count()
comparacion_freq <- as.data.frame(c('1989','1990'))
colnames(comparacion_freq) <- c('year')
comparacion_freq$R.Bui <- num_buil$n
comparacion_freq$E.Bui <- rpois(2,lambda_building)
comparacion_freq$R.Con <- num_cont$n
comparacion_freq$E.Con <- rpois(2,lambda_contents)
comparacion_freq$R.Pro <- num_prof$n
comparacion_freq$E.Pro <- rpois(2,lambda_profits)
comparacion_freq

# Siniestralidad
sin_buil.89 <- sum(prueba$building[which(prueba$year=='1989')])
sin_cont.89 <- sum(prueba$contents[which(prueba$year=='1989')])
sin_prof.89 <- sum(prueba$profits[which(prueba$year=='1989')])

sin_tot.89 <- sin_buil.89 + sin_prof.89 + sin_cont.89

sin_buil.90 <- sum(prueba$building[which(prueba$year=='1990')])
sin_cont.90 <- sum(prueba$contents[which(prueba$year=='1990')])
sin_prof.90 <- sum(prueba$profits[which(prueba$year=='1990')])

sin_tot.90 <- sin_buil.90 + sin_prof.90 + sin_cont.90

# Prueba con Lognormal
comparacion_sins <- as.data.frame(c('1989','1990'))
colnames(comparacion_sins) <- c('year')
comparacion_sins$R.Bui <- c(sin_buil.89,sin_buil.90)
comparacion_sins$E.Bui <- c(sum(rlnorm(comparacion_freq$E.Bui[1],mean_log_b+0.5,sd_log_b)),sum(rlnorm(comparacion_freq$E.Bui[2],mean_log_b+0.5,sd_log_b)))
comparacion_sins$R.Con <- c(sin_cont.89,sin_buil.90)
comparacion_sins$E.Con <- c(sum(rlnorm(comparacion_freq$E.Con[1],mean_log_c+0.5,sd_log_c)),sum(rlnorm(comparacion_freq$E.Con[2],mean_log_c+0.5,sd_log_c)))
comparacion_sins$R.Pro <- c(sin_prof.89,sin_prof.90)
comparacion_sins$E.Pro <- c(sum(rlnorm(comparacion_freq$E.Pro[1],mean_log_p+0.5,sd_log_p)),sum(rlnorm(comparacion_freq$E.Pro[1],mean_log_p+0.5,sd_log_p)))
comparacion_sins$R.Tot <- c(sin_tot.89,sin_tot.90)
comparacion_sins$E.Tot <- c(comparacion_sins$E.Bui[1]+comparacion_sins$E.Con[1]+comparacion_sins$E.Pro[1],comparacion_sins$E.Bui[2]+comparacion_sins$E.Con[2]+comparacion_sins$E.Pro[2])
comparacion_sins

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# Agregación de siniestros 

# Estimación de datos totales
datos_estimados_b <- rlnorm(sum(n_building$n),meanlog = mean_log_b, sdlog = sd_log_b)
datos_estimados_c <- rlnorm(sum(n_contents$n),meanlog = mean_log_c, sdlog = sd_log_c)
datos_estimados_c[length(datos_estimados_c):length(datos_estimados_b)] <- 0
datos_estimados_p <- rlnorm(sum(n_profits$n),meanlog = mean_log_p, sdlog = sd_log_p)
datos_estimados_p[length(datos_estimados_p):length(datos_estimados_b)] <- 0
datos_estimados_t <- datos_estimados_b+datos_estimados_c+datos_estimados_p
# Modelo de distribución lognormal estimado
Par.Lognormal_est <- fitdistr(datos_estimados_t,"lognormal")
mean_log_est <- as.double(Par.Lognormal_p$estimate['meanlog'])
sd_log_est <- as.double(Par.Lognormal_p$estimate['sdlog'])
Completo.Unique_est <- sort(unique(datos_estimados_t))
dlognormal_est <- dlnorm(Completo.Unique_est,mean_log_est,sd_log_est)
dlognormal_est <- dlognormal_est/sum(dlognormal_est)
Par.Lognormal_est

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## Primas 

# Cálculo de prima base  total
qu <- as.double(Completo.Unique)
prima_total <- sum(qu*dlognormal)

# Cálculo de cuantíl para VaR al 99.9% para cada cobertura
s <- 0 
i <- 1
while(s < 0.999){
  s <- s + dlognormal[i]
  i <- i + 1
}
prima_var.999_t <- qu[i]

# Principio de utilidad cero con función exponencial
alpha <- 0.1
prima_utz <- (1/alpha)*log(sum(exp(alpha*qu)*dlognormal))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# Aplicamos el límite de cobertura a building y total
datos_limit <- entrenamiento
datos_limit$building[which(datos_limit$building>13)] <- 13
datos_limit$total[which(datos_limit$total>21)] <- 21

# Modelo Lognormal para datos de entrenamiento limitados
Par.Lognormal_limit <- fitdistr(datos_limit$total[which(datos_limit$total>0)],"lognormal")
Par.Lognormal_limit
mean_log_limit <- as.double(Par.Lognormal_limit$estimate['meanlog'])
sd_log_limit <- as.double(Par.Lognormal_limit$estimate['sdlog'])
Completo.Unique_limit <- sort(unique(datos_limit$total[which(datos_limit$total>0)]))
dlognormal_lt <- dlnorm(Completo.Unique_limit,mean_log_t,sd_log_t)
dlognormal_lt <- dlognormal_lt/sum(dlognormal_lt)
ggplot(data=NULL,aes(x=Completo.Unique_limit,y=dlognormal_lt))+geom_path()+labs(title='Distribución Lognormal para total limitada',x='Monto de reclamo',y='f(t)')

# Modificación con Deducible, Coaseguro, Límite de cobertura
qu_estrella <- as.double(Completo.Unique_limit)
deducible <- 0.1
qu_estrella <- (1-deducible)*qu_estrella
coaseguro <- 0.95
qu_estrella <- coaseguro*qu_estrella

# Cálculo de prima base  total
prima_total_lt <- sum(qu_estrella*dlognormal_lt)

# Cálculo de cuantíl para VaR al 99.9% para cada cobertura
s <- 0 
i <- 1
while(s < 0.999){
  s <- s + dlognormal_lt[i]
  i <- i + 1
}
prima_var.999_lt <- qu_estrella[i]

# Principio de utilidad cero con función exponencial
alpha <- 0.1
prima_utz_lt <- (1/alpha)*log(sum(exp(alpha*qu_estrella)*dlognormal_lt))




