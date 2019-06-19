install.packages("tidyverse")
install.packages("lubridate")
install.packages("MASS")
install.packages("fitdistrplus")
library("lubridate")
library("tidyverse")
library("ggplot2")
library("MASS")
library("fitdistrplus")

datos <- read_csv("DanishInsurance_MultivarData_Full.csv")
datos$date <- mdy(datos$date)
head(datos)

datos_entrenamiento <- datos[1:517,]
datos_prueba <- datos[518:833,]

tail(datos_entrenamiento)

#Por año
siniestros_anio <- datos_entrenamiento %>%
                  group_by(year(datos_entrenamiento$date))
                 
siniestros_1980 <- siniestros_anio[siniestros_anio$`year(datos_entrenamiento$date)`== 1980, ]
siniestros_1981 <- siniestros_anio[siniestros_anio$`year(datos_entrenamiento$date)`== 1981, ]
siniestros_1982 <- siniestros_anio[siniestros_anio$`year(datos_entrenamiento$date)`== 1982, ]


#Por semestre
ps_1980 <- siniestros_1980[month(siniestros_1980$date) <= 6,]
ss_1980 <- siniestros_1980[month(siniestros_1980$date) > 6, ]
ps_1981 <- siniestros_1981[month(siniestros_1981$date) <= 6,]
ss_1981 <- siniestros_1981[month(siniestros_1981$date) > 6,]
ps_1982 <- siniestros_1982[month(siniestros_1982$date) <= 6,]
ps_1981 <- siniestros_1982[month(siniestros_1982$date) > 6,]

siniestros_semana <- datos_entrenamiento %>%
                      group_by(week(datos_entrenamiento$date)) 
                      

ns7 <- semanas_7 %>%
      group_by(year(date)) %>%
      summarise(n())

n1 <- semana_1 %>%
        group_by(year(date)) %>%
          summarise(n())

s_1980 <- siniestros_1980 %>%
          group_by(week(date)) %>%
           summarise(n())
  
s_1981 <- siniestros_1981 %>%
          group_by(week(date)) %>%
            summarise(n())

s_1982 <- siniestros_1982 %>%
            group_by(week(date)) %>%
               summarise(n())

DF <- rbind(s_1980, s_1981, s_1982)
secuencia <- c(min(frecuencia):max(frecuencia))
frecuencia <- DF$`n()`
frecuencia <- c(frecuencia, 0)

siniestros_semana
pois_total <- fitdistr(frecuencia, "poisson", method ="mle")
geom_total <- fitdistr(frecuencia, "geometric", method = "mle")
nbin_total <- fitdistr(frecuencia, "negative binomial")

hist(frecuencia, freq = FALSE, xlim = c(0, 9), ylim = c(0:1/2), xlab = "frecuencia")
par(new = TRUE)
plot(dpois(secuencia, pois_total$estimate), type = "l", xlim = c(0,9), ylim = c(0: 1/2), col = "green", xlab = " ", ylab = " ")
par(new = TRUE)
plot(dgeom(secuencia, geom_total$estimate), type = "l", xlim = c(0,9), ylim = c(0:1/2), col ="red", xlab = "", ylab =  " ")
par(new=TRUE)
plot(dnbinom(x = secuencia, size = nbin_total$estimate[[1]], mu = nbin_total$estimate[[2]]), type = "p", xlim = c(0,9), ylim = c(0:1/2), col = "blue", xlab = "", ylab ="")


#distribucion agregada anual

secuencia_2 <- c(0:400)
plot(dpois(secuencia_2, (52*pois_total$estimate)), type = "l", col = "gold", xlim = c(0,400), ylab = "density", main = "Distribución Agregada Anual")

siniestros_anio %>%
  group_by(`year(datos_entrenamiento$date)`) %>%
  summarise(n()) 


#Modelos para severidad

severidad <- datos_entrenamiento$total
secuencia_severidad <- c(min(severidad): max(severidad))


gamm_est <- fitdistr(severidad, densfun = "gamma", method = "BFGS")
chi <- fitdistr(severidad, densfun = "chi-squared", start  = list(0), method = "Brent", lower = 0.01, upper = 400)
normal_est <- fitdistr(severidad, densfun = "normal", method = "mle")
logit <- fitdistr(severidad, densfun = "logistic")
t_est<-fitdistr(severidad, densfun="t",method="SANN")


hist(severidad, freq = FALSE , xlim = c(0,270), ylim = c(0, 1/5), breaks = 50 )
par(new = TRUE)
plot(dgamma(secuencia_severidad, shape = gamm_est$estimate[[1]], rate = gamm_est$estimate[[2]]), type = "l", xlim = c(0,270), ylim =c(0, 1/5), col = "green", xlab ="", ylab = "")
par(new = TRUE)
plot(dchisq(secuencia_severidad, chi$estimate), type = "l", xlim = c(0,270), ylim = c(0, 1/5), col = "blue", xlab ="", ylab = "")
par(new = TRUE)
plot(dt(secuencia_severidad, df = t_est$estimate[[3]], ncp = t_est$estimate[[1]]),  type ="l",  xlim = c(0,270), ylim = c(0,1/5), col = "orange", xlab = "", ylab="")

#Modelos para severidad sin dato atipico
severidad_no_out <- datos_entrenamiento %>%
                    filter(total < 200)

severidad_no_out <- severidad_no_out$total
secuencia_severidad_no_out <- c(min(severidad_no_out): max(severidad_no_out))

gamm_est_no_out <- fitdistr(severidad_no_out, densfun = "gamma", method = "BFGS")
chi_no_out <- fitdistr(severidad_no_out, densfun = "chi-squared", start  = list(0), method = "Brent", lower = 0.01, upper = 400)
normal_est_no_out <- fitdistr(severidad_no_out, densfun = "normal", method = "mle")
logit_no_out <- fitdistr(severidad_no_out, densfun = "logistic")
t_est_no_out <- fitdistr(severidad_no_out, densfun="t",method="SANN")


hist(severidad_no_out, freq = FALSE , xlim = c(0,80), ylim = c(0, 1/2), breaks = 50, main = "Severidad sin dato atípico" )
par(new = TRUE)
plot(dgamma(secuencia_severidad_no_out, shape = gamm_est_no_out$estimate[[1]], rate = gamm_est_no_out$estimate[[2]]), type = "l", xlim = c(0,80), ylim =c(0, 1/2), col = "green", xlab ="", ylab = "")
par(new = TRUE)
plot(dchisq(secuencia_severidad_no_out, chi_no_out$estimate), type = "l", xlim = c(0,80), ylim = c(0, 1/2), col = "blue", xlab ="", ylab = "")
par(new = TRUE)
plot(dt(secuencia_severidad_no_out, df = t_est_no_out$estimate[[3]], ncp = t_est_no_out$estimate[[1]]),  type ="l",  xlim = c(0,80), ylim = c(0,1/2), col = "orange", xlab = "", ylab="")



#Agregación de riesgos
#Simulación estocastica
k <- 10000
S <- c(rep(0, k))

for (i in 1:k){
  N <- rpois(30, 52*pois_total$estimate)
  N_barra <- mean(N)
  N_barra_techo <- as.integer(N_barra) 

  Sim_gamma <- rgamma(N_barra_techo, shape = gamm_est$estimate[[1]], rate = gamm_est$estimate[[2]])
  S[i] <- sum(Sim_gamma)
}

S_gorro <- mean(S)


#Modelo extendido
siniestros_1983 <- datos_prueba[year(datos_prueba$date) == 1983,] 

s_1983 <- siniestros_1983 %>%
            group_by(week(date)) %>%
            summarise(n())

DF_extendido <- rbind(DF, s_1983)
frecuencia_extendida <- DF_extendido$`n()`
frecuencia_extendida <- c(frecuencia_extendida, 0)
secuencia_extendidad <- c(min(frecuencia_extendida): max(frecuencia_extendida))

pois_total_extendido <- fitdistr(frecuencia_extendida, "poisson", method = "mle")

datos_extendidos <- rbind(datos_entrenamiento, siniestros_1983)
severidad_extendida <- datos_extendidos$total
secuencia_severidad_extendida <- c(min(severidad_extendida): max(severidad_extendida))

gamm_est_extendido <- fitdistr(severidad_extendida, "gamma", method = "BFGS")

S_ext <- c(rep(0, k))

for (i in 1:k){
  N_ext <- rpois(30, 52*pois_total_extendido$estimate)
  N_barra_ext <- mean(N_ext)
  N_barra_techo_ext <- as.integer(N_barra_ext) 
  
  Sim_gamma_ext <- rgamma(N_barra_techo_ext, shape = gamm_est_extendido$estimate[[1]], rate = gamm_est_extendido$estimate[[2]])
  S_ext[i] <- sum(Sim_gamma_ext)
}

S_gorro_ext <- mean(S_ext)


#Boxplots

datos_entrenamiento %>%
  ggplot(aes(x=as.factor(year(date)), y = total)) + geom_boxplot()
  
datos_prueba %>%
  ggplot(aes(x= as.factor(year(date)), y = total)) + geom_boxplot()


entrena_prueba <- rbind(datos_entrenamiento, datos_prueba)

entrena_prueba %>%
  ggplot(aes(x = as.factor(year(date)), y = total)) + geom_boxplot()

entrena_prueba_no_out <- entrena_prueba %>%
                          filter(total < 200)

entrena_prueba_no_out %>%
  ggplot(aes(x = as.factor(year(date)), y = total)) + geom_boxplot()




#Estadisticas

datos_entrenamiento %>%
  group_by(year(date)) %>%
  summarise(n())

datos_prueba %>%
  group_by(year(date)) %>%
  summarise(n())

datos %>%
  group_by(year(date)) %>%
  summarise(sum(total))

datos %>%
  group_by(year(date)) %>%
  summarise()

datos_prueba %>%
  summarise(n())

datos_entrenamiento %>%
  summarise(n())


#Prima de Riesgo Agregada Anual
#Base

N_pura <- rpois(1000, 52*pois_total$estimate)
S_pura <- rgamma(1000, shape = gamm_est$estimate[[1]], rate = gamm_est$estimate[[2]])

media_pura_p <- mean(N_pura)
media_pura_g <- mean(S_pura)

prima_pura <- media_pura_g*media_pura_p


#TVar alpha = 0.999 (1983)

est_normal_var <- fitdistr(S, "normal")
est_t_var <- fitdistr(S, "t")
secuencia_pvar <- c(min(S): max(S))

hist(S, freq = FALSE, xlim = c(min(S), max(S)), ylim = c(0, 8/1000))
par(new = TRUE)
plot(x = secuencia_pvar, y =dnorm(secuencia_pvar, mean = est_normal_var$estimate[[1]], sd = est_normal_var$estimate[[2]]),    type = "l", col = "blue", xlim = c(min(S), max(S)), ylim = c(0, 8/1000))
par(new = TRUE)
plot(x = secuencia_pvar ,y = dt(secuencia_pvar, df = est_t_var$estimate[[3]], ncp = est_t_var$estimate[[1]]), type = "l", col = "red", xlim = c(min(S), max(S)), ylim = c(0, 8/1000))


hist(S_ext, freq = FALSE, xlim = c(min(S_ext), max(S_ext)), ylim = c(0, 1/100))
par(new = TRUE)
plot(x = secuencia_pvar_ext, y =dnorm(secuencia_pvar_ext, mean = est_normal_var_ext$estimate[[1]], sd = est_normal_var_ext$estimate[[2]]), type = "l", col = "blue", xlim = c(min(S_ext), max(S_ext)), ylim = c(0,1/100), xlab="", ylab="")


dist_pvar <- dnorm(secuencia_pvar, mean = est_normal_var$estimate[[1]], sd = est_normal_var$estimate[[2]])
prima_var <- qnorm(0.999, mean = est_normal_var$estimate[[1]], sd = est_normal_var$estimate[[2]])


#TVaR alpha 0.999 (1984)
est_normal_var_ext <- fitdistr(S_ext, "normal")
secuencia_pvar_ext <- c(min(S_ext): max(S_ext))

hist(S, freq = FALSE, xlim = c(min(S), max(S)), ylim = c(0, 8/1000))
par(new = TRUE)
plot(x = secuencia_pvar, y =dnorm(secuencia_pvar, mean = est_normal_var$estimate[[1]], sd = est_normal_var$estimate[[2]]),    type = "l", col = "blue", xlim = c(min(S), max(S)), ylim = c(0, 8/1000), xlab ="", ylab ="")
par(new = TRUE)
plot(x = secuencia_pvar ,y = dt(secuencia_pvar, df = est_t_var$estimate[[3]], ncp = est_t_var$estimate[[1]]), type = "l", col = "red", xlim = c(min(S), max(S)), ylim = c(0, 8/1000))

dist_pvar_ext <- dnorm(secuencia_pvar_ext, mean = est_normal_var_ext$estimate[[1]], sd = est_normal_var_ext$estimate[[2]])
prima_var_ext <- qnorm(0.999, mean = est_normal_var_ext$estimate[[1]], sd = est_normal_var_ext$estimate[[2]])

#Utilidad cero
#Consideramos principio exponencial

f_putilidad_norm <- function(alpha, mu, sd){
  (1/alpha)*(mu*alpha + ((alpha^2)*(sd^2))/2)
}


prima_utilidad_1 <- f_putilidad_norm(0.1, est_normal_var$estimate[[1]], est_normal_var$estimate[[2]])
prima_utilidad_2 <- f_putilidad_norm(0.05, est_normal_var$estimate[[1]], est_normal_var$estimate[[2]])

prima_utilidad_1_ext <- f_putilidad_norm(0.1, est_normal_var_ext$estimate[[1]], est_normal_var_ext$estimate[[2]])
prima_utilidad_2_ext <- f_putilidad_norm(0.05, est_normal_var_ext$estimate[[1]], est_normal_var_ext$estimate[[2]])
