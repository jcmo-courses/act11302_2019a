#Tarea1
#David Jimenez Cooper
#C.U.: 155626

library(repmis)

data <- source_data("https://github.com/JCMO-ITAM/Data4Analysis/blob/master/d4a_allstateclaim_data.csv?raw=true")
datos <- as.data.frame(data)
datos <- sample(data$Claim_Amount,as.integer(nrow(data)*0.1))

J <- length(datos)
n0 <- length(which(datos==0))
theta_0_est=n0/J

theta0 <- seq(.001, .999, .001)
lik_theta0 <- dbeta(theta0, 1 + n0, 1 + (J-n0) )

plot(theta0, lik_theta0, xlim=c(0,1), ylim=c(0, 1.25 * max(lik_theta0,1.6)),type = "l", ylab= "Verosimilitud", lty = 3,xlab= "theta_0", las=1, main="",lwd=2,cex.lab=1.5, cex.main=1.5, col = "darkblue")

#Se crea la variable indicadora sobre los datos observados (0 si no hay sinistro, 1 o.c.)
Y <- c()
for (i in 1:length(datos)){
  if(datos[i]!=0){
    Y <- c(Y,1)
  }else{
    Y <- c(Y,0)
  }
}

#Con base en los datos se crean k muestras de tamaño n
k=125
n=50
muestras=data.frame(sample(Y,n))
for (i in 1:(k)){
  muestra=sample(Y,n)
  muestras=data.frame(muestras,muestra)
}
muestras=as.matrix(muestras)

#Suma de cada muestra 
vsuma=c()
for (i in 1:k) {
  binx=sum(muestras[,i])
  vsuma=c(vsuma,binx)
}

#Distribucion Binomial
##Se toman las k muestras y se suman para simular k X´s con distribucion binomial(n,p)

f_p_ver <- function(p,n,k,datos){
  aux=1
  f=1
  for (i in 1:k){
    aux=choose(n,datos[i])*aux
  }
  f=f*aux*p^(sum(datos))*(1-p)^((k*n)-sum(datos))
  return(f)
}
vp=seq(0,1,0.01)
est_p=vp[which.max(f_p_ver(vp,n,k,vsuma))]

plot(vp,f_p_ver(vp,n,k,vsuma),type="l",main="Verosimilitud Binomial",ylab="funcion verosimilitud",xlab="valores del parametro p")
abline(v=est_p,col="red")

#Distribucion Poisson
##Se retoman las k muestras de tamaño n de los ensayos bernoullis como si cada muestra fuera un determinado peroido de tiempo
##Tambien se retoma la suma de cada muestra para simular las X´s con distribucion poisson

f_l_ver <- function(lamda,datos){
  aux=1
  f=1
  for (i in 1:k){
    aux=factorial(datos[i])*aux
  }
  f=f*(1/aux)*exp(-lamda*k)*lamda^(sum(datos))
}
maximo=vsuma[which.max(vsuma)]
vl=seq(0,maximo,0.01)
est_l=vl[which.max(f_l_ver(vl,vsuma))]

plot(vl,f_l_ver(vl,vsuma),type="l",main="Verosimilitud Poisson",ylab="funcion verosimilitud",xlab="valores del parametro lamda")
abline(v=est_l,col="red")

#Distribucion Geometrica
##Se retoman las k muestras de tamaño n, pero se cuenta el numero de relizaciones hasta el primer exito (siniestro)

vcontar=c()
for (i in 1:k){
  cont=1
  if (sum(muestras[,i]!=0)){
    while (muestras[,i][cont]==0){
      cont=cont+1
    }
  }else{cont=n}
  vcontar=c(vcontar,cont)
}

f_pg_ver <- function(p,datos){
  f=(1-p)^(sum(datos)-k)*p^k
}

est_pg=vp[which.max(f_pg_ver(vp,vcontar))]

plot(vp,f_pg_ver(vp,vcontar),type="l",main="Verosimilitud Geometrica",ylab="funcion verosimilitud",xlab="valores del parametro p")
abline(v=est_pg,col="red")

#Complemento
#Considerando que 99% del portafolio tiene una exposicion de 0.7

#Propuesta para la distribucion binomial: p=0.7
#Propuesta para la distribucion poisson: lamda=0.7n=35
#propuesta para la distribucion geometrica: p=0.7

#Consolidacion de informacion

fp_ver_cons <- function(p,n,k,datos){
  aux=1
  f=1
  for (i in 1:k){
    aux=choose(n,datos[i])*aux
  }
  f=f*aux*(p*0.7)^(sum(datos))*((1-p)*0.3)^((k*n)-sum(datos))
  return(f)
}

est_cp=vp[which.max(fp_ver_cons(vp,n,k,vsuma))]

plot(vp,fp_ver_cons(vp,n,k,vsuma),type="l",main="Verosimilitud Binomial",ylab="funcion verosimilitud",xlab="valores del parametro p")
abline(v=est_cp,col="red")
print(paste("Estimador verosimilitud de binomial con datos: ",est_p))
print(paste("Estimador verosimilitud de binomial con informacion consolidada: ",est_cp))


fl_ver_cons <- function(lamda,datos){
  aux=1
  f=1
  for (i in 1:k){
    aux=factorial(datos[i])*aux
  }
  f=f*(1/aux)*exp(-lamda*k)*lamda^(sum(datos))
  f=f*exp(-35)*35
}
maximo=vsuma[which.max(vsuma)]
vl=seq(0,maximo,0.01)
est_cl=vl[which.max(fl_ver_cons(vl,vsuma))]

plot(vl,fl_ver_cons(vl,vsuma),type="l",main="Verosimilitud Poisson",ylab="funcion verosimilitud",xlab="valores del parametro lamda")
abline(v=est_cl,col="red")

print(paste("Estimador verosimilitud de poisson con datos: ",est_l))
print(paste("Estimador verosimilitud de poisson con informacion consolidada: ",est_cl))


fpg_ver_cons <- function(p,datos){
  f=((1-p)*0.3)^(sum(datos)-k)*(p*0.7)^k
}

est_cpg=vp[which.max(fpg_ver_cons(vp,vcontar))]

plot(vp,fpg_ver_cons(vp,vcontar),type="l",main="Verosimilitud Geometrica",ylab="funcion verosimilitud",xlab="valores del parametro p")
abline(v=est_cpg,col="red")

print(paste("Estimador verosimilitud de geometrica con datos: ",est_pg))
print(paste("Estimador verosimilitud de geometrica con informacion consolidada: ",est_cpg))

