if(!require("repmis")){install.packages("repmis")}
library("repmis")
data <- source_data("https://github.com/JCMO-ITAM/Data4Analysis/blob/master/d4a_allstateclaim_data.csv?raw=true")

dbinom(x,10,0.6)
rpois(x , 50)
rgeom(100, 0.4)

#install.packages("bbmle")
#install.packages("stats4")
library(stats4) # para la función mle
library(bbmle) # para la función mle2


set.seed(123)
rnorm(x ,mean = 10,sd = 2)


set.seed(123)
dbinom(n,100,.6)
rpois(x , 50)
rgeom(100, 0.4)

NegLogLik = function(mu,sigma){-sum(dnorm(x,mu,sigma,log = TRUE))}

EMV1 = mle(NegLogLik, start = list(mu=10, sigma=5))

summary(NegLogLik(50,5))

plot(NegLogLik(10,5))

