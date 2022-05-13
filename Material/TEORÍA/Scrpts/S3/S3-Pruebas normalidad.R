#Del pdf 2

###################################################################
#Prueba Chi Cuadrado de Pearson: Ajuste a una distribución teórica#
###################################################################

#Frecuencias observadas
datos<-c(34,25,11,7,3)

#Probabilidades
prob<-c(dpois(0:3,1),1-ppois(3,1))
chisq.test(datos,p=prob)

#Otra manera:
library(vcd)
x<-c(rep(0,34),rep(1,25),rep(2,11),rep(3,7),rep(4,3))
y<-table(x)
res<-goodfit(y,type="poisson",method="MinChisq")
summary(res)




##Ejemplo extra

############################
#Prueba de Anderson-Darling#
############################

set.seed(40)
#Generando datos 
tiempo<-rexp(50,1/3)

library(vioplot)
vioplot(tiempo,col=2,horizontal=T)


#Prueba:
install.packages("goftest")
library(goftest)
set.seed(50)

#ad.test(variable,"dist. teórica",estimated=T)
ad.test(tiempo,"pexp",estimated=T)



## Ejemplo extra 2
set.seed(99)
estatura<-rnorm(80,170,5)

#Gráfico previo
library(vioplot)
vioplot(estatura,col=3,horizontal=T)

#Usar esta!
#ad.test del paquete goftest
goftest::ad.test(estatura,"pnorm",mean(estatura), sd(estatura),estimated=F)

#ad. test del paquete nortest
library(nortest)
nortest::ad.test(estatura)

##################
#Prueba D'Agostino
##################
install.packages("PoweR")
library(PoweR)
statcompute(6, estatura)

######################
#Prueba de Jarque-Bera
######################
install.packages("moments")
library(moments)
jarque.test(estatura)

#Otra manera:
library(normtest)
jb.norm.test(estatura)
ajb.norm.test(estatura)
