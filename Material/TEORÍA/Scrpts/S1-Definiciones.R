###################################
#   SEMANA 1 - INTRODUCCIÓN       #
###################################

##Funciones de densidad y de distribución acumulada
x<-seq(-4,4,0.1)  #Genera una secuencia de -4 a 4 con espaciamiento de 0.1)
y1<-pnorm(x) #P(X<x)
y2<-dnorm(x) #f(x)   #P(X=a)
plot(x,y1,type="l",col="blue",ylab="")
points(x,y2,type="l", col="red",ylab="")

#Otra forma:
plot(x,y1,type="l", col="blue",ylab="")
lines(x,y2,type="l",col="red",ylab="")


##Principales distribuciones de probabilidades
install.packages("smoothmest")
#Divide la pantalla gráfica en 2 filas y 2 columnas
par(mfrow=c(2,2))
x1<-seq(0,1,0.01)
y1<-dunif(x1)
plot(x1,y1,type= "l",main="Uniforme",xlab="x", ylab="",
     col="red")
x2<-seq(0,4,0.01)
y2<-dexp(x2)
plot(x2,y2,type= "l", main="Exponencial", xlab="x",
     ylab="", col="red")

library(smoothmest)
x3<-seq(-4,4,0.01)
y3<- ddoublex (x3)
plot(x3,y3,type= "l", main="Laplace", xlab="x", ylab="",
     col="red")
x4<-seq(-6,6,0.01)
y4<-dcauchy(x4)
plot(x4,y4,type= "l", main="Cauchy", xlab="x", ylab="",
     col="red")



#TLC: Comprobación de que a pesar de 
#terner mayor muestra, la variable mantiene su comportamiento
#Lo que se asemeja a una normal es el estadístico (media, etc..)
par(mfrow=c(2,1))
poblacion=rexp(5000,1/4)
muestra=sample(poblacion,80)
library(vioplot)
vioplot(poblacion,col=3,horizontal=T)
vioplot(muestra,col=3,horizontal=T)

## Distribución binomial
par(mfrow=c(1,2))
x <- 0:10
pX <- dbinom(x, size = 10, prob = 0.3)
plot(x, pX, type = "h", ylab = "Pr(X = x)", ylim = c(0,
                                                     0.4), main = "binomial(n = 10, p = 0.5)")
points(x, pX, pch = 21)
x <- 0:20
pX <- dbinom(x, size = 20, prob = 0.5)
plot(x, pX, type = "h", ylab = "P(X = x)", ylim = c(0,
                                                    0.2), main = "binomial(n = 20, p = 0.5)")
points(x, pX, pch = 21)


#Distribución Acumulada Empírica
set.seed(20)
datos<-round(sort(rnorm(10,5,1)),1)
par(mfrow=c(1,1))
plot.ecdf(datos)
y<-pnorm(datos,5,1)
points(datos,y,type="l",col=6)

