
#Ejercicio 1:
#Binomial
#la funcion chicuadrado, permite trabajar con datos agrupados
#para eso tenemos la funcion table 
#revisar si hay valores esperados menos a 5.

library(readxl)
asesoria2<-read_excel("PD01.xlsx")
summary(asesoria2)
#CÃ¡lculo de la cantidad de platos de fondo maximo:
sum(asesoria2$Nplatos>3)
tabla<-c(47,(120-47))
barplot(tabla,col=3)
#Prueba de hipÃ³tesis
binom.test(47,73,0.3,"g")
#p-value = 1.386e-09 < 0.05 se rechaza la ho.

#Ejercicio 2:
#Prueba de Kolmogorov-Smirnov
plot.ecdf(asesoria2$Tiempo,col=3)
hist(asesoria2$Tiempo,col=2)
boxplot(asesoria2$Tiempo,col=2,horizontal = T)
library(vioplot)
vioplot(asesoria2$Tiempo,col=2,horizontal=T)

ks.test(asesoria2$Tiempo, "pexp", rate=asesoria2$Tiempo)

#Ejercicio 3:
#Prueba de Anderson-Darling:
library(goftest)
set.seed(10)
goftest::ad.test(asesoria2$Tiempo, "pexp", estimated=T)

#Ejercicio 4:

#Prueba de Ajuste a la Multinomial

tabla<-table(asesoria2$Tiempo)
barplot(tabla,col=5)

relacion<-c(12,40,15,18,15)
prob<-c(0.12,0.40,0.15,0.18,0.15)

chisq.test(tabla,p=prob)
res2<-chisq.test(tabla,p=relacion,rescale.p = T)
res2$expected # mayores a 5

library(RVAideMemoire)
multinomial.test(as.vector(tabla),prob)

######################################nota
#################################################################
#Prueba Chi Cuadrado de Pearson-Ajuste a una distribución teórica: Pregunta 5
#################################################################nota###
tabla3<-table(caso1$Postres)
plot(tabla3,col=4)
proba<-dbinom(0:5,5,0.5)
res4<-chisq.test(tabla3,p=proba)

res4$expected

tabla4<-c(sum(tabla3[1:2]),tabla3[3:4],sum(tabla3[5:6]))
proba4<-c(sum(proba[1:2]),proba[3:4],sum(proba[5:6]))
chisq.test(tabla4,p=proba4)

qchisq(0.03,3,lower.tail = F)
library(fastGraph)
shadeDist(qchisq(0.03,3,lower.tail = F),"dchisq",3,lower.tail=F)




