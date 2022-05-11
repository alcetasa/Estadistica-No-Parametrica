
################
#Prueba Binomial: Pregunta 1
################
#C?lculo de la cantidad de clientes frecuentes

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library("readxl")
Caso<- read_excel("Caso 1.xlsx")

sum(Caso$Concurrencia>5)
tabla<-c(19,31)
barplot(tabla,col=4)
#Prueba de hip?tesis
binom.test(19,50,0.4,"g")
#Intervalo de confianza
binom.test(19,50,conf.level = 0.97)

###############################
#Prueba de Kolmogorov-Smirnov : Pregunta 2 a.1
###############################
plot.ecdf(Caso$Monto,col=3)
hist(Caso$Monto,col=2)
boxplot(Caso$Monto,col=4,horizontal = T)
library(vioplot)
vioplot(Caso$Monto,col=5,horizontal=T)

ks.test(Caso$Monto,"punif",min(Caso$Monto),max(Caso$Monto))

##################################
#Prueba de Ajuste a la Multinomial: Pregunta
##################################

tabla2<-table(Caso$Menú)
barplot(tabla2,col=5)


rela<-c(2,3,4,1)
prob<-rela/sum(rela)
chisq.test(tabla2,p=prob)
res2<-chisq.test(tabla2,p=rela,rescale.p = T)
res2$expected

library(RVAideMemoire)
multinomial.test(as.vector(tabla2),prob)

#################################################################
#Prueba Chi Cuadrado de Pearson-Ajuste a una distribuci?n te?rica: Pregunta 5
#################################################################
tabla3<-table(Caso$Postres)
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

###################################
#Prueba de Anderson-Darling: Pregunta 2 a2
#######################################

hist(Caso$Monto)
library(goftest)
set.seed(30)
goftest::ad.test(Caso$Monto,"punif", estimated=T)

#######################################
#Prueba de Cramer von Mises:
#######################################

set.seed(30)
goftest::cvm.test(Caso$Monto,"punif", estimated=T)

#######################################
#Prueba de Shapiro Wilk
#######################################

shapiro.test(Caso$Monto)

library(vioplot)
vioplot((Caso$Monto),col=4,horizontal = T)



library(PoweR)
statcomput(24,Caso$Monto)

JarqueBera(7,Caso$Monto)


#####################
#Prueba de D´Agostino
#####################
library(PoweR)
statcompute(24, Caso$Monto)

library(fBasics)
dagoTest(Caso$Monto)

######################
#Prueba de Jarque-Bera
######################
library(moments)
jarque.test(Caso$Monto)
library(fBasics)
jarqueberaTest(Caso$Monto)

######################
#Prueba de Lilliefors
######################
library(nortest)
lillie.test(Caso$Monto)



set.seed(20)
r<-1000
sw<-c()
ad<-c()
pe<-c()
jb<-c()
for (i in 1:r){
  datos<-rexp(30,1/10)
  sw[i]<-shapiro.test(datos)$p.value
  ad[i]<-ad.test(datos)$p.value
  pe[i]<-pearson.test(datos)$p.value
  jb[i]<-jarque.test(datos)$p.value
}
psw<-mean(ifelse(sw<0.05,1,0))
psw
pad<-mean(ifelse(ad<0.05,1,0))
pad
ppe<-mean(ifelse(pe<0.05,1,0))
ppe
pjb<-mean(ifelse(jb<0.05,1,0))
pjb
