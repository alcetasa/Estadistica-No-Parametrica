
################
#Prueba Binomial: Pregunta 1
################
#C?lculo de la cantidad de clientes frecuentes
sum(Caso$Concurrencia>5) #cumplen con la condicion
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
#Prueba de Ajuste a la Multinomial: Pregunta 3
##################################

tabla2<-table(Caso$Men?)
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

############################
#Prueba de Cramer von Mises:
############################
goftest::cvm.test(Caso$Monto,"punif", estimated=T)

#######################
#Prueba de Shapiro-Wilk
#######################
library(vioplot)
vioplot(Caso$Monto,col=4,horizontal = T)
shapiro.test(Caso$Monto)


#####################
#Prueba de D?Agostino
#####################

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

#Comparaci?n de pruebas de normalidad: Potencia de prueba

set.seed(20)
r<-1000
sw<-c()
ad<-c()
pe<-c()
jb<-c()
for (i in 1:r){
  datos<-rexp(300,1/10)
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

#################
#Prueba de Rachas: Pregunta 6
#################
library(tseries)
me<-median(Caso$Concurrencia)
x<-factor(Caso$Concurrencia>me)
tseries::runs.test(x)
library(fastGraph)
shadeDist(qnorm(c(0.03/2,1-0.03/2)),"dnorm")


###############################
#Prueba de Simetr?a: Pregunta 7
################################
plot(table(Caso$Satisfacci?n),col=4)
library(lawstat)
symmetry.test(Caso$Satisfacci?n,boot=F,option="CM")
symmetry.test(Caso$Satisfacci?n,boot=F,option="M")
symmetry.test(Caso$Satisfacci?n,boot=F,option="MGG")

########################################
#Prueba de Signos y Wilcoxon: Pregunta 7
########################################
library(exactRankTests)
wilcox.exact(Caso$Satisfacci?n,mu=7,alternative="l")

#Intervalo de confianza
wilcox.exact(Caso$Satisfacci?n,conf.int=TRUE,conf.level=0.97)

#Con fines pr?cticos
library(BSDA)
SIGN.test(Caso$Satisfacci?n,md=7,alternative="l")
SIGN.test(Caso$Satisfacci?n,conf.level = 0.97)
