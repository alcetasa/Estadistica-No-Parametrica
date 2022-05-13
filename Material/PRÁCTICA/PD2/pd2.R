banco<-read.delim("clipboard")

#Pregunta 1
clasica= subset(banco,Tarjeta=="Clásica")
library(exact2x2)
mcnemar.exact(clasica$Catenov, clasica$Catedic)


#Pregunta 2
plateada=subset(banco,Tarjeta=="Plateada")
library(vioplot)
vioplot(clasica$Deunov,plateada$Deunov,col=3, horizontal=T)

#Prueba
ks.test(clasica$Deunov,plateada$Deunov)


#Pregunta 3




#Pregunta 4
ks.test(clasica$Deudic,plateada$Deudic)

#p-value = 0.9534

library(exactRankTests)
wilcox.exact(clasica$Deudic,plateada$Deudic, alternative = "g", mu=0.1, paired=F)
wilcox.exact(clasica$Deudic,plateada$Deudic, alternative = "t", paired=F,conf.int = T, conf.level = 0.97)

#Pregunta 5
boxplot(tele$Horas~tele$Género)
library(coin)
conover_test(clasica$Deudic~plateada$Deudic)
#pvalor= 

