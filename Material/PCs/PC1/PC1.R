datos=read.delim("clipboard")


#Pregunta 1 : Kolmogorov-Smirnov
HVO=subset(datos,datos$empresa=="HVO")
#ks.test(variable,"dist teórica",como estimarías los parámetros en base a esa dist)
ks.test(HVO$tiempo,"punif",min(HVO$tiempo),max(HVO$tiempo))


#Pregunta 2 : Rachas
HVO=subset(datos,datos$empresa=="HVO")
x=sort(HVO$tiempo)

quantile(x,prob=seq(0,1,1/4))
#cuantil25=quantile(x,0.25)

library(tseries)
runs.test(as.factor(HVO$tiempo>97))


#Pregunta 3

#Prueba de simetría

APV=subset(datos,datos$empresa=="APV")
library(lawstat)
#Poner boot=F, para quitar bootstrap

#Prueba MGG (Usar esta!-Máas poderosa)
symmetry.test(APV$tiempo,option="MGG",boot=F)

#simetrico


##Prueba de Wilcoxon 

#Método exacto (Usar este!)
library(exactRankTests)

#wilcox.exact(resta$Satisfacción,mu=valor hipotético,alternative="ltipo de prueba")
wilcox.exact(APV$tiempo,mu=107,alternative="t")


# Intervalo de confianza
#Siempre: tipo de prueba ="t"
wilcox.exact(APV$tiempo,alternative="t",conf.int=T,conf.level=0.96)


#Pregunta 4: Binomial
#Se necesita la cantidad de éxitos
##table: para que arroje la cantidad de TRUE or FALSE de la condición dada


table(APV$estrellas!=5)
#Método exacto
#binom.test(cant.éxitos, #obs. totales, val. hipotético(pi0),"g" )
#tipo de prueba: 
#"t" : bilateral(H1:pi!=pi0)
#"l" : unilateral(H1:pi<pi0)
#"g" : unilateral(H1:pi>pi0)
binom.test(17,73,0.55,"t")

#Intervalo de confianza

#TIPO DE PRUEBA: Siempre: "t"
binom.test(17,73,0.55,"t",0.96)



#5
#########################################################
#Prueba Chi Cuadrado de Pearson: Ajuste a la multinomial#
#########################################################
tabla<-table(APV$origen)
prob<-c(2,1,2,6,8)/19

# Método exacto
library(RVAideMemoire)
#Pasar la tabla inicial a vector
tabla1<-as.vector(tabla)
multinomial.test(tabla1,prob)


