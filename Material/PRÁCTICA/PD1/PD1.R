################
#PD1 - Solución#
################

feria<-read.delim("clipboard")
#alfa=0.02
#Pregunta 1: Anderson -Darling
primer<-subset(feria,Dia=="Primer")

library(goftest)

goftest::ad.test(primer$Tiempo,"pexp",1/mean(primer$Tiempo), estimated = F)
#pvalor:0.0001546<alfa, rechaza Ho
#Conclusión: Es cierta la afirmación de



#Pregunta 2: Rachas
#alfa=0.02
library(tseries)
runs.test(as.factor(primer$Gasto>100))
#pvalor=0.5074>alfa, no se rechaza H0
#La afirmación del administrador es correcto


#Pregunta 3: Binomial
#alfa=0.02
np<-primer$Nplatos>3
table(np)
binom.test(13,30,0.3,"g")
#pvalor<0.08447,se rechaza H0

#IC:
#binom.test(17,30,0.3,"t",conf.level = 0.98)
binom.test(17,30,0.3,"t",conf.level = 0.98)

#Pregunta 4: Prueba Chi Cuadrado de Pearson -Ajuste a la Multinomial
tabla<-table(primer$Plato)
prob<-c(0.12,0.40,0.15,0.15,0.18)
res<-chisq.test(tabla,p=prob)
res$expected


library(RVAideMemoire)
multinomial.test(as.vector(tabla),prob)

#Pregunta 5: Wilcoxon
library(lawstat)
symmetry.test(primer$Personas,option="MGG",boot=F)

library(exactRankTests)
wilcox.exact(primer$Personas,mu=3,alternative="l")

wilcox.exact(primer$Personas,alternative="t",conf.int = T,
             conf.level = 0.98)
