################
#PD1 - Solución#
################

feria<-read.delim("clipboard")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library("readxl")
feria<- read_excel("PD1 (1).xlsx")




library(pacman)
p_load("goftest","vioplot","RVAideMemoire2","PoweR",
       "moments","tseries","lawstat","exactRankTests",
       "BSDA","outliers")

#Los datos se encuentran en el archivo PD1.xlsx; brinde todas sus conclusiones
# a un nivel de significación de 0.02.

#Pregunta 1:
#Si la distribución del tiempo de permanencia en la feria es de tipo asimétrico
#positivo indicaría que muchos visitantes permanecen poco tiempo en la feria y
#eso podría perjudicar las ventas.

#El administrador afirma que para los asistentes el primer día a la feria, el tiempo
#de permanencia en la feria no se ajusta a una distribución exponencial. ¿La
# afirmación del administrador es cierta?
#Use una prueba basada en el logaritmo de la función de distribución.

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
