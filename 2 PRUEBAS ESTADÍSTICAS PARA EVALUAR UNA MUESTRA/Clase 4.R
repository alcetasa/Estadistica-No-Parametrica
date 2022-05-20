caso<-read.delim("clipboard")
mujer<-subset(Caso,Género=="Femenino")
hombres<-subset(Caso,Género=="Masculino")

###################
#Prueba de Mc Nemar
###################
table(mujer$TrámitesA,mujer$TrámitesD)

#Aproximada
mcnemar.test(mujer$TrámitesA,mujer$TrámitesD)

#Exacto
library(exact2x2)
mcnemar.exact(mujer$TrámitesA,mujer$TrámitesD)


###########################
#Prueba de Signo o Wilcoxon
###########################

#Prueba de Simetría
library(lawstat)
symmetry.test(hombres$WebA-hombres$WebD,boot=F)
library(exactRankTests)
wilcox.exact(hombres$WebD,hombres$WebA,mu=0,alternative="g",
             paired=T)

wilcox.exact(hombres$WebD,hombres$WebA,conf.int = T, 
             conf.level = 0.96, paired=T)

#A manera de práctica
library(BSDA)
SIGN.test(hombres$WebD,hombres$WebA,md=0,alternative="g")
SIGN.test(hombres$WebD,hombres$WebA,alternative="t",conf.level=0.96)
