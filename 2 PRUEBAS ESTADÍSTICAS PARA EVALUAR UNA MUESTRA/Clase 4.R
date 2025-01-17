caso<-read.delim("clipboard")
mujer<-subset(Caso,G�nero=="Femenino")
hombres<-subset(Caso,G�nero=="Masculino")

###################
#Prueba de Mc Nemar
###################
table(mujer$Tr�mitesA,mujer$Tr�mitesD)

#Aproximada
mcnemar.test(mujer$Tr�mitesA,mujer$Tr�mitesD)

#Exacto
library(exact2x2)
mcnemar.exact(mujer$Tr�mitesA,mujer$Tr�mitesD)


###########################
#Prueba de Signo o Wilcoxon
###########################

#Prueba de Simetr�a
library(lawstat)
symmetry.test(hombres$WebA-hombres$WebD,boot=F)
library(exactRankTests)
wilcox.exact(hombres$WebD,hombres$WebA,mu=0,alternative="g",
             paired=T)

wilcox.exact(hombres$WebD,hombres$WebA,conf.int = T, 
             conf.level = 0.96, paired=T)

#A manera de pr�ctica
library(BSDA)
SIGN.test(hombres$WebD,hombres$WebA,md=0,alternative="g")
SIGN.test(hombres$WebD,hombres$WebA,alternative="t",conf.level=0.96)
