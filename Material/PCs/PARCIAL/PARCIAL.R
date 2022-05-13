datos=read.delim("clipboard")
help(pLepage)
library(NSM3)

ca<-subset(datos,empresa=="Ca")
ub=subset(datos,empresa=="Ub")

library(DescTools)

RunsTest(ca$ingreso2,ub$ingreso2, alternative="two.sided")

#Método aproximado: Usar este! 
pLepage(ca$ingreso2,ub$ingreso2, method="Asymptotic")
################################

options(digits = 8)
scipen=8888
library(lawstat)
symmetry.test(ub$ingreso1,option="MGG",boot=F)

library(exactRankTests)

#wilcox.exact(resta$Satisfacción,mu=valor hipotético,alternative="ltipo de prueba")
wilcox.exact(ub$ingreso1,mu=125,alternative="g")


# Intervalo de confianza
#Siempre: tipo de prueba ="t"
wilcox.exact(ub$ingreso1,,alternative="t",
             conf.int=T,conf.level=0.98)
##########################################################33

be=subset(datos,empresa=="Be")

tabla<-table(be$combustible)
rela<-c(5,4,7)
prob=rela/sum(rela)
# Método exacto
library(RVAideMemoire)
#Pasar la tabla inicial a vector
tabla1<-as.vector(tabla)
multinomial.test(tabla1,prob)
###################################################3
di=subset(datos,empresa=="Di")
  

library(outliers)

#Two.side=T: superior o inferior
grubbs.test(di$ingreso4,type=11,two.sided=T)

###############################################



library(exact2x2)
mcnemar.exact(di$papeleta1, di$papeleta2)
