#Lectura de datos
datos<-read.delim("clipboard")
options(digits = 8)
#------------------------#
#      PREGUNTA 1
#------------------------#

#Creación de subconjunto
#alfa=0.06
HVO<-subset(datos,empresa=="HVO")
nex<-subset(datos,empresa=="Nex")

#Prueba de rachas
#Prueba de Wald-Wolfowitz
library(DescTools)
RunsTest(HVO$tiempov, nex$tiempov, alternative="two.sided")
#pvalor=0.100>0.06, acepta H0

#Prueba de Mann-Whitney
library(exactRankTests)
wilcox.exact(HVO$tiempov,  nex$tiempov, alternative = "t", mu=0, paired=F)
#Se acepta H0

#------------------------#
#      PREGUNTA 2
#------------------------#
options(digits = 8)
APV<-subset(datos, empresa=="APV")
#Primero realizamos prueba de MGG
library(lawstat)

#Si se cumple el supuesto:

library(exactRankTests)
wilcox.exact(APV$tiempos, APV$tiempoj, mu=16, alternative="g", paired="T")

#-Intervalo de confianza
wilcox.exact(APV$tiempos, APV$tiempoj, alternative="t",conf.int=T ,paired=T, conf.level=0.94)


#------------------------#
#      PREGUNTA 3
#------------------------#
DP<-subset(datos, empresa=="DP")
#Prueba de McNemar
library(exact2x2)
mcnemar.exact(DP$gusta2, DP$gusta4)


------------------------#
#      PREGUNTA 4
#------------------------#

HVO<-subset(datos, empresa=="HVO")
DP<-subset(datos, empresa=="DP")
#Se prueba  Kolmogorov-Smirnov:
ks.test(HVO$tiempos,DP$tiempos)

#Si se cumple H0, se prueba Mann-Whitney:
library(exactRankTests)
wilcox.exact(HVO$tiempos,DP$tiempos, alternative = "t", mu=0, paired=F)

library(exactRankTests)
ansari.exact(HVO$tiempos,DP$tiempos, alternative = "t")




#------------------------#
#      PREGUNTA 5
#------------------------#

library(PairedData)
#Primero se prueba simetría
symmetry.test(HVO$tiempoj-HVO$tiempod, boot=F)

#Si existe simetría:
grambsch.Var.test(HVO$tiempoj, HVO$tiempod, alternative="l")


