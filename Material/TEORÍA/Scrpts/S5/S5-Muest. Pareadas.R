#*** CASO 2 ****

#Lectura de datos
tele<-read.delim("clipboard")


#--Pruebas para una muestra pareada--

##################################
# Pregunta 1 -PRUEBA DE MC NEMAR #
##################################

#Se crea un subconjunto de lo que te piden, es este caso: género femenino
fem<-subset(tele,Género=="Femenino")

#a) Plantear hipótesis:
#H0: La entrevista no tuvo un efecto significativo
#H1: La entrevista tuvo un efecto significativo

#b)Nivel de significancia:0.04

#c) Prueba:

#Método aproximado con correccióm
mcnemar.test(fem$TrámitesA, fem$TrámitesD, correct=T)

#Método exacto
library(exact2x2)
mcnemar.exact(fem$TrámitesA, fem$TrámitesD)

#d)pvalor (método exacto): 0.07031>alfa, no se rechaza H0
#e)Conclusión: No existe sificiente evidencia estadística para rechazar Ho.
#Por lo tanto, no se afirma que la entrevista tuvo un efecto significativo.

#Nota: odds ratio: cociente entre las cantidades de cambio(b y c)
tabla<-table(fem$TrámitesA, fem$TrámitesD)
#no-sí (7)
#sí-no (1)
#odds:7/1=7


###########################################
# Pregunta 2 -PRUEBA DE SIGNOS O WILCOXON #
###########################################

mas<-subset(tele,Género=="Masculino")

#Primero realizamos prueba de MGG

#Ho:As=0
#H1!=0
#alfa=0.04
library(lawstat)
symmetry.test(mas$WebA-mas$WebD, boot=F)
#pvalor=0.05151 >alfa, no se rechaza H0
#Conclusión: se cumple el supuesto de simetría

#Se puede usar prueba de Wilcoxon
#H0:Me(D9=Me(A) ---- Me(D)-Me(A)=0
#H1:Me(D)<Me(A) ----  Me(D)-Me(A)>0


#Prueba de Wilcoxon
library(exactRankTests)
wilcox.exact(mas$WebD, mas$WebA, mu=0, alternative="g", paired="T")

#Intervalo de confianza
wilcox.exact(mas$WebD, mas$WebA, alternative="t", conf.int=T ,paired=T, conf.level=0.96)

#Estadístico: V = 141.5
#pvalor=0.08962>alfa, no se rechaza H0
#Conclusión: No podemos afirmar que el puntaje mediano de la actitud hacia la web
#se ha incrementado.


#Prueba de Signos (Por fines prácticos)
library(BSDA)
SIGN.test(mas$WebD, mas$WebA, mu=0, alternative="g")
#p-valor = 0.1316

#Intervalo de confianza
SIGN.test(mas$WebD, mas$WebA, alternative="t",
          conf.level = 0.96)

###########################################
# Pregunta 3 -PRUEBA DE GRAMBSCH          #
###########################################
library(PairedData)
#Primero se prueba simetría
symmetry.test(mas$WebA-mas$WebD, boot=F)
#Existe simetría

#Hipótesis:
#H0:teta(antes)<=teta(después)
#H1:teta(antes)>teta (después)

#alfa=0.04

#Prueba
grambsch.Var.test(mas$WebA, mas$WebD,alternative="g")

#estadístico:z = 0.97213
#pvalor = 0.97213> alfa, no se rechaza H0

#########################################################

#--Pruebas para dos muestras independientes ---

###############################################
# Pregunta 1 -PRUEBA DE KOLMMOGOROV-SMIRNOV   #
###############################################

#Hipótesis:
#H0: F(xh)= F(xm)
#H0: F(xh)!= F(xm)

#alfa=0.04

#Análisis gráfico
plot.ecdf(fem$Ingresos, col=4)
plot.ecdf(mas$Ingresos, col=6, add=T )
library(vioplot)
vioplot(fem$Ingresos, mas$Ingresos,col=3, horizontal=T)


#Prueba
ks.test(mas$Ingresos, fem$Ingresos)

#pvalor=0.38>alfa, no se rechaza H0


###############################################
# Pregunta 2 -PRUEBA DE MANN WHITNEY          #
###############################################
#Gráficamente
library(vioplot)
vioplot(mas$Ingresos,fem$Ingresos,col=6, horizontal=T)

#Hipótesis:
#H0:Me(M)-Me(H)=0
#H1:Me(M)-Me(H)>10


#alfa= 0.04

#Prueba
library(exactRankTests)
wilcox.exact(fem$Ingresos, mas$Ingresos, alternative = "g", mu=10, paired=F)

#pvalor=0.7957>alfa, no se rechaza H0 
#La afirmación es falsa

#INtervalo de confianza para la diferencia de medianas
wilcox.exact(fem$Ingresos, mas$Ingresos, alternative = "t", paired=F, conf.level = 0.96)


###############################################
# Pregunta 3 -PRUEBA DE MANN FLIGNER          #
###############################################

#Prueba de forma:
ks.test(mas$Familiaridad, fem$Familiaridad)
#pvalor=0.001875<alfa, se rechaza H0

#Prueba de asimetría
symmetry.test(mas$Familiaridad, boot=F)

symmetry.test(fem$Familiaridad, boot=F)
#ambos son simétricos

###Prueba de Fligner
library(RVAideMemoire)
#help(fp.test)
#delta = 2valor hipotético"
fp.test(mas$Familiaridad, fem$Familiaridad,  alternative = "two.sided")
#pvalor=1.596e-07>alfa,se rechaza H0


###############################################
# Pregunta 4 -PRUEBA DE CONOVER               #
###############################################
boxplot(tele$Horas~tele$Género)
library(coin)
conover_test(tele$Horas~as.factor(tele$Género))
#pvalor= 0.7222>alfa, no se rechaza H0


#Otra manera
###############################################
# Pregunta 4 -PRUEBA DE MOSES                 #
###############################################
library(DescTools)
MosesTest(fem$Horas,mas$Horas)
#pvslor=0.8713>alfa, no se rechaza H0


##Otra manera
###############################################
# Pregunta 4 -PRUEBA DE MOOD                #
###############################################

mood.test(fem$Horas,mas$Horas, alternative="t")
#p-value = 0.4955>alfa, no se rechaza H0

#valores criticos d ela prueba z
#c(qnorm(0.04/2),qnorm(1-0.04/2))



###############################
# Pregunta 5 -PRUEBA DE MOOD  #
###############################
#Probando posición
library(exactRankTests)
wilcox.exact(mas$Ingresos, fem$Ingresos, alternative = "t", mu=0, paired=F)
#pvalor=0.572>alfa
###Se cumple igualdad de posición

#Probando forma:
ks.test(mas$Ingresos, fem$Ingresos)
#pvalor: 0.3818>alfa
##Se cumple el supuesto de igualdad de forma


##Se pasa a aplicar prueba de mood
#H1: teta(Hombres)>teta(Hombres)
mood.test(mas$Ingresos,fem$Ingresos, alternative = "greater")
#pvalor=0.291>alfa, no se rechaza H0

###############################################
# Pregunta 5 - ANSARI BRADLEY                #
###############################################

#Método aproximado
ansari.test(mas$Ingresos,fem$Ingresos, alternative = "greater")


##Método exacto
library(exactRankTests)
ansari.exact(mas$Ingresos,fem$Ingresos, alternative = "g")
#pvalor=0.8404>alfa

###############################################
# Pregunta 5 - SIEGEL TUKEY                #
###############################################

library(DescTools)
SiegelTukeyTest(mas$Ingresos,fem$Ingresos, alternative = "g")
#pavlor=0.8352


#USAR ESTA!!!
library(PMCMRplus)
siegelTukeyTest(mas$Ingresos,fem$Ingresos,alternative="g")
#p-value = 0.1374


###############################################
# Pregunta 6- PRUEBA DE LEPAGE                #
###############################################
help(pLepage)
library(NSM3)
#Método aproximado: Usar este! 
pLepage(mas$Ingresos,fem$Ingresos, method="Asymptotic")

