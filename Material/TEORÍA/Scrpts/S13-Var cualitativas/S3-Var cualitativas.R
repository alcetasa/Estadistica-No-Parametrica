#######################################
# PRUEBAS PARA VARIABLES CUALITATIVAS,#
# MEDIDAS DE ASOCIACIÓN Y CORRELACIÓN #
#######################################

caso=read.delim("clipboard")

#---------------------------#
#  PRUEBA DE INDEPENDENCIA  #
#---------------------------#
tru=subset(caso, Ciudad==1)
chisq.test(tru$Género, tru$Película)
qchisq(0.03,3,lower.tail=F)
#gl: filas-1, columnas-1
library(fastGraph)
shadeDist(qchisq(0.03,3,lower.tail=F),"dchisq",3,lower.tail = F)


#----------------------------------------#
#  PRUEBA DE HOMOGENEIDAD DE PROPORCIONES  #
#----------------------------------------#
tabla2=table(caso$Ciudad, caso$Lectura)
chisq.test(tabla2)
qchisq(0.03,3,lower.tail=F)

#------------------------
#Prueba Exacta de Fisher
#------------------------
#Del ejemplo del pdf
tabla5<-matrix(c(5,2,1,7),2,2)
fisher.test(tabla5,alternative = "g")

table(caso$Ciudad,caso$Twitter)
tabla6<-matrix(c(111,87,100,124),2,2)
fisher.test(tabla6,alternative="g")

#-----------------
#Prueba de M-H-C
#-----------------
table(caso$Género,caso$Lectura,caso$Ciudad)
mantelhaen.test(caso$Genero,caso$Lectura,caso$Ciudad)

tabla<-array(c(69,75,89,87,39,57,30,72,46,45,66,67,5,3,5,8,3,3,6,8),dim=c(2,2,5))
mantelhaen.test(tabla)

#-------------------
#Medidas Asociación
#--------------------
#V de Cramer
library(vcd)
tabla1<-table(tru$G?nero,tru$Pel?cula)
chisq.test(tabla1)
assocstats(tabla1)

#Coeficiente Phi
library(psych)
tabla2<-table(iqu$G?nero,iqu$Lectura)
phi(tabla2)
assocstats(tabla2)
library(DescTools)
Phi(iqu$G?nero,iqu$Lectura)

#Tener cuidado
library(DescTools)
Assocs(tabla1)

#------------------------
#Correlación de Spearman
#------------------------

cor.test(tum$Facebook,tum$Tlectura,method="s",alternative="t")

#----------------------
#Correlación de Kendall
#----------------------

cor.test(tum$Facebook,tum$Tlectura,method="k",alternative="t")

#---------------------
#Correlación Parcial
#---------------------
library(ppcor)
pcor.test(puc$Facebook,puc$Tlectura,puc$Tcaminata,
          method="k")

