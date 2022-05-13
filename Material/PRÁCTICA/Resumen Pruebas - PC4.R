pd4<-read.delim("clipboard")
sas<-subset(pd4,Distrito=="Santa Anita"|
              Distrito=="San Juan de Lurigancho"|
              Distrito=="El Agustino")
sas<-subset(pd4, Distrito!= "Los Olivos")


#######################################
# PRUEBAS PARA VARIABLES CUALITATIVAS #
# Y MEDIDAS DE CORRELACIÓN            #
#######################################
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

#av<-les$Vivienda<25
table(caso$Género,caso$Lectura,caso$Ciudad)
mantelhaen.test(caso$Genero,caso$Lectura,caso$Ciudad)

tabla<-array(c(69,75,89,87,39,57,30,72,46,45,66,67,5,3,5,8,3,3,6,8),dim=c(2,2,5))
mantelhaen.test(tabla)

#MEDIDAS DE CORRELACIÓN
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
          method="k") #kendall

#MEDIDAS DE ASOCIACIÓN
#-----------------------
# Coeficiente de V Cramer
#-----------------------
library(vcd)
tabla<-table(tru$Género, tru$Pelicula)
assocstats(tabla)

#----------------------------------------
# Coeficiente de Contingencia de Pearson
#----------------------------------------

#-----------------
# Coeficiente Phi
#-----------------
library(psych)
tabla2<-table(iqu$Género, iqu$Lectura)
phi(tabla2)
#usar este!
assocstats(tabla2)

########################
#Medidas de Concordancia
########################
#spearman
cor(aya$Opini?n1,aya$Opini?n2,method="s")

#índice Gamma
tabla<-table(aya$Opinión1,aya$Opinión2)
library(vcdExtra)
GKgamma(tabla)

#Índice de Gamma
library(ryouready)
ord.gamma(tabla)
#Índice D de Sommers
ord.somers.d(tabla)
#índice de Tau-b Tau-c
ord.tau(tabla)

library(DescTools)
Assocs(tabla)