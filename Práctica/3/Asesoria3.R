#==================================================================#
#                      Solución Asesoria 3                         #
#==================================================================#
#PC2- Asesoria 3
Exporta<-read.delim("clipboard")
head(Exporta)
VariedadH<-subset(Exporta,Variedad==1)
VariedadF<-subset(Exporta,Variedad==2)
#------------------------------------------------------------------#
#  EJERCICIO 1                                                     #
#------------------------------------------------------------------#
#Sirve para probar si la hipotesis nula de que las medianas de la poblacion son iguales 
#Las variables deben ser al menos cuantitativas y de escala de intervalo.

#Ho: Med(A) = Med(D)
#Ha: Med(A) =! Med(D)

library(BSDA)
Grasa1<-VariedadH[,6]
Grasa2<-VariedadH[,7]
SIGN.test(Grasa1,Grasa2)
library(EnvStats)


#------------------------------------------------------------------#
#  EJERCICIO 2                                                     #
#------------------------------------------------------------------#
#Parte a
#-----------------
#La prueba de rachas: se usa para probar la aleatoriedad de una serie de observaciones
#cuando cada observaciónpuede ser asignada a una o dos categorias.
#Puede ser menor igual a un valor determinado o mayor a este valor.
#Ejemplo: 
#los productos que cumplen las especificaciones de calidad, seran clasificados como
#adecuados para la venta ( + ) y no clasificados para la venta ( - ).
#Y asi se pueden dar clasificaciones a juicio de investigadores.

#ho: la secuencia de observaciones es aleatoria
#ha: la secuencia de observaciones no es aleatoria

FibraH<-VariedadH[,3]
FibraF<-VariedadF[,3]
library(DescTools)
RunsTest(FibraH,FibraF)

#Parte b
#-----------------
mood.test(FibraH,FibraF) #mediana


#------------------------------------------------------------------#
#  EJERCICIO 3                                                     #
#------------------------------------------------------------------#
#----Forma1---------------
PesoH<-VariedadH[,2]
library(tseries)
quantile(PesoH,0.55)
runs.test(as.factor(PesoH>255.05))

#------------------------------------------------------------------#
#  EJERCICIO 4                                                     #
#------------------------------------------------------------------#

Ho: los pesos de las paltas se ajustan a una distribucion normal.
Ha: los pesos de las paltas no se ajustan a una distribucion normal.

library(vioplot)
vioplot(Exporta$Peso,col=4,horizontal = T)
shapiro.test(Exporta$Peso)

#------------------------------------------------------------------#
#  EJERCICIO 5                                                     #
#------------------------------------------------------------------#

hist(Exporta$Satifaccion)
library(e1071)
library(moments)
library(lawstat)
#Ho se presenta simetria en los datos

?symmetry.test
symmetry.test(Exporta)
symmetry.test(Exporta$Satifaccion,boot=F,option="MGG")
symmetry.test(Exporta$Satifaccion,boot=F,option="M")
symmetry.test(Exporta$Satifaccion,boot=F,option="MGG")

########################################
#Prueba de Wilcoxon:
########################################

library(exactRankTests)
wilcox.exact(Exporta$Satifaccion,mu=7,alternative="l")

