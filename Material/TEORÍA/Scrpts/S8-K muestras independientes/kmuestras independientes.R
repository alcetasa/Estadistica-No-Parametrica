###################################################
# PRUEBAS PARA EVALUAR K MUESTRAS INDEPENDIENTES  #
###################################################

datos=read.delim("clipboard")
c1<-subset(datos,Centro==1)
c2<-subset(datos,Centro==2)
c3<-subset(datos,Centro==3)
c4<-subset(datos,Centro==4)

#PRUEBAS PARA COMPARAR UN PARÁMETRO DE LOCACIÓN:
#----------------------#
# Prueba de la mediana #
#----------------------#
#H0: me1=me2=me3=me4
#H1: Al menos una media es diferente a las demás
#Análisis previo
boxplot(datos$Peso1~datos$Centro,col=3)
tapply(datos$Peso1, datos$Centro, mean)
#Prueba
library(agricolae)
Median.test(datos$Peso1,datos$Centro,alpha = 0.03)

#Grafico
library(fastGraph)
shadeDist(qchisq(0.03,3,lower.tail = F),"dchisq",3,
          lower.tail = F)

#---------------------------#
# Prueba de Kruskal-Wallis  #
#---------------------------#
#Análisis previo
library(vioplot)
vioplot(c1$Peso1,c2$Peso1,c3$Peso1,c4$Peso1,col=4, horizontal = T)
#Prueba
library(agricolae)
kruskal(datos$Peso1, datos$Centro, alpha=0.03, console=T)
#si se le agrega group=F, da intervalos

#----------------------------#
# Prueba de Van Der Waerden  #
#----------------------------#
waerden.test(datos$Peso1,datos$Centro,alpha=0.03,console=T)

#-------------------------------#
# Prueba de Jonckheere-Terpstra #
#-------------------------------#
library(clinfun)
jonckheere.test(datos$Peso2,datos$Centro, alternative="i")
#"d" es decreciente, "i":incremento

library(clinfun)
jonckheere.test(clinica$Peso2,clinica$Centro,
                alternative="i")
library(DescTools)
cen<-factor(datos$Centro,ordered=T)
JonckheereTerpstraTest(datos$Peso2,cen,alternative="i")



#PRUEBAS PARA COMPARAR UN PARÁMETRO DE ESCALA:

#---------------------#
# Prueba de Levene    #
#---------------------#
library(lawstat)
#utilizar la correción por la mediana
levene.test(datos$Peso1,datos$Centro)

#Grafico
library(fastGraph)
qf(0.05,4-1,143-4,lower.tail =F) 
shadeDist(qf(0.03,4-1,143-4,lower.tail =F) ,"df",3,139,
          lower.tail = F)

#------------------------------#
# Prueba de Fligner-Killeen    #
#------------------------------#
fligner.test(datos$Peso1, datos$Centro)

#--------------------#
# Prueba de Klotz    #
#--------------------#
library(coin)
klotz_test(datos$Peso1~factor(datos$Centro))

#--------------------#
# Prueba de Conover  #
#--------------------#
library(coin)
conover_test(datos$Peso1~factor(datos$Centro))

#Prueba de comparación
M1<-c1$Peso1
M2<-c2$Peso1
M3<-c3$Peso1
M4<-c4$Peso1
n1<-length(M1)
n2<-length(M2)
n3<-length(M3)
n4<-length(M4)
n<-n1+n2+n3+n4
xbar1<-mean(M1)
xbar2<-mean(M2)
xbar3<-mean(M3)
xbar4<-mean(M4)
M1c<-(M1-xbar1)^2 
M2c<-(M2-xbar2)^2 
M3c<-(M3-xbar3)^2
M4c<-(M4-xbar4)^2
rdc<-rank(c(M1c,M2c,M3c,M4c))
rdc2<-rdc^2
T1<-sum(rdc2[1:n1])
T2<-sum(rdc2[(n1+1):(n1+n2)])
T3<-sum(rdc2[(n1+n2+1):(n1+n2+n3)])
T4<-sum(rdc2[(n1+n2+n3+1):n])
Tbar<-(T1+T2+T3+T4)/n
Vt<-(sum(rdc^4)-n*Tbar^2)/(n-1)
Tcon<-((T1^2/n1+T2^2/n2+T3^2/n3+T4^2/n4)-n*Tbar^2)/Vt
1-pchisq(Tcon,4-1)

#diferencia 1-2
dif12<-abs(T1/n1-T2/n2)
dif12
#valor crítico
vc12<-qt(0.985,n-3)*sqrt((1/n1+1/n2)*Vt*((n-1-Tcon)/(n-4))) 
vc12


#diferencia 1-3
dif13<-abs(T1/n1-T3/n3)
dif13
#valor crítico
vc13<-qt(0.985,n-3)*sqrt((1/n1+1/n3)*Vt*((n-1-Tcon)/(n-4))) 
vc13

#diferencia 1-4
dif14<-abs(T1/n1-T4/n4)
dif14
#valor crítico
vc14<-qt(0.985,n-3)*sqrt((1/n1+1/n4)*Vt*((n-1-Tcon)/(n-4))) 
vc14

#diferencia 2-3
dif23<-abs(T2/n2-T3/n3)
dif23
#valor critico
vc23<-qt(0.985,n-3)*sqrt((1/n2+1/n3)*Vt*((n-1-Tcon)/(n-4))) 
vc23

#diferencia 2-4
dif24<-abs(T2/n2-T4/n4)
dif24
#valor critico
vc24<-qt(0.985,n-3)*sqrt((1/n2+1/n4)*Vt*((n-1-Tcon)/(n-4))) 
vc24

#diferencia 3-4
dif34<-abs(T2/n3-T4/n4)
dif34
#valor critico
vc34<-qt(0.985,n-3)*sqrt((1/n3+1/n4)*Vt*((n-1-Tcon)/(n-4))) 
vc34
   