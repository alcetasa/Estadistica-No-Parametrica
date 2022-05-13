#Lectura de datos
datos=read.delim("clipboard")
#Subconjunto
c1<-subset(datos,Centro==1)


##################################################
# PRUEBAS PARA EVALUAR K MUESTRAS INDEPENDIENTES #                         #
##################################################

#PRUEBAS PARA COMPARAR UN PARÁMETRO DE LOCACIÓN:

#----------------------#
# Prueba de la mediana #
#----------------------#
#Análisis previo
boxplot(datos$Peso1~datos$Centro,col=3)
tapply(datos$Peso1, datos$Centro, mean)
#Prueba
library(agricolae)
Median.test(datos$Peso1,datos$Centro,alpha = 0.03)

#---------------------------#
# Prueba de Kruskal-Wallis  #
#---------------------------#
#Análisis previo
library(vioplot)
vioplot(c1$Peso1,c2$Peso1,c3$Peso1,c4$Peso1,col=4, horizontal = T)
#Prueba
library(agricolae)
tapply(datos$Peso1, datos$Centro, mean)
kruskal(datos$Peso1, datos$Centro, alpha=0.03, console=T)
#si se le agrega group=F, da intervalos


#----------------------------#
# Prueba de Van Der Waerden  #
#----------------------------#
tapply(datos$Peso1, datos$Centro, mean)
waerden.test(datos$Peso1,datos$Centro,alpha=0.03,console=T)


#-------------------------------#
# Prueba de Jonckheere-Terpstra #
#-------------------------------#
library(DescTools)
cen<-factor(datos$Salón,ordered=T)
JonckheereTerpstraTest(datos$E1,cen,alternative="d")

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

##################################################
# PRUEBAS PARA EVALUAR UNA MUESTRA K RELACIONADA #                         #
##################################################

#PRUEBAS PARA UNA VARIABLE CUALITATIVA BINARIA

#--------------------#
# Prueba Q de Cochran 
#--------------------#
c3= subset(cli, Centro==3)
library(RVAideMemoire)
y=c(c3$Colesterol1,c3$Colesterol2,c3$Colesterol3)
trat=trat<-as.factor(c(rep(1,40),rep(2,40),rep(3,40)))
#trat=factor(rep(1:3,rep(40,3)))
bloq=rep(1:40,3)
cochran.qtest(y~trat|bloq,alpha=0.03,p.method="none")


#PRUEBAS PARA COMPARAR UN PARÁMETRO DE LOCACIÓN

#--------------------#
# Prueba  de Friedman
#--------------------#
library(agricolae)
c1= subset(cli, Centro==1)
y2=c(c1$Peso1,c1$Peso2,c1$Peso3)
trat2=trat<-as.factor(c(rep(1,33),rep(2,33),rep(3,33)))
bloq2=rep(1:33,3)
friedman(bloq2,trat2,y2, alpha=0.03,console=T)

#------------------------------#
# Pregunta 1 - Prueba  de Quade
#------------------------------#
#Es una muestra k relacionada, porque es solo una muestra (salón B)
#k=4
B=subset(datos, Salón=="B")
y=c(B$E1,B$E2,B$E3, B$E4)
trat=trat<-as.factor(c(rep(1,24),rep(2,24),rep(3,24),rep(4,24)))
bloq=rep(1:24,4)
quade.test(y,trat,bloq)
#pvalor<alfa,Se rechaza H0, se realizan comparaciones

#comparaciones:
library(PMCMR)
posthoc.quade.test(y,trat,bloq,p.adj="none")
#      1      2      3     
# 2 0.0036    -      -     
# 3 0.0036 1.0000    -     
# 4 0.9065 0.0236 0.0236

#1-2 | *  
#1-3 | *  
#1-4 | ns 
#2-3 | ns 
#2-4 | *  
#3-4 | *  

com=tapply(y,trat,median)
#de mayor a menor
#2 |   A
#3 |   A
#4 |   B
#1 |   B
#Son las más difíciles porque tienen menor puntaje

#--------------------#
#  Prueba  de Page
#--------------------#
c2= subset(cli, Centro==2)
library(DescTools)
y3=c(c2$Peso1, c2$Peso2, c2$Peso3)
trat3=trat<-as.factor(c(rep(1,32),rep(2,32),rep(3,32)))
bloq3=rep(1:32,3)
PageTest(y3,trat3,bloq3)
#pesos de menor a mayor