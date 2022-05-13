datos=read.delim("clipboard")

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
 

#------------------------------#
#   Pregunta 2 - Q de Cochran
#------------------------------#
A=subset(datos, Salón=="A")
library(RVAideMemoire)
y2=c(A$CG1,A$CG2,A$CG3, A$CG4, A$CG5)
trat2=factor(rep(1:5,rep(34,5)))
bloq2=rep(1:34,5)
cochran.qtest(y2~trat2|bloq2,alpha=0.04,p.method="none")
#p-value = 0.02902 < alfa, Se rechaza H0
#Comparaciones
#      1      2       3      4
#2 0.049042   -       -      -
#3 0.332306 0.3877    -      -
#4 0.004425 0.4807 0.05737   -
#5 0.189247 0.8036 0.80362 0.2863

#1-2 | ns
#1-3 | ns
#1-4 | *
#1-5 | ns
#2-3 | ns
#2-4 | ns
#2-5 | ns
#3-4 | ns
#3-5 | ns
#4-5 | ns

#cuadro resumen (segun los valores anteriores de mayor a menor)
#1 |   A
#3 |   A
#5 |   A B
#2 |   A B
#4 |     B

#Las más fáciles estan primera (mayor proporcion de correctas)
#1,3,5,2

#--------------------------------#
# Pregunta 3 - Prueba de Terpstra
#--------------------------------#
#Grupos diferentes
library(DescTools)
cen<-factor(datos$Salón,ordered=T)
JonckheereTerpstraTest(datos$E1,cen,alternative="d")

#--------------------------------#
# Pregunta 4 - Prueba de Conover
#--------------------------------#
library(coin)
ABC=subset(datos,Salón!="D")
conover_test(ABC$E3~as.factor(ABC$Salón))
##revisar

M1<-A$E3
M2<-B$E3
M3<-C$E3
n1<-length(M1)
n2<-length(M2)
n3<-length(M3)

n<-n1+n2+n3
xbar1<-mean(M1)
xbar2<-mean(M2)
xbar3<-mean(M3)
M1c<-(M1-xbar1)^2 
M2c<-(M2-xbar2)^2 
M3c<-(M3-xbar3)^2

rdc<-rank(c(M1c,M2c,M3c))
rdc2<-rdc^2
T1<-sum(rdc2[1:n1])
T2<-sum(rdc2[(n1+1):(n1+n2)])
T3<-sum(rdc2[(n1+n2+1):(n1+n2+n3)])
Tbar<-(T1+T2+T3)/n
Vt<-(sum(rdc^4)-n*Tbar^2)/(n-1)
Tcon<-((T1^2/n1+T2^2/n2+T3^2/n3)-n*Tbar^2)/Vt
1-pchisq(Tcon,3-1)
#1 y 2
dif<-abs(T1/n1-T2/n2)
vc<-qt(1-0.04/2,n-3)*sqrt((1/n1+1/n2)*Vt*((n-1-Tcon)/(n-3))) 
#1 y 3
dif<-abs(T1/n1-T3/n3)
vc<-qt(1-0.04/2,n-3)*sqrt((1/n1+1/n3)*Vt*((n-1-Tcon)/(n-3))) 
#2 y 3
dif<-abs(T2/n2-T3/n3)
vc<-qt(1-0.04/2,n-3)*sqrt((1/n2+1/n3)*Vt*((n-1-Tcon)/(n-3)))



#sE ORDENA SEGUN T
sa=subset(datosnp,Salón=="A")
sb=subset(datosnp,Salón=="B")
sc=subset(datosnp,Salón=="C")
compara=function(dato1,dato2,dato3,alfa){
  n1<-length(dato1)
  n2<-length(dato2)
  n3<-length(dato3)
  n<-n1+n2+n3
  xbar1<-mean(dato1)
  xbar2<-mean(dato2)
  xbar3<-mean(dato3)
  M1c<-(dato1-xbar1)^2 
  M2c<-(dato2-xbar2)^2 
  M3c<-(dato3-xbar3)^2
  rdc<-rank(c(M1c,M2c,M3c))
  rdc2<-rdc^2
  T1<-sum(rdc2[1:n1])
  T2<-sum(rdc2[(n1+1):(n1+n2)])
  T3<-sum(rdc2[(n1+n2+1):n])
  Tbar<-(T1+T2+T3)/n
  Vt<-(sum(rdc^4)-n*Tbar^2)/(n-1)
  Tcon<-((T1^2/n1+T2^2/n2+T3^2/n3)-n*Tbar^2)/Vt
  
  cat("DIF 1-2= ",abs(T1/n1-T2/n2)," VC 1-2= ",qt((1+alfa)/2,n-3)*sqrt((1/n1+1/n2)*Vt*((n-1-Tcon)/(n-3)))," \n")
  cat("DIF 1-3= ",abs(T1/n1-T3/n3)," VC 1-3= ",qt((1+alfa)/2,n-3)*sqrt((1/n1+1/n3)*Vt*((n-1-Tcon)/(n-3)))," \n")
  cat("DIF 2-3= ",abs(T2/n2-T3/n3)," VC 2-3= ",qt((1+alfa)/2,n-3)*sqrt((1/n2+1/n3)*Vt*((n-1-Tcon)/(n-3)))," \n")
  
}
compara(sa$E3,sb$E3,sc$E3,0.96)

#--------------------------------#
# Pregunta 5 - Prueba de 
#--------------------------------#

#H0:me1=me2=me3=me4
#H1:me4<me3<me2<me4

library(DescTools)

y=c(d$E4,d$E3,d$E2,d$E1)
bloque=rep(1:34,4)
trat=factor(rep(1:4,rep(34,4)))

PageTest(y~trat|bloque)

#p-value = 0.7704