datos=read.delim("clipboard")
b1=subset(datos,datos$local=="B1")
b2=subset(datos,datos$local=="B2")
b3=subset(datos,datos$local=="B3")
c1=subset(datos,datos$local=="C1")
c2=subset(datos,datos$local=="C2")
c3=subset(datos,datos$local=="C3")


##Pregunta 1 - Kruskal Wallis
#Prueba
datos2=rbind(c2,b2,b1)
library(agricolae)
kruskal(datos2$puntaje4, datos2$local, alpha=0.03, 
        console=T)

##Pregunta 2- Q de Chochran
library(RVAideMemoire)
y3=c(c3$puntaje1,c3$puntaje3,c3$puntaje4)

y3n=c()

for (i in 1:length(y3)){
  if(y3[i]>=11){y3n=c(y3n,1)}else{y3n=c(y3n,0)}
}

bloque3=rep(1:25,3)
trat3=factor(c(rep(1,25),rep(2,25),rep(4,25)))
cochran.qtest(y3n~trat3|bloque3,alpha = 0.03,
              p.method = "none")

##Pregunta 3- Page
library(DescTools)
y3=c(c1$puntaje1, c1$puntaje2, c1$puntaje4)
trat3=trat<-as.factor(c(rep(1,25),rep(2,25),rep(3,25)))
bloq3=rep(1:25,3)
PageTest(y3,trat3,bloq3)
#pesos de menor a mayor

#Pregutna 4 - Jonckeere-Terpstra
#locales C2, C1 y B1

#ingresos: C2<C1<B1, entonces, el rendimiento en la hipotesis nula sería:
#rendimiento: B1<C1<C2

#H0: me1=me2=me3
#H1: me1<me2<me3 

datos1=rbind(c2,c1,b1)

cen<-factor(datos1$local,ordered=T)

library(DescTools)
JonckheereTerpstraTest(datos1$puntaje4,cen,
                       alternative = "i")

##Pregunta 5 -Levene
library(lawstat)
datos5=rbind(c2,b3,c3)
#utilizar la correción por la mediana
levene.test(datos5$puntaje2,datos5$local, location="median")
