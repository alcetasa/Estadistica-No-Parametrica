
#*** CASO 1 ***

#Lectura de datos
resta<-read.delim("clipboard")

#######################################
#Preg 2 a-2: Prueba de Anderson-Darling
#######################################

#a)Plantear hipótesis
#H0:Los datos se ajustan a una distribución uniforme
#H1:Los datos no se ajustan a una distribución uniforme

#b)nivel de significació: 0.03

#Gráfico previo:
library(vioplot)
vioplot(resta$Monto,col=3,horizontal=T)

#c) Prueba:
#Esar esta!
library(goftest)
RNGkind(sample.kind="Rounding")
set.seed(30)
goftest::ad.test(resta$Monto,"punif",min(resta$Monto), max(resta$Monto), estimated=F)


library(nortest)
nortest::ad.test(resta$Monto)

#d) pvalor:0<alfa, se rechaza H0
#e)Conclusión: A un nivel de significancia de 0.03 se 
#concluye que no existe evidencia estadística para aceptar H0. 
#Por lo tanto, la afirmación del gerente es correcta.



################################
#Preg 2b: Prueba de Jarque-Bera
################################

#a)Plantear hipótesis
#H0: El monto gastado en la última visita tiene un comportamiento normal
#H0: El monto gastado en la última visita no tiene un comportamiento normal

#b) Nivel de significación: 0.03

#c)Prueba
library(moments)
jarque.test(resta$Monto)

###Otra manera
install.packages("tseries")
library(tseries)
jarque.bera.test(resta$Monto)

#Valor crítico: (para contrastar el estadístico, si se hace manualmente)
qchisq(0.03,2,lower.tail=F)

#Valor crítico gráficamente:
library(fastGraph)
shadeDist(qchisq(0.03,2,lower.tail=F),"dchisq",2,
          lower.tail=F)

#d)Estadpistico: JB = 0.29424
#e)pvalor: 0.8632>alfa, no se acepta H0
#f)Conclusión: El monto gastado en la última visita no tiene un 
#comportamiento normal




#######################################
#Preg 4: Prueba Chi Cuadrado de Pearson
#######################################

#a) Plantear hipótesis:
#H0: pi(i)=pi(i)0, i=1,2...k
#H1: Al menos un plato

#b)Nivel de significación: 0.03

#Gráfico previo
barplot(tabla,col=4)

#c)Prueba:
#Método aproximado:
tabla<-table(resta$Menú)
prob<-c(1,1,1,1)/4
resu<-chisq.test(tabla,p=prob)
resu$expected

#Otra manera: Método exacto
library(RVAideMemoire)
#Pasar la tabla inicial a vector
tabla1<-as.vector(tabla)
multinomial.test(tabla1,prob)

#d) pvalor: 0.3445>alfa, no se rechaza H0
#e)Conclusión:



#######################################
#Preg 5: Prueba Chi Cuadrado de Pearson
#######################################
#a) Plantear hipótesis
#H0:El num de postres que el cliente está dispuesto a comer al mes
#se ajusta a una dist, binomial
#H1:El num de postres que el cliente está dispuesto a comer al mes
#no se ajusta a una dist, binomial

#b)Nivel de significancia: 0.03

#c)Prueba:
tabla2<-table(resta$Postres);tabla2
#Gráfico previo:
plot(0:5,tabla2,type="h",col=6,main="Gráfico de líneas")
#Probabilidades
#prob<-dbinom(dimensión de x, máximo valor de x,prob éxito(si no te lo dan se estima))
prob<-dbinom(0:5,5,0.5)

chisq.test(tabla2,p=prob)
#Comentario: esperados menores a 5

res$expected
#Se sumarán los valores de las frecuencias menor a 5 con la siguiente (posición) columna
#Fijarse en la tabla inicial, esos valores se suman


#La nueva tabla sería:
obs<-c(18,11,9,12)

#Tambipen se suman según la posición hallada antes
probc<-c(prob[1]+prob[2],prob[3],prob[4],prob[5]+prob[6])

#Aplicar nuevamente la prueba, con las correcciones:
chisq.test(obs,p=probc)
#d) Estadístico de prueba:X-squared = 12.848

#Verificar si el p valor está bien, eso dependerá si
#los grados de libertad estás correctos: DF=K-m-1
#m: 0, si el valor de pi ya era conocido (no se estimó)
#m: 1, si el pi se estimó

#e) pvalor:0.004977 < alfa, se rechaza H0


#Valor crítico:
qchisq(0.03,3,lower.tail=F)

#Gráfico valor crítico:
shadeDist(qchisq(0.03,3,lower.tail=F),"dchisq",3,
          lower.tail=F)


#########################
#Preg 6: Prueba de Rachas
#########################
#a) Planter hipótesis:
#H0: Las obs son aleatorias con respecto al número mediano de veces que 
#un cliente va al restaurante
#H1: Las obs no son aleatorias con respecto al número mediano de veces que 
#un cliente va al restaurante

#b) alfa=0.03

#c)Prueba:
library(tseries)
medi<-median(resta$Concurrencia)
runs.test(as.factor(resta$Concurrencia>medi))

#d)Estadístico:= 1.0445
#e) pvalor: 0.2963>alfa, no se rechaza H0

#Gráficamente:
library(fastGraph)
shadeDist(qnorm(c(0.03/2,1-0.03/2),lower.tail=F),"dnorm")

##Pregunta 7 

#Hipótesis:
#H0:Me>=7
#H1:Me<7
#Alfa 0.03


###################
# Prueba de simetría
###################

library(lawstat)
#Poner boot=F, para quitar bootstrap

#Prueba MGG (Usar esta!-Máas poderosa)
symmetry.test(resta$Satisfacción,option="MGG",boot=F)

#Prueba Cabilio-Masaro
symmetry.test(resta$Satisfacción,option="CM",boot=F)

#Estadístico:1.1985
#pvalor: 0.2307>alfa, no se rechaza H0
#Conclusión se cumple la simetría.

#Como se cumple la simetría, uso Wilcoxon

###################
#Prueba de Wilcoxon
###################

wilcox.test(resta$Satisfacción,mu=7,alternative="l")

#Método exacto (Usar este!)
library(exactRankTests)

#wilcox.exact(resta$Satisfacción,mu=valor hipotético,alternative="ltipo de prueba")
wilcox.exact(resta$Satisfacción,mu=7,alternative="l")


# Intervalo de confianza
#Siempre: tipo de prueba ="t"
wilcox.exact(resta$Satisfacción,mu=7,alternative="t",
             conf.int=T,conf.level=0.97)
#estadístico:V = 341
#pvalor:0.01308<alfa, se rechaza H0
#conclusión: mediana menor a 7.


#Por fines prácticos (Solo para probar)

#################
#Prueba de Signos
#################
library(BSDA)
SIGN.test(resta$Satisfacción,md=7,alternative="l")
#Intervalo de confianza
SIGN.test(resta$Satisfacción,alternative="t",
          conf.level = 0.97)

################################
#Preguna  -Prueba de detección de outliers
################################
##Probamos con Dixon
library(outliers)
dixon.test(resta$Monto)
#No se puede realizar la prueba de Dixon 
#porque n>30

#Análisis exploratorio
boxplot(resta$Monto)
#Aparentemente no hay outliers

library(vioplot)
vioplot(resta$Monto,col=4,horizontal=T)

#########################
#Preg 8: Prueba de Grubbs
#########################
library(outliers)

#Two.side=T: superior o inferior
grubbs.test(resta$Monto,type=10,two.sided=T)


#pvalor: 0.3656>alfa, no se rechaza H0
#Conclusión:No podemos afirmar que existen personas con
#gasto por consumo diferenciado.
