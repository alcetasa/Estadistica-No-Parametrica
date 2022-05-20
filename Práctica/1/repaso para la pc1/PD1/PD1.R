################
#PD1 - Solución#
################

feria<-read.delim("clipboard")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library("readxl")
feria<- read_excel("PD1 (1).xlsx")

str(feria)


library(pacman)
p_load("goftest","vioplot","RVAideMemoire2","PoweR",
       "moments","tseries","lawstat","exactRankTests",
       "BSDA","outliers","EnvStats")

#Los datos se encuentran en el archivo PD1.xlsx; brinde todas sus conclusiones
# a un nivel de significación de 0.02.

#Pregunta 1:
#Si la distribución del tiempo de permanencia en la feria es de tipo asimétrico
#positivo indicaría que muchos visitantes permanecen poco tiempo en la feria y
#eso podría perjudicar las ventas.

#El administrador afirma que para los asistentes el primer día a la feria, el tiempo
#de permanencia en la feria no se ajusta a una distribución exponencial. ¿La
# afirmación del administrador es cierta?
#Use una prueba basada en el logaritmo de la función de distribución.

#alfa=0.02

#Pregunta 1: Anderson -Darling
primer<-subset(feria,Dia=="Primer")

library(goftest)

goftest::ad.test(primer$Tiempo,"pexp",1/mean(primer$Tiempo), estimated = F)

#pvalor:0.0001546<alfa, rechaza Ho
#Conclusión: Es cierta la afirmación de



#Pregunta 2: Rachas

#2. Si el gasto de cada persona es superior a 100 soles se puede concluir que el
#consumo del visitante a la feria es aceptable.
#El administrador afirma que, para las personas que asistieron el primer día a la
#feria la muestra de visitantes con respecto al criterio de ser aceptable fue
#seleccionada aleatoriamente.

#alfa=0.02
library(tseries)
tseries::runs.test(as.factor(primer$Gasto>100))

#pvalor=0.5074>alfa, no se rechaza H0
#La afirmación del administrador es correcto



#Pregunta 3: Binomial

#3. Se comenta que una persona puede comer más de tres platos de fondo en su
#visita a la feria gastronómica. Para las personas que asistieron el primer día a la
#feria, el administrador afirma que la proporción de visitantes a la feria que
#comieron más de 3 platos de fondo es superior al 30%.

#También se le pide que estime un intervalo del 98% de confianza para la
#proporción de visitantes a la feria que comieron a lo más tres platos.


#alfa=0.02
np<-primer$Nplatos>3
table(np)
#binom.test(#TRUE ,TOTALNP,PROPORCION,"g")
binom.test(13,30,0.3,"g")

#pvalor<0.08447,se rechaza H0

#IC:

#binom.test(17,30,0.3,"t",conf.level = 0.98)
binom.test(17,30,0.3,"t",conf.level = 0.98)

#Pregunta 4: Prueba Chi Cuadrado de Pearson -Ajuste a la Multinomial


#4. En la novena edición de la Feria Gastronómica la preferencia por los platos fue
#la siguiente: Anticucho 12%, Chancho al palo 40%, Lomo Saltado 15%, Pollo a
#la brasa 18%, y otros platos 15%.
#El administrador afirma que, para las personas que asistieron el primer día a la
#feria en la novena edición, la distribución de la preferencia de los platos no es la
#misma que la de los asistentes en la octava edición.

tabla<-table(primer$Plato)
prob<-c(0.12,0.40,0.15,0.15,0.18)
res<-chisq.test(tabla,p=prob)
res$expected


library(RVAideMemoire)
multinomial.test(as.vector(tabla),prob)

#Pregunta 5: Wilcoxon

#5. Una feria gastronómica es considerada una feria familiar, se espera que las
#personas vayan acompañadas de familiares o amigos. Para las personas que
#asistieron el primer día a la feria, el administrador afirma que el número mediano
#de personas que acompañaron al entrevistado es inferior a 3.
#También se solicita un intervalo de confianza al 98% para la mediana.

library(lawstat)
symmetry.test(primer$Personas,option="MGG",boot=F)

library(exactRankTests)
wilcox.exact(primer$Personas,mu=3,alternative="l")

wilcox.exact(primer$Personas,alternative="t",conf.int = T,
             conf.level = 0.98)
