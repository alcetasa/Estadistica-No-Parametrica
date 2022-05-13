###*** CASO 1 ***###

#Lectura de datos
resta<-read.delim("clipboard")

#Solución pregunta (1)

#################
#Prueba Binomial#
#################

#a) Plantear hipótesis
#H0: pi<=0.4
#H1: pi>0.4

#b) Nivel de significación: 0.03


#c) Prueba

#Se necesita la cantidad de éxitos
#En el caso: Será éxito si va al menos 6 veces
#al mes al restaurante

##table: para que arroje la cantidad de TRUE or FALSE de la condición dada
table(resta$Concurrencia>5)

#Método exacto
#binom.test(cant.éxitos, obs. totales, val. hipotético(pi0),"g" )
#tipo de prueba: 
#"t" : bilateral(H1:pi!=pi0)
#"l" : unilateral(H1:pi<pi0)
#"g" : unilateral(H1:pi>pi0)
binom.test(19,50,0.4,"g")

#d) p valor:0,6644 > alfa, no se rechaza H0 
#e) Conclusión: 
#A un nivel de significación de 0.03, no existe suficiente evidencia
#estadística para rechar H0.No podemos afirmar que la proporción de clientes
#con alta fidelidad siempre ha sido mayor a 0.4.


#Intervalo de confianza

binom.test(19,50,0.4,"t",0.97)

#Métodos aproximados
#Con corrección
prop.test(19,50,0.4,"g")
prop.test(19,50,0.4,"t",0.97)
#Sin corrección
prop.test(19,50,0.4,"g",correct=F)




#Solución pregunta (2)

#Parte a1

##############################
#Prueba de Kolmogorov-Smirnov#
##############################


#a) Plantear hipótesis
#H0: monto gastado se distribuye como una uniforme
#H1: monto gastado no se distribuye como una uniforme


#b) Nivel de significación: 0.03

#c)Prueba:
library(vioplot)
par(mfrow=c(2,1))
boxplot(resta$Monto,col=4)
plot(density(resta$Monto),col=4)

#Gráfico de violín:
par(mfrow=c(1,1))
vioplot(resta$Monto,col=4,horizontal=T)
#Nota: por las curvas es probable de que los datos no se ajusten a una uniforme

#Gráfica de la dist. acumulada empírica
plot.ecdf(resta$Monto,col=3)


#ks.test(variable,"dist teórica",como estimarías los parámetros en base a esa dist)
ks.test(resta$Monto,"punif",min(resta$Monto),
        max(resta$Monto))

#d) Estadístico: D=0.261
#e)pvalor= 0.002<alfa, Se rechaza H0
#f)Conclusión: A un ni vel de significación de 0.03 existe suficiente
#evidencia estadística para rechazar H0. Por lo tanto, lo que mencionó el gerente era cierto.



#Pregunta b)

#########################################################
#Prueba Chi Cuadrado de Pearson: Ajuste a la multinomial#
#########################################################

#a) Plantear hipótesis
#H0: La preferencia por los tipos de menú está en relación 2:3:4:1
#H1: La preferencia por los tipos de menú no está en relación 2:3:4:1

#b) Nivel de significación: 0.03


#c) Prueba
#Valores observados 
tabla1<-table(resta$Menú)
#Relación dada
rela<-c(2,3,4,1)
#Probabilidades
prob<-rela/sum(rela)

#En términos de probabilidad
chisq.test(tabla1,p=prob)

#En términos de relación
chisq.test(tabla1,p=rela,rescale.p = T)


#¿Cómo saber si hay valores esperados menores a 5?
res<-chisq.test(tabla1,p=rela,rescale.p = T)
#Valores esperados
res$expected

#Valor crítico  = 8.947287
#qchisq(1-nivel de sign.,#categorías-1,lower.tail = F)
qchisq(0.03,3,lower.tail = F)

#Otra manera para hallar el valor crítico: Gráficamente: 
install.packages("fastGraph")
library(fastGraph)
shadeDist(qchisq(0.03,3,lower.tail = F),"dchisq",3,lower.tail = F)


#d) Estadístico: X-squared = 15.317
#e)pvalor= 0.0016 <alfa, Se rechaza H0
#f)Conclusión: A un ni vel de significación de 0.03 existe suficiente
#evidencia estadística para rechazar H0. Por lo tanto, podemos afirmar que la
#preferencia por los tipos de menú está en relación 2,3,4,1.

