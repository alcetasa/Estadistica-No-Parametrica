########################
##PRACTICA DIRIGIDA 1###
########################

#Distribución Binomial
#1
#a.La variable de estudio B(60,0.02)
#  x: numero de roturas por mes
#b E(x)= np
   60*0.02 
#  V(x)= npq
   60*0.02*0.98
#c. P(X=x)
dbinom(0,60,0.02) #0.2975531
#d P(X=5)
dbinom(5,60,0.02) #0.005753035
#e P(X>=5) = 1-P(X<5)
1-pbinom(4,60,0.02)
#f P(5<=X<=25)
pbinom(c(4, 25), 60, 0.02)
#g 
qbinom(0.68, 60, 0.02)
#El valor de la variable  que deja a su derecha un 32% de las observaciones es el mismo que deja a su izquierda el 68% restante.
#Por tanto, debemos calcular el cuantil de orden 0.68
#h
rbinom(30, 60, 0.02)
#2. 
qbinom(0.50,12,0.10)
#1-P(x<5)
1-pbinom(4,12,0.10)
pbinom(3,12,0.10)
#grafico:
x <- 0:12
pX <- dbinom(x, size = 12, prob = 0.02)
plot(x, pX, type = "h", ylab = "Pr(X = x)", ylim = c(0, 1), main = "binomial(n = 12, p = 0.02)")
points(x, pX, pch = 21)
#3. La probabilidad de tener una respuesta correcta es de: 1/4
# una respuesta errada es: 3/4 , n=5 preguntas, 20 puntos
# x: la nota obtenida en el examen final
#a.P(x<=10); B(20,3/4)# desapruebes 
pbinom(10,20,0.75)

#probabilidad apruebes: B(20,0.25)

#b.P(x>=11)=1-P(x<11)

1-pbinom(11,20,0.25)

#la maxima nota:
dbinom(20,20,0.25)

#Distribucion Normal

#4. N(110,16)

#a. P(110<x<115)
pnorm(115,110,4)-pnorm(110,110,4)

#b. P(X>105)=1-P(X<=105)= 
1-pnorm(105, mean = 110, sd = 4)

#c. 
pnorm(100, mean = 110, sd = 4)

#d
qnorm(0.80,110,4)

#e
rnorm(15, mean = 110, sd = 4)


#5 Exportacion a Europa
#N(167.85,10.37)

#a Europa:

pnorm(179,167.85,10.37)

#b America

pnorm(195,167.85,10.37)-pnorm(165,167.85,10.37)

#c. n=9
#B(9,0.8588612) # de la pregunta a.
#P(x=7)
dbinom(7,9,1-0.8588612)

#8
#x: peso de las personas
# P(x1+x2+x3+x4>300)=P(x1+x2+x3+x4/4>300/4)=P(x1+x2+x3+x4/4>75)
#estandarizamos con los datos:
1-pnorm(75,71,7/sqrt(4))

#9
#nivel de confianza =0.92 =1- alpha  (alpha 0.08)


#intervalo de confianza de  1-alpha= 0.08; 1-alpha/2=1-0.08/2=0.96
qnorm(0.96)
ICS=32.7+qnorm(0.96)*(12.64/sqrt(65))
ICI=32.7-qnorm(0.96)*(12.64/sqrt(65))

#Tarea
#Resolver los ejercicios, 9b y el ejercicio 6.
#Fecha de entrega, jueves 8:00 am













