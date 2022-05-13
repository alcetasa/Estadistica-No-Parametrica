###########################################
# PRUEBAS PARA UNA MUESTRA K RELACIONADA  #
###########################################
cli=read.delim("clipboard")

#PRUEBAS PARA UNA VARIABLE CUALITACTIVA BINARIA

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


#PRUEBAS PARA EVALUAR UN PARÁMETRO DE LOCACIÓN

#--------------------#
# Prueba  de Friedman
#--------------------#
library(agricolae)
c1= subset(cli, Centro==1)
y2=c(c1$Peso1,c1$Peso2,c1$Peso3)
trat2=trat<-as.factor(c(rep(1,33),rep(2,33),rep(3,33)))
bloq2=rep(1:33,3)
friedman(bloq2,trat2,y2, alpha=0.03,console=T)

#--------------------#
#  Prueba  de Quade
#--------------------#
quade.test(y2,trat2,bloq2,p.adj="none")
#comparaciones:
library(PMCMR)
posthoc.quade.test(y2,trat2,bloq2)

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


#-----------------------#
# Prueba de Durbin (pdf)
#-----------------------#
library(agricolae)
estudiantes<-gl(14,4) #repetir del 1 al 14, 4 veces 
definicion<-
  c(1,3,4,6,2,3,5,6,1,2,6,7,4,5,6,7,2,5,7,8,1,4,7,8,3,4,5,8,1
    ,2,3,8,3,6,7,8,1,5,6,8,2,4,6,8,1,2,4,5,2,3,4,7,1,3,5,7)
puntajes<-
  c(3,1,4,2,4,1,3,2,2,4,1,3,2,4,1,3,4,3,2,1,1,2,4,3,1,3,4,2,2
    ,4,1,3,1,2,3,4,1,4,2,3,4,2,1,3,1,3,2,4,4,2,1,3,2,1,4,3)
durbin.test(estudiantes,definicion,puntajes,group=T,console
            =T)

#--------------------------------
#Prueba de Skilling-Mac (del pdf)
#--------------------------------
library(Skillings.Mack)
B <- rep(c(1,2,3,4,5,6,7,8,9),rep(4,9))
G <- rep(c('A','B','C','D'),9)
y <- c(3.2,4.1,3.8,4.2, 3.1,3.9,3.4,4.0, 4.3,3.5,4.6,4.8,
       3.5,3.6,3.9,4.0, 3.6,4.2,3.7,3.9, 4.5,4.7,3.7, NA,NA
       ,4.2,3.4,NA , 4.3,4.6,4.4,4.9, 3.5, NA,3.7, 3.9)
Ski.Mack(y,groups = G,blocks = B)


