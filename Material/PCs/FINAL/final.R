##################
#  EXAMEN FINAL  #
##################
datos=read.delim("clipboard")

azu=subset(datos,provincia=="Azuay")
bol=subset(datos,provincia=="Bolivar")
cañ=subset(datos,provincia=="Cañar")
elo=subset(datos,provincia=="El Oro")
esm=subset(datos,provincia=="Esmeraldas")
gua=subset(datos,provincia=="Guayas")
loj=subset(datos,provincia=="Loja")
man=subset(datos,provincia=="Manabí")
pic=subset(datos,provincia=="Pichincha")
san=subset(datos,provincia=="Santa Elena")

##Pregunta 1
#H0: me1=me2=me3=me4
#H1: Al menos una media es diferente a las demás

mp1=rbind(pic,gua,azu,loj)

library(agricolae)
waerden.test(mp1$monto5, mp1$provincia, alpha=0.02,
             console=T)

##Pregunta 2
ciu2<-subset(datos,provincia=="Cañar"|
              provincia=="Esmeraldas"|
              provincia=="Bolivar"|
              provincia=="Guayas")
tabla2=table(ciu2$genero, ciu2$provincia)
chisq.test(tabla2)
qchisq(0.02,3,lower.tail=F)

ciu2=rbind(cañ,esm,bol,gua)


##Pregunta 3
#H0:pi1=pi2=pi3=pi4
#H1:al menos una proporción es diferente

library(RVAideMemoire)

y1=c(esm$monto1,esm$monto3,esm$monto4,esm$monto5)
y=y1>180
bloque1=rep(1:166,4)
trat1=factor(rep(1:4,rep(166,4)))
cochran.qtest(y~trat1|bloque1,alpha = 0.02,
              p.method = "none")

##Pregunta 4
ciu4=rbind(azu,bol)
tabla4=table(ciu4$genero,ciu4$provincia)
#tabla6<-matrix(c(60,67,75,79),2,2)
fisher.test(tabla4,alternative="l")
#fisher.test(tabla6,alternative="l")

##Pregunta 5
mp5=rbind(pic,bol,elo,san)
fligner.test(mp5$monto2, mp5$provincia)

