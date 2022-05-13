datos=read.delim("clipboard")

##Pregunta 1
ciu<-subset(datos,ciudad=="Trujillo"|
              ciudad=="Cusco"|
              ciudad=="Huancayo"|
              ciudad=="Arequipa")
table(ciu$genero, ciu$compañia, ciu$ciudad)
mantelhaen.test(ciu$genero, ciu$compañia, ciu$ciudad, alternative="g")

#tabla1=table(ciu$genero, ciu$compañia, ciu$ciudad)
#mantelhaen.test(tabla1, alternative="g")

#tabla<-array(c(26,26,23,22,25,29,32,27,27,16,23,22,23,34,22,17),dim=c(2,2,3))
#mantelhaen.test(tabla, alternative="g")

##Pregunta 2
caj=subset(datos, ciudad=="Cajamarca")
chisq.test(caj$compañia, caj$tiempo)



##Pregunta 3
chic=subset(datos, ciudad=="Chiclayo")
cor.test(chic$opinion2, chic$opinion4, method="s",alternative="t")


##Preguunta 4
ciu4<-subset(datos,ciudad=="Iquitos"|
              ciudad=="Trujillo"|
              ciudad=="Cajamarca"|
              ciudad=="Cusco")
tabla4=table(ciu4$ciudad,ciu4$compañia)
chisq.test(tabla4)


##Pregunta 5
ciu5=subset(datos,ciudad=="Cajamarca"|
              ciudad=="Lima")
table(ciu5$destino,ciu5$ciudad)
tabla5<-matrix(c(49,42,44,50),2,2)
fisher.test(tabla5,alternative="g")




