pd4<-read.delim("clipboard")
sas<-subset(pd4,Distrito=="Santa Anita"|
              Distrito=="San Juan de Lurigancho"|
              Distrito=="El Agustino")
sas<-subset(pd4, Distrito!= "Los Olivos")
sa<-subset(pd4, Distrito== "Santa Anita")
les<-subset(pd4,Distrito!="San Juan de Lurigancho")
lo<-subset(pd4,Distrito=="Los Olivos")


#Pregunta 1: Prueba de Homogeneidad de Subpoblaciones
chisq.test(sas$Distrito,sas$Gas_natural)
qchisq(0.03,2,lower.tail = F)

#Pregunta 2: Correlación de Spearman
cor.test(sa$Vivienda,sa$Electricidad,method="s",
         alternative="g")

#Pregunta 3: M-H-C
av<-les$Vivienda<25
table(les$Reclamo,av,les$Distrito)
mantelhaen.test(les$Reclamo,av,les$Distrito)
mantelhaen.test(av,les$Reclamo,les$Distrito)
tabla<-array(c(14,40,11,42,15,41,14,52,12,28,14,41),dim=c(2,2,3))
mantelhaen.test(tabla)

#Pregunta 4: Prueba Exacta de Fisher
table(les$UsoE,les$Distrito)
tabla2<-matrix(c(35,72,32,63),2,2)
fisher.test(tabla2,alternative="l")

#Pregunta 5: Prueba de Independencia
chisq.test(lo$UsoE,lo$Antigüedad)
qchisq(0.03,4,lower.tail = F)
