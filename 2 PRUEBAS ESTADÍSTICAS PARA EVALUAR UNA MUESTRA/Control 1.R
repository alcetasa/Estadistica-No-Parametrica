setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


tabla2<-c(14,22,11,11)
sum(tabla2)
rela<-c(1,2,1,3)

prob<-rela/sum(rela)
chisq.test(tabla2,p=prob)
res2<-chisq.test(tabla2,p=rela,rescale.p = T)
res2$expected

library(RVAideMemoire)
multinomial.test(as.vector(tabla2),prob)