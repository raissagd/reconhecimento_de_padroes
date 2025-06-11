rm(list=ls())
library(mlbench)

data('BreastCancer')
xyall<-data.matrix(na.omit(BreastCancer))
xall<-xyall[,-11]
yall<-(xyall[,11]-1.5) * 2 # 0,1 -> -1,1

ic1<-which(yall == 1)
ic2<-which(yall == -1)
xc1<-xall[ic1,]
xc2<-xall[ic2,]

m1<-colMeans(xc1)
m2<-colMeans(xc2)

mall<-colMeans(xall)

#apply(xc1, 2, sd)
fscore<-(abs(m1 - mall) + abs(m2 - mall)) / (apply(xc1, 2, sd) + apply(xc2, 2, sd))
order(fscore)

plot(fscore)

# variáveis com melhores f-score: 2, 3, 4 e 7

# montar um classificador com as variáveis selecionadas (3, 4 e 7), e compare o desempenho com o classificador original (considernado todas as variáveis)

plot(xc1[,3],xc1[,4], col= 'red', xlim=c(0,10), ylim=c(0,10), xlab='Variável 3', ylab='Variável 4', main='Comparação de Classificadores')
par(new=TRUE)
plot(xc2[,3],xc2[,4], col= 'blue', xlim=c(0,10), ylim=c(0,10), xlab='', ylab='', axes=FALSE)

x1select<-xc1[,c(2,3,4,7)]
x2select<-xc2[,c(2,3,4,7)]