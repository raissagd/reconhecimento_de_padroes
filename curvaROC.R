rm(list = ls())

library(rgl)
library(mlbench)
data(PimaIndiansDiabetes)

fnormal1var<-function(x,m,r) {
    y<-(1/(sqrt(2*pi*r*r)))*exp(-0.5 * ((x-m)/(r))^2)
    return (y)
}

N1<-30
N2<-30
xc1<- rnorm(N1, mean = 2, sd = 1)
xc2<- rnorm(N2, mean = 4, sd = 1)

m1<-mean(xc1)
m2<-mean(xc2)

s1<-sd(xc1)
s2<-sd(xc2)

yplot1<-matrix(0, ncol = 1, nrow = N1)
yplot2<-matrix(0, ncol = 1, nrow = N2)

plot(xc1, yplot1, col='red',xlim=c(0,6),ylim=c(0, 1),xlab='X',ylab='Y')
par(new=TRUE)
plot(xc2, yplot2, col='blue',xlim=c(0,6),ylim=c(0, 1),xlab='X',ylab='Y')

xrange<-seq(0,6,0.1)
fN1<-fnormal1var(xrange,m1,s1)
fN2<-fnormal1var(xrange,m2,s2)

par(new=TRUE)
plot(xrange, fN1, col='red',xlim=c(0,6),ylim=c(0,1),xlab='X',ylab='Y', type='l')
par(new=TRUE)
plot(xrange, fN2, col='blue',xlim=c(0,6),ylim=c(0,1),xlab='X',ylab='Y', type='l')

TN<-matrix()
TP<-matrix()

ci<-1
for(i in xrange) {
    TN[ci]<-sum(1 * (xc1<i))/N1
    TP[ci]<-sum(1 * (xc2>i))/N2
    ci<-ci+1
}

par(new=TRUE)
plot(xrange, TN, col='red',xlim=c(0,6),ylim=c(0,1),xlab='X',ylab='Y', type='l')
par(new=TRUE)
plot(xrange, TP, col='blue',xlim=c(0,6),ylim=c(0,1),xlab='X',ylab='Y', type='l')

par(new=FALSE)
plot(1 - TN, TP, type='l', col='purple', xlim=c(0,1), ylim=c(0,1), xlab='False Positive Rate (1 - Specificity)', ylab='True Positive Rate (Sensitivity)', main='Curva ROC')
abline(a=0, b=1, lty=2, col='gray')