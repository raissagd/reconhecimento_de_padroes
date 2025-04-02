rm(list=ls())

fnormal1var<-function(x,m,r) {
    y<-(1/(sqrt(2*pi*r*r)))*exp(-0.5 * ((x-m)/(r))^2)
    return (y)
}

xc1<-rnorm(100, mean=3, sd=0.6)
xc2<-rnorm(50, mean=5, sd=0.6)

N1<-length(xc1)
N2<-length(xc2)

pc1<-N1/(N1+N2)
pc2<-N2/(N1+N2)

s1<-sd(xc1)
m1<-mean(xc1)

s2<-sd(xc2)
m2<-mean(xc2)

plot(xc1,matrix(0,nrow=N1, ncol=1), col='red',xlim=c(0,8),ylim=c(0,1.5))
par(new=TRUE)
plot(xc2,matrix(0,nrow=N2, ncol=1), col='blue',xlim=c(0,8),ylim=c(0,1.5))

xrange<-seq(0,8,0.2)
fcx1<-fnormal1var(xrange,m1,s1)
fcx2<-fnormal1var(xrange,m2,s2)

par(new=TRUE)
plot(xrange,fcx1, type='l', col='red',xlim=c(0,8),ylim=c(0,1.5),xlab='x', ylab='f(x)')
par(new=TRUE)
plot(xrange,fcx2, type='l', col='blue',xlim=c(0,8),ylim=c(0,1.5),xlab='x', ylab='f(x)')

px<-pc1*fx1+pc2*fx2

pc1x<-(fx1*pc1)/px
pc2x<-(fx2*pc2)/px

yhat<-(sign(pc1x-pc2x)+1)/2

par(new=TRUE)
plot(xrange,yhat,col='black',xlim=c(0,8),ylim=c(0,1.5),type='l')

# ----------------------------------------------
# 2D
rm(list=ls())
library('plot3D')
library('rgl')

pdfnvar<-function(x,m,K,n) {
    ((1/(sqrt((2*pi)^n*(det(K)))))* exp(-0.5*(t(x-m) %*% (solve(K)) %*% (x-m))))
}

s1<-1
s2<-1
ro<-0.8

m1<-matrix(c(3,3),byrow=T,ncol=1)
K1<-matrix(c(s1^2,ro*s1*s2,ro*s1*s2,s2^2),byrow=T,ncol=2)

n<-2

seqi<-seq(0,6,0.1)
seqj<-seq(0,6,0.1)
M1<-matrix(1,nrow=length(seqi),ncol=length(seqj))

ci<-0
for(i in seqi){
    ci<-ci+1
    cj<-0
    for(j in seqj){
        cj<-cj+1
        x<-matrix(c(i,j),byrow=T,ncol=1)
        M1[ci,cj]<-pdfnvar(x,m1,K1,n)
    }
}

contour(seqi,seqj,M1)
persp3d(seqi,seqj,M1, col='red')