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

# -------------------------------------------
# Bayes com distrbuição normal multivariada
rm(list = ls())
library('plot3D')

pdfnvar<-function(x,m,K,n) {
    ((1/(sqrt((2*pi)^n*(det(K)))))* exp(-0.5*(t(x-m) %*% (solve(K)) %*% (x-m))))
}

N<-100
xc1<-matrix(rnorm(N),ncol = 2) * 0.55 + 2 # mean=2, sd=0.55
xc2<-matrix(rnorm(N),ncol = 2) * 0.35 + 4 # mean=4, sd=0.35

m11<-mean(xc1[,1])
m12<-mean(xc1[,2])
m1<-matrix(c(m11,m12),ncol=1, nrow=2)
k1<-cov(xc1)

m21<-mean(xc2[,1])
m22<-mean(xc2[,2])
m2<-matrix(c(m21,m22),ncol=1, nrow=2)
k2<-cov(xc2)

seqi<-seq(0,6,0.1)
seqj<-seq(0,6,0.1)
np<-length(seqi)

M1<-matrix(1,nrow=np,ncol=np)
M2<-matrix(1,nrow=np,ncol=np)
M3<-matrix(1,nrow=np,ncol=np)

ci<-0
for (i in seqi) {
    ci<-ci+1
    cj<-0
    for(j in seqj){
        cj<-cj+1
        x<-matrix(c(i,j),ncol=1,nrow=2)
        M1[ci,cj]<-pdfnvar(x,m1,k1,2)
        M2[ci,cj]<-pdfnvar(x,m2,k2,2)
        M3[ci,cj]<-M1[ci,cj] + M2[ci,cj]
    }
}

ribbon3D(seqi,seqj,M1,clim = c(0,2),colkey = F)
ribbon3D(seqi,seqj,M2,clim = c(0,2),add = T, colkey = F)
#ribbon3D(seqi,seqj,M3,clim = c(0,2),add = T, colkey = F)

scatter3D(xc1[,1],xc1[,2],matrix(0,nrow=dim(xc1)[1]),add=T,col='blue',colkey = F)
scatter3D(xc2[,1],xc2[,2],matrix(0,nrow=dim(xc2)[1]),add=T,col='red',colkey = F)
#scatter3D(m1[1],m1[2],0,col='blue',add=T,cex=2)
#scatter3D(m2[1],m2[2],0,col='red',add=T,cex=2)