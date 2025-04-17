rm(list = ls())
library('plot3D')
library('mlbench')

fnormal1var<-function(x,m,r){
    ((1/sqrt(2*pi*r*r)))*exp(-0.5 * ((x-m)*(x-m))/(r*r))
}

N = 100
u1 = 2
u2 = 4
s1 = 0.6
xc1<-rnorm(N)*s1 + u1
xc2<-rnorm(N)*s1 + u2

y<-rep(0,N)

plot(xc1,y,col='blue',xlab='X',ylab='',xlim = c(0,6), ylim = c(0,1))
par(new=T)
plot(xc2,y,col='red',xlab='',ylab='',xlim = c(0,6), ylim = c(0,1))


xgrid = seq(0,6,0.05)
pxc1 = fnormal1var(xgrid,mean(xc1),s1)
pxc2 = fnormal1var(xgrid,mean(xc2),s1)

par(new=T)
plot(xgrid,pxc1,type = 'l',col='blue',xlab='X',ylab='',xlim = c(0,6), ylim = c(0,1))
par(new=T)
plot(xgrid,pxc2,type = 'l',col='red',xlab='',ylab='',xlim = c(0,6), ylim = c(0,1))

##### KDE
h = 0.3
pkdetotal = rep(0,length(xgrid))
for(i in (1:N)){
    pkde = fnormal1var(xgrid,xc1[i],h)/N
    par(new=T)
    plot(xgrid,pkde,type = 'l',col='green',xlab='',ylab='',xlim = c(0,6), ylim = c(0,1))
    pkdetotal = pkdetotal + pkde
}

par(new=T)
plot(xgrid,pkdetotal,type = 'l',col='green',xlab='X',ylab='',xlim = c(0,6), ylim = c(0,1))

pkdetotal = rep(0,length(xgrid))
for(i in (1:N)){
    pkde = fnormal1var(xgrid,xc2[i],h)/N
    par(new=T)
    plot(xgrid,pkde,type = 'l',col='purple',xlab='',ylab='',xlim = c(0,6), ylim = c(0,1))
    pkdetotal = pkdetotal + pkde
}

par(new=T)
plot(xgrid,pkdetotal,type = 'l',col='purple',xlab='X',ylab='',xlim = c(0,6), ylim = c(0,1))