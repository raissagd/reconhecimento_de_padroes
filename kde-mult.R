rm(list = ls())

kde_multi <- function(xi, xall, h) {
    # xi: the point at which to evaluate the density
    # xall: the data points
    # h: the bandwidth
    N<-dim(xall)[1] # number of data points
    n<-dim(xall)[2] # number of dimensions

    xirow<-matrix(xi, ncol=n, nrow=1)
    xirep<-matrix(xirow, ncol=n, nrow=N, byrow=TRUE)
    matdif<-(xall - xirep) * (xall - xirep) # squared differences
    dximat<-rowSums(matdif)/ (h*h) # squared differences divided by h^2
    emat <- exp(-dximat / 2) # exponentiation of the negative squared differences
    pxi <- sum(emat) / ((N * sqrt(2 * pi) * h) ^ n) # density estimation

    return(pxi)
}

N<-30
h<-0.5
m1<-c(2, 2)
m2<-c(4, 4)
m3<-c(2, 4)
m4<-c(4, 2)

g1 <- matrix(rnorm(N * 2, sd = 0.6), nrow = N, ncol = 2) + matrix(m1, N, 2, byrow = TRUE)
g2 <- matrix(rnorm(N * 2, sd = 0.6), nrow = N, ncol = 2) + matrix(m2, N, 2, byrow = TRUE)
g3 <- matrix(rnorm(N * 2, sd = 0.6), nrow = N, ncol = 2) + matrix(m3, N, 2, byrow = TRUE)
g4 <- matrix(rnorm(N * 2, sd = 0.6), nrow = N, ncol = 2) + matrix(m4, N, 2, byrow = TRUE)

xc1 <- rbind(g1, g2)
xc2 <- rbind(g3, g4)

xall<-rbind(g1, g2, g3, g4)
yall <- rbind(matrix(-1, nrow = (2 * N), ncol = 1), matrix(1, nrow = (2 * N), ncol = 1))

#pxc1<-kdemulti(xc1[1,], xc1, h)
#pxc2<-kdemulti(xc2[1,], xc2, h)
#print(pxc1/pxc2)

# Geração da matriz de densidade
seqx1x2 <- seq(0, 6, 0.1)
lseq <- length(seqx1x2)
MZ <- matrix(nrow = lseq, ncol = lseq)

for (i in 1:lseq) {
    for (j in 1:lseq) {
        x1 <- seqx1x2[i]
        x2 <- seqx1x2[j]
        x1x2 <- as.matrix(cbind(x1, x2))
        MZ[i, j] <- kde_multi(x1x2, xall, h)
    }
}

# Plotando tudo em um único gráfico
plot(xc1[, 1], xc1[, 2], 
     col = "purple", pch = 19, 
     xlim = c(0, 6), ylim = c(0, 6), 
     xlab = "X1", ylab = "X2", 
     main = "Densidade de Kernel Multivariada com Dados")

points(xc2[, 1], xc2[, 2], col = "darkgreen", pch = 19)

# Adicionando curvas de nível
contour(seqx1x2, seqx1x2, t(MZ), 
        add = TRUE, drawlabels = FALSE, 
        nlevels = 10, col = "blue", lwd = 1.5)