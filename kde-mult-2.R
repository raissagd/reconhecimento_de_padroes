rm(list = ls())
library(rgl)

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

Nall<-dim(xall)[1]
N1<-dim(xc1)[1]
N2<-dim(xc2)[1]
pc1<-N1/Nall
pc2<-N2/Nall

seqx1x2<-seq(0,6,0.1)
lseq<-length(seqx1x2)
MZ<-matrix(nrow=lseq, ncol=lseq)
cr<-0
for (i in 1:lseq) {
  for (j in 1:lseq) {
    cr<-cr+1
    x1<-seqx1x2[i]
    x2<-seqx1x2[j]
    x1x2<-as.matrix((cbind(x1, x2)))
    pxc1<-kde_multi(x1x2, xc1, h)
    pxc2<-kde_multi(x1x2, xc2, h)
    MZ[i, j] <-1*(pxc1 > (pxc2 / pxc1) * pxc2) # 1 se pxc1 > pxc2, 0 caso contrário
  }
}

contour(seqx1x2, seqx1x2, t(MZ), 
        xlab = "X1", ylab = "X2", 
        main = "Densidade de Kernel Multivariada", 
        nlevels = 10) # aumente os níveis para ver mais curvas
par(new = TRUE)
plot(xc1[, 1], xc1[, 2], col = "red", xlim = c(0, 6), ylim = c(0, 6), xlab = "X1", ylab = "X2", main = "", pch = 19)
par(new = TRUE)
plot(xc2[, 1], xc2[, 2], col = "purple", xlim = c(0, 6), ylim = c(0, 6), xlab = "X1", ylab = "X2", main = "", pch = 19)

persp3d(seqx1x2, seqx1x2, MZ, xlim = c(0,6), ylim = c(0,6), col = "lightblue", xlab = "X1", ylab = "X2", zlab = "Densidade de Kernel Multivariada", main = "Densidade de Kernel Multivariada")

# ------------------------------------------------------------------------
# Gráfico das Densidades Estimadas

pxc1vec <- numeric(Nall)
pxc2vec <- numeric(Nall)
for (i in 1:Nall) {
  pxc1vec[i] <- kde_multi(xall[i, ], xc1, h)
  pxc2vec[i] <- kde_multi(xall[i, ], xc2, h)
}

pxc1c2 <- cbind(pxc1vec, pxc2vec)
# Corrigir vetor yall para ser vetorial (não matriz coluna)
yvec <- as.vector(yall)

# Mapear cores de acordo com as classes
cores <- ifelse(yvec == -1, "red", "purple")

# Plotar os pontos no espaço das probabilidades
plot(pxc1vec, pxc2vec,
     col = cores, pch = 19,
     xlab = "p(x | Classe -1)", ylab = "p(x | Classe +1)",
     main = "Espaço das Densidades Estimadas (KDE)")
abline(0, 1, col = "black", lty = 2)

