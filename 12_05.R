rm(list = ls())

f<-function(x, m) (x-m)^2

xrange<-seq(0, 10, 0.5)
f1<-f(xrange, 3)
f2<-f(xrange, 6)

plot(xrange, f1, type='b', xlim=c(0,10), ylim=c(0,100), col='red')
par(new=TRUE)
plot(xrange, f2, type='b', xlim=c(0,10), ylim=c(0,100), col='blue')

colvec<-c('purple', 'black')

icol<-((xrange >= 3) & (xrange <= 6)) + 1

plot(f1, f2, pch=21, bg=colvec[icol], col='black', cex=1.5)

# --------------------------------------------------
rm(list = ls())

# Geração dos dados
n <- 2
N1 <- 50
sd1 <- 0.2
xc1 <- matrix(rnorm(n * N1, sd = sd1), nrow = N1, ncol = n) + matrix(c(2, 2), nrow = N1, ncol = n, byrow = TRUE)

N2 <- 50
sd2 <- 0.2
xc2 <- matrix(rnorm(n * N2, sd = sd2), nrow = N2, ncol = n) + matrix(c(4, 4), nrow = N2, ncol = n, byrow = TRUE)

# Plot dos dados
plot(xc1[, 1], xc1[, 2], col = 'red', xlim = c(0, 6), ylim = c(0, 6), xlab = "x1", ylab = "x2", pch = 16)
par(new = TRUE)
plot(xc2[, 1], xc2[, 2], col = 'blue', xlim = c(0, 6), ylim = c(0, 6), xlab = "", ylab = "", axes = FALSE, pch = 16)

# Dados combinados
X <- rbind(xc1, xc2)
Y <- rbind(matrix(-1, nrow = N1, ncol = 1), matrix(1, nrow = N2, ncol = 1))

# Vetor de pesos para o classificador linear (ajustado para ter fronteira na região)
w <- matrix(c(-10, 2, 2), ncol = 1, nrow = 3)

# Grade de pontos
seqi <- seq(0, 6, 0.1)
seqj <- seq(0, 6, 0.1)
M1 <- matrix(0, nrow = length(seqi), ncol = length(seqj))

# Cálculo de f(x) = w^T x_aug para cada ponto da grade
ci <- 0
for (i in seqi) {
  ci <- ci + 1
  cj <- 0
  for (j in seqj) {
    cj <- cj + 1
    xt <- c(i, j)
    xtaug <- matrix(c(1, xt), ncol = 1, nrow = 3)
    M1[ci, cj] <- t(w) %*% xtaug
  }
}

# Traça a linha onde f(x) = 0 (fronteira de decisão)
par(new = TRUE)
contour(seqi, seqj, M1, levels = 1, xlim = c(0, 6), ylim = c(0, 6), drawlabels = FALSE, lwd = 2, col = "black")

# Margem
Xaug<-cbind(-1, X)
d1<-cbind(-1, xc1) %*% w
i1max<-max(d1)

d2<-cbind(-1, xc2) %*% w
i2min<-min(d2)
0.5 * (i2min - i1max)