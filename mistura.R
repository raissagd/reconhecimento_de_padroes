# Começando com o KMEANS
rm(list = ls())

# Função pdfnvar e mymix estão declaradas abaixo:
pdfnvar <- function(x, m, K, n) {
  (1 / (sqrt((2 * pi)^n * (det(K))))) * exp(-0.5 * (t(x - m) %*% solve(K) %*% (x - m)))
}

mymix <- function(x, inlist) {
  ng <- length(inlist)
  klist <- list()
  mlist <- list()
  pglist <- list()
  nglist <- list()
  n <- dim(inlist[[1]])[2]
  
  for (i in 1:ng) {
    klist[[i]] <- cov(inlist[[i]])
    mlist[[i]] <- colMeans(inlist[[i]])
    nglist[[i]] <- dim(inlist[[i]])[1]
  }
  
  N <- sum(unlist(nglist))
  for (i in 1:ng) {
    pglist[[i]] <- nglist[[i]] / N
  }
  
  Px <- 0
  for (i in 1:ng) {
    Px <- Px + pglist[[i]] * pdfnvar(x, mlist[[i]], klist[[i]], n)
  }
  
  return(Px)
}

# Gerando os dados
N <- 100
m1 <- c(2, 2)
m2 <- c(4, 4)
g1 <- matrix(rnorm(N * 2, sd = 0.6), nrow = N, ncol = 2) + matrix(m1, N, 2, byrow = TRUE)
g2 <- matrix(rnorm(N * 2, sd = 0.6), nrow = N, ncol = 2) + matrix(m2, N, 2, byrow = TRUE)
xc1 <- rbind(g1, g2)

m3 <- c(2, 4)
m4 <- c(4, 2)
g3 <- matrix(rnorm(N * 2, sd = 0.6), nrow = N, ncol = 2) + matrix(m3, N, 2, byrow = TRUE)
g4 <- matrix(rnorm(N * 2, sd = 0.6), nrow = N, ncol = 2) + matrix(m4, N, 2, byrow = TRUE)
xc2 <- rbind(g3, g4)

plot(xc1[, 1], xc1[, 2], col = "purple", xlim = c(0, 6), ylim = c(0, 6), xlab = "X1", ylab = "X2", main = "Mistura de Normais", pch = 19)
par(new = TRUE)
plot(xc2[, 1], xc2[, 2], col = "purple", xlim = c(0, 6), ylim = c(0, 6), xlab = "X1", ylab = "X2", main = "", pch = 19)

X <- rbind(xc1, xc2)

# K-means
k <- 4
kmeansret <- kmeans(X, k)
plot(X[, 1], X[, 2], col = kmeansret$cluster, xlim = c(0, 6), ylim = c(0, 6), xlab = "X1", ylab = "X2", main = "Agrupamento com K-means", pch = 19)

# Matriz de distâncias ordenada
X_ord <- X[order(kmeansret$cluster), ]
image(as.matrix(dist(X_ord)), main = "Distância entre os pontos")

# Separando os clusters
xclusters <- list()
for (i in 1:k) {
  ici <- which(kmeansret$cluster == i)
  xclusters[[i]] <- X[ici, ]
}

# Varredura dos pontos em uma grade para calcular a densidade da mistura
x_seq <- seq(0, 6, length.out = 100)
y_seq <- seq(0, 6, length.out = 100)
z <- matrix(0, nrow = length(x_seq), ncol = length(y_seq))

for (i in 1:length(x_seq)) {
  for (j in 1:length(y_seq)) {
    ponto <- c(x_seq[i], y_seq[j])
    z[i, j] <- mymix(ponto, xclusters)
  }
}

# Gráfico 3D da densidade da mistura
persp(x_seq, y_seq, z,
      theta = 30, phi = 30, expand = 0.5,
      col = "lightblue", ticktype = "detailed",
      xlab = "X1", ylab = "X2", zlab = "Densidade",
      main = "Densidade da Mistura de Normais")