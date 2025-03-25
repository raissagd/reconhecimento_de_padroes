rm(list = ls())

library('plot3D')

# Função de densidade normal multivariada
pdfnvar <- function(x, m, K, n) {
  coef <- 1 / (sqrt((2 * pi)^n * det(K)))
  expoente <- exp(-0.5 * t(x - m) %*% solve(K) %*% (x - m))
  return(coef * expoente)
}

# Função k-NN com estimativa bayesiana usando a densidade normal multivariada
myknn <- function(X, Y, xt, k, h) {
  N <- dim(X)[1]  # Número total de pontos no dataset
  n <- dim(X)[2]  # Dimensão dos pontos
  xtmat <- matrix(xt, nrow = N, ncol = n, byrow = TRUE)
  seqk <- order(rowSums((X - xtmat) * (X - xtmat)))  # Ordena pela distância euclidiana
  K <- h * diag(n)  # Matriz de covariância isotrópica (hI)
  
  Xmatk <- as.matrix(X[seqk[1:k], ])
  xt <- matrix(xt, nrow = n, ncol = 1)
  
  sumk <- 0
  for (i in 1:k) {
    sumk <- sumk + Y[seqk[i]] * pdfnvar(X[seqk[i], ], xt, K, n)
  }
  
  yhat <- sign(sumk)
  return(yhat)
}

# Parâmetros iniciais
s1 <- 0.921
s2 <- 0.921
nc <- 100

# Geração dos dados
xc1 <- matrix(rnorm(nc * 2), ncol = 2) * s1 + t(matrix(c(2, 2), nrow = 2, ncol = nc))
xc2 <- matrix(rnorm(nc * 2), ncol = 2) * s2 + t(matrix(c(4, 4), nrow = 2, ncol = nc))

y1 <- array(1, c(nc, 1))
y2 <- array(-1, c(nc, 1))

X <- rbind(xc1, xc2)
Y <- rbind(y1, y2)

k <- 10  # Número de vizinhos
h <- 0.05   # Parâmetro da matriz K = hI

# Grid para avaliação do modelo
seqi <- seq(1, 6, 0.1)
seqj <- seq(1, 6, 0.1)
M1 <- matrix(0, nrow = length(seqi), ncol = length(seqj))

# Classificação dos pontos do grid
ci <- 0
for (i in seqi) {
  ci <- ci + 1
  cj <- 0
  for (j in seqj) {
    cj <- cj + 1
    xt <- matrix(c(i, j), nrow = 2)
    result <- myknn(X, Y, xt, k, h)
    M1[ci, cj] <- result[[1]]
  }
}

# Plotagem dos dados e da fronteira de decisão
plot(xc1[, 1], xc1[, 2], col = 'purple', pch = 19, xlim = c(0, 6), ylim = c(0, 6), 
     main = paste("k =", k, ", h =", h))
points(xc2[, 1], xc2[, 2], col = 'green', pch = 19)
par(new = TRUE)
contour(seqi, seqj, M1, levels = 0, xlim = c(0, 6), ylim = c(0, 6))

# -----------------------------------------------------------------------------------------
# como os pontos se comportam em função dos parametros
projetaX <- function(X, Y, xt, k, h) {
  N <- dim(X)[1]  # Número total de pontos no dataset
  n <- dim(X)[2]  # Dimensão dos pontos
  xtmat <- matrix(xt, nrow = N, ncol = n, byrow = TRUE)
  seqk <- order(rowSums((X - xtmat) * (X - xtmat)))  # Ordena pela distância euclidiana
  K <- h * diag(n)  # Matriz de covariância isotrópica (hI)
  
  Xmatk <- as.matrix(X[seqk[1:k], ])
  xt <- matrix(xt, nrow = n, ncol = 1)
  
  sumk1 <- 0
  sumk2 <- 0
  for (i in 1:k) {
    if(Y[seqk[i]] == -1) {
      sumk1 <- sumk1+pdfnvar(X[seqk[i],],xt,K,n)
    } else {
      sumk2 <- sumk2+pdfnvar(X[seqk[i],],xt,K,n)
    }
  }
  
  return(list(sumk1, sumk2))
}

N <- nrow(X)  # Obtém o número total de pontos no dataset

# Inicializa os vetores de projeção corretamente
p1 <- numeric(N)  # Para classe -1
p2 <- numeric(N)  # Para classe +1

# Percorre todos os pontos do conjunto de dados
for(i in 1:N) {
  retx <- projetaX(X, Y, as.matrix(X[i, ]), k, h)
  p1[i] <- retx[[1]]  # Soma das probabilidades para classe -1
  p2[i] <- retx[[2]]  # Soma das probabilidades para classe +1
}

# Plota os pontos no espaço das probabilidades
cores <- ifelse(Y == -1, "red", "green")  # Classe -1 será vermelha, classe +1 será azul
plot(p1, p2, col = cores, pch = 16, xlab = "Soma para Classe -1", ylab = "Soma para Classe +1", main = paste("Projeção dos Pontos no Espaço das Probabilidades", " ", "k =", k, " ", "h =", h))
abline(0, 1, col = "black", lwd = 2, lty = 2)

