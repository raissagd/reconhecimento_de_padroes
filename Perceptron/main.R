rm(list = ls())

# Carregar biblioteca para visualização
library('plot3D')

# Função de treinamento do Perceptron com tolerância ao erro
perceptron <- function(X, Y, lr = 0.01, epochs = 1000, tolerance = 0.01) {
  # Inicializa pesos e bias
  W <- runif(ncol(X))  # Pesos aleatórios
  b <- runif(1)         # Bias aleatório
  
  # Algoritmo do Perceptron
  for (epoch in 1:epochs) {
    for (i in 1:nrow(X)) {
      # Cálculo da saída predita
      z <- sum(W * X[i, ]) + b
      y_pred <- ifelse(z >= 0, 1, -1)
      
      # Verifica se o erro é maior que a tolerância
      error <- abs(y_pred - Y[i])
      
      if (error > tolerance) {
        # Atualiza os pesos e bias se o erro for maior que a tolerância
        W <- W + lr * Y[i] * X[i, ]
        b <- b + lr * Y[i]
      }
    }
  }
  
  return(list(W = W, b = b))
}

# Parâmetros iniciais
s1 <- 0.921
s2 <- 0.921
nc <- 100

# Geração dos dados das duas classes gaussianas
set.seed(42)  # Para reprodutibilidade
target1 <- matrix(rnorm(nc * 2), ncol = 2) * s1 + t(matrix(c(2, 2), nrow = 2, ncol = nc))
target2 <- matrix(rnorm(nc * 2), ncol = 2) * s2 + t(matrix(c(5, 5), nrow = 2, ncol = nc))

y1 <- array(1, c(nc, 1))   # Classe +1
y2 <- array(-1, c(nc, 1))  # Classe -1

X <- rbind(target1, target2)
Y <- rbind(y1, y2)

# Treinamento do Perceptron com tolerância ao erro
model <- perceptron(X, Y, tolerance = 0.1)  # Você pode ajustar a tolerância conforme necessário
W <- model$W
b <- model$b

# Plotagem dos dados
plot(target1[, 1], target1[, 2], col = 'purple', pch = 19, xlim = c(0, 6), ylim = c(0, 6), xlab = "X1", ylab = "X2")
points(target2[, 1], target2[, 2], col = 'green', pch = 19)

# Plotar a fronteira de decisão
abline(-b / W[2], -W[1] / W[2], col = "black", lwd = 2)
