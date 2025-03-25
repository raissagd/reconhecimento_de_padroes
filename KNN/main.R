# Remove todas as variáveis do ambiente de trabalho para começar com um ambiente limpo
# rm(list=ls())

# Carrega a biblioteca plot3D, utilizada para visualizações em 3D e contornos
library('plot3D')

# Definição dos parâmetros de escala para os grupos de pontos e o número de pontos por classe
s1 <- 0.921  # Escala do primeiro grupo de pontos (desvio padrão da distribuição normal)
s2 <- 0.921  # Escala do segundo grupo de pontos
nc <- 100  # Número de pontos em cada classe

# Geração de dois conjuntos de pontos (xc1 e xc2), cada um com distribuição normal
# O grupo xc1 é centrado em (2,2) e o grupo xc2 é centrado em (3,3)
xc1 <- matrix(rnorm(nc * 2), ncol = 2) * s1 + t(matrix(c(2, 2), nrow = 2, ncol = nc))
xc2 <- matrix(rnorm(nc * 2), ncol = 2) * s2 + t(matrix(c(4, 4), nrow = 2, ncol = nc))

# Criação dos rótulos das classes: 1 para o grupo xc1 e -1 para o grupo xc2
y1 <- array(1, c(nc, 1))
y2 <- array(-1, c(nc, 1))

# Plotagem inicial dos pontos das duas classes com cores diferentes
plot(xc1[, 1], xc1[, 2], col = 'red', xlim = c(0, 6), ylim = c(0, 6))
points(xc2[, 1], xc2[, 2], col = 'blue')

# Junta os pontos de ambas as classes em uma única matriz X e os rótulos em Y
X <- rbind(xc1, xc2)
Y <- rbind(y1, y2)

# Define o número de vizinhos a serem considerados no algoritmo k-NN
k <- 20

# Importa a função myknn do arquivo externo "myknn.R"
# Esse arquivo deve conter a implementação do algoritmo k-NN
source("C:/Users/Lenovo/Downloads/2025-01/Reconhecimento de Padrões/Práticas/KNN/myknn.R")

# Definição das sequências que representam os pontos do grid para estimar as densidades
seqi <- seq(1, 6, 0.1)  # Sequência no eixo X
seqj <- seq(1, 6, 0.1)  # Sequência no eixo Y

# Cria uma matriz para armazenar os resultados da classificação em cada ponto do grid
M1 <- matrix(0, nrow = length(seqi), ncol = length(seqj))

# Percorre todos os pontos do grid, aplicando o k-NN para estimar a classe de cada ponto
ci <- 0
for (i in seqi) {
  ci <- ci + 1
  cj <- 0
  for (j in seqj) {
    cj <- cj + 1
    # Cria um ponto de teste (xt) para ser classificado
    xt <- matrix(c(i, j), nrow = 2)
    # Aplica o k-NN para classificar o ponto xt
    result <- myknn(X, Y, xt, k)
    # Armazena o resultado da classificação na matriz M1
    M1[ci, cj] <- result[[1]]
  }
}

# Plota novamente os pontos das duas classes
plot(xc1[, 1], xc1[, 2], col = 'red', xlim = c(0, 6), ylim = c(0, 6))
points(xc2[, 1], xc2[, 2], col = 'blue')

# Adiciona o contorno de decisão do classificador k-NN sobre o gráfico
par(new = TRUE)
contour(seqi, seqj, M1, levels = 0, xlim = c(0, 6), ylim = c(0, 6))