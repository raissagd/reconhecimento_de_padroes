# Limpar ambiente
rm(list = ls())

# Carregar pacotes necessários
library(rgl)
library(mlbench)
data(PimaIndiansDiabetes)

# Função kernel density estimation
kde_multi <- function(xi, xall, h) {
  # xi: o ponto onde avaliar a densidade
  # xall: os pontos de dados
  # h: a largura de banda
  N <- dim(xall)[1] # número de pontos de dados
  n <- dim(xall)[2] # número de dimensões
  xirow <- matrix(xi, ncol=n, nrow=1)
  xirep <- matrix(xirow, ncol=n, nrow=N, byrow=TRUE)
  matdif <- (xall - xirep) * (xall - xirep) # diferenças quadradas
  dximat <- rowSums(matdif) / (h*h) # diferenças quadradas divididas por h^2
  emat <- exp(-dximat / 2) # exponencial das diferenças quadradas negativas
  pxi <- sum(emat) / ((N * sqrt(2 * pi) * h) ^ n) # estimativa de densidade
  return(pxi)
}

# Preparar dados do PimaIndiansDiabetes
# Converter dados para matriz numérica e normalizar
dados <- PimaIndiansDiabetes
dados_numericos <- as.matrix(dados[, 1:8])  # Todas as variáveis exceto a classe
dados_scaled <- scale(dados_numericos)  # Padronizar todas as variáveis

# Separar por classe (diabéticos e não diabéticos)
xc1 <- dados_scaled[dados$diabetes == "neg", ]  # Não diabéticos
xc2 <- dados_scaled[dados$diabetes == "pos", ]  # Diabéticos

# Combinar todos os dados
xall <- rbind(xc1, xc2)

# Criar vetor de classes (-1 para negativos, 1 para positivos)
yall <- c(rep(-1, nrow(xc1)), rep(1, nrow(xc2)))

# Parâmetros
Nall <- dim(xall)[1]
N1 <- dim(xc1)[1]
N2 <- dim(xc2)[1]
pc1 <- N1/Nall
pc2 <- N2/Nall
# Ajustar h para dimensionalidade mais alta (regra do polegar para KDE multivariado)
# h = σ * n^(-1/(d+4)) onde σ é desvio padrão (1 para dados padronizados), n é tamanho da amostra, d é dimensões
h <- 5 * (Nall)^(-1/(8+4))  # Ajustado para 8 dimensões

# Calcular densidades para todos os pontos usando TODAS as variáveis
cat("Calculando densidades usando todas as 8 variáveis...\n")
pxc1vec <- numeric(Nall)
pxc2vec <- numeric(Nall)
for (i in 1:Nall) {
  pxc1vec[i] <- kde_multi(xall[i, ], xc1, h)
  pxc2vec[i] <- kde_multi(xall[i, ], xc2, h)
}

# Fazer classificação baseada nas densidades completas (8 variáveis)
y_pred <- ifelse(pxc1vec > pxc2vec, -1, 1)

# Cálculo das métricas usando todas as variáveis
acuracia <- mean(y_pred == yall)
erro_classificacao <- 1 - acuracia
log_verossimilhanca <- sum(log(ifelse(yall == -1, pxc1vec, pxc2vec)))

# Matriz de confusão
tabela <- table(Previsto = ifelse(y_pred == -1, "Não-Diabético", "Diabético"),
                Real = ifelse(yall == -1, "Não-Diabético", "Diabético"))

# Exibir resultados da classificação com todas as variáveis
cat("\n===== RESULTADOS USANDO TODAS AS 8 VARIÁVEIS =====\n")
cat("Acurácia:", round(acuracia, 4), "\n")
cat("Erro de Classificação:", round(erro_classificacao, 4), "\n")
cat("Log-Verossimilhança:", round(log_verossimilhanca, 4), "\n")
print(tabela)

# Mapear cores de acordo com as classes
cores <- ifelse(yall == -1, "blue", "red")

# Plotar os pontos no espaço das probabilidades (TODAS as variáveis)
# Plotar log das probabilidades para visualização melhor
plot(-log(pxc1vec), -log(pxc2vec),
     col = cores, pch = 19,
     xlab = "log(p(x | Não-Diabético))", 
     ylab = "log(p(x | Diabético))", 
     main = "Espaço das Densidades Estimadas")
abline(0, 1, col = "black", lty = 2)
legend("bottomright", legend = c("Não-Diabético", "Diabético"), 
       col = c("blue", "red"), pch = 19)

# Testar vários valores de h e armazenar acurácias
h_values <- seq(0.02, 1.2, by = 0.02)  # Valores de h a testar
acuracias <- numeric(length(h_values))

for (j in seq_along(h_values)) {
  h_temp <- h_values[j]
  pxc1vec_temp <- numeric(Nall)
  pxc2vec_temp <- numeric(Nall)
  
  for (i in 1:Nall) {
    pxc1vec_temp[i] <- kde_multi(xall[i, ], xc1, h_temp)
    pxc2vec_temp[i] <- kde_multi(xall[i, ], xc2, h_temp)
  }
  
  # Classificação com prioris incluídas (recomendado)
  y_pred_temp <- ifelse(pxc1vec_temp * pc1 > pxc2vec_temp * pc2, -1, 1)
  acuracias[j] <- mean(y_pred_temp == yall)
}

# Plotar acurácia em função de h
#plot(h_values, acuracias, type = "b", pch = 19, col = "darkgreen", xlab = "h (largura de banda do KDE)", ylab = "Acurácia da Classificação", main = "Acurácia vs. h")
#grid()
