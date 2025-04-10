rm(list = ls())

library(mlbench)     # Para carregar o dataset BreastCancer
library(RSNNS)       # Para a função splitForTrainingAndTest

# Define a função de densidade da distribuição normal multivariada
pdfnvar <- function(x, m, K) {
  n <- length(x)
  (1 / sqrt((2 * pi)^n * det(K))) * exp(-0.5 * t(x - m) %*% solve(K) %*% (x - m))
}

data("BreastCancer")
xyall <- na.omit(BreastCancer)  # Remove linhas com NA

# Converte a variável alvo 'Class' em -1 (benign) e 1 (malignant)
xyall$Class <- ifelse(xyall$Class == "malignant", 1, -1)

# Converte todo o dataframe para matriz numérica
xyall <- data.matrix(xyall)

# Separa os dados em conjunto de treino e teste (70% treino, 30% teste)
# As colunas 2 a 10 são as variáveis preditoras, coluna 11 é a classe
xy <- splitForTrainingAndTest(xyall[, 2:10], xyall[, 11], ratio = 0.3)

X <- xy$inputsTrain
Y <- xy$targetsTrain

Xtest <- xy$inputsTest
Ytest <- xy$targetsTest

# Identifica os índices das amostras de cada classe no treino
ic1 <- which(Y == -1)
ic2 <- which(Y == 1)

# Separa os dados de treino por classe
xc1 <- X[ic1, ]   # Classe -1 (benign)
xc2 <- X[ic2, ]   # Classe  1 (malignant)

# Número de amostras em cada classe e total
Nc1 <- length(ic1)
Nc2 <- length(ic2)
N <- Nc1 + Nc2

# Probabilidades a priori das classes
Pc1 <- Nc1 / N
Pc2 <- Nc2 / N

# Calcula a média (vetor) e a matriz de covariância para cada classe
m1 <- colMeans(xc1)
m2 <- colMeans(xc2)
K1 <- cov(xc1)
K2 <- cov(xc2)

# Número de amostras no teste
Ntst <- length(Ytest)
Yhattst <- numeric(Ntst)  # Vetor para guardar as predições

# Classificação Bayesiana para cada ponto de teste
for (i in 1:Ntst) {
  Pc1_x <- pdfnvar(Xtest[i, ], m1, K1) * Pc1  # Verossimilhança * priori da classe -1
  Pc2_x <- pdfnvar(Xtest[i, ], m2, K2) * Pc2  # Verossimilhança * priori da classe 1
  Yhattst[i] <- sign(Pc2_x - Pc1_x)           # Classifica com base na maior probabilidade
}

# Calcula a acurácia
accuracy <- sum(Yhattst == Ytest) / Ntst
print(paste("Acurácia:", accuracy))

# Matriz de confusão: comparação entre predições e rótulos verdadeiros
MC <- table(Yhattst, Ytest)

# Extrai valores da matriz de confusão
TN <- MC["-1", "-1"]  # Verdadeiro Negativo
FP <- MC["1", "-1"]   # Falso Positivo
FN <- MC["-1", "1"]   # Falso Negativo
TP <- MC["1", "1"]    # Verdadeiro Positivo

# Calcula especificidade e sensibilidade
Esp <- TN / (TN + FP)     # Especificidade = TN / (TN + FP)
Sens <- TP / (TP + FN)    # Sensibilidade = TP / (TP + FN)

# Exibe os resultados
print(paste("Especificidade:", Esp))
print(paste("Sensibilidade:", Sens))