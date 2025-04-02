rm(list = ls())
library(mlbench)
library(caret)
library(dplyr)

# Carregar os dados
data("BreastCancer")
data2 <- BreastCancer

# Remover dados faltantes
data2 <- data2[complete.cases(data2), ]

# Remover a coluna ID
data2 <- data2[, -1]

# Converter variáveis para numéricas
data2[, 1:9] <- lapply(data2[, 1:9], function(x) as.numeric(as.character(x)))

# Mapear as classes para valores binários (maligno = 0, benigno = 1)
data2$Class <- ifelse(data2$Class == "malignant", 0, 1)
data2$Class <- as.numeric(data2$Class)

# Definir a validação cruzada com 10 folds
set.seed(42)
k_folds <- createFolds(data2$Class, k = 10, list = TRUE)

# Função para treinar um Perceptron Clássico
train_perceptron <- function(train_data, lr = 0.1, epochs = 100) {
  X <- as.matrix(train_data[, -10])  # Features
  y <- as.numeric(train_data$Class)  # Labels
  
  # Inicializar pesos aleatórios
  weights <- runif(ncol(X) + 1, -1, 1)  # +1 para o bias
  
  # Adicionar coluna de bias (+1)
  X <- cbind(1, X)
  
  # Treinamento
  for (epoch in 1:epochs) {
    for (i in 1:nrow(X)) {
      z <- sum(weights * X[i, ])  # Produto interno
      y_pred <- ifelse(z >= 0, 1, 0)  # Função degrau
      error <- y[i] - y_pred  # Cálculo do erro
      weights <- weights + lr * error * X[i, ]  # Atualização dos pesos
    }
  }
  return(weights)
}

# Função para fazer previsões
predict_perceptron <- function(X, weights) {
  X <- as.matrix(X)
  X <- cbind(1, X)  # Adicionar bias
  predictions <- ifelse(X %*% weights >= 0, 1, 0)
  return(predictions)
}

# Vetor para armazenar as acurácias
accuracies <- c()

# Loop sobre os folds
for (i in 1:10) {
  test_indexes <- k_folds[[i]]
  test_data <- data2[test_indexes, ]
  train_data <- data2[-test_indexes, ]
  
  # Treinar o Perceptron
  weights <- train_perceptron(train_data)
  
  # Aplicar ao conjunto de teste
  test_features <- test_data[, -10]  # Apenas features
  pred <- predict_perceptron(test_features, weights)
  
  # Calcular a acurácia
  acc <- sum(pred == test_data$Class) / nrow(test_data)
  accuracies <- c(accuracies, acc)
}

# Calcular acurácia média e desvio padrão
accuracy_mean <- mean(accuracies, na.rm = TRUE)
accuracy_sd <- sd(accuracies, na.rm = TRUE)

# Exibir resultados
cat("Acurácias para cada fold:", accuracies, "\n")
cat("Acurácia Média:", accuracy_mean, "\n")
cat("Desvio Padrão:", accuracy_sd, "\n")


