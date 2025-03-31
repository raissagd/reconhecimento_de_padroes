rm(list = ls())
library(mlbench)
library(caret)
library(dplyr)
library(neuralnet)

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

# Função para treinar um Perceptron Simples
train_adaline_perceptron <- function(train_data) {
  formula <- as.formula("Class ~ .")
  model <- neuralnet(formula, data = train_data, hidden = 1, linear.output = FALSE, threshold = 0.01)
  return(model)
}

# Vetor para armazenar as acurácias
accuracies <- c()

# Loop sobre os folds
for (i in 1:10) {
  test_indexes <- k_folds[[i]]
  test_data <- data2[test_indexes, ]
  train_data <- data2[-test_indexes, ]
  
  # Treinar o modelo
  model <- train_adaline_perceptron(train_data)
  
  # Verificar se a rede foi treinada corretamente
  if (is.null(model$net.result)) {
    next  # Pular fold se o modelo não convergir
  }
  
  # Aplicar ao conjunto de teste
  test_features <- test_data[, -10]  # Certificar que apenas as features são usadas
  pred <- compute(model, test_features)$net.result
  pred_class <- ifelse(pred > 0.5, 1, 0)
  
  # Calcular a acurácia
  acc <- sum(pred_class == test_data$Class) / nrow(test_data)
  accuracies <- c(accuracies, acc)
}

# Calcular acurácia média e desvio padrão
accuracy_mean <- mean(accuracies, na.rm = TRUE)
accuracy_sd <- sd(accuracies, na.rm = TRUE)

# Exibir resultados
cat("Acurácias para cada fold:", accuracies, "\n")
cat("Acurácia Média:", accuracy_mean, "\n")
cat("Desvio Padrão:", accuracy_sd, "\n")

