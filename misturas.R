library(mlbench)
library(mclust)
library(caret)
library(klaR)

set.seed(123)
data <- mlbench.spirals(400, cycles = 1, sd = 0.05)
X <- data$x
y <- as.factor(data$classes)

folds <- createFolds(y, k = 10)
accuracies <- numeric(10)

best_model <- NULL
best_accuracy <- 0
best_trainX <- NULL
best_trainY <- NULL
best_predY <- NULL

for (i in 1:10) {
  test_idx <- folds[[i]]
  trainX <- X[-test_idx, ]
  trainY <- y[-test_idx]
  testX <- X[test_idx, ]
  testY <- y[test_idx]
  
  # Nomear colunas
  colnames(trainX) <- c("X1", "X2")
  colnames(testX) <- c("X1", "X2")
  
  # Ajuste do modelo GMM (para fins de inspeção, opcional aqui)
  gmm <- Mclust(trainX)
  
  # Classificador de Bayes
  model <- NaiveBayes(x = trainX, grouping = trainY)
  pred_class <- predict(model, testX)$class
  
  # Acurácia
  acc <- mean(pred_class == testY)
  accuracies[i] <- acc
  
  if (acc > best_accuracy) {
    best_accuracy <- acc
    best_model <- model
    best_trainX <- trainX
    best_trainY <- trainY
    best_predY <- predict(model, trainX)$class
  }
}


cat("Acurácias por fold:\n")
print(accuracies)
cat(sprintf("\nAcurácia média: %.4f\n", mean(accuracies)))
cat(sprintf("Desvio padrão: %.4f\n", sd(accuracies)))

plot(best_trainX, col = as.numeric(best_predY), pch = 19,
     main = "Superfície de Separação - Melhor Fold",
     xlab = "X1", ylab = "X2")
