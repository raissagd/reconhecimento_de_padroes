rm(list = ls())
library(mlbench)
library(caret)
library(e1071)
library(ggplot2)
library(kernlab)
library(stats)
library(dplyr)

# 1. Carregar dados
data(Glass)
dados <- Glass

# 2. Classe majoritária
classe_majoritaria <- as.numeric(names(which.max(table(dados$Type))))

# 3. Resposta binária
y_bin <- ifelse(dados$Type == classe_majoritaria, 1, 2)

# 4. Normalizar preditores
x <- scale(dados[, -10])
dados_bin <- data.frame(x, Type = as.factor(y_bin))

# 5. Grid de parâmetros
tune_grid <- expand.grid(
  C = 2^(-2:5),
  sigma = 2^(-5:1)
)

# 6. Treinamento com controle modificado para salvar predições
ctrl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final"
)

set.seed(123)
modelo <- train(
  Type ~ .,
  data = dados_bin,
  method = "svmRadial",
  tuneGrid = tune_grid,
  trControl = ctrl
)

# 6b. Exibir melhores parâmetros, acurácia e desvio padrão
melhores_parametros <- modelo$bestTune
melhor_linha <- modelo$results[
  modelo$results$C == melhores_parametros$C &
    modelo$results$sigma == melhores_parametros$sigma, ]

cat("Melhores parâmetros encontrados:\n")
print(melhores_parametros)

cat(sprintf("\nAcurácia média: %.4f\n", melhor_linha$Accuracy))
cat(sprintf("Desvio padrão da acurácia: %.4f\n", melhor_linha$AccuracySD))

# 6c. Acurácia por fold
predicoes <- modelo$pred
predicoes <- predicoes[
  predicoes$C == melhores_parametros$C &
    predicoes$sigma == melhores_parametros$sigma, ]  # apenas predições do melhor modelo

predicoes$Resample <- gsub("Fold", "", predicoes$Resample)

acuracia_por_fold <- predicoes %>%
  group_by(Resample) %>%
  summarise(acuracia = mean(pred == obs))

cat("\nAcurácia por fold:\n")
print(acuracia_por_fold)

# 7. Modelo final e vetores de suporte
melhor_modelo <- modelo$finalModel
melhor_sigma <- modelo$bestTune$sigma
indices_vs <- alphaindex(melhor_modelo)[[1]]  # <- PEGA VETORES DE SUPORTE

# 8. Kernel com sigma ótimo
K <- as.matrix(kernelMatrix(rbfdot(sigma = melhor_sigma), x))

# 9. MDS
mds_result <- cmdscale(as.dist(1 - K), k = 2)

# 10. DataFrame de plotagem
plotdata <- data.frame(
  Dim1 = mds_result[,1],
  Dim2 = mds_result[,2],
  Type = as.factor(y_bin),
  is_vs = FALSE
)
plotdata$is_vs[indices_vs] <- TRUE  # marcar vetores de suporte

# 11. Plot
ggplot(plotdata, aes(x = Dim1, y = Dim2)) +
  geom_point(aes(color = Type), size = 3) +  # pontos normais
  geom_point(data = subset(plotdata, is_vs), color = "black", shape = 1, size = 4, stroke = 1.2) +  # contorno preto nos vetores de suporte
  labs(title = "Espaço de Similaridades com Vetores de Suporte") +
  theme_minimal()
