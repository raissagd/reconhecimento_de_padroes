# Instale os pacotes se necessário
# install.packages("caret")
# install.packages("klaR")
# install.packages("RnavGraphImageData")

library(RnavGraphImageData)
library(caret)
library(klaR)

# 1. Carregar base Olivetti
data(faces)
faces <- t(faces)  # cada linha é uma imagem

# Nomear colunas
colnames(faces) <- paste0('a.', 1:ncol(faces))
rownames(faces) <- NULL

# Gerar rótulos (1 a 40, 10 imagens por pessoa)
labels <- rep(1:40, each = 10)

# 2. Normalizar e aplicar PCA
meanx <- colMeans(faces)
X_centered <- scale(faces, center = TRUE, scale = FALSE)
S <- cov(X_centered)
eigS <- eigen(S)

# 3. Visualizar autovalores
autovalores <- eigS$values
plot(autovalores[1:150], type = 'b', xlab = 'Componente Principal', ylab = 'Autovalor',
     main = "Autovalores dos 150 primeiros Componentes", col = "blue", pch = 20)

# 1. Selecionar número menor de componentes
k <- 50
projX <- X_centered %*% eigS$vectors[, 1:k]

# 2. Variância explicada acumulada até k = 50
var_explicada <- cumsum(eigS$values) / sum(eigS$values)
cat(sprintf("Variância explicada com %d componentes: %.2f%%\n", 
            k, var_explicada[k] * 100))

# 3. Reformular conjunto de dados com nova projeção
classe_escolhida <- 1
rotulos_binarios <- ifelse(labels == classe_escolhida, "Classe", "Outros")
projX_df <- as.data.frame(projX)
projX_df$classe <- as.factor(rotulos_binarios)

# 4. Executar o classificador Bayesiano com k = 50
set.seed(42)
n_exec <- 10
taxas_acerto <- numeric(n_exec)

for (i in 1:n_exec) {
  trainIndex <- createDataPartition(projX_df$classe, p = 0.5, list = FALSE)
  trainData <- projX_df[trainIndex, ]
  testData <- projX_df[-trainIndex, ]
  
  modelo <- NaiveBayes(classe ~ ., data = trainData)
  pred <- predict(modelo, testData)$class
  acuracia <- sum(pred == testData$classe) / length(pred)
  taxas_acerto[i] <- acuracia * 100
  cat(sprintf("Execução %d: %.2f%%\n", i, taxas_acerto[i]))
}

# 5. Resultados finais
media_acerto <- mean(taxas_acerto)
desvio_acerto <- sd(taxas_acerto)

cat(sprintf("\nTaxa média de acerto com %d componentes: %.2f%%\n", k, media_acerto))
cat(sprintf("Desvio padrão da acurácia: %.2f%%\n", desvio_acerto))

# 6. Tabela final
resultado_df <- data.frame(Execucao = 1:n_exec, Acuracia = taxas_acerto)
resultado_df[n_exec + 1, ] <- c("Média", media_acerto)
resultado_df[n_exec + 2, ] <- c("Desvio Padrão", desvio_acerto)

print(resultado_df, row.names = FALSE)
