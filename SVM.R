rm(list=ls())
library("kernlab")

# Gerar dados
set.seed(123)
xc1 = replicate(2, rnorm(50) + 4)
xc2 = replicate(2, rnorm(50) + 2)
xin = rbind(xc1, xc2)
yin = rbind(matrix(-1, 50, 1), matrix(1, 50, 1))

# Plot dos dados
plot(xc1[,1], xc1[,2], col = "red", pch = 19, xlim = c(0, 6), ylim = c(0, 6), xlab = "x1", ylab = "x2")
points(xc2[,1], xc2[,2], col = "blue", pch = 19)

# Treinar SVM
svmtrein <- ksvm(xin, yin, type = "C-svc", kernel = 'rbfdot', kpar = list(sigma = 0.1), C = 15)
yhat <- predict(svmtrein, xin, type = "response")
ai <- SVindex(svmtrein)
points(xin[ai, 1], xin[ai, 2], col = "black")

# Análise com diferentes valores de h (sigma)
seqh <- seq(0.01, 8, 0.5)
sumcov <- numeric(length(seqh))  # Melhor que matrix com ncol=1
ch <- 1

for (h in seqh) {
  # Matriz de distâncias euclidianas
  dall <- as.matrix(dist(xin, diag = TRUE, upper = TRUE))
  kall <- exp(-(dall * dall) / (2 * (h * h)))
  
  k11 <- kall[1:50, 1:50]
  k12 <- kall[1:50, 51:100]
  K22 <- kall[51:100, 51:100]
  K21 <- kall[51:100, 1:50]
  
  P11 <- rowSums(k11)
  P12 <- rowSums(k12)
  P22 <- rowSums(K22)
  P21 <- rowSums(K21)
  
  p1 <- cbind(P11, P12) / 50
  p2 <- cbind(P21, P22) / 50
  pall <- cbind(p1, p2)
  
  cova <- cov(pall)
  if (all(!is.na(cova))) {
    sumcov[ch] <- sum(cova)
  } else {
    sumcov[ch] <- NA
  }
  
  plot(p1[,1], p1[,2], col = 'red', xlim = c(0,1), ylim = c(0,1), pch = 19, xlab = "p1", ylab = "p2", main = paste("h =", round(h, 2)))
  points(p2[,1], p2[,2], col = 'blue', pch = 19)
  
  Sys.sleep(0.5)
  ch <- ch + 1
}

# Pausa opcional
Sys.sleep(1)

# Plot final
plot(seqh, sumcov, type='b', xlab="h (sigma)", ylab="Soma das covariâncias", main="Soma das covariâncias para cada h")
