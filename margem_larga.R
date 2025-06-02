set.seed(42)
rm(list = ls())

# ------------------------------------------------------------------------
# Geração dos dados
n <- 2
N1 <- 50
sd1 <- 0.2
xc1 <- matrix(rnorm(N1 * n, sd = sd1), ncol = n, nrow = N1) + matrix(c(2, 2), nrow = N1, ncol = n, byrow = TRUE)

N2 <- 50
sd2 <- 0.2
xc2 <- matrix(rnorm(N2 * n, sd = sd2), ncol = n, nrow = N2) + matrix(c(4, 4), nrow = N2, ncol = n, byrow = TRUE) 

X <- rbind(xc1, xc2)
X_aug <- cbind(1, X)  # adiciona a coluna de bias
Y <- c(rep(-1, N1), rep(1, N2))  # rótulos para as classes
plot(xc1[,1], xc1[,2], col = "red", pch = 19, xlim = c(0, 5), ylim = c(0, 5), xlab = "x1", ylab = "x2")
points(xc2[,1], xc2[,2], col = "blue", pch = 19)

# ------------------------------------------------------------------------
# Função para calcular a margem
calc_margin_braga <- function(w, X_aug, Y) {
  margins <- (X_aug %*% w)
  imax_neg = which(margins == max(margins[margins < 0]))[1]
  imin_pos = which(margins == min(margins[margins > 0]))[1]
  return(list(imax_neg, imin_pos, margins))  # margem geométrica
}

# ------------------------------------------------------------------------
# Algoritmo Perceptron  + salvando margens

max_iter <- 1000
w <- rep(0, n + 1)  # pesos iniciados com 0 (inclui bias)
margin_history <- numeric(max_iter)
w_history <- list()

for (epoch in 1:max_iter) {
  updated <- FALSE
  index = sample(nrow(X_aug))
  for (i in index) {
    if (Y[i] * (t(w) %*% X_aug[i,]) <= 0) {
      w <- w + Y[i] * X_aug[i,]
      updated <- TRUE
    }
  }
  if (!updated) break  # convergiu
}

margins <- calc_margin_braga(w, X_aug, Y)

margem_neg <- margins[[3]][margins[[1]]]
margem_pos <- margins[[3]][margins[[2]]]

# ------------------------------------------------------------------------
# Plotando os dados e as margens

plot(xc1[,1], xc1[,2], col = "red", pch = 19, xlim = c(0, 5), ylim = c(0, 5), xlab = "x1", ylab = "x2")
points(xc2[,1], xc2[,2], col = "blue", pch = 19)
abline(a = -w[1]/w[3], b = -w[2]/w[3], col = "black", lwd = 2)  # linha de decisão