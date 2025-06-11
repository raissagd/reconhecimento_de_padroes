# ================================================================
# 1) LIMPEZA DO AMBIENTE E CARREGAMENTO DE PACOTES
# ================================================================
rm(list = ls())
graphics.off()

library(mlbench)   # Para carregar o dataset BreastCancer
library(RSNNS)     # Para splitForTrainingAndTest

# ================================================================
# 2) FUNÇÃO AUXILIAR: PDF DA GAUSSIANA MULTIVARIADA
# ================================================================
pdfnvar <- function(x, m, K) {
  # x: vetor (dimensão n)
  # m: vetor de médias (dimensão n)
  # K: matriz de covariância (n x n)
  n <- length(x)
  dens <- (1 / sqrt((2 * pi)^n * det(K))) *
    exp(-0.5 * t(x - m) %*% solve(K) %*% (x - m))
  as.numeric(dens)
}

# ================================================================
# 3) CARREGAR E PREPARAR OS DADOS
# ================================================================
data("BreastCancer")
bc <- na.omit(BreastCancer)      # Remove linhas com NA

# Converter a variável alvo 'Class' em -1 (benign) e +1 (malignant)
bc$Class <- ifelse(bc$Class == "malignant", 1, -1)

# Converter todo o data.frame em matriz numérica
# Note: a coluna  1 é 'Id' (vamos ignorar), colunas 2 a 10 são atributos, coluna 11 é a classe
xyall <- data.matrix(bc)

# Separamos em xall (todas as variáveis preditoras) e yall (classe)
xall <- xyall[, 2:10]           # colunas 2..10 = atributos numéricos
yall <- as.numeric(xyall[, 11]) # já está em -1 e +1

# ================================================================
# 4) CÁLCULO DO F‐SCORE PARA CADA VARIÁVEL
# ================================================================
# Índices das amostras de cada classe
ic1 <- which(yall ==  1)  # índice classe +1 (malignant)
ic2 <- which(yall == -1)  # índice classe -1 (benign)

xc1 <- xall[ic1, , drop = FALSE]  # dados malignos
xc2 <- xall[ic2, , drop = FALSE]  # dados benignos

# Médias de cada classe e média global
m1   <- colMeans(xc1)       # média para classe +1
m2   <- colMeans(xc2)       # média para classe -1
mall <- colMeans(xall)      # média global de xall

# Desvio padrão de cada classe
sd1 <- apply(xc1, 2, sd)
sd2 <- apply(xc2, 2, sd)

# Cálculo do F‐score para cada atributo j = 1..9 (que correspondem às colunas originais 2..10)
#   F_j = (|m1_j - mall_j| + |m2_j - mall_j|) / (sd1_j + sd2_j)
fscore <- (abs(m1 - mall) + abs(m2 - mall)) / (sd1 + sd2)

# Plot do F‐score
plot(
  fscore,
  type   = "h",
  lwd    = 2,
  xlab   = "Índice da Variável (original: colunas 2 a 10 do dataset)",
  ylab   = "Fscore",
  main   = "Fscore de Cada Atributo (BreastCancer)"
)
abline(h = 0, col = "gray70")

# Vamos imprimir também os três maiores fscore para confirmação:
ord <- order(fscore, decreasing = TRUE)
cat("Três variáveis de maior F‐score (em xall):\n")
print(ord[1:3])      # posições em xall
cat("Correspondem às colunas originais:\n")
print((ord[1:3]) + 1)  # +1 porque xall corresponde às colunas 2:10 de bc

# Pelo gráfico / pela ordenação de fscore, vemos que as três variáveis de melhor F‐score
# correspondem aos índices EM xall: 2, 3 e 6
# (ou seja, colunas originais do objeto bc: 3, 4 e 7 -> se você fosse usar índice absoluto, mas aqui
# queremos usar os índices relativos dentro de xall, que são 2, 3 e 6).

# ================================================================
# 5) DIVISÃO TREINO (70%) / TESTE (30%)
# ================================================================
set.seed(123)  # Para reprodutibilidade
split <- splitForTrainingAndTest(xall, yall, ratio = 0.3)

Xtr <- split$inputsTrain    # dados de treino (70%)
Ytr <- split$targetsTrain   # classes de treino
Xte <- split$inputsTest     # dados de teste (30%)
Yte <- split$targetsTest    # classes de teste

# ================================================================
# 6) ESTIMATIVA BAYESIANA – TODOS OS ATRIBUTOS (COLUNAS 1 a 9 de Xtr)
# ================================================================
# Identificar índices das amostras de cada classe no treino
ic1_tr <- which(Ytr == -1)  # benign no treino
ic2_tr <- which(Ytr ==  1)  # malignant no treino

xc1_tr <- Xtr[ic1_tr, , drop = FALSE]
xc2_tr <- Xtr[ic2_tr, , drop = FALSE]

Nc1 <- length(ic1_tr)
Nc2 <- length(ic2_tr)
Ntr <- Nc1 + Nc2

# Probabilidades a priori
Pc1 <- Nc1 / Ntr    # P(classe = -1)
Pc2 <- Nc2 / Ntr    # P(classe = +1)

# Médias e covariâncias para cada classe (todas as 9 variáveis)
m1_tr <- colMeans(xc1_tr)
m2_tr <- colMeans(xc2_tr)
K1_tr <- cov(xc1_tr)
K2_tr <- cov(xc2_tr)

# Classificar cada ponto do conjunto de teste
Ntst <- length(Yte)
Yhat_all <- numeric(Ntst)

for (i in 1:Ntst) {
  p1 <- pdfnvar(Xte[i, ], m1_tr, K1_tr) * Pc1   # dens*prior para classe -1
  p2 <- pdfnvar(Xte[i, ], m2_tr, K2_tr) * Pc2   # dens*prior para classe +1
  Yhat_all[i] <- sign(p2 - p1)                  # se p2>p1, classifica como +1; senão -1
}

# Métricas com todas as variáveis
acc_all <- sum(Yhat_all == Yte) / Ntst

MC_all <- table(Predito = Yhat_all, Verdadeiro = Yte)
TN_all <- MC_all["-1", "-1"]
FP_all <- MC_all["1",  "-1"]
FN_all <- MC_all["-1",  "1"]
TP_all <- MC_all["1",   "1"]

Especificidade_all <- TN_all / (TN_all + FP_all)
Sensibilidade_all  <- TP_all / (TP_all + FN_all)

# ================================================================
# 7) ESTIMATIVA BAYESIANA – APENAS ATRIBUTOS 2, 3 e 6 (DE Xtr)
# ================================================================
sel_idx <- c(2, 3, 6)  # Escolhemos as 3 variáveis de maior F‐score em xall

Xtr_sel <- Xtr[, sel_idx, drop = FALSE]  # treino só com colunas 2,3,6
Xte_sel <- Xte[, sel_idx, drop = FALSE]  # teste só com colunas 2,3,6

xc1_tr_sel <- Xtr_sel[ic1_tr, , drop = FALSE]
xc2_tr_sel <- Xtr_sel[ic2_tr, , drop = FALSE]

m1_tr_sel <- colMeans(xc1_tr_sel)
m2_tr_sel <- colMeans(xc2_tr_sel)
K1_tr_sel <- cov(xc1_tr_sel)
K2_tr_sel <- cov(xc2_tr_sel)

Yhat_sel <- numeric(Ntst)

for (i in 1:Ntst) {
  p1 <- pdfnvar(Xte_sel[i, ], m1_tr_sel, K1_tr_sel) * Pc1
  p2 <- pdfnvar(Xte_sel[i, ], m2_tr_sel, K2_tr_sel) * Pc2
  Yhat_sel[i] <- sign(p2 - p1)
}

# Métricas só com as 3 variáveis
acc_sel <- sum(Yhat_sel == Yte) / Ntst

MC_sel <- table(Predito = Yhat_sel, Verdadeiro = Yte)
TN_sel <- MC_sel["-1", "-1"]
FP_sel <- MC_sel["1",  "-1"]
FN_sel <- MC_sel["-1",  "1"]
TP_sel <- MC_sel["1",   "1"]

Especificidade_sel <- TN_sel / (TN_sel + FP_sel)
Sensibilidade_sel  <- TP_sel / (TP_sel + FN_sel)

# ================================================================
# 8) EXIBIR RESULTADOS FINAIS
# ================================================================

cat("=== CLASSIFICADOR BAYESIANO COM TODAS AS VARIÁVEIS (9 ATRIBUTOS) ===\n")
cat(sprintf("Acurácia:       %.4f\n", acc_all))
cat(sprintf("Especificidade: %.4f\n", Especificidade_all))
cat(sprintf("Sensibilidade:  %.4f\n\n", Sensibilidade_all))
print("Matriz de Confusão (todas as variáveis):")
print(MC_all)
cat("\n")

cat("=== CLASSIFICADOR BAYESIANO COM AS VARIÁVEIS SELECIONADAS (2, 3 e 6 de xall) ===\n")
cat(sprintf("Acurácia:       %.4f\n", acc_sel))
cat(sprintf("Especificidade: %.4f\n", Especificidade_sel))
cat(sprintf("Sensibilidade:  %.4f\n\n", Sensibilidade_sel))
print("Matriz de Confusão (variáveis 2,3,6):")
print(MC_sel)
