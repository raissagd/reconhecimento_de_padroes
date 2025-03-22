myknn <- function(X, Y, xt, k) {
  # X: Matriz de pontos do dataset (conjunto de treinamento)
  # Y: Vetor de rótulos (-1 e 1, representando as duas classes)
  # xt: Vetor de teste (o ponto que queremos classificar)
  # k: Número de vizinhos a considerar no algoritmo k-NN
  
  # Obtém o número de pontos no conjunto de treinamento (N) e a dimensão dos pontos (n)
  N <- dim(X)[1]  # Número total de pontos no dataset
  n <- dim(X)[2]  # Dimensão dos pontos (deve ser 2 para esse exemplo)
  
  # Cria uma sequência de 1 até N, representando os índices dos pontos no dataset
  seqi <- seq(1, N, 1)
  
  # Inicializa um vetor para armazenar as distâncias entre o ponto de teste e cada ponto do dataset
  mdist <- matrix(nrow = N, ncol = 1)
  
  # Calcula a distância euclidiana entre o ponto de teste xt e cada ponto do dataset X
  for (i in seqi) {
    xc <- X[i, ]  # Seleciona o i-ésimo ponto do dataset
    # Calcula a distância euclidiana: sqrt((x1 - xt1)^2 + (x2 - xt2)^2)
    mdist[i] <- sqrt(sum((xc - t(xt))^2))
  }
  
  # Ordena os índices das distâncias em ordem crescente (do menor para o maior)
  ordmdist <- order(mdist)
  
  # Reorganiza os rótulos Y de acordo com a ordem das distâncias (primeiros k são os mais próximos)
  ordY <- Y[ordmdist]
  
  # Calcula a soma dos rótulos dos k vizinhos mais próximos
  # Se a soma for positiva, a classe será 1; se for negativa, a classe será -1
  yxt <- sign(sum(ordY[1:k]))
  
  # Cria uma lista com três elementos:
  # 1. yxt: Rótulo previsto para o ponto xt
  # 2. mdist: Vetor com as distâncias calculadas
  # 3. ordY: Vetor com os rótulos ordenados pelos vizinhos mais próximos
  retlist <- list(yxt, mdist, ordY)
  
  # Retorna a lista de resultados
  return(retlist)
}