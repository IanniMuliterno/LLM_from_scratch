library(stringr)

nomes <- readr::read_csv("https://dfalbel.github.io/llm-do-zero/assets/nomes.csv")$nome

nomes <- tolower(nomes)

ngrams <- function(nomes, n) {
  ngrams <- list()
  for (nome in nomes) {
    nome <- paste(c(rep(".", n - 1), nome, "."), collapse = "")
    nome <- stringr::str_split_1(nome, "")
    while(length(nome) >= n) {
      ngrams[[length(ngrams) + 1]] <- nome[1:n] 
      nome <- nome[-1]
    }
  }
  ngrams
}

c <- 3
dataset <- ngrams(nomes, n = c + 1)
dataset[1:5]

X <- do.call(rbind, dataset)
ys <- X[, ncol(X)] # a última coluna é a que queremos prever
xs <- X[, -ncol(X)] # removes a última coluna aqui

xs[1:5,]

library(torch)
vocab <- structure(1:5, names = c("a", "b", "c", "d", "e"))

v <- nnf_one_hot(vocab[c("a", "e")], num_classes = 5)
v
