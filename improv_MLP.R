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
ys <- X[, ncol(X)] # last column is the one we want to predict
xs <- X[, -ncol(X)] # remove last column for the predictor dataset

xs[1:5,]


library(torch)
vocab <- structure(1:5, names = c("a", "b", "c", "d", "e"))

v <- nnf_one_hot(vocab[c("a", "e")], num_classes = 5)
v

W <- torch_rand(5,5)

# defining weight matrice

m <- 3 #dimension that we want to use to represent each character
vocab <- sort(unique(c(xs,ys)))
vocab <- structure(seq_along(vocab), names = vocab)

# C represents the weights
C <- torch_randn(length(vocab), m, requires_grad = TRUE)
e1 <- C[vocab[xs[,1]]]
e2 <- C[vocab[xs[,2]]]
e3 <- C[vocab[xs[,3]]]

e <- torch_cat(list(e1,e2,e3),dim = 2)
e[1, drop=FALSE]
e[2]

C[2]


# apllying Bengio el al 2003
h <- 5
H <- torch_randn(c*m, h, requires_grad = TRUE)
d <- torch_zeros(h, requires_grad = TRUE)
hidden <- torch_tanh(torch_matmul(e, H) + d)
U <- torch_randn(h, length(vocab), requires_grad = TRUE)
b <- torch_randn(length(vocab), requires_grad = TRUE)
logits <- torch_matmul(hidden, U) + b
counts <- torch_exp(logits)
probs <- counts / torch_sum(counts, dim=2, keepdim=TRUE)


nll <- nnf_cross_entropy(counts, vocab[ys])
nllh_log(probs) * nnf_one_hot(vocab[ys], length(vocab))) / length(ys)
nll
