library(torch)
library(stringr)

data <- readr::read_file('https://dfalbel.github.io/llm-do-zero/assets/machado.txt')
cat(stringr::str_sub(data,100,1000))

vocab <- sort(unique(stringr::str_split_1(data, '')))
vocab
vocab <- structure(seq_along(vocab), names = vocab)
vocab

# token is the unit we want to predict, it can be words for example but we are going to use character

data <- stringr::str_split_1(data, '')

get_batch <- function(split, block_size = 8, batch_size = 4) {
  
}

# implement layer by layer
GPT <- nn_module {
  
  #output embedding = initialize
  
  #linear (it's considered the head of the transformer) = forward
  
  #step function is an easier alternative to:
  
}