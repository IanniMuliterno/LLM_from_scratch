Value <- function(data, children=list(), op = "", name=NULL, grad_fn = function() {}) {
  structure(
    rlang::env(
      data = data,
      children = children,
      op = op,
      id = uuid::UUIDgenerate(),
      name = name,
      grad = 0,
      grad_fn = grad_fn
    ),
    class= "Value"
  )
}

print.Value <- function(x) {
  cat("<Value data=", x$data, ">\n")
}

`+.Value` <- function(x, y) {
  out <- Value(
    x$data + y$data,
    children = list(x, y),
    op = "+",
    name = paste(x$name, "+", y$name)
  )
  
  out$grad_fn <- function() {
    x$grad <- x$grad + 1 * out$grad
    y$grad <- y$grad + 1 * out$grad
  }
  
  out
}

`*.Value` <- function(x, y) {
  out <- Value(
    x$data * y$data,
    children = list(x, y),
    op = "*",
    name = paste(x$name, "*", y$name)
  )
  
  out$grad_fn <- function() {
    x$grad <- x$grad + y$data * out$grad
    y$grad <- y$grad + x$data * out$grad
  }
  
  out
}

# -> implementar -, /, ^, sin, cos, tan, tanh

topo_sort <- function(node, topo = list(), visited = c()) {
  if (node$id %in% visited) return(list(topo, visited))
  visited <- c(visited, node$id)
  for (child in node$children) {
    .[topo, visited] <- topo_sort(child, topo, visited)
  }
  topo <- c(topo, node)
  list(topo, visited)
}

backprop <- function(value, visited = c()) {
  value$grad <- 1
  .[topo, ..] <- topo_sort(value)
  for (node in rev(topo)) {
    node$grad_fn()
  }
}


a <- Value(2, name="a")
b <- Value(-3, name = "b")
c <- Value(4 , name = "c")

d <- a * b
d$name <- "d"

e <- d + c
e$name <- "e"

f <- Value(-4, name = "f")
L <- e*f
L$name <- "L"

plot_graph(L)
backprop(L)

a <- Value(2, name = "a")
b <- a + a

b$name <- "b"

backprop(b)

plot_graph(b)

