library(DiagrammeR)
library(dotty)

plot_graph <- function(value, ...) {
  graph <- create_graph() |> 
    add_global_graph_attrs("rankdir", "LR", "graph") |>
    add_global_graph_attrs("ranksep", "equally", "graph") |>
    add_global_graph_attrs("layout", "dot", "graph")
  
  .[graph, ..] <- plot_graph_impl(graph, value)
  graph <- graph |>
    select_nodes(conditions = type == "data") |>
    set_node_attrs_ws(node_attr = shape, value = "record") |>
    set_node_attr_to_display("d") |>
    clear_selection() |>
    set_node_attrs(fontsize, 12) |>
    set_node_attrs(node_attr = "fixedsize", values = FALSE) |>
    set_graph_directed()
  
  render_graph(graph, ...)
}

plot_graph_impl <- function(graph, v, parent = NULL, visited = c()) {
  if (v$id %in% visited) {
    return(list(graph, visited))
  }
  visited <- c(visited, v$id)
  
  # Add itself to the graph
  display <- if (length(v$name) && v$name != "") {
    glue::glue("{v$name} | data: {format(v$data)}")
  } else {
    glue::glue("value: {format(v$data)}")
  }
  
  if (!is.null(v$grad)) {
    display <- glue::glue("{display} | grad: {format(v$grad)}")
  }
  
  graph <- graph |>
    add_node(
      label = v$id, 
      node_data = node_data(d = display), 
      type = "data"
    ) 
  
  # Add the op to the graph
  if (v$op != "") {
    graph <- graph |>
      add_node(
        label = paste0(v$id, "_op"), 
        node_data = node_data(d = v$op), 
        type = "op"
      ) |>
      add_edge(from = paste0(v$id, "_op"), to = v$id)
  }
  
  # Add edges to parent
  if (!is.null(parent)) {
    graph <- graph |> add_edge(
      from = as.character(v$id), 
      to = paste0(parent$id, "_op")
    )
  }
  
  # Add children
  for (child in v$children) {
    .[graph, visited] <- plot_graph_impl(graph, child, parent = v, visited = visited)
  }
  
  list(graph, visited)
}


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
################################# Neuron #################################
# in order to create a multilayer perceptron ( MLP) we need to define the neurons
# then the layers and then combine them into a MLP

Neuron <- R6::R6Class(
  lock_objects = FALSE,
  public = list(
    initialize = function(nin = 4) {
      self$ws <- lapply(seq_len(nin), function(i) {
        Value(rnorm(1), name = paste0("w", i))
      })
      self$b <- Value(0, name="b")
    },
    forward = function(x) {
      z <- self$b
      for (i in seq_along(x)) {
        z <- z + x[[i]] * self$ws[[i]]
      }
      z
    }
  )
)

n1 <- Neuron$new(nin = 4)
n2 <- Neuron$new(nin = 4)
n3 <- Neuron$new(nin = 4)

n1$ws
n1$b

x <- c(
  Value(-3, name="x1"),
  Value(-2, name="x2"),
  Value(1, name="x3"),
  Value(4, name="x4")
)


x
o <- n1$forward(x)
o

plot_graph(o)
################################# Layer #################################
Layer <- R6::R6Class(
  lock_objects = FALSE,
  public = list(
    initialize = function(nin, nout, act) {
      
      self$neurons <- lapply(seq_len(nout), function(x){
        Neuron$new(nin = nin)
      })
      self$act <- act
      
      self$forward <- function(x) {
        lapply(self$neurons, function(n) {
        self$act(n$forward(x))
      }) 
      }
      }
  )
)

l <- Layer$new(4,5,identity)
l$neurons[[1]]
l$forward(x)



################################# MLP #################################

MLP <- R6::R6Class(
  classname = "MLP",
  lock_objects = FALSE,
  public = list(
    initialize = function(nin, nouts, act) {
      szs <- c(nin, nouts)
      self$layers <- list()
      for (i in seq_along(nouts)) {
        self$layers[[i]] <- Layer$new(szs[i], szs[i+1], act)
      }
    },
    forward = function(x) {
      out <- x
      for (layer in self$layers) {
        out <- layer$forward(out)
      }
      if (length(out) == 1) out[[1]] else out
    }
  )
)
################################# Practice #################################
# we'll need to implement tanh and ^ functions to proceed

`^.Value` <- function(x , y) {
  out <- Value(
    x$data ^ y
    ,children = list(x)
    ,op = "^"
    ,name = paste(x$name,"^",y)
  )
  out$grad_fn <- function() {
    x$grad <- x$grad + y * (x$grad^(y - 1)) * out$grad
  }
  out
}

`tanh.Value` <- function(x) {
  
  t <-  tanh(x$data)
  out <- Value(
    t
    ,children = list(x)
    ,op = "tanh"
    ,name = paste0("tanh(",x$name,")")
  )
  out$grad_fn <- function() {
    x$grad <- x$grad + (1 - t^2) * out$grad
  }
  out
}

#sum is positive = 1
x <- list(list(-3,2,1,-1), # 0
          list(10,-2,-1,-1), # 1
          list(-3,-2,5,0), #0
          list(-4,-2,1,-1),# 0
          list(3,2,8,-1), # 1
          list(0,2,1,1)) #1



M_L_P <- MLP$new(nin = 4,nout = c(4,5,3,1),act = tanh)

lapply(x, M_L_P$forward)
