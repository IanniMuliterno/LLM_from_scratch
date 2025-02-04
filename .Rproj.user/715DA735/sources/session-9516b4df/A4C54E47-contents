# our goal is to create a library that's capable of calculating gradients

# some cool things that will happen here:
#demonstrate inplace in R
#chain rule
#topological sorting

#Memo: why am I doing this 
#1. the core of neural network is gradient descent
#2. if we want to calculate gradient we need back propagation
#3. back propagation is just an efficient application of chain rule
#4. chain rule is just a technique to calculate derivative of a function
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
######################### derivative ########################
# Let's start by defining derivative
# df/dx = (f(x + h) - f(x))/h when h tends to zero
# that means the derivative represent how much f changes if x changes just a little, I mean, almost 0

# 
# f <- function(x) {
#   x^2
# }
# OR :
f <- \(x) x^2

h <- 1e-9
x <- 2

(f(x + h) - f(x))/h

######################### chain rule ########################
# the derivative of a composite function is the product of the derivates of inner and outer functions
# if Z(x) = g(f(x)) then  Z'(x) = g'(f(x)) * f'(x)
  
  x <- 4
  z <- \(x) (x^2 + 1)^0.5
  
  #then g(f(x)) = f(x)^0.5 and f(x) = x^2 + 1
    f <- \(x) (x^2 + 1)
    g <- \(x) x^(1/2)
  
  # the derivative of z is
  ((g(x + h) - g(x))/h)  *  ((f(x + h) - f(x))/h)
  
  # to prove that the result above is correct we can write down the formula of z'
    # = (1/(2*x^0.5)) * 2*x
    # = 2*x/2*x^0.5
    # = x^0.5
    # if x = 4 then x^0.5 is 2
  
######################### multiple variables | partial derivative ########################
# when we have a function with multiple variables the derivative of each function represents how much each
# variable makes the function change if the other variables remain the same
  
f_xy <- \(x,y) x*y

# df_xy/dy = x
# df_xy/dx = y

print("the vector containing the partial derivative in relationship to each variable is called gradient")
    # for example, for f_xy the gradient woud be c(y,x)
    # for x*y + z the gradient would be c(y,x,1)
    
print('we want to go in the opposite direction of the derivative in order to minimize the function, because')

f <- \(x) x^3 - 3*x^2 + 2
f_prime <- \(x) 3*x^2 - 6*x

x_values <- seq(-1, 3, 0.1)
f_values <- sapply(x_values, f)
f_prime_values <- sapply(x_values, f_prime)
                   
                   
plot(x_values, f_values,ylim = c(-5,5))
lines(x_values, f_prime_values, col='red', lwd = 2)

#critic points | inflection
# where f derivative is 0
points(0,f(0), col = 'green', pch=19)
points(2,f(2), col = 'green', pch=19)
print('if at a certain point x f_prime presents a positive result that means you need smaller x values to decrease the result')
print('if at a certain point x f_prime presents a negative result that means you need greater x values to decrease the result')


######################### micrograd ########################
a <- 2
b <- 3
c <- 4
f <- 2
d <- a*b 
e <- d + c
L <- e*f 

# suppose L is a loss fucntion with variables a,b,c, and f, that we want to minimize
# that means we need to find it's gradient in relationship to a,b,c and f
# that means [dL/da,dL/db,dL/dc,dL/df]
# with chain rule we know that
# dL/da = dL/de * de/dd * dd/da
'dL/da = f*1*b'
'dL/db = f*1*a'
'dL/dc = f*1'
'dL/dd = f*1'
'dL/df = e'


Value <- function(data, children = list(),op = "", name= NULL, grad_fn = function() {}) {
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
    class = "Value"
  )
}

print.Value <- function(x, ...) {
  cat(paste0("<Value(data= ",x$data,", op= ",x$op, ")>\n"))
}


a <- Value(2)
print(a)

# we can also rewrite this using R6 package
library(R6)
library(uuid)

# Define the R6 class
ValueClass <- R6Class(
  "Value",
  public = list(
    data = NULL,
    children = list(),
    op = "",
    id = NULL,
    name = NULL,
    
    # Initialize method
    initialize = function(data, children = list(), op = "", name = NULL) {
      self$data <- data
      self$children <- children
      self$op <- op
      self$id <- UUIDgenerate()
      self$name <- name
    },
    
    # Print method
    print = function(...) {
      cat(paste0("<Value(data= ", self$data, ", op= ", self$op, ")>\n"))
    }
  )
)

# Create an instance of the class
aa <- ValueClass$new(2)

# Print the instance
aa$print()


  ########################## implement basic function to Value ##########################

`+.Value` <- function(x, y) {
  if(!inherits(y, "Value")) y <- Value(y)
  out <- Value(x$data + y$data, 
        children = list(x,y),
        op = "+",
        name = glue::glue("{x$name}+{y$name}"))
  
  out$grad_fn <- function() {
    
    x$grad <- 1.0 * out$grad
    y$grad <- 1.0 * out$grad
  }
  
  out
}

`-.Value` <- function(x, y) {
  if(!inherits(y, "Value")) y <- Value(y)
  out <- Value(x$data - y$data, children = list(x,y), op = "-", name = glue::glue("{x$name}-{y$name}"))
  
  out$grad_fn <- function() {
    
    x$grad <- 1.0 * out$grad
    #y$grad <- -1.0 * out$grad
    y$grad <- -out$grad
  }
  
  out
}

`*.Value` <- function(x, y) {
  if(!inherits(y, "Value")) y <- Value(y)
  out <- Value(x$data * y$data, children = list(x,y), op = "*", name = glue::glue("{x$name}*{y$name}"))
  
  out$grad_fn <- function() {
    
    x$grad <- out$grad * y$data
    y$grad <- out$grad * x$data
  }
  
  out
}

`/.Value` <- function(x, y) {
  if(!inherits(y, "Value")) y <- Value(y)
  out <- Value(x$data / y$data, children = list(x,y), op = "/", name = glue::glue("{x$name}/{y$name}"))
  
  out$grad_fn <- function() {
    
    x$grad <- (1.0 * out$grad) / y$data
    y$grad <- (-1.0 * x$data * out$grad) / (y$data)^2
    
  }
  
  out
}  

`^.Value` <- function(x, y) {
  if(!is.numeric(y, "Value")) stop("y must be numeric")
  out <- Value(x$data ^ y, children = list(x), op = "^", name = glue::glue("{x$name}^{y}"))
  
  out$grad_fn <- function() {
    
    x$grad <- y$data * (x$data)^(y$data - 1) * out$grad
  }
  
  out
}  

# variables of L
a <- Value(2, name = "a")
b <- Value(4, name = "b")
c <- Value(3, name = "c")
f <- Value(2, name = "f")

# sub-functions of L
d <- a*b
e <- d + c
print(d)
print(e)
L <- e*f
print(L)

plot_graph(L)
L$name <- "L"
d$name <- "d"
e$name <- "e"
plot_graph(L)

########################## calculate grad without grad_fn ##########################

L$grad <- 1 #dL/dL
e$grad <- f$data #dL/de
f$grad <- e$data #dL/df

d$grad <- e$grad*1 #dL/de * de/dd
c$grad <- e$grad*1 #dL/de * de/dc

b$grad <- d$grad*a$data #dL/de * de/dd * dd/db = d$grad * dd/db
a$grad <- d$grad*b$data 

plot_graph(L)

print("what we just did is backpropagation")

########################## calculate grad with grad_fn ##########################
rm(a,b,c,d,e,f,L)

# variables of L
a <- Value(2, name = "a")
b <- Value(4, name = "b")
c <- Value(3, name = "c")
f <- Value(2, name = "f")

# sub-functions of L
d <- a*b
e <- d + c
print(d)
print(e)
L <- e*f
print(L)

plot_graph(L)
L$name <- "L"
d$name <- "d"
e$name <- "e"
plot_graph(L)

L$grad <- 1
L$grad_fn()
plot_graph(L)
f$grad_fn()
plot_graph(L)
e$grad_fn()
plot_graph(L)
d$grad_fn()
c$grad_fn()
b$grad_fn()
a$grad_fn()
plot_graph(L)


########################## implem. backpropagation ##########################
