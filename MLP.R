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

# Define the Value class
Value <- R6::R6Class(
  "Value",
  public = list(
    value = NULL,
    name = NULL,
    initialize = function(value, name = "") {
      self$value <- value
      self$name <- name
    },
    # Define the `*` operator for multiplication
    `*` = function(other) {
      if (inherits(other, "Value")) {
        Value$new(self$value * other$value, name = paste0(self$name, "*", other$name))
      } else {
        Value$new(self$value * other, name = paste0(self$name, "*", other))
      }
    },
    # Define the `+` operator for addition
    `+` = function(other) {
      if (inherits(other, "Value")) {
        Value$new(self$value + other$value, name = paste0(self$name, "+", other$name))
      } else {
        Value$new(self$value + other, name = paste0(self$name, "+", other))
      }
    },
    # Print method for better output
    print = function() {
      cat("Value:", self$value, "Name:", self$name, "\n")
    }
  )
)

# Define the Neuron class
Neuron <- R6::R6Class(
  lock_objects = FALSE,
  public = list(
    initialize = function(nin = 4) {
      self$ws <- lapply(seq_len(nin), function(i) {
        Value$new(rnorm(1), name = paste0("w", i))
      })
      self$b <- Value$new(0, name="b")
    },
    forward = function(x) {
      z <- self$b
      for (i in seq_along(x)) {
        z$value <- z$value + x[[i]]$value * self$ws[[i]]$value
      }
      z
    }
  )
)

# Create neurons
n1 <- Neuron$new(nin = 4)
n2 <- Neuron$new(nin = 4)
n3 <- Neuron$new(nin = 4)

# Print weights and bias
n1$ws
n1$b

# Define input vector
x <- list(
  Value$new(-3, name="x1"),
  Value$new(-2, name="x2"),
  Value$new(1, name="x3"),
  Value$new(4, name="x4")
)

# Print input vector
x

# Perform forward pass
o <- n1$forward(x)
o



plot_graph(o[[1]])

xs <- list(
  c(-3, -2, 1, 5),
  c(-3, -2, 1, 2),
  c(-2, -1, 2, 3),
  c(-1, -3, 2, 1)
)

xs <- lapply(seq_along(xs), function(i) {
  lapply(seq_along(xs[[i]]), function(j) Value(xs[[i]][j], name = paste0("x", i, j)))
})

ys <- c(1, -1, 1, -1)
ys <- lapply(
  seq_along(ys),
  function(i) Value(ys[i], name = paste0("y", i))
)


for (i in 1:100) {
  
  os <- lapply(xs, net$forward)
  
  L <- (os[[1]] - ys[[1]])^2 +
    (os[[2]] - ys[[2]])^2 +
    (os[[3]] - ys[[3]])^2 +
    (os[[4]] - ys[[4]])^2
  
  backprop(L)
  
  for (layer in net$layers) {
    for(neuron in layer$neurons) {
      for(w in neuron$ws) {
        w$data <- w$data - 0.001 * w$grad
        w$grad <- 0
      }
      neuron$b$data <- neuron$b$data - 0.001 * neuron$b$grad
      neuron$b$grad <- 0
    }
  }
  
  print(L)
}


