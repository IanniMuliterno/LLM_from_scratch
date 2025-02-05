library(R6)
# 
# MyClass <- R6Class("MyClass",
#                    public = list(
#                      initialize = function(value) {
#                        private$my_field <- value  # Use 'private' for internal data
#                      },
#                      get_value = function() {
#                        return(private$my_field)
#                      },
#                      set_value = function(new_value) {
#                        private$my_field <- new_value
#                      }
#                    ),
#                    private = list(
#                      my_field = NULL  # Initialize fields in 'private'
#                    )
# )
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

Neuron <- R6::R6Class(
  classname = "Neuron",
  lock_objects = FALSE,
  public = list(
    initialize = function(nin, act) {
      self$nin <- nin
      self$act <- act
      self$W <- lapply(seq_len(nin), function(i) Value(rnorm(1), name = paste0("w", i)))
      self$b <- Value(rnorm(1), name = "b")
    },
    forward = function(x) {
      z <- self$b
      for (i in seq_along(self$W)) {

        z <- z + self$W[[i]] * x[[i]]
      }
      self$act(z)
    }
  )
)

neuron <- Neuron$new(2, function(x) x)
x <- list(Value(1, name = "x1"), Value(2, name = "x2"))
out <- neuron$forward(x)
out


