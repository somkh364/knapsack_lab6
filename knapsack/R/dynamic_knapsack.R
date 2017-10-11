#
#' Dynamic approach using pseudocode algorithm.
#'
#' @param x data frame with two columns \code{w} (weight) and \code{v} (value) of items to place in the knapsack
#' 
#' @param W the maximum weight (numeric) the knapsack can hold
#'
#' @return theoretical maximum \code{$value} (knapsack value) composed of \code{$elements} (which items)
#' 
#' @export
#'
#'

knapsack_dynamic <- function(x, W) {
  # Error handling 
  if(!all(is.data.frame(x), 
          dim(x)[2] == 2,
          "v" %in% names(x),
          "w" %in% names(x)
  )) {
    stop('x has to be of the type data.frame with two columns.')
  }
  
  if(!all(is.numeric(x$w), is.numeric(x$v))) {
    stop('Values in the data.frame must be numeric and greater than 0.')
  }
  
  if (!all(x > 0)) {
    stop('Values in the data.frame must be numeric and greater than 0.')
  }
  
  if (!all(is.numeric(W), W > 0)) {
    stop("Weight value must be numeric and greater than 0.")
  }
  
  if (!all(all(x$w %% 1 == 0), W %% 1 == 0)) {
    stop("Weight values must be integers.")
  }
  
  
  # The algorithm does not follow the pseudocode on wiki, but rather this 
  # explanation https://www.youtube.com/watch?v=8LusJS5-AGo . It is the same
  # but the matrix is rotated, plus it is explained how to identify actual
  # elements.
  
  n <- nrow(x)
  
  # Accessing vector is much faster than accessing a list (which a data frame is)
  # So we crate vectors for each column
  xw <- x$w
  xv <- x$v
  
  # Pre-allocate matrix
  m <- matrix(NA, nrow = n, ncol = W + 1)
  m[, 1] <- 0
  
  # Identify max values
  for (e in 1:n) { # up to this item is allowed to use
    for (w in 2:(W + 1)) { # max allowed weight in these iteration
      if (xw[e] > w - 1) {
        # max(m[e - 1, w], 0) is needed, because if we are in the no values
        # in the row 0
        m[e, w] <- max(m[e - 1, w], 0)
      } else {
        # max(m[e - 1, w - xw[e]], 0) is needed, because if we are in the 
        # first row, then e - 1 returns numeric(0), but numeric(0) plus any
        # number is numeric(0). Thus, m[e, w - 1] will always be larger. Which 
        # is not what we want.
        m[e, w] <- max(m[e - 1, w], 
                       max(m[e - 1, w - xw[e]], 0) + xv[e])
      }
    }
  }
  
  # Indetify elements
  elements <- c()
  e <- n
  w <- W + 1
  done <- FALSE
  repeat {
    if (m[e, w] == 0) {
      break
    }
    
    if (m[e, w] > m[e - 1, w]) {
      elements <- c(elements, e)
      w <- w - xw[e]
      e <- e - 1
    } else {
      e <- e - 1
    }
  }
  
  solution <- list()
  solution$value <- m[n, W + 1]
  solution$elements <- rev(elements)
  return(solution)
}
