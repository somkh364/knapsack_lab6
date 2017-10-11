# 
#' Greedy Heuristic approach for the knapsack problem
#'
#' @param x data frame with two columns \code{w} (weight) and \code{v} (value) of items to place in the knapsack
#' 
#' @param W the maximum weight (numeric) the knapsack can hold
#' 
#' @param fast whether to use the rcpp optimization
#'
#' @return theoretical maximum \code{$value} (knapsack value) composed of \code{$elements} (which items)
#' 
#' @export
#' 

greedy_knapsack <- function(x, W, fast = FALSE) {
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
  
  n <- nrow(x)
  
  # Add column with initial position
  x$element <- 1:n
  # Sort the data frame according to value-weight ratio in decreasing order
  x <- x[order(x$v / x$w, decreasing = TRUE), ]
  
  value <- 0
  remainingWeight <- W
  elements <- c()
  # Pick elements with high ratio (higher in the data frame) first. If the weight limit
  # allows, pick the ones with smaller ratio
  for (e in 1:n) {
    if (x$w[e] <= remainingWeight) {
      value <- value + x$v[e]
      remainingWeight <- remainingWeight - x$w[e]
      elements <- c(elements, x$element[e])
    } 
  }
  
  solution <- list()
  solution$value <- value
  solution$elements <- elements
  
  return(solution)
}
#Generate knapsack objects
