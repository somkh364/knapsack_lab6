
#' Brute force search for knapsack problem
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

brute_force_knapsack <- function(x, W) {
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
  
  if (!any(x$w <= W)) {
    stop('All elements are heavier than W.')
  }
  
  # Setting the starting values for the loop
  
  elements <- which.max(x$w <= W)
  weight <- x$w[elements]
  value <- x$v[elements]
  
  res <- list(value = value, elements = elements)
  
  # The for loop creates all possible combinations of items. It starts with 
  # combining 2 elements. With each iteration it will increase the number of 
  # elements by 1 until the sequence is finished (i == W/minx$w) or one of the
  # conditions to return the result is fulfilled.
  
  for (i in seq(from = 2, to = floor(W/min(x$w)))) {
    
    # Create all  possible combinations as well as of the sum
    # of the weights and values for each combination
    
    all_combinations <- utils::combn(as.integer(row.names(x)), i)
    all_weights <- utils::combn(x$w, i, sum)
    all_values <- utils::combn(x$v, i, sum)
    
    possible_combination <- which(all_weights <= W)
    max_value <- which.max(all_values[possible_combination])
    
    tmp_weight <- all_weights[possible_combination[max_value]]
    tmp_value <- all_values[possible_combination[max_value]]
    tmp_elements <- all_combinations[, possible_combination[max_value]]
    
    # Check whether the value increased in comparison to the iteration before
    # or not. If the values increased the tmp_ values will be the new 
    # preliminary results.
    
    if (any(tmp_value > value, is.na(value))) {
      weight <- tmp_weight
      value <- tmp_value
      elements <- tmp_elements
      
      # If the current weight + the smallest weight in the vector of weights
      # exceeds the limit W the algorithm will return the current result.
      
      if ((weight + min(x$w)) >= W) {
        res$value <- value
        res$elements <- elements
        return(res)
      }
    }
    else {
      return(res)
    }
  }
}
