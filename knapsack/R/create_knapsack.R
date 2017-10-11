#' Generates a knapsack object
#' 
#' @param n number of elements in the knapsack object (numeric)
#' 
#' @return data frame with two columns \code{w} (weight) and \code{v} (value)
#' @export

create_knapsack <- function(n) {
  set.seed(42)
  knapsack_objects <-
    data.frame(
      w=sample(1:4000, size = n, replace = TRUE),
      v=stats::runif(n = n, 0, 10000)
    )
  return(knapsack_objects)
}