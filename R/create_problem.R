#' Function to create a large dataset for the Knapsack problem
#'
#' @param nmax size of the dataset
#'
#' @return a data.frame of size nmax
#' @export
#' @import stats
#'
#'
#' @examples
#' x <- create_problem(2000)
#' print(head(x))
create_problem <- function(nmax){
  suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
  ##old sampler used for backward compatibility
  ## suppressWarnings() can be used so that the above warning is not displayed
  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
  n <- nmax
  knapsack_objects <-
    data.frame(
      w=sample(1:4000, size = n, replace = TRUE),
      v=runif(n = n, 0, 10000)
    )
  return(knapsack_objects)
}
