#16,3500: Time = 150ms
#19,3500 single core : 1560 , parallel : 2830. gain : - 1270 ms
#' Profling function for Brute force method
#'
#' @param number number of row to select from randomly generated dataset
#' @param W maximum weight limit
#' @param parallel parallel flag, default = FALSE.
#' @import profvis
#' @return profvis object
#' @export
#'
#' @examples
#' profile_bruteforce(16,3500)
profile_bruteforce <- function(number=16,W=3500,parallel=FALSE){
  x <- create_problem(2000)

  p <- profvis({
   r<- brute_force_knapsack(x[1:number,],W,parallel = parallel)
   print(r)
  })

  return(p)
}

#' Profling function for Dynamic method
#'
#' @param number number of row to select from randomly generated dataset
#' @param W maximum weight limit
#' @import profvis
#' @return profvis object
#' @export
#'
#' @examples
#' profile_dynamic(500,3500)

#500,3500: Time = 2400ms
profile_dynamic <- function(number=500,W=3500){
  x <- create_problem(2000)

  p <- profvis({
    knapsack_dynamic(x[1:number,],W)
  })

  return(p)
}

#' Profling function for Greedy Heuristic method
#'
#' @param number number of row to select from randomly generated dataset
#' @param W maximum weight limit
#' @import profvis
#' @return profvis object
#' @export
#'
#' @examples
#' profile_greedy(1000000,3500)

#1000000,3500: Time = 790ms
profile_greedy <- function(number=1000000,W=3500){
  x <- create_problem(number)

  p <- profvis({
    greedy_knapsack(x, W)
  })

  return(p)
}
