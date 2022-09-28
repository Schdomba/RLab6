profile_bruteforce <- function(number=16,W=3500){
  x <- create_problem(2000)

  p <- profvis({
    knapsack_bruteforce(x[1:number,],W)
  })

  return(p)
}
