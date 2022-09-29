#16,3500: Time = 150ms

profile_bruteforce <- function(number=16,W=3500){
  x <- create_problem(2000)

  p <- profvis({
    knapsack_brute_force(x[1:number,],W)
  })

  return(p)
}

#500,3500: Time = 2400ms
profile_dynamic <- function(number=500,W=3500){
  x <- create_problem(2000)

  p <- profvis({
    knapsack_dynamic(x[1:number,],W)
  })

  return(p)
}

#1000000,3500: Time = 790ms
profile_greedy <- function(number=1000000,W=3500){
  x <- create_problem(number)

  p <- profvis({
    greedy_knapsack(x, W)
  })

  return(p)
}
