
#' Title
#'
#' @param x a data.frame with two variables v(alue) and w(eight)
#' @param W the size of the napsack
#'
#' @return
#' @export
#'
#' @examples
knapsack_bruteforce <- function(x, W){
  stopifnot(is.data.frame(x),
            "v" %in% names(x),
            "w" %in% names(x)
            )
  possible <- rep(list(c(TRUE,FALSE)),length(x$w))
  #list of all possible combinations
  comb <- expand.grid(possible)

  sum_up <- function(bool_vec){
    w_sum <- sum(x$w[bool_vec])
    if(w_sum > W){
      v_sum <- 0
    }
    else{
      v_sum <- sum(x$v[bool_vec])
    }
    return(c(weight=w_sum, value=v_sum))
  }

  possib_knapsacks <- t(apply(X=comb,MARGIN=1,FUN=sum_up))
  max_val <- max(possib_knapsacks[,'value'])
  best_index <- which(possib_knapsacks[,'value'] == max_val)
  best_comb <- comb[best_index,]

  package_nums <- which(best_comb == TRUE)
  return_lst <- list(value=max_val,elements=package_nums)

  return(return_lst)
}
