
#' Title
#'
#' @param x a data.frame with two variables v(alue) and w(eight)
#' @param W the size of the napsack
#'
#' @return
#' @export
#'
#' @examples
knapsack_brute_force <- function(x, W,parallel=FALSE){
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
  if(parallel == TRUE){
    num_cores <- detectCores()
    print(num_cores)
    #clust <- makeCluster(num_cores)
    #clusterExport(clust,comb)
    possib_knapsacks <- t(mclapply(mc.cores = num_cores,X=comb,MARGIN=1,FUN=sum_up))
    stopCluster(clust)
  }
  else{
    possib_knapsacks <- t(apply(X=comb,MARGIN=1,FUN=sum_up))
  }
  max_val <- max(possib_knapsacks[,'value'])
  best_index <- which(possib_knapsacks[,'value'] == max_val)
  best_comb <- comb[best_index,]

  package_nums <- which(best_comb == TRUE)
  return_lst <- list(value=max_val,elements=package_nums)

  return(return_lst)
}

knapsack_dynamic <- function(x, W){
  n <- length(x$w)
  m <- matrix(0,nrow=n+1,ncol=W+1)

  for(i in 1:n){
    #print(paste("i=",i))
    for(j in 1:(W+1)){
      #print(paste("j=",j))
      if(x$w[i] > j-1){
        m[i+1,j] <- m[i,j]
      }
      else{
        m[i+1,j] <- max(m[i,j],m[i,j-x$w[i]]+x$v[i])
      }
    }
  }

  knapback <- function(i,j){
    if (i == 1){
      return()
    }
    if(m[i,j] > m[i-1, j]){
        ret = c(i, knapback(i-1, j-x$w[i-1]))
        return(ret)
    }
    else{
      return(knapback(i-1,j))
    }
  }

  item_vec <- knapback(n+1,W+1)
  return_lst <- list(value=m[n+1,W+1],elements=rev(item_vec-1))
  return(return_lst)
}

greedy_knapsack <- function(x, W){
  x$ratio <- x$v / x$w
  x <- x[order(x$ratio,method="radix",decreasing=TRUE),] #profile later, which sort is fastest
  current_W <- 0
  current_v <- 0
  elem_vec <- c()
  for(i in 1:length(x$w)){
    if((current_W + x$w[i]) <= W){
      current_W <- current_W + x$w[i]
      current_v <- current_v + x$v[i]
      elem_vec <- append(elem_vec,rownames(x)[i])
    }
  }
  return_lst <- list(value=current_v,elements=elem_vec)
  return(return_lst)
}
