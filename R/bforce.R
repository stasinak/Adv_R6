rm(list = ls())
set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )

bf_ks_sol <- function(x, W) {
  
  if(!is.data.frame(x) | ncol(x)!=2) stop("The input object is not of data.frame type.\n")
  if(!(all(colnames(x)==c("v", "w")) | all(colnames(x)==c("w", "v"))))
    stop("The data.frame should have the columns named 'v' and 'w'.")
  if(!is.numeric(W) | length(W)!=1 | W<=0) stop("The total weight (W) should be a positive scalar")
  
  rownames(x) <- 1:nrow(x)
  too_big <- which(x$w>W)
  x <- x[-too_big,]
  n <- nrow(x)
  
  best_val <- 0
  best_ind <- 0
  i <- 1
  bits <- intToBits(i)
  
  while(bits[n+1]==0) {
    ind <- which(bits==1)
    if(sum(x$w[ind])<=W & sum(x$v[ind])>best_val) {
      best_val <- sum(x$v[ind])
      best_ind <- as.numeric(rownames(x)[ind])
    }
    i <- i+1
    bits <- intToBits(i)
  }
  
  return(list(value = best_val, elements = best_ind))
}


bf_ks_sol(x = knapsack_objects[1:8, ], 3500)