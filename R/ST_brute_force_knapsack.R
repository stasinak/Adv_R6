# Speed testing

library(profvis)
set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )
x = knapsack_objects[1:12, ]
W = 3500

# Original
profvis({
  rownames(x) <- 1:nrow(x)
  # too_big <- which(x$w>W)
  # if(length(too_big)!=0)
  #   x <- x[-too_big,]
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
  list(value = best_val, elements = best_ind)
})

####################################################################################################

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )
x = knapsack_objects[1:12, ]
W = 3500

# Pre-processing of the data
profvis({
  rownames(x) <- 1:nrow(x)
  too_big <- which(x$w>W)
  if(length(too_big)!=0)
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
  list(value = best_val, elements = best_ind)
})

# From 70 ms to 20 ms --> Gain of 50 ms approx