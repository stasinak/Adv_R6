# Speed testing

### DYNAMIC

# Original

library(profvis)

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )
x = knapsack_objects[1:12, ]
W = 3500

profvis({
  item_count = nrow(x)
  x$order = 1:item_count
  x = x[order(x$w),]
  table = matrix(0, item_count, W+1)
  # set 1st row
  table[1, ] = c(rep(0, x$w[1]), rep(x$v[1], ncol(table)-x$w[1]))
  for(i in 2:nrow(table)){
    for(j in 2:ncol(table)){
      w_lim = j-1
      if(w_lim < x$w[i])
        table[i,j] = table[i-1, j]
      else
        table[i,j] = max(table[i-1, j], (x$v[i] + table[i-1, j-x$w[i]]))
    }
  }
  
  # finding elements
  i <- item_count
  j <- W+1
  ind <- c()
  
  while(j>1) {
    if(i>1) {
      if(table[i, j] == table[i-1, j]) 
        i <- i-1
      else {
        ind <- c(ind, x$order[i])
        j <- j - x$w[i]
        i <- ifelse(i>1, i-1, i)
      }
    }
    else {
      ind <- c(ind, x$order[i])
      j <- 1
    }
  }
  return(list(value=table[item_count,W+1], elements=ind))
})

# The problem is defining the matrix

####################################################################################################

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )
x = knapsack_objects[1:12, ]
W = 3500

profvis({
  item_count = nrow(x)
  x$order = 1:item_count
  x = x[order(x$w),]
  too_big <- which(x$w>W)
  if(length(too_big)!=0)
    x <- x[-too_big,]
  item_count = nrow(x)
  s <- x$w[1]+x$w[2]
  table = matrix(0, item_count, W+1)
  # set 1st row
  table[1, ] = c(rep(0, x$w[1]), rep(x$v[1], ncol(table)-x$w[1]))
  j_prec <- 1
  #print(c("i", "j", "j_prec", "n_row", "n_col"))
  
  for(i in 2:nrow(table)){
    
    j <- x$w[i]+1
    #print(c(i, j, j_prec, item_count-i+1, j-j_prec+1))
    table[i:item_count, j_prec:(j-1)] <- 
      matrix(table[i-1, j_prec:(j-1)], item_count-i+1, j-j_prec, byrow = T)
    
    while( j<=(W+1) ) {
      
      if(j-1 <= s) {
        
        if(j-1 < x$w[i]) 
          table[i,j] = table[i-1, j]
        else
          table[i,j] = max(table[i-1, j], (x$v[i] + table[i-1, j-x$w[i]]))
        j <- j+1
        
      }
      
      else {
        j_prec <- x$w[i]+1
        table[i, (j-1):W+1] <- table[i, j-1]
        if(i<item_count)
          s <- s+x$w[i+1]
        j <- W+2
      }
      
    }
  }
  
  # finding elements
  i <- item_count
  j <- W+1
  ind <- c()
  
  while(j>1) {
    if(i>1) {
      if(table[i, j] == table[i-1, j]) 
        i <- i-1
      else {
        ind <- c(ind, x$order[i])
        j <- j - x$w[i]
        i <- ifelse(i>1, i-1, i)
      }
    }
    else {
      ind <- c(ind, x$order[i])
      j <- 1
    }
  }
  return(list(value=table[item_count,W+1], elements=ind))
})


test <- matrix(NA, 3, 6)
test[1:3, 1:2] <- matrix(c(0,3), 3, 2, byrow = T)
test
