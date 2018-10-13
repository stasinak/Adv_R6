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

profvis::profvis({
  title <- "DYNAMIC: original"
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

# The problem is defining the matrix: takes 400 ms in total

####################################################################################################

# Previous discard of too big data

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )
x = knapsack_objects[1:12, ]
W = 3500

profvis({
  title <- "DYNAMIC: filter too heavy observations"
  item_count = nrow(x)
  x$order = 1:item_count
  too_big <- which(x$w>W)
  if(length(too_big)!=0)
    x <- x[-too_big,]
  x = x[order(x$w),]
  item_count = nrow(x)
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

# Now it's about 340 ms: the matrix is still the biggest problem
# Idea to optimize: columns are made by the same value if the weight 
# is too small for the units considered

####################################################################################################

# Vectorized allocation of matrix


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
  table[1, ] = c(rep(0, x$w[1]), rep(x$v[1], (W+1)-x$w[1]))
  j_prec <- 1
  title <- "DYNAMIC: vectorized version"
  
  for(i in 2:item_count){
    
    j <- x$w[i]+1
    if(j_prec!=j) {
      table[i:item_count, j_prec:(j-1)] <- 
        matrix(table[i-1, j_prec:(j-1)], item_count-i+1, j-j_prec, byrow = T)
    }
    
    while( j<=(W+1) ) {
      
      if((j-1) <= s) {
        
        if((j-1) < x$w[i]) 
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
  list(value=table[item_count,W+1], elements=ind)
})

####################################################################################################

# With while()

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )
x = knapsack_objects[1:12, ]
W = 3500

profvis::profvis({
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
  i <- 2
  title <- "DYNAMIC: vectorized with while"
  
  while(i<=item_count) {
    
    j <- x$w[i]+1
    if(j_prec!=j) {
      table[i:item_count, j_prec:(j-1)] <- 
        matrix(table[i-1, j_prec:(j-1)], item_count-i+1, j-j_prec, byrow = T)
    }
    
    while( j<=(W+1) ) {
      
      if((j-1) <= s) {
        
        #print(paste("J-1 vale", j-1, "; i vale", i, "e x$w[i]", x$w[i]))
        if((j-1) < x$w[i]) 
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
    
    i <- i+1
    
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

