library(profvis)

# Original

set.seed(42)
n <- 400000
knapsack_objects <-
  data.frame(
    w=sample(1:400000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )
x = knapsack_objects
W = 3500000

profvis::profvis({
  my_weight=0
  heuristic<-x$v/x$w
  
  indexes<-c()
  total_value<-0
  
  while (my_weight < W) {
    max_heuristic = max(heuristic)
    # if(max_heuristic==0)
    #   break
    index <- which(heuristic == max_heuristic)
    if(x$w[index] <= W-my_weight){
      my_weight <- my_weight + x$w[index]
      indexes <- c(indexes,index)
      total_value <- total_value + x$v[index]
      
    }
    else
      break
    heuristic[index]<-0
  }
  
  test1 <- list(value=total_value, elements=indexes)
})

# Long time: 7070 ms (mainly due to the code in which we get the indexes)

##############################################################################################
# Order the dataset

set.seed(42)
n <- 400000
knapsack_objects <-
  data.frame(
    w=sample(1:400000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )
x = knapsack_objects
W = 3500000

profvis::profvis({
  my_weight=0
  heuristic<-x$v/x$w
  n <- nrow(x)
  x$id <- 1:n
  x <- x[order(heuristic, decreasing = T),]
  
  indexes<-c()
  total_value<-0
  i <- 1
  
  while (my_weight < W | i<=n) {
    if(x$w[i] <= W-my_weight){
      my_weight <- my_weight + x$w[i]
      indexes <- c(indexes, x$id[i])
      total_value <- total_value + x$v[i]
    }
    else {
      break
    }
    i <- i+1
  }
  
  test2 <- list(value=total_value, elements=indexes)
})

# Fast! 350 ms