set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )

greedy_knapsack<-function(x,W){
  my_weight=0
  heuristic<-x$v/x$w

  indexes<-c()
  total_value<-0

  while (my_weight < W) {
    index <- which(heuristic == max(heuristic))
    if(x$w[index] <= W-my_weight){
      my_weight <- my_weight + x$w[index]
      indexes <- c(indexes,index)
      total_value <- total_value + x$v[index]

    }
    else
      break
    heuristic[index]<-0
  }

  return(list(value=total_value,elements=indexes))
}
greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
