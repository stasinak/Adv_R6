set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )

#' Title greedy_knapsack
#'
#' @param x Data frame which consists the value and the weight for each object.
#' @param W The knapsack size
#' @description This function uses Greedy heuristic.
#' It does not return an exact result but it will reduce the computational complexity.
#' @return  the function returns the maximum knapsack value and which element of the data frame.
#' @export
greedy_knapsack<-function(x,W){
  
  if(!is.data.frame(x) | ncol(x)!=2) 
    stop("The input object is not of data.frame type.\n")
  if(!(all(colnames(x)==c("v", "w")) | all(colnames(x)==c("w", "v"))))
    stop("The data.frame should have the columns named 'v' and 'w'.")
  if(!is.numeric(x$v)) 
    stop("Column of values (v) is not of the expected type (numeric).")
  if(!is.numeric(x$w)) 
    stop("Column of weights (w) is not of the expected type (numeric).")
  if(!is.numeric(W) | length(W)!=1 | W<=0) 
    stop("The total weight (W) should be a positive scalar.")
  
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

  return(list(value=total_value,elements=indexes))
}
greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
