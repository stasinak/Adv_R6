#' Title
#'
#' @param x Data frame which consists the value and the weight for each object.
#' @param W The knapsack size.
#' @description This function can solve the knapsack problem exact by iterating over all possible values of W.
#' @return maximum knapsack value and which elements (rows in the data.frame).
#' @export
#' 
dynamic_knapsack = function(x, W){
  if(!is.data.frame(x) | ncol(x)!=2) stop("The input object is not of data.frame type.\n")
  if(!(all(colnames(x)==c("v", "w")) | all(colnames(x)==c("w", "v"))))
    stop("The data.frame should have the columns named 'v' and 'w'.")
  if(!is.numeric(W) | length(W)!=1 | W<=0) stop("The total weight (W) should be a positive scalar")
  
  item_count = nrow(x)
  ordered_x = x[order(x$w),]
  table = matrix(c(rep(0,item_count*(W+1))), ncol=W+1)
  # set 1st row
  table[1, ] = c(rep(0, ordered_x$w[1]), rep(ordered_x$v[1], ncol(table)-ordered_x$w[1]))
  for(i in 2:nrow(table)){
    for(j in 2:ncol(table)){
      w_lim = j-1
      if(w_lim < ordered_x$w[i])
        table[i,j] = table[i-1, j]
      else
        table[i,j] = max(table[i-1, j], (ordered_x$v[i] + table[i-1, j-ordered_x$w[i]]))
    }
  }
  return(table[item_count,W+1])
}
# x = data.frame(w=c(1,3,4,5), v=c(1,4,5,7))
# dynamic_knapsack(x, 7)
# x=knapsack_objects[1:8, ]
# W = 3500
# dynamic_knapsack(x, W)
