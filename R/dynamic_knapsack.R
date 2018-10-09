#' Title
#'
#' @param x Data frame which consists the value and the weight for each object.
#' @param W The knapsack size.
#' @description This function can solve the knapsack problem exact by iterating over all possible values of W.
#' @return maximum knapsack value and which elements (rows in the data.frame).
#' @export
dynamic_knapsack <- function(x, W) {

  if(!is.data.frame(x) | ncol(x)!=2)
    stop("The input object is not of data.frame type.\n")
  if(!(all(colnames(x)==c("v", "w")) | all(colnames(x)==c("w", "v"))))
    stop("The data.frame should have the columns named 'v' and 'w'.")
  if(!is.numeric(W) | length(W)!=1 | W<=0)
    stop("The total weight (W) should be a positive scalar")

  rownames(x) <- 1:nrow(x)
    # We give rownames so that after filtering we can acquire original positions of elements
  too_big <- which(x$w>W)
    # Index of units with individual weight major than the threshold
  x <- x[-too_big,]
    # Filtered dataset
  x <- x[order(x$w),]
    # Order according to weight
  n <- nrow(x)

  s <- x$w[1]
    # To optimize calculation of m: it's going to be the sum of all the weights from 1 to i
    # If the value of the weight in column J is greater than s then all remaining cells
    # of row I will have the last value calculated for that row
  m <- matrix(0, n, W+1)
  first_line <- c(rep(0, x$w[1]), rep(x$v[1], (W+1)-x$w[1]))
    # We initialize the first line
  m[1,] <- first_line

  for(i in 2:n) {

    for(j in 2:W+1) {

      if( (j-1)>s ) {
        m[i, j:W+1] <- m[i, j-1]
        s <- ifelse(i<n, s+x$w[i], s)
        break
      }

      if( x$w[i] > (j-1) )
        m[i, j] <- m[i-1, j]
      else
        m[i, j] <- max(m[i-1, j], m[i-1, j-x$w[i]] + x$v[i])

    }
  }

  # Find optimal values
  i <- n
  j <- W+1
  ind <- c()

  while(j>0) {
    print(c("i:", i, "; j:", j))
    if(i>1 && m[i, j] == m[i-1, j]) i <- i-1
    else {
      ind <- c(ind, i)
      j <- j - x$w[i]
      i <- ifelse(i>1, i-1, i)
    }
  }

  return(list(value = max(m), elements = sort(as.numeric(rownames(x)[ind]))))
}

dynamic_knapsack(knapsack_objects[1:8, ], 3500)
dynamic_knapsack(x = knapsack_objects[1:12,], W = 3500)
dynamic_knapsack(x = knapsack_objects[1:8,], W = 2000)
dynamic_knapsack(x = knapsack_objects[1:12,], W = 2000)
