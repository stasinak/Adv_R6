
knapsack_dynamic <- function(x, W) {
  
  if(!is.data.frame(x) | ncol(x)!=2) stop("The input object is not of data.frame type.\n")
  if(!(all(colnames(x)==c("v", "w")) | all(colnames(x)==c("w", "v"))))
    stop("The data.frame should have the columns named 'v' and 'w'.")
  if(!is.numeric(W) | length(W)!=1 | W<=0) stop("The total weight (W) should be a positive scalar")
  
  rownames(x) <- 1:nrow(x)
  too_big <- which(x$w>W)
  x <- x[-too_big,]
  x <- x[order(x$w),]
  n <- nrow(x)
  
  m <- matrix(0, n, W+1)
  s <- x$w[1]
  for(i in 1:n) {
    for(j in 2:W+1) {
      if( (j-1)>s ) {
        m[i, j:W+1] <- m[i, j-1]
        s <- ifelse(i<n, s+x$w[i], s)
        break
      }
      if( x$w[i] > (j-1) ) 
        m[i, j] <- m[i, j-1]
      else 
        m[i, j] <- max(m[i-1, j], m[i-1, j-x$w[i]] + x$v[i])
    }
  }
  
  # i <- n
  # j <- W
  # ind <- c()
  # 
  # while(i!=1 | j!=1) {
  #   print(i, j)
  #   if(m[i, j] == m[i-1, j]) i <- i-1
  #   else {
  #     ind <- c(ind, i)
  #     j <- j - x$w[i]
  #     i <- i-1
  #   }
  # }
  # 
  # return(value = max(m), elements = sort(as.numeric(rownames(x[ind,]))))
  return(m)
}

knapsack_dynamic(x = knapsack_objects[1:8, ], 3500)
