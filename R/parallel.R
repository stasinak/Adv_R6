library(parallel)

# Calculate the number of cores
no_cores <- detectCores()-1

# Initiate cluster
cl <- makeCluster(no_cores)

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )

#' Title brute_force_knapsack
#'
#' @param x Data frame which consists the value and the weight for each object.
#' @param W The knapsack size.
#' @description this function is guaranteed to give a correct answer in all situations.
#' The function enumerates all different combinations by using a binary representation of the numbers
#' 1 to 2n and include all elements of that is equal to 1 in the binary representation.
#' @return The functoin returns a list containing two named objects:
#' \itemize{
#'   \item "value": maximum knapsack value;
#'   \item "elements": a vector containing the indexes of the objects (rows of data.frame) used to obtain the final result.
#' }
#' @export
brute_force_knapsack <- function(x, W,parallel = FALSE) {

  rownames(x) <- 1:nrow(x)
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
  
  if(parallel==TRUE){
    clusterExport(cl,"ronmanes(x)","n,best_val","best_ind","i","bits") 
    clusterEvalQ(cl,ronmanes(x),n,best_val,best_ind,i,bits) 
    par_function<-
    parLapply(cl = cl,par_function)
    stopCluster(cl)  
    }
  
  return(list(value = best_val, elements = best_ind))
}

