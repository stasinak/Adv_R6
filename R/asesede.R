set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )



library(parallel)

no_cores <- detectCores()-1
cl <- makeCluster(no_cores)

asd<-function(x,W){
   
  rownames(x) <- 1:nrow(x)
  n <- nrow(x)
  best_val <- 0
  best_ind <- 0
  clusterExport(cl,list("x","best_val","best_ind","W") , envir = environment())
  tzouri_fouri<-function(i){
    
    ind <- which(intToBits(i) == 1)
    
    if(sum(x$w[ind])<=W & sum(x$v[ind])>best_val){
      best_val <<- sum(x$v[ind])
      best_ind <<- as.numeric(rownames(x)[ind])
      # return(list(value = best_val, elements = best_ind))
    } 
    
  }
  # stopCluster()
  #clusterEvalQ(cl, best_val)
  # return(a)
  #return(clusterEvalQ(cl,best_val))
  clusterExport(cl,"best_val",envir = environment()) 
  best_val = clusterEvalQ(cl, best_val)
  best_ind = clusterEvalQ(cl, best_ind)
  a=clusterApply(cl,1:(2^n-1),tzouri_fouri)
  return(list(value = best_val, elements = best_ind))
  # return(list(value = best_val, elements = best_ind))
}
res = asd(knapsack_objects[1:8,],3500)
res

