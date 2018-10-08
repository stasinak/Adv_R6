set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
  )





bf_ks_sol = function(x, W){
  len_x = nrow(x)
  best_value = 0
  best_indexes = c()
  for(cmb in 1:(2^len_x-1)){
    curr_combination = intToBits(cmb)
    tmp_indexes = which(curr_combination[1:len_x]==01)
    tmp_weight = sum(x[tmp_indexes,]$w)
    tmp_value = sum(x[tmp_indexes,]$v)
    if(tmp_weight < W){
      print(tmp_indexes)
      if(tmp_value > best_value){
        best_indexes = tmp_indexes
        best_value = tmp_value
      }
    }
  }
  return(list(value=best_value, elements=best_indexes))
}


bf_ks_sol(knapsack_objects[1:8, ], 3500)
