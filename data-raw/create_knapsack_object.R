set.seed(42)
n <-1000000
knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000))
devtools::use_data(knapsack_objects)
