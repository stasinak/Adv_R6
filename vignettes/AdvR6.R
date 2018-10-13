## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE---------------------------------------------------------
#  devtools::install_github("stasinak/Adv_R6")

## ------------------------------------------------------------------------
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)

## ------------------------------------------------------------------------
system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))

## ------------------------------------------------------------------------
dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)
dynamic_knapsack(x = knapsack_objects[1:12,], W = 3500)

## ------------------------------------------------------------------------
system.time(dynamic_knapsack(x = knapsack_objects[1:500,], W = 3500))

## ------------------------------------------------------------------------
greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)

## ------------------------------------------------------------------------
set.seed(42)
n <-1000000
knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000))
system.time(greedy_knapsack(x = knapsack_objects[1:1000000,], W = 2000))

## ------------------------------------------------------------------------


brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

