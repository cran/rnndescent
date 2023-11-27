## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rnndescent)

## ----build knn----------------------------------------------------------------
iris_knn <- rnnd_knn(data = iris, k = 5)

## ----neighbor graph-----------------------------------------------------------
lapply(iris_knn, function(x) {
  head(x, 3)
})

## ----iris split---------------------------------------------------------------
iris_even <- iris[seq_len(nrow(iris)) %% 2 == 0, ]
iris_odd <- iris[seq_len(nrow(iris)) %% 2 == 1, ]

## ----build index--------------------------------------------------------------
iris_index <- rnnd_build(iris_even, k = 5)

## ----index neighbor graph-----------------------------------------------------
lapply(iris_index$graph, function(x) {
  head(x, 3)
})

## ----query index--------------------------------------------------------------
iris_odd_nn <- rnnd_query(
  index = iris_index,
  query = iris_odd,
  k = 5
)
lapply(iris_odd_nn, function(x) {
  head(x, 3)
})

## ----query knn data-----------------------------------------------------------
iris_knn_improved <- rnnd_query(
  index = iris_index,
  query = iris_even,
  init = iris_index$graph,
  k = 5
) 

## ----sum distances------------------------------------------------------------
c(
  sum(iris_index$graph$dist),
  sum(iris_knn_improved$dist)
)

## ----neighbor overlap---------------------------------------------------------
neighbor_overlap(iris_index$graph, iris_knn_improved)

## ----parallel-----------------------------------------------------------------
iris_index <- rnnd_build(data = iris_even, k = 5, n_threads = 2)

