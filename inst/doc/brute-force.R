## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rnndescent)

## ----brute force knn----------------------------------------------------------
iris_nbrs <- brute_force_knn(iris, k = 15)
lapply(iris_nbrs, function(x) {
  head(x[, 1:5], 3)
})

## ----brute force query--------------------------------------------------------
iris_even <- iris[seq_len(nrow(iris)) %% 2 == 0, ]
iris_odd <- iris[seq_len(nrow(iris)) %% 2 == 1, ]
iris_query_nbrs <-
  brute_force_knn_query(
    query = iris_even,
    reference = iris_odd,
    k = 15
  )
lapply(iris_query_nbrs, function(x) {
  head(x[, 1:5], 3)
})

