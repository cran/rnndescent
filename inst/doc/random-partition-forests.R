## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rnndescent)

## ----build a forest-----------------------------------------------------------
iris_forest <- rpf_build(iris, leaf_size = 15)

## ----query a forest-----------------------------------------------------------
iris_query <-
  rpf_knn_query(
    query = iris,
    reference = iris,
    forest = iris_forest,
    k = 15
  )

## ----forest knn---------------------------------------------------------------
iris_knn <- rpf_knn(iris, k = 15)

## ----forest knn with forest---------------------------------------------------
iris_knn_with_forest <-
  rpf_knn(iris[1:50, ], k = 15, ret_forest = TRUE)
iris_query_virginica <-
  rpf_knn_query(
    query = iris[51:150, ],
    reference = iris[1:50, ],
    forest = iris_knn_with_forest$forest,
    k = 15
  )

## ----binary matrix------------------------------------------------------------
binary_data <- matrix(as.logical(rbinom(1000, 1, 0.5)), ncol = 10)

## ----binary knn implicit------------------------------------------------------
bin_knn_imp <-
  rpf_knn(binary_data,
    k = 15,
    metric = "hamming",
    margin = "implicit"
  )

## ----binary knn explicit------------------------------------------------------
bin_knn_exp <-
  rpf_knn(binary_data,
    k = 15,
    metric = "hamming",
    margin = "explicit"
  )

## ----filter-------------------------------------------------------------------
iris_filtered <-
  rpf_filter(
    nn = iris_query,
    forest = iris_forest,
    n_trees = 1
  )

