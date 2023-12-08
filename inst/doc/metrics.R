## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rnndescent)

## ----binary data--------------------------------------------------------------
set.seed(42)
binary_data <- matrix(sample(c(0, 1), 100, replace = TRUE), ncol = 10)
head(binary_data)

## ----hamming------------------------------------------------------------------
nn <- brute_force_knn(binary_data, k = 4, metric = "hamming")

## ----logical data-------------------------------------------------------------
logical_data <- binary_data == 1
head(logical_data)

## ----logical hamming----------------------------------------------------------
logical_nn <- brute_force_knn(logical_data, k = 4, metric = "hamming")

## ----compare------------------------------------------------------------------
all.equal(nn, logical_nn)

