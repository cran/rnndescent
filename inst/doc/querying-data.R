## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rnndescent)

## ----iris split---------------------------------------------------------------
iris_even <- iris[seq_len(nrow(iris)) %% 2 == 0, ]
iris_odd <- iris[seq_len(nrow(iris)) %% 2 == 1, ]

## ----brute force query--------------------------------------------------------
brute_nbrs <- brute_force_knn_query(
  query = iris_odd,
  reference = iris_even,
  k = 15
)

## ----brute force query results------------------------------------------------
lapply(brute_nbrs, function(m) {
  head(m[, 1:5])
})

## ----rpf_knn_query------------------------------------------------------------
rpf_index <- rpf_build(iris_even)
rpf_nbrs <- rpf_knn_query(
  query = iris_odd,
  reference = iris_even,
  forest = rpf_index,
  k = 15
)

## ----graph_knn_query----------------------------------------------------------
graph_nbrs <- graph_knn_query(
  query = iris_odd,
  reference = iris_even,
  reference_graph = rpf_nbrs,
  k = 15
)

## ----iris binarize------------------------------------------------------------
numeric_iris <- iris[, sapply(iris, is.numeric)]
logical_iris <- sweep(numeric_iris, 2, colMeans(numeric_iris), ">")
logical_iris_even <- logical_iris[seq_len(nrow(logical_iris)) %% 2 == 0, ]
logical_iris_odd <- logical_iris[seq_len(nrow(logical_iris)) %% 2 == 1, ]
head(logical_iris_even)

## ----iris binarize brute force------------------------------------------------
iris_logical_brute_nbrs <- brute_force_knn_query(
  query = logical_iris_odd,
  reference = logical_iris_even,
  k = 15,
  metric = "hamming"
)

## ----iris search with binary init---------------------------------------------
graph_nbrs <- graph_knn_query(
  query = iris_odd,
  reference = iris_even,
  reference_graph = rpf_nbrs,
  init = iris_logical_brute_nbrs$idx,
  k = 15
)

## ----iris forest init---------------------------------------------------------
forest_init_nbrs <- graph_knn_query(
  query = iris_odd,
  reference = iris_even,
  reference_graph = rpf_nbrs,
  init = rpf_index,
  k = 15
)

## ----iris search graph--------------------------------------------------------
set.seed(42)
iris_search_graph <- prepare_search_graph(
  data = iris_even,
  graph = rpf_nbrs,
  diversify_prob = 0.1,
  pruning_degree_multiplier = 1.5
)

## ----iris search graph histogram----------------------------------------------
search_graph_edges <- diff(iris_search_graph@p)
hist(search_graph_edges,
  main = "Distribution of search graph edges", xlab = "# edges"
)
range(search_graph_edges)

## ----iris use a search graph--------------------------------------------------
search_nbrs <- graph_knn_query(
  query = iris_odd,
  reference = iris_even,
  reference_graph = iris_search_graph,
  init = rpf_index,
  k = 15
)

