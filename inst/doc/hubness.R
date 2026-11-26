## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rnndescent)

## ----set seed-----------------------------------------------------------------
set.seed(42)

## ----2D Gaussian--------------------------------------------------------------
n_points <- 1000
low_dim <- 2
g2d <- matrix(rnorm(n_points * low_dim), ncol = low_dim)

## ----brute force 2D results---------------------------------------------------
g2d_nnbf <- brute_force_knn(g2d, k = 15, metric = "euclidean")

## ----nearest neighbor descent 2D results--------------------------------------
g2d_nnd <- nnd_knn(g2d, k = 15, metric = "euclidean")

## ----2D nnd accuracy----------------------------------------------------------
neighbor_overlap(g2d_nnbf, g2d_nnd, k = 15)

## ----1000D Gaussian-----------------------------------------------------------
hi_dim <- 1000
g1000d <- matrix(rnorm(n_points * hi_dim), ncol = hi_dim)

## ----brute force 1000D results------------------------------------------------
g1000d_nnbf <- brute_force_knn(g1000d, k = 15, metric = "euclidean")

## ----nearest neighbor descent 1000D results-----------------------------------
g1000d_nnd <- nnd_knn(g1000d, k = 15, metric = "euclidean")

## ----1000D nnd accuracy-------------------------------------------------------
neighbor_overlap(g1000d_nnbf, g1000d_nnd, k = 15)

## ----NN-distances-distribution, fig.alt = c("Histogram of normalized 2D neighbor distances, concentrated at low values with a right tail.", "Histogram of normalized 1000D neighbor distances, concentrated at higher values in a roughly symmetric distribution.")----
hist(
  g2d_nnbf$dist[, -1] / max(g2d_nnbf$dist[, -1]),
  xlab = "distances",
  main = "2D 15-NN"
)
hist(
  g1000d_nnbf$dist[, -1] / max(g1000d_nnbf$dist[, -1]),
  xlab = "distances",
  main = "1000D 15-NN"
)

## ----NND-distances-distribution, fig.alt = "Histogram of normalized approximate 1000D neighbor distances, concentrated at higher values in a roughly symmetric distribution."----
hist(
  g1000d_nnd$dist[, -1] / max(g1000d_nnd$dist[, -1]),
  xlab = "distances",
  main = "1000D 15-NND"
)

## ----distance relative RMS error----------------------------------------------
nn_rrmsev <- function(nn, ref) {
  sqrt(
    apply((nn$dist[, -1] - ref$dist[, -1])^2, 1, sum) /
      apply(ref$dist[, -1]^2, 1, sum)
  )
}

## ----histogram-of-distance-difference, fig.alt = "Histogram of relative RMS distance errors: many items have zero or small errors, with a tail of larger errors."----
g1000d_rrmse <- nn_rrmsev(g1000d_nnd, g1000d_nnbf)
hist(g1000d_rrmse, main = "1000D distance error", xlab = "Relative RMS error")

## ----hist-1000D, fig.alt = "Histogram of per-item neighbor overlap in 1000 dimensions, showing a range of accuracies with many high values."----
g1000d_nnd_acc <-
  neighbor_overlap(g1000d_nnbf, g1000d_nnd, k = 15, ret_vec = TRUE)$overlaps
hist(g1000d_nnd_acc, main = "1000D accuracy", xlab = "accuracy", xlim = c(0, 1))

## ----rrmse-vs-acc, fig.alt = "Scatter plot of relative RMS distance error against neighbor overlap; errors vary at a given accuracy."----
plot(
  g1000d_nnd_acc,
  g1000d_rrmse,
  main = "RRMSE vs accuracy",
  xlab = "accuracy",
  ylab = "RRMSE"
)

## ----2D k-occurrences---------------------------------------------------------
g2d_bfko <- k_occur(g2d_nnbf, k = 15)
summary(g2d_bfko)

## ----number of anti-hubs in 2D case-------------------------------------------
sum(g2d_bfko == 1)

## ----2D-k-occurrence-histogram, fig.alt = "Histogram of exact 2D k-occurrences, concentrated around 15 with few extreme values."----
hist(g2d_bfko, main = "2D 15-NN", xlab = "k-occurrences")

## ----1000D-k-occurrences, fig.alt = "Histogram of exact 1000D k-occurrences, strongly right-skewed with a long tail extending to about 300."----
g1000d_bfko <- k_occur(g1000d_nnbf$idx, k = 15)
hist(g1000d_bfko, main = "1000D 15-NN", xlab = "k-occurrences")

## ----zoomed-k-occurrences, fig.alt = "Histogram of exact 1000D k-occurrences clipped at the maximum 2D value, showing many low counts and a pile-up at the clipping limit."----
hist(
  pmin(g1000d_bfko, max(g2d_bfko)),
  main = "1000D 15-NN zoomed",
  xlab = "k-occurrences"
)

## ----numerical summary of 1000D k-occurrence distribution---------------------
summary(g1000d_bfko)

## ----number of anti-hubs in the 1000D case------------------------------------
sum(g1000d_bfko == 1)

## ----1000D-NND-k-occurences, fig.alt = "Histogram of approximate 1000D k-occurrences, strongly right-skewed with a long tail."----
g1000d_nndko <- k_occur(g1000d_nnd$idx, k = 15)
hist(g1000d_nndko, main = "1000D 15-NND", xlab = "k-occurrences")

## ----zoomed-NND-k-occurrences, fig.alt = "Histogram of approximate 1000D k-occurrences clipped at the maximum 2D value, showing many low counts and a pile-up at the clipping limit."----
hist(
  pmin(g1000d_nndko, max(g2d_bfko)),
  main = "1000D 15-NND zoomed",
  xlab = "k-occurrences"
)

## ----1000D NND k-occurences numeric summary-----------------------------------
summary(g1000d_nndko)
sum(g1000d_nndko == 1)

## ----approximate-vs-true-1000D-k-occurrence, fig.alt = "Scatter plot of exact against approximate 1000D k-occurrences with an equality line; the strong relationship tends to fall below the line at high counts."----
plot(
  g1000d_nndko,
  g1000d_bfko,
  xlab = "approximate",
  ylab = "exact",
  xlim = c(0, max(g1000d_nndko, g1000d_bfko)),
  ylim = c(0, max(g1000d_nndko, g1000d_bfko)),
  main = "1000D k-occ"
)
abline(a = 0, b = 1)
cor(g1000d_nndko, g1000d_bfko, method = "pearson")

## ----zoomed-approximate-vs-true-1000D-k-occurrence, fig.alt = "Scatter plot of exact against approximate 1000D k-occurrences at low counts, with an equality line; approximate counts tend to be higher."----
plot(
  g1000d_nndko,
  g1000d_bfko,
  xlab = "approximate",
  ylab = "exact",
  xlim = c(0, max(g2d_bfko)),
  ylim = c(0, max(g2d_bfko)),
  main = "1000D low k-occ"
)
abline(a = 0, b = 1)

## ----predicting-accuracy-with-NND-k-occurrence, fig.alt = "Scatter plot of neighbor overlap against approximate k-occurrence: high-count hubs have high accuracy, while low-count items span a wider range."----
plot(
  g1000d_nndko,
  g1000d_nnd_acc,
  xlab = "NND k-occ",
  ylab = "accuracy",
  xlim = c(0, max(g1000d_nndko, g1000d_bfko)),
  main = "1000D acc vs NND k-occ"
)

## ----proportion of items with < 90% accuracy----------------------------------
sum(g1000d_nnd_acc < 0.9)

## ----max k-occurrence for lower accuracy items--------------------------------
max(g1000d_nndko[g1000d_nnd_acc < 0.9])

## ----how many items-----------------------------------------------------------
sum(g1000d_nndko <= max(g1000d_nndko[g1000d_nnd_acc < 0.9]))

## ----how many items at a lower accuracy thresold------------------------------
sum(g1000d_nndko <= max(g1000d_nndko[g1000d_nnd_acc < 0.8]))

## ----unconverged NND----------------------------------------------------------
g1000d_nnd_iter1 <- nnd_knn(g1000d, k = 15, metric = "euclidean", n_iters = 1)
g1000d_nndkoi1 <- k_occur(g1000d_nnd_iter1$idx, k = 15)

## ----unconverged NND accuracy-------------------------------------------------
neighbor_overlap(g1000d_nnbf, g1000d_nnd_iter1, k = 15)

## ----unconverged-ko-distribution, fig.alt = "Histogram of 1000D k-occurrences after one NND iteration, with many low counts and a long right tail."----
hist(g1000d_nndkoi1, main = "1000D 15-NND (1 iter)", xlab = "k-occurrences")

## ----unconverged-ko-distribution-zoomed, fig.alt = "Histogram of 1000D k-occurrences after one NND iteration, clipped at the maximum 2D count; many counts are low and others pile up at the limit."----
hist(
  pmin(g1000d_nndkoi1, max(g2d_bfko)),
  main = "1000D 15-NND (1 iter, zoomed)",
  xlab = "k-occurrences"
)

## ----unconverged ko distribution numerical summary----------------------------
summary(g1000d_nndkoi1)
sum(g1000d_nndkoi1 == 1)

## ----unconverged-2D-NND, fig.alt = "Histogram of 2D k-occurrences after one NND iteration, concentrated around 15 with relatively few extreme counts."----
g2d_nnd_iter1 <- nnd_knn(g2d, k = 15, metric = "euclidean", n_iters = 1)
g2d_nndkoi1 <- k_occur(g2d_nnd_iter1$idx, k = 15)
hist(g2d_nndkoi1, main = "2D 15-NND (1 iter)", xlab = "k-occurrences")
summary(g2d_nndkoi1)
sum(g2d_nndkoi1 == 1)
neighbor_overlap(g2d_nnbf, g2d_nnd_iter1, k = 15)

## ----NND 1000D weight by degree-----------------------------------------------
g1000d_nnd_w <-
  nnd_knn(g1000d, k = 15, metric = "euclidean", weight_by_degree = TRUE)
neighbor_overlap(g1000d_nnbf, g1000d_nnd_w)

## ----NND 1000D truncated 30---------------------------------------------------
g1000d_nnd_k30 <- nnd_knn(g1000d, k = 30, metric = "euclidean")
neighbor_overlap(g1000d_nnbf, g1000d_nnd_k30, k = 15)

## ----NND 1000D max_candidates 30----------------------------------------------
g1000d_nnd_mc30 <- nnd_knn(
  g1000d,
  k = 15,
  metric = "euclidean",
  max_candidates = 30
)
neighbor_overlap(g1000d_nnbf, g1000d_nnd_mc30)

## ----NND 1000D tol 0----------------------------------------------------------
g1000d_nnd_tol0 <- nnd_knn(g1000d, k = 15, metric = "euclidean", delta = 0)
neighbor_overlap(g1000d_nnbf, g1000d_nnd_tol0)

## ----NND 1000D repeat---------------------------------------------------------
g1000d_nnd_rep <- nnd_knn(g1000d, k = 15, metric = "euclidean")
neighbor_overlap(g1000d_nnbf, g1000d_nnd_rep, k = 15)

## ----merge--------------------------------------------------------------------
g1000d_nnd_merge <- merge_knn(list(g1000d_nnd, g1000d_nnd_rep))
neighbor_overlap(g1000d_nnbf, g1000d_nnd_merge, k = 15)

## ----NND-100D-compare-accuracy, fig.alt = "Scatter plot comparing per-item neighbor overlap across two NND runs, showing substantial variation between runs."----
g1000d_nnd_rep_acc <-
  neighbor_overlap(g1000d_nnbf, g1000d_nnd_rep, k = 15, ret_vec = TRUE)$overlaps
plot(
  g1000d_nnd_acc,
  g1000d_nnd_rep_acc,
  main = "1000D NND accuracy comparison",
  xlab = "accuracy run 1",
  ylab = "accuracy run 2"
)
cor(g1000d_nnd_acc, g1000d_nnd_rep_acc)

## ----prepare graph------------------------------------------------------------
g1000d_search_graph <-
  prepare_search_graph(
    data = g1000d,
    graph = g1000d_nnd,
    metric = "euclidean",
    diversify_prob = 1,
    pruning_degree_multiplier = 1.5
  )

## ----histogram-of-k-occurrences-of-search-graph, fig.alt = "Histogram of search graph k-occurrences, right-skewed with many low counts and a long tail."----
g1000d_sgko <- k_occur(g1000d_search_graph)
hist(g1000d_sgko, main = "search graph k-occurrences", xlab = "k-occurrences")
summary(g1000d_sgko)
sum(g1000d_sgko == 0)

## ----search with prepared graph-----------------------------------------------
g1000d_search <-
  graph_knn_query(
    query = g1000d,
    reference = g1000d,
    reference_graph = g1000d_search_graph,
    k = 15,
    metric = "euclidean",
    init = g1000d_nnd,
    epsilon = 0.1
  )

## ----accuracy with search graph-----------------------------------------------
neighbor_overlap(g1000d_nnbf, g1000d_search, k = 15)

## ----search with neighbor graph-----------------------------------------------
g1000d_nnd_search <-
  graph_knn_query(
    query = g1000d,
    reference = g1000d,
    reference_graph = g1000d_nnd,
    k = 15,
    metric = "euclidean",
    init = g1000d_nnd,
    epsilon = 0.1
  )
neighbor_overlap(g1000d_nnbf, g1000d_nnd_search, k = 15)

## ----search with neighbor graph, no back-tracking-----------------------------
g1000d_nnd_search0 <-
  graph_knn_query(
    query = g1000d,
    reference = g1000d,
    reference_graph = g1000d_nnd,
    k = 15,
    metric = "euclidean",
    init = g1000d_nnd,
    epsilon = 0
  )
neighbor_overlap(g1000d_nnbf, g1000d_nnd_search0, k = 15)

## ----search with search graph, no back-tracking-------------------------------
g1000d_search0 <-
  graph_knn_query(
    query = g1000d,
    reference = g1000d,
    reference_graph = g1000d_search_graph,
    k = 15,
    metric = "euclidean",
    init = g1000d_nnd,
    epsilon = 0
  )
neighbor_overlap(g1000d_nnbf, g1000d_search0, k = 15)

## ----search at most 10% of the data-------------------------------------------
g1000d_nnd_search_max <-
  graph_knn_query(
    query = g1000d,
    reference = g1000d,
    reference_graph = g1000d_nnd,
    k = 15,
    metric = "euclidean",
    init = g1000d_nnd,
    epsilon = 0.1,
    max_search_fraction = 0.1
  )
neighbor_overlap(g1000d_nnbf, g1000d_nnd_search_max, k = 15)

