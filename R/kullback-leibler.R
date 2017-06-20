#' Kullback-Leibler Distance
#' 
#' Compute Kullback-Leibler symmetric distance
#' 
#' @param X,Y input data
#' @param k The number of nearest neighbors to search. 
#' @references S. Boltz, E. Debreuve and M. Barlaud, "kNN-based high-dimensional Kullback-Leibler distance for tracking," Image Analysis for Multimedia Interactive Services, 2007. WIAMIS '07.
kl_dist <- function(X, Y, k = 10){
  kl_divergence(X, Y, k = k) + kl_divergence(Y, X, k = k)
}

#' @importFrom FNN knn.dist
kl_divergence <- function(X, Y, k = 10){
  # convert to matrices
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  # dimensions
  d <- ncol(X)
  stopifnot(d == ncol(Y))
  nX <- nrow(X)
  nY <- nrow(Y)
  # knn distances
  knn_dist_X <- FNN::knn.dist(data = X, k = k)
  knn_dist_XY <- FNN::knnx.dist(data = Y, query = X, k = k)
  # mean log distance
  m_X <- mean(log(knn_dist_X[, k]))
  m_XY <- mean(log(knn_dist_XY[, k]))
  # 
  log(nY / (nX-1)) + d * m_XY - d * m_X
}

#' @importFrom FNN knn.dist
knn_entropy <- function(X, k = ceiling(sqrt(nrow(X)))){
  # convert to matrices
  X <- as.matrix(X)
  # dimensions
  d <- ncol(X)
  n <- nrow(X)
  # volumes
  v <- 2 * pi^(d/2) / d / gamma(d/2)
  knn_dist <- FNN::knn.dist(data = X, k = k)
  m <- mean(log(knn_dist[, k]))
  # crossentropy
  log(v * n) - digamma(k) + d * m
}

#' @importFrom FNN knn.dist
knn_crossentropy <- function(X, Y, k = ceiling(sqrt(nrow(Y)))){
  # convert to matrices
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  # dimensions
  d <- ncol(X)
  stopifnot(d == ncol(Y))
  n <- nrow(Y)
  # volumes
  v <- 2 * pi^(d/2) / d / gamma(d/2)
  knn_dist <- FNN::knnx.dist(data = Y, query = X, k = k)
  m <- mean(log(knn_dist[, k]))
  # crossentropy
  log(v * n) - digamma(k) + d * m
}