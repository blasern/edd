#' Wasserstein Distance
#' 
#' Compute Wasserstein distance
#' 
#' @param X,Y input data
#' @examples 
#' X <- rexp(80, rate = 0.2)
#' Y <- rexp(120, rate = 0.4)
#' wasserstein_dist(X, Y)
#' @importFrom transport wasserstein1d
#' @export
wasserstein_dist <- function(X, Y){
  transport::wasserstein1d(X, Y)
}
