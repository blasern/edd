#' Wasserstein Distance
#' 
#' Compute Wasserstein distance
#' 
#' @param X,Y input data
#' @param p The power p of the pth Wasserstein distance
#' @examples 
#' X <- rexp(80, rate = 0.2)
#' Y <- rexp(120, rate = 0.4)
#' wasserstein_dist(X, Y)
#' 
#' X <- matrix(X, ncol = 4)
#' Y <- matrix(Y, ncol = 4)
#' wasserstein_dist(X, Y)
#' @importFrom transport wasserstein1d
#' @export
wasserstein_dist <- function(X, Y, p = 1){
  if (is.null(dim(X)) || ncol(X) == 1){
    return(transport::wasserstein1d(X, Y, p = p))
  }
  else {  
    return(transport::wasserstein(transport::wpp(X, rep(1/nrow(X), nrow(X))),
                                  transport::wpp(Y, rep(1/nrow(Y), nrow(Y))), 
                                  p = p))
  }
}
