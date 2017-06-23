#' Lp Distance
#' 
#' Compute Lp distance
#' 
#' @param X,Y input data
#' @param p the power p
#' @examples 
#' X <- rexp(80, rate = 0.2)
#' Y <- rexp(120, rate = 0.4)
#' Lp_dist(X, Y, p = 2)
#' @importFrom rdist cdist
#' @export
Lp_dist <- function (X, Y, p = 2) 
{
  D <- rdist::cdist(X, Y, metric = "minkowski", p = p)
  return((mean(D^p)) ^ (1/p))
}