#' Moment distance
#' 
#' Compute moment distance
#' 
#' @param X,Y input data
#' @param p the power p
#' @examples 
#' X <- rexp(80, rate = 0.2)
#' Y <- rexp(120, rate = 0.4)
#' moment_dist(X, Y, p = 1)
#' @importFrom rdist cdist
#' @export
moment_dist <- function (X, Y, p = 1) 
{
  mpx <- mean(abs(X) ^ p) ^ (1/p)
  mpy <- mean(abs(Y) ^ p) ^ (1/p)
  D <- abs(mpx - mpy)
  return((mean(D^p)) ^ (1/p))
}