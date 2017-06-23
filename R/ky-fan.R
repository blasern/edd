#' Ky Fan Distance
#' 
#' Compute Ky Fan distance
#' 
#' @param X,Y input data
#' @param metric The metric to use
#' @examples 
#' X <- rexp(80, rate = 0.2)
#' Y <- rexp(120, rate = 0.4)
#' ky_fan_dist(X, Y, metric = "euclidean")
#' @importFrom rdist cdist
#' @export
ky_fan_dist <- function (X, Y, metric = "euclidean") 
{
  D <- rdist::cdist(X, Y, metric = metric)
  D2 <- D/(1+D)
  return(mean(D2))
}