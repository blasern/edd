#' Hausdorff Distance
#' 
#' Compute Hausdorff distance
#' 
#' @param X,Y input data
#' @param metric The metric to use
#' @examples 
#' X <- rexp(80, rate = 0.2)
#' Y <- rexp(120, rate = 0.4)
#' hausdorff_dist(X, Y, metric = "euclidean")
#' @importFrom rdist cdist
#' @export
hausdorff_dist <- function (X, Y, metric = "euclidean") 
{
  D <- rdist::cdist(X, Y, metric = metric)
  dhd_XY <- max(apply(D, 1, min))
  dhd_YX <- max(apply(D, 2, min))
  return(max(dhd_XY, dhd_YX))
}