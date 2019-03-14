#' Kolmogorov Distance
#' 
#' Compute Kolmogorov distance
#' 
#' @param X,Y input data
#' @examples 
#' X <- rexp(80, rate = 0.2)
#' Y <- rexp(120, rate = 0.4)
#' kolmogorov_dist(X, Y)
#' @importFrom stats ks.test
#' @export
kolmogorov_dist <- function (X, Y) 
{
  return(as.numeric(stats::ks.test(X, Y)$statistic))
}
