#' Pairwise distances
#' 
#' Compute pairwise distances
#' 
#' @param X,Y input data
#' @param method used method
#' @param ... more arguments passed to method
#' @examples 
#' X <- list(rexp(80, rate = 0.2), rexp(80, rate = 0.2))
#' Y <- list(rexp(120, rate = 0.4), rexp(120, rate = 0.4))
#' # cdist
#' distribution_cdist(X, Y, method = "wasserstein")
#' distribution_cdist(X, Y, method = "kl", k = 5)
#' distribution_cdist(X, Y, method = "hausdorff", metric = "euclidean")
#' distribution_cdist(X, Y, method = "ky_fan", metric = "euclidean")
#' 
#' # pdist
#' distribution_pdist(X, method = "wasserstein")
#' distribution_pdist(X, method = "kl", k = 5)
#' distribution_pdist(X, method = "hausdorff", metric = "euclidean")
#' distribution_pdist(X, method = "ky_fan", metric = "euclidean")
#' @export
distribution_cdist <- function(X, Y, method = c("wasserstein", "kl", "hausdorff", "ky_fan"), ...){
  method <- match.arg(method)
  distance_method <- switch(method, 
                            "wasserstein" = wasserstein_dist, 
                            "kl" = kl_dist, 
                            "hausdorff" = hausdorff_dist, 
                            "ky_fan" = ky_fan_dist)
  dist_mat <- mapply(distance_method, 
                     rep(X, each = length(Y)), 
                     rep(Y, length = length(X)), 
                     MoreArgs = list(...))
  matrix(as.numeric(dist_mat), nrow = length(X), byrow = TRUE)
}

#' @rdname distribution_cdist
#' @export
distribution_pdist <- function(X, method = c("wasserstein", "kl", "hausdorff"), ...){
  distribution_cdist(X = X, Y = X, method = method, ...)
}

