context("test-distribution_dist")

test_that("cdist dimensions are correct", {
  # generate data
  X <- list(rexp(80, rate = 0.2), rexp(80, rate = 0.2))
  Y <- list(rexp(120, rate = 0.4), rexp(120, rate = 0.4), rexp(60, rate = 0.1))
  
  # cdist
  expect_equal(dim(distribution_cdist(X, Y, method = "wasserstein")), c(2, 3))
  expect_equal(dim(distribution_cdist(X, Y, method = "kolmogorov")), c(2, 3))
  expect_equal(dim(distribution_cdist(X, Y, method = "kl", k = 5)), c(2, 3))
  expect_equal(dim(distribution_cdist(X, Y, method = "Lp", p = 2)), c(2, 3))
  expect_equal(dim(distribution_cdist(X, Y, method = "moment", p = 1)), c(2, 3))
  expect_equal(dim(distribution_cdist(X, Y, method = "hausdorff", metric = "euclidean")), c(2, 3))
  expect_equal(dim(distribution_cdist(X, Y, method = "ky_fan", metric = "euclidean")), c(2, 3))
})

test_that("pdist dimensions are correct", {
  # generate data
  X <- list(rexp(120, rate = 0.4), rexp(120, rate = 0.4), rexp(60, rate = 0.1))
  
  # cdist
  expect_equal(dim(distribution_pdist(X, method = "wasserstein")), c(3, 3))
  expect_warning(distribution_pdist(X, method = "kolmogorov"))
  expect_equal(dim(distribution_pdist(X, method = "kolmogorov")), c(3, 3))
  expect_equal(dim(distribution_pdist(X, method = "kl", k = 5)), c(3, 3))
  expect_equal(dim(distribution_pdist(X, method = "Lp", p = 2)), c(3, 3))
  expect_equal(dim(distribution_pdist(X, method = "moment", p = 1)), c(3, 3))
  expect_equal(dim(distribution_pdist(X, method = "hausdorff", metric = "euclidean")), c(3, 3))
  expect_equal(dim(distribution_pdist(X, method = "ky_fan", metric = "euclidean")), c(3, 3))
})

test_that("pdist dimensions are correct", {
  # generate data
  X <- list(rexp(120, rate = 0.4), rexp(120, rate = 0.4), rexp(60, rate = 0.1))
  
  # pdist
  expect_equal(dim(distribution_pdist(X, method = "wasserstein")), c(3, 3))
  expect_warning(distribution_pdist(X, method = "kolmogorov"))
  expect_equal(dim(distribution_pdist(X, method = "kolmogorov")), c(3, 3))
  expect_equal(dim(distribution_pdist(X, method = "kl", k = 5)), c(3, 3))
  expect_equal(dim(distribution_pdist(X, method = "Lp", p = 2)), c(3, 3))
  expect_equal(dim(distribution_pdist(X, method = "moment", p = 1)), c(3, 3))
  expect_equal(dim(distribution_pdist(X, method = "hausdorff", metric = "euclidean")), c(3, 3))
  expect_equal(dim(distribution_pdist(X, method = "ky_fan", metric = "euclidean")), c(3, 3))
})

test_that("rdist returns dist objects", {
  # generate data
  X <- list(rexp(120, rate = 0.4), rexp(120, rate = 0.4), rexp(60, rate = 0.1))
  
  # rdist
  expect_warning(distribution_rdist(X, method = "kolmogorov"))
  expect_s3_class(distribution_rdist(X, method = "wasserstein"), 'dist')
  expect_s3_class(distribution_rdist(X, method = "kolmogorov"), 'dist')
  expect_s3_class(distribution_rdist(X, method = "kl", k = 5), 'dist')
  expect_s3_class(distribution_rdist(X, method = "Lp", p = 2), 'dist')
  expect_s3_class(distribution_rdist(X, method = "moment", p = 1), 'dist')
  expect_s3_class(distribution_rdist(X, method = "hausdorff", metric = "euclidean"), 'dist')
  expect_s3_class(distribution_rdist(X, method = "ky_fan", metric = "euclidean"), 'dist')
})