test_that("cpt-kpc applicable to univariate X,Y,Z", {
  n <- 20; p <- 1; q <- 1
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
  kpc_params <- list(kernlab::vanilladot(), 1, 'V1~V2')
  if(requireNamespace("kernlab", quietly = TRUE)){
    withr::local_package("kernlab")
    res <- kpc_graph(X, Y, Z, kpc_params[[1]], kpc_params[[2]], kpc_params[[3]])
    expect_true(res$p >= 0 && res$p <= 1 &&
                length(res) == 2)
  }
})

test_that("cpt-kpc applicable to multivariate X,Z, univariate Y", {
  n <- 20; p <- 10; q <- 2
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
  kpc_params <- list(kernlab::rbfdot(1/(2*stats::median(stats::dist(X))^2)), 2, 'V1~V2')
  if(requireNamespace("kernlab", quietly = TRUE)){
    withr::local_package("kernlab")
    res <- kpc_graph(X, Y, Z, kpc_params[[1]], kpc_params[[2]], kpc_params[[3]])
    expect_true(res$p >= 0 && res$p <= 1 &&
                      length(res) == 2)
  }
})
