test_that("DNCIT applicable to univariate Gaussian CI X,Y,Z", {
  n <- 1000; p <- 1; q <- 1; n_seeds <- 100
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)

  expect_true(DNCIT(X, Y, Z)$p < 1)
})
