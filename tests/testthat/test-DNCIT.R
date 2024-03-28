test_that("DNCIT applicable to univariate X,Y,Z", {
  n <- 100; p <- 1; q <- 1
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)

  expect_true(DNCIT(X, Y, Z)$p < 1)
})

test_that("DNCIT applicable to multivariate X, univariate Y,Z", {
  n <- 100; p <- 1; q <- 1
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)

  expect_true(DNCIT(X, Y, Z)$p < 1)
})

test_that("DNCIT applicable to multivariate X,Z, univariate Y", {
  n <- 100; p <- 10; q <- 2
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)

  expect_true(DNCIT(X, Y, Z)$p < 1)
})
