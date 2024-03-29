test_that("fcit applicable to univariate X,Y,Z", {
  n <- 1000; p <- 1; q <- 1
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
  res <- r.fcit(X,Y,Z)
  expect_true(res$p >= 0 && res$p <= 1 &&
                length(res) == 2)
})

test_that("cpt-kpc applicable to multivariate X, univariate Y,Z", {
  n <- 1000; p <- 10; q <- 1
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
  res <- r.fcit(X,Y,Z)
  expect_true(res$p >= 0 && res$p <= 1 &&
                length(res) == 2)
})

test_that("cpt-kpc applicable to multivariate X,Z, univariate Y", {
  n <- 1000; p <- 10; q <- 2
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
  res <- r.fcit(X,Y,Z)
  expect_true(res$p >= 0 && res$p <= 1 &&
                length(res) == 2)
})
