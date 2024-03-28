test_that("Deep-RCoT applicable to univariate X,Y,Z", {
  n <- 20; p <- 1; q <- 1
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)

  expect_true(DNCIT(X, Y, Z, cit = "RCOT")$p >= 0 && DNCIT(X, Y, Z)$p <= 1)
})

test_that("Deep-RCoT applicable to multivariate X, univariate Y,Z", {
  n <- 20; p <- 1; q <- 1
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)

  expect_true(DNCIT(X, Y, Z, cit = "RCOT")$p >= 0 && DNCIT(X, Y, Z)$p <= 1)
})

test_that("Deep-RCoT applicable to multivariate X,Z, univariate Y", {
  n <- 20; p <- 10; q <- 2
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)

  expect_true(DNCIT(X, Y, Z, cit = "RCOT")$p >= 0 && DNCIT(X, Y, Z)$p <= 1)
})

test_that("timestamp() difference is correct", {
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  start.time <- Sys.time()
  start.time.timestamp <- timestamp()
  end.time <- Sys.time()
  end.time.timestamp <- timestamp()
  expect_equal(end.time.timestamp - start.time.timestamp, end.time - start.time, tolerance = 1e-3)
})
