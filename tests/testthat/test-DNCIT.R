test_that("Deep-RCoT applicable to univariate X,Y,Z", {
  n <- 20; p <- 1; q <- 1
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)

  res <- DNCIT(X, Y, Z, cit = "RCOT")
  expect_true(res$p >= 0 && res$p <= 1 &&
                as.numeric(res$runtime) >= 0 &&
                length(res) ==3)
})

test_that("Deep-RCoT applicable to multivariate X, univariate Y,Z", {
  n <- 20; p <- 1; q <- 1
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)

  res <- DNCIT(X, Y, Z, cit = "RCOT")
  expect_true(res$p >= 0 && res$p <= 1 &&
                as.numeric(res$runtime) >= 0 &&
                length(res) ==3)
})

test_that("Deep-RCoT applicable to multivariate X,Z, univariate Y", {
  n <- 20; p <- 10; q <- 2
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)

  res <- DNCIT(X, Y, Z, cit = "RCOT")
  expect_true(res$p >= 0 && res$p <= 1 &&
                as.numeric(res$runtime) >= 0 &&
                length(res) ==3)
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
