# helper function to skip tests if we don't have the 'fcit' module
skip_if_no_fcit <- function() {
  have_fcit <- reticulate::py_module_available("fcit")
  if (!have_fcit)
    skip("fcit not available for testing")
}


test_that("fcit applicable to univariate X,Y,Z", {
  skip_if_no_fcit()

  n <- 1000; p <- 1; q <- 1
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
  res <- r_fcit(X,Y,Z)
  expect_true(res$p >= 0 && res$p <= 1 &&
                length(res) == 2)
})

test_that("cpt-kpc applicable to multivariate X,Z, univariate Y", {
  skip_if_no_fcit()

  n <- 1000; p <- 10; q <- 2
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
  res <- r_fcit(X,Y,Z)
  expect_true(res$p >= 0 && res$p <= 1 &&
                length(res) == 2)
})
