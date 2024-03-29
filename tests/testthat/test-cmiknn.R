# helper function to skip tests if we don't have the 'CMIknn' module
skip_if_no_CMIknn <- function() {
  have_CMIknn <- reticulate::py_module_available("tigramite.independence_tests.cmiknn")
  if (!have_CMIknn)
    skip("CMIknn not available for testing")
}

test_that("cmiknn applicable to univariate X,Y,Z", {
  skip_if_no_CMIknn()
  n <- 20; p <- 1; q <- 1
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
  for(knn in c(0.1,2)){
      res <- cmiknn(X, Y, Z, knn=knn)
      expect_true(res$p >= 0 && res$p <= 1 &&
                    length(res) == 2)
  }
})

test_that("cmiknn applicable to multivariate X, univariate Y,Z", {
  skip_if_no_CMIknn()
  n <- 20; p <- 10; q <- 1
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
  for(knn in c(0.1,2)){
    res <- cmiknn(X, Y, Z, knn=knn)
    expect_true(res$p >= 0 && res$p <= 1 &&
                  length(res) == 2)
  }
})

test_that("cmiknn applicable to multivariate X,Z, univariate Y", {
  skip_if_no_CMIknn()
  n <- 20; p <- 10; q <- 2
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
  for(knn in c(0.1,2)){
    res <- cmiknn(X, Y, Z, knn=knn)
    expect_true(res$p >= 0 && res$p <= 1 &&
                  length(res) == 2)
  }
})
