test_that("Deep-CPT-KPC applicable to univariate X,Y,Z", {
  n <- 20; p <- 1; q <- 1
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
  for(kernel in c(kernlab::vanilladot(), kernlab::rbfdot(1/(2*stats::median(stats::dist(X))^2)),
                  kernlab::laplacedot(1/(2*stats::median(stats::dist(X))^2)))){
    for(knn in c(1,2)){
      kpc_params <- list(kernel, knn, 'V1~V2')
      res <- kpc_graph(X, Y, Z, k=kpc_params[[1]], Knn = kpc_params[[2]],
                       model.formula.YZ=kpc_params[[3]])
      expect_true(res >= 0 && res  <= 1)
    }
  }
})

test_that("Deep-CPT-KPC applicable to multivariate X, univariate Y,Z", {
  n <- 20; p <- 10; q <- 1
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
  for(kernel in c(kernlab::vanilladot(), kernlab::rbfdot(1/(2*stats::median(stats::dist(X))^2)),
                  kernlab::laplacedot(1/(2*stats::median(stats::dist(X))^2)))){
    for(knn in c(1,2)){
      kpc_params <- list(kernel, knn, 'V1~V2')
      res <- kpc_graph(X, Y, Z, k=kpc_params[[1]], Knn = kpc_params[[2]],
                       model.formula.YZ=kpc_params[[3]])
      expect_true(res >= 0 && res  <= 1)
    }
  }
})

test_that("Deep-CPT-KPC applicable to multivariate X,Z, univariate Y", {
  n <- 20; p <- 10; q <- 2
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
  for(kernel in c(kernlab::vanilladot(), kernlab::rbfdot(1/(2*stats::median(stats::dist(X))^2)),
                  kernlab::laplacedot(1/(2*stats::median(stats::dist(X))^2)))){
    for(knn in c(1,2)){
      kpc_params <- list(kernel, knn, 'V1~V2')
      res <- kpc_graph(X, Y, Z, k=kpc_params[[1]], Knn = kpc_params[[2]],
                       model.formula.YZ=kpc_params[[3]])
      expect_true(res >= 0 && res  <= 1)
    }
  }
})
