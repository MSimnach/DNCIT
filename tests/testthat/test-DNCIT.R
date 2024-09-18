test_that("DNCIT default setting", {
  skip_if_not_installed("kernlab")

  n <- 20; p <- 10; q <- 2
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)

  res <- DNCIT(X, Y, Z, embedding_map_with_parameters ='feature_representations')
  expect_true(res$p >= 0 && res$p <= 1 &&
                as.numeric(res$runtime) >= 0 &&
                length(res) ==3)
})

test_that("Deep-RCoT applicable to multivariate X,Z, univariate Y", {
  n <- 20; p <- 10; q <- 2
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)

  cit_with_parameters <- list(cit = "RCOT", params_cit = list())
  res <- DNCIT(X, Y, Z, embedding_map_with_parameters ='feature_representations', cit_with_parameters = cit_with_parameters)
  expect_true(res$p >= 0 && res$p <= 1 &&
                as.numeric(res$runtime) >= 0 &&
                length(res) ==3)
})

test_that("Deep-waldtest applicable to multivariate X,Z, univariate Y", {
  n <- 20; p <- 10; q <- 2
  X <- matrix(rnorm(n*p), nrow = n, ncol = p)
  Y <- matrix(rnorm(n), nrow = n)
  Z <- matrix(rnorm(n*q), nrow = n, ncol = q)

  cit_with_parameters <- list(cit = "wald", params_cit = NULL)
  res <- DNCIT(X, Y, Z, embedding_map_with_parameters ='feature_representations', cit_with_parameters = cit_with_parameters)
  expect_true(res$p >= 0 && res$p <= 1 &&
                as.numeric(res$runtime) >= 0 &&
                length(res) ==3)

  colnames(Z) <- c("Z1", "Z2")
  colnames(Y) <- "Y1"
  colnames(X) <- paste0("X", 1:ncol(X))
  lm_formula <- stats::as.formula(paste("Y1 ~ . + I(Z1^2)+Z1:Z2 - Y1"))
  cit_with_parameters <- list(cit = "wald", params_cit = list(lm_formula = lm_formula))
  res <- DNCIT(X, Y, Z, embedding_map_with_parameters ='feature_representations', cit_with_parameters = cit_with_parameters)
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
