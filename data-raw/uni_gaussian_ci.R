## code to prepare `univariate Gaussian CI X,Y,Z` dataset

n <- 1000; p <- 1; q <- 1
X <- matrix(rnorm(n*p), nrow = n, ncol = p)
Y <- matrix(rnorm(n), nrow = n)
Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
uni_gaussian_ci <- cbind(X, Y, Z)
usethis::use_data(uni_gaussian_ci, overwrite = TRUE)
