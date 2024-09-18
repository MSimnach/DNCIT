#' Fast conditional independence test
#'
#' @param X A nxp-matrix of n p-dimensional vectors
#' @param Y A nx1-matrix
#' @param Z A nxq-matrix of n q-dimensional confounder (optional)
#'
#' @return list of p-value and -1 as test statistic, because the FCIT does not return a test statistic
#' @export
#'
#' @examplesIf reticulate::py_module_available('fcit')
#' n <- 20; p <- 10; q <- 2
#' X <- matrix(rnorm(n*p), nrow = n, ncol = p)
#' Y <- matrix(rnorm(n), nrow = n)
#' Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
#' r_fcit(X, Y, Z)
r_fcit <- function(X,Y,Z){
  if(!reticulate::py_module_available('fcit')){
    stop("fcit not found. Please install the fcit package in your python environment. You can find help regarding the installion in `vignette(Installation)`.")
  }
  res <- list()
  res$p <- fcit$fcit$test(X, Y, Z)
  res$Sta <- -1
  return(res)
}
