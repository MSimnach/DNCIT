#' Fast conditional independence test
#'
#' @param X A nxp-matrix of n p-dimensional vectors
#' @param Y A nx1-matrix
#' @param Z A nxq-matrix of n q-dimensional confounder (optional)
#'
#' @return list of p-value and -1 as test statistic, because the CIT does not return a test statistic
#' @export
#'
#' @examples
#' n <- 20; p <- 10; q <- 2
#' X <- matrix(rnorm(n*p), nrow = n, ncol = p)
#' Y <- matrix(rnorm(n), nrow = n)
#' Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
#' r.fcit(X,Y,Z)
r.fcit <- function(X,Y,Z){
  res <- list()
  res$p <- fcit$fcit$test(X, Y, Z)
  res$Stat <- -1
  return(res)
}
