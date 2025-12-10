## dcor_cpt
#' Conditional permutation-distance correlation conditional independence test
#'
#' Conditional independence test (CIT) based on the combination of the conditional permutation to control the T1E and
#' distance correlation as test statistic.
#'
#' @param X A nxp-matrix of n p-dimensional vectors
#' @param Y A nx1-matrix
#' @param Z A nxq-matrix of n q-dimensional confounder (optional)
#' @param index exponent of Euclidean distance used in distance correlation (default: 1)
#' @param model.formula.YZ A formula for the generalized additive model approximating the conditional distribution of Y given Z
#' @param M Number of conditional permutations
#' @param nstep Parameter inside MCMC to generate conditional permutations
#'
#' @return list of p-value and distance correlation for observed X,Y,Z
#' @export
#'
#' @examples
#' n <- 20; p <- 10; q <- 2
#' X <- matrix(rnorm(n*p), nrow = n, ncol = p)
#' Y <- matrix(rnorm(n), nrow = n)
#' Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
#' if(requireNamespace("energy", quietly = TRUE)){
#'   withr::local_package("energy")
#'   dcor_cpt(X, Y, Z, model.formula.YZ='V1~V2+V3')
#' }
dcor_cpt <- function(X, Y, Z, index = 1, model.formula.YZ, M = 30, nstep = 50){
  Y.CPT <- cpt_gam(Y,Z, model.formula.YZ, M = M, nstep = nstep)
  T.dcor <- energy::dcor(Y, X, index = index)
  T.dcor.vec <- rep(100000, M)
  for (j in 1:M){
    T.dcor.vec[j] <- energy::dcor(Y.CPT[,j], X, index = index)
  }
  res <- c()
  res$p <- (1 + sum(T.dcor.vec >= T.dcor)) / (1+M)
  res$Sta <- T.dcor
  return(res)
}

