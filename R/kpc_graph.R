## KPC graph knn
#' Conditional permutation-kernel partial correlation conditional independence test
#'
#' Conditional independence test (CIT) based on the combination of the conditional permutation to control the T1E and
#' Kernel partial correlation as test statistic.
#'
#' @param X A nxp-matrix of n p-dimensional vectors
#' @param Y A nx1-matrix
#' @param Z A nxq-matrix of n q-dimensional confounder (optional)
#' @param k A kernel chosen from available kernel in kernlab
#' @param Knn Number of nearest neighbors for KPCgraph
#' @param model.formula.YZ A formula for the generalized additive model approximating the conditional distribution of Y given Z
#' @param M Number of conditional permutations
#' @param nstep Parameter inside MCMC to generate conditional permutations

#' @return list of p-value and kernel partial correlation for observed X,Y,Z
#' @export
#'
#' @examples
#' n <- 20; p <- 10; q <- 2
#' X <- matrix(rnorm(n*p), nrow = n, ncol = p)
#' Y <- matrix(rnorm(n), nrow = n)
#' Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
#' kpc_graph(X, Y, Z, k=kernlab::rbfdot(1/(2 * stats::median(stats::dist(Y))^2)),
#' Knn = 1, model.formula.YZ='V1~V2+V3')
kpc_graph <- function(X, Y, Z, k, Knn, model.formula.YZ, M = 30, nstep = 50){
  Y.CPT <- cpt_gam(Y,Z, model.formula.YZ, M = M, nstep = nstep)
  T.graph <- KPC::KPCgraph(Y=X,Z=Y, X=Z, k=k, Knn=Knn)
  T.graph.vec <- rep(100000, M)
  for (j in 1:M){
    T.graph.vec[j] <- KPC::KPCgraph(Y=X, Z=Y.CPT[,j], X=Z, k=k, Knn=Knn)
  }
  res <- c()
  res$p <- (1 + sum(T.graph.vec >= T.graph)) / (1+M)
  res$Sta <- T.graph
  return(res)
}

