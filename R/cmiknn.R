#' Conditional mutial information-k-nearest-neighbors conditional independence test
#'
#' Conditional independence test (CIT) based on the conditional mutual information (CMI) with k-nearest-neighbors (k-NN) estimation
#'
#' @param X A nxp-matrix of n p-dimensional vectors
#' @param Y A nx1-matrix
#' @param Z A nxq-matrix of n q-dimensional confounder (optional)
#' @param knn Number of nearest-neighbors which determines the size of hyper-cubes around each (high-dimensional) sample point. If smaller than 1, this is computed as a fraction of the sample size as knn=n*knn. For knn larger or equal to 1, knn=knn
#' @param shuffle_neighbors Number of nearest-neighbors within Z for the shuffle surrogates which determines the size of hyper-cubes around each (high-dimensional) sample point.
#' @param sig_samples Number of samples for shuffle significance test.
#' @param transform (default: ‘ranks’) Whether to transform the array beforehand by standardizing or transforming to uniform marginals.
#'
#' @return list of p-value and the conditional mutual information of X,Y given Z
#' @export
#'
#' @examplesIf reticulate::py_module_available('tigramite')
#' n <- 20; p <- 1; q <- 1
#' X <- matrix(rnorm(n*p), nrow = n, ncol = p)
#' Y <- matrix(rnorm(n), nrow = n)
#' Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
#' cmiknn(X, Y, Z)
cmiknn <- function(X,Y,Z, knn=0.15, shuffle_neighbors=5, sig_samples=500, transform="ranks") {
  if(!reticulate::py_module_available('tigramite')){
    stop("tigramite not found. Please install the tigramite package in your python environment. You can find help regarding the installion in `vignette(Installation)`.")
  }

  if(knn<1){
    knn <- as.integer(nrow(X)*knn)
  }else{
    knn <- as.integer(knn)
  }

  cmi_knn <- CMIknn$CMIknn(significance="shuffle_test",
                    knn=knn,                    ## Adapt. Have a look at the docs
                    shuffle_neighbors=as.integer(shuffle_neighbors),
                    sig_samples=as.integer(sig_samples),   ## ADAPT IF YOU WANT, I recommend 500 minimum
                    sig_blocklength=as.integer(1),    ## Since you're not using time series (only relevant if z=None)
                    transform=transform
  )
  res <- cmi_knn$run_test_raw(np$array(X), np$array(Y), np$array(Z))
  res$p <- res[[2]]
  res$Sta <- res[[1]]
  res[[2]] <- NULL
  res[[1]] <- NULL
  return(res)
}
