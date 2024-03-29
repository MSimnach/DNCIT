### DNCIT
#' Conditional Independence Test for images and scalars, given vector-valued confounders
#'
#' Deep nonparametric conditional Independence Test (CIT) (DNCIT) consisting of a wrapper for
#' (nonparametric) CITs applied to feature representations of images and scalars, given confounders,
#' and embedding maps applied to the images to obtain feature representations.
#'
#' The function allows you to specify an embedding map which maps the images onto feature representations.
#' If no embedding map is specified, X should be a matrix of feature representations of the images.
#'
#' The function allows you to specify a CIT from a list of CITs. The default is the Randomized Correlation
#' Test (RCOT) since it performed best in the paper. However, a recommendation based on the sample size
#' and the feature representation dimension is given in the console before performing the CIT.
#'
#'
#' @param X A nxp-matrix of n p-dimensional (vectorized) images or a feature representation of the images
#' @param Y A nx1-matrix of n univariate target variables
#' @param Z A q-dimensional vector-valued confounder (optional)
#' @param embedding_map An embedding map computing feature representations from the images (optional)
#' @param cit The CIT applied to the feature representations, Y and Z. Default is "RCOT"
#' @param params_CIT A list of parameters for the CIT
#'
#' @return list of p-value, test statistic and runtime
#' @importFrom RCIT RCoT
#' @importFrom momentchi2 hbe
#' @export
#'
#' @examples
#' library(momentchi2)
#' n <- 100; p <- 10; q <- 2
#' X <- matrix(rnorm(n*p), nrow = n, ncol = p)
#' Y <- matrix(rnorm(n), nrow = n)
#' Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
#' res <- DNCIT(X, Y, Z)
DNCIT <- function(X, Y, Z, embedding_map = NULL, cit = "RCOT", params_CIT = list()) {
  if (!(is.matrix(X) && is.matrix(Y) && is.matrix(Z))) {
    return("Please input variables as matrices")
  }
  ## feature representations of X
  if (is.null(embedding_map)) {
    X <- X
  } else {
    X <- embedding_map(X)
  }
  ## nonparametric CIT
  if (cit == "RCOT") {
    start_time <- timestamp()
    res <- RCIT::RCoT(X, Y, Z)
    end_time <- timestamp()
    res$runtime <- difftime(end_time, start_time, units = "secs")
  }else if(cit=='kpc_graph'){
    k <- params_CIT[[1]]
    start_time <- timestamp()
    resu <- kpc_graph(X,Y,Z, k=k, Knn=as.numeric(params_CIT[[2]]), model.formula.YZ=params_CIT[[3]])
    end_time <- timestamp()
    res$runtime <- difftime(end_time, start_time, units = "secs")
  }else if(cit=='cmiknn'){
    start_time <- timestamp()
    res <- cmiknn(np$array(X), np$array(Y), np$array(Z))
    end_time <- timestamp()
    res$runtime <- difftime(end_time, start_time, units = "secs")
  }
  return(res)
}

timestamp <- function(time = Sys.time()) {
 withr::local_locale(c("LC_TIME" = "C"))
 withr::local_timezone("UTC")
 return(time)
}


