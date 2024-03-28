### DNCIT
#' Deep nonparametric Conditional Independence Test (DNCIT)
#'
#' @param X A nxp-matrix of n p-dimensional (vectorized) images or a feature representation of the images
#' @param Y A scalar
#' @param Z A q-dimensional vector-valued confounder (optional)
#' @param embedding_map A embedding map computing feature representations from the images (optional)
#' @param cit The nonparametric CIT applied to the feature representations, Y and Z. Default is "RCOT"
#' @param params_CIT A list of parameters for the nonparametric CIT
#'
#' @return p-value and test statistic as vector
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
    if (params_CIT[[1]]=='1'){
      k = kernlab::vanilladot()
    }else if (params_CIT[[1]]=='2') {
      k = kernlab::rbfdot(1/(2*stats::median(stats::dist(X))^2))
    }else if (params_CIT[[1]]=='3') {
      k = kernlab::laplacedot(1/(2*stats::median(stats::dist(X))^2))
    }else if (params_CIT[[1]]=='4') {
      k = kernlab::tanhdot()
    }else if (params_CIT[[1]]=='5') {
      k = kernlab::splinedot()
    }else if (params_CIT[[1]]=='6') {
      k = kernlab::besseldot(sigma=1/(2*stats::median(stats::dist(X))^2))
    }
    start_time <- timestamp()
    resu <- kpc_graph(X,Y,Z, k=k, Knn=as.numeric(params_CIT[[2]]), model.formula.YZ=params_CIT[[3]])
    end_time <- timestamp()
    res$runtime <- difftime(end_time, start_time, units = "secs")
  }else if(cit=='CMIknn'){
    CMIknn <- reticulate::import('tigramite.independence_tests.cmiknn')$CMIknn
    np <- reticulate::import('numpy')
    cmi_knn <- CMIknn(significance="shuffle_test",
                     knn=as.integer(1),                    ## Adapt. Have a look at the docs
                     shuffle_neighbors=as.integer(5),
                     sig_samples=as.integer(50),   ## ADAPT IF YOU WANT, I recommend 500 minimum
                     sig_blocklength=as.integer(1),    ## Since you're not using time series (only relevant if z=None)
                     transform=r_to_py("ranks")    ## This is the parameter used in my AISTATS paper...
    )
    # Check if X is a matrix
    if (!is.matrix(X)) {
      # If X is not a matrix, try converting it to a matrix
      if (is.vector(X)) {
        # If X is a vector, reshape it into a matrix
        dim(X) <- c(length(X), 1)
      } else {
        stop("X must be a matrix or a vector.")
      }
    }
    # Check if X is a matrix
    if (!is.matrix(Y)) {
      # If X is not a matrix, try converting it to a matrix
      if (is.vector(Y)) {
        # If X is a vector, reshape it into a matrix
        dim(Y) <- c(length(Y), 1)
      } else {
        stop("X must be a matrix or a vector.")
      }
    }
    # Check if X is a matrix
    if (!is.matrix(Z)) {
      # If X is not a matrix, try converting it to a matrix
      if (is.vector(Z)) {
        # If X is a vector, reshape it into a matrix
        dim(Z) <- c(length(Z), 1)
      } else {
        stop("X must be a matrix or a vector.")
      }
    }
    cmi_knn$run_test_raw(np$array(X), np$array(Y), np$array(Z))
    CMIknn$run_test_raw(np$array(X), np$array(Y), np$array(Z))
  }
  return(res)
}

timestamp <- function(time = Sys.time()) {
 withr::local_locale(c("LC_TIME" = "C"))
 withr::local_timezone("UTC")
 return(time)
}


