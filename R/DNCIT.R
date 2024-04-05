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
#' @param params_cit A list of parameters for the CIT
#'
#' @return list of p-value, test statistic and runtime
#' @importFrom RCIT RCoT
#' @importFrom momentchi2 hbe
#' @importFrom GeneralisedCovarianceMeasure gcm.test
#' @importFrom utils modifyList
#' @export
#'
#' @examples
#' library(momentchi2)
#' n <- 100; p <- 10; q <- 2
#' X <- matrix(rnorm(n*p), nrow = n, ncol = p)
#' Y <- matrix(rnorm(n), nrow = n)
#' Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
#' res <- DNCIT(X, Y, Z)
DNCIT <- function(X, Y, Z, embedding_map = NULL, cit = "RCOT", params_cit = list()) {
  ### feature representations of X
  if (is.null(embedding_map)) {
    X <- X
  }else if (embedding_map == 'open_ai_clip'){
    X <- open_ai_clip(X)
  }else {
    X <- embedding_map(X)
  }



  #### nonparametric CIT
  if (!(is.matrix(X))){
    return("The feature representation of X is not a matrix")
  }
  if(!(is.matrix(Y))){
    return("Y should be given as a nx1-matrix")
  }
  if(!(is.matrix(Z))) {
    return("Confounder Z should be a nxp-matrix")
  }

  ## apply nonparametric CIT
  if (cit == "RCOT") {
    start_time <- timestamp()
    res <- RCIT::RCoT(X, Y, Z)
    end_time <- timestamp()
    res$runtime <- difftime(end_time, start_time, units = "secs")
  }else if(cit=='kpc_graph'){
    updated_parameters <- update_params(kpc_graph, X,Y,Z, params_cit)

    start_time <- timestamp()
    resu <- kpc_graph(X,Y,Z, k=params_cit[[1]], Knn=as.numeric(params_cit[[2]]), model.formula.YZ=params_cit[[3]])
    end_time <- timestamp()
    res$runtime <- difftime(end_time, start_time, units = "secs")
  }else if(cit=='cmiknn'){
    updated_parameters <- update_params(cmiknn, X,Y,Z, params_cit)

    start_time <- timestamp()
    res <- do.call(cmiknn, updated_parameters)
    end_time <- timestamp()
    res$runtime <- difftime(end_time, start_time, units = "secs")
  }else if(cit=='fcit'){
    start_time <- timestamp()
    res <- r_fcit(np$array(X), np$array(Y), np$array(Z))
    end_time <- timestamp()
    res$runtime <- difftime(end_time, start_time, units = "secs")
  }else if(cit=='gcm'){
    updated_parameters <- update_params(gcm.test, X,Y,Z, params_cit)

    start_time <- timestamp()
    res <- do.call(GeneralisedCovarianceMeasure::gcm.test, updated_parameters)
    res$reject <- NULL
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

update_params <- function(cit.fct, X,Y,Z, new_params=list()){
  default_parameters <- as.list(formals(cit.fct))

  # Initialize a list to store valid new parameters
  valid_new_parameters <- list()

  # Iterate through new parameters, remove invalid ones, and print a warning
  for (param_name in names(new_params)) {
    if (param_name %in% names(default_parameters)) {
      # Valid parameter, add to the list of valid parameters
      valid_new_parameters[[param_name]] <- new_params[[param_name]]
    } else {
      # Invalid parameter, print a warning
      warning(paste("Parameter", param_name, "is not a valid parameter of the", cit.fct, "function."))
    }
  }

  updated_parameters <- utils::modifyList(default_parameters, valid_new_parameters)
  updated_parameters$X <- X
  updated_parameters$Y <- Y
  updated_parameters$Z <- Z
  return(updated_parameters)
}


