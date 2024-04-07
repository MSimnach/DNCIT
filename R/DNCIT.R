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
#' @param embedding_map_with_parameters A list for the embedding map and its parameters. Default is "open_ai_clip" and "PIL", select 'feature_representations' if X already a feature representation.
#' @param cit_with_parameters The cit and its parameters applied to the feature representations, Y and Z. Default is "RCOT"
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
#' res <- DNCIT(X, Y, Z, embedding_map_with_parameters = 'feature_representations')
DNCIT <- function(X, Y, Z,
                  embedding_map_with_parameters = 'feature_representations',
                  cit_with_parameters = NULL) {
  #### Embedding map to obtain feature representations of X
  if (is.matrix(X) && embedding_map_with_parameters == 'feature_representations') {
    X <- X
  }else if(is.matrix(X) && !(embedding_map_with_parameters == 'feature_representations')){

  }else if (is.character(X)){
    # Get a list of all files in the directory
    dir_path <- X
    all_files <- list.files(dir_path)
    embedding_map <- embedding_map_with_parameters['embedding_map']
    data_loader <- embedding_map_with_parameters['data_loader']

    if (embedding_map == 'feature_representations' && data_loader == 'npz'){
      if(length(all_files) != 1){
        stop("The directory should contain exactly one .npz file.")
      }
        X_npz <- np$load(paste0(dir_path, all_files[1]))
        ids_npz <- X_npz$files
        features_dim <- length(X_npz$get(ids_npz[1]))
        n <- length(ids_npz)
        X <- matrix(NA, nrow=n, ncol=features_dim, dimnames = list(ids_npz, NULL))
        for (id in ids_npz){
          X[id,] <- X_npz$get(id)
        }
    }else if (embedding_map == 'open_ai_clip' && data_loader == 'PIL'){
        X_list <- list()
        for (file in all_files){
          file_path <- paste0(dir_path, file)
          img <- PIL_image$open(file_path)
          feature_rep <- r_open_ai_clip(img)
          X_list <- append(X_list, list(feature_rep))
        }
        X <- do.call(rbind, X_list)
    }else if(embedding_map == 'tensor' && data_loader == 'png'){
      img <- png::readPNG(all_files)
    }else{
      return("X is a string (potentially directory with images) but embedding_map_with_parameters does not correspond to any implemented embedding map.")
    }
  }else{
    embedding_map <- embedding_map_with_parameters['embedding_map']
    data_loader <- embedding_map_with_parameters['data_loader']
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

  # load cit and parameters
  if (is.null(cit_with_parameters)) {
    cit <- "RCOT"
  }else{
    cit <- cit_with_parameters['cit']
    params_cit <- cit_with_parameters['params_cit']
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
  }else{
    return("The specified CIT is not implemented.")
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


