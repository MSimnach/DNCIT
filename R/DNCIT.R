### DNCIT
#' Conditional Independence Test for images and scalars, given vector-valued confounders
#'
#' Deep nonparametric conditional Independence Test (CIT) (DNCIT) consisting of a modular framework for
#' (nonparametric) CITs applied to feature representations of images and scalars, given confounders,
#' and embedding maps applied to the images to obtain feature representations.
#'
#' The function allows you to specify an embedding map which maps the images onto feature representations.
#' If no embedding map is specified, X should be a matrix of feature representations of the images.
#'
#' The function allows you to specify a CIT from a list of CITs. The default is the Randomized Correlation
#' Test (RCOT) since it performed best for sample sizes above 1100 in the paper.
#'
#' @param X A nxp-matrix of n p-dimensional (vectorized) images or a feature representation of the images
#' @param Y A nx1-matrix of n univariate target variables
#' @param Z A q-dimensional vector-valued confounder (optional)
#' @param embedding_map_with_parameters A list for the embedding map and its parameters. Default is "open_ai_clip" and "PIL", select 'feature_representations' if X already a feature representation.
#' @param cit_with_parameters The cit and its parameters applied to the feature representations, Y and Z. Default is "RCOT"
#'
#' @return list of p-value, test statistic and runtime
#' @export
#'
#' @examples
#' if (requireNamespace("RCIT", quietly = TRUE) && requireNamespace("momentchi2", quietly = TRUE)) {
#'   withr::local_package("momentchi2")
#'   withr::local_package("RCIT")
#'   n <- 100; p <- 10; q <- 2
#'   X <- matrix(rnorm(n*p), nrow = n, ncol = p)
#'   Y <- matrix(rnorm(n), nrow = n)
#'   Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
#'   res <- DNCIT(X, Y, Z, embedding_map_with_parameters = 'feature_representations')
#' }
DNCIT <- function(X, Y, Z,
                  embedding_map_with_parameters = 'feature_representations',
                  cit_with_parameters = list(cit = 'RCOT', params_cit = list(seed=123))) {
  #### Embedding map to obtain feature representations of X
  if (is.matrix(X) && ('feature_representations' %in% embedding_map_with_parameters)) {
    X <- X
  }else if(is.matrix(X) && ('tucker_decomposition' %in% embedding_map_with_parameters)){
    use_default <- is.null(embedding_map_with_parameters$'dim_reduced')
    X <- ifelse(use_default, tucker_decomposition(imgs_array=X), tucker_decomposition(embedding_map_with_parameters,imgs_array=X))
  }else if (is.character(X)){
    # Get a list of all files in the directory
    dir_path <- X
    all_files <- list.files(dir_path)
    embedding_map <- embedding_map_with_parameters['embedding_map']
    data_loader <- embedding_map_with_parameters['data_loader']

    if (embedding_map == 'feature_representations' && data_loader == 'npz'){
      if(length(all_files) != 1){
        stop("The directory should contain exactly one .npz file.")
      }else{
        X_npz <- np$load(paste0(dir_path, all_files[1]))
        ids_npz <- X_npz$files
        features_dim <- length(X_npz$get(ids_npz[1]))
        n <- length(ids_npz)
        X <- matrix(NA, nrow=n, ncol=features_dim, dimnames = list(ids_npz, NULL))
        for (id in ids_npz){
          X[id,] <- X_npz$get(id)
        }
      }
    }else if (embedding_map == 'open_ai_clip' && data_loader == 'PIL'){
      if(is.null(embedding_map_with_parameters['params_open_ai_clip'])){
        X <- r_open_ai_clip(embedding_map_with_parameters, dir_path)
      }else{
        X <- r_open_ai_clip(embedding_map_with_parameters, dir_path)
      }
    }else if(embedding_map == 'tucker_decomposition' && data_loader == 'png'){
      use_default <- is.null(embedding_map_with_parameters$'dim_reduced')
      X <- ifelse(use_default, tucker_decomposition(img_dir_path=dir_path), tucker_decomposition(embedding_map_with_parameters,img_dir_path=dir_path))
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
  if(!(is.matrix(Z)) && !(is.null(Z))) {
    return("Confounder Z should be a nxp-matrix")
  }

  # match row names of X, Y, Z
  if(is.null(row.names(X))){
    #print('X, Y and Z are expected to have the same row order, since now rownames for matching were provided.')
  }else if(is.null(Z)){
    if(!(all(rownames(Y) %in% rownames(X))&& all(rownames(X) %in% rownames(Y)))){
      return("The row names of X and Y should consist of the same names to match ids.")
    }else{
      order_X <- rownames(X)
      Y <- Y[order_X, , drop = FALSE]
    }
  }else if(!(all(rownames(X) %in% rownames(Y)) && all(rownames(X) %in% rownames(Z)) &&
            all(rownames(Y) %in% rownames(X)) && all(rownames(Y) %in% rownames(Z)) &&
            all(rownames(Z) %in% rownames(X)) && all(rownames(Z) %in% rownames(Y)))){
    return("The row names of X, Y, and Z should consist of the same names to match ids.")
  }else{
    order_X <- rownames(X)
    Y <- Y[order_X, , drop = FALSE]
    Z <- Z[order_X, , drop = FALSE]
  }

  # load cit and parameters
  cit <- cit_with_parameters$'cit'
  params_cit <- cit_with_parameters$'params_cit'

  ## apply nonparametric CIT
  if (cit == "RCOT") {
    if (!requireNamespace("RCIT", quietly = TRUE)) {
      stop(
        "Package \"RCIT\" must be installed to use this function.",
        call. = FALSE
      )
    }
    updated_parameters <- update_params_cits(RCIT::RCoT, X,Y,Z, params_cit)

    start_time <- timestamp()
    res <- do.call(RCIT::RCoT, updated_parameters)
    end_time <- timestamp()
    res$runtime <- difftime(end_time, start_time, units = "secs")
  }else if(cit=='cpt_kpc'){
    updated_parameters <- update_params_cits(kpc_graph, X,Y,Z, params_cit)

    start_time <- timestamp()
    res <- do.call(kpc_graph, updated_parameters)
    end_time <- timestamp()
    res$runtime <- difftime(end_time, start_time, units = "secs")
  }else if(cit=='cpt_dcor'){
    updated_parameters <- update_params_cits(dcor_cpt, X,Y,Z, params_cit)

    start_time <- timestamp()
    res <- do.call(dcor_cpt, updated_parameters)
    end_time <- timestamp()
    res$runtime <- difftime(end_time, start_time, units = "secs")
  }else if(cit=='cmiknn'){
    updated_parameters <- update_params_cits(cmiknn, X,Y,Z, params_cit)

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
    updated_parameters <- update_params_cits(GeneralisedCovarianceMeasure::gcm.test, X,Y,Z, params_cit)

    start_time <- timestamp()
    res <- do.call(GeneralisedCovarianceMeasure::gcm.test, updated_parameters)
    res[['p']] <- res[['p.value']]
    res[['Sta']] <- res[['test.statistic']]
    res[['p.value']] <- NULL
    res[['test.statistic']] <- NULL
    res$reject <- NULL
    end_time <- timestamp()
    res$runtime <- difftime(end_time, start_time, units = "secs")
  }else if(cit=='cpi'){
    if (!requireNamespace("cpi", quietly = TRUE)) {
      stop(
        "Package \"cpi\" must be installed to use this function.",
        call. = FALSE
      )
    }
    yxz <- do.call(cbind, list(Y,X,Z))
    colnames(yxz) <- paste0("V", 1:ncol(yxz))

    # default task, learner and measure
    if(is.null(params_cit)){
      tsk_yxz <-  mlr3::as_task_regr(yxz, target = "V1")
      lrn_ranger <- mlr3::lrn("regr.ranger", verbose = FALSE)
      measure <- mlr3::msrs("regr.mse")
      resampling <- mlr3::rsmp("cv", folds = 5)

      params_cit <- list(task = tsk_yxz, learner = lrn_ranger,
                         resampling = resampling, groups = list(X = 2:(ncol(X)+1)))
    }

    updated_parameters <- update_params_cits(cpi::cpi, X,Y,Z,params_cit)[1:13]
    start_time <- timestamp()
    cpitest <- do.call(cpi::cpi, updated_parameters)
    end_time <- timestamp()
    res <- list(p = cpitest$p.value, Sta = cpitest$CPI)
    res$runtime <- difftime(end_time, start_time, units = "secs")
  }else if(cit=='pred_cit'){
    updated_parameters <- update_params_cits(pred_cit, X,Y,Z, params_cit)

    start_time <- timestamp()
    res <- do.call(pred_cit, updated_parameters)
    end_time <- timestamp()
    res$runtime <- difftime(end_time, start_time, units = "secs")
  #}else if(cit=='ccit'){
  #  if(!reticulate::py_module_available('ccit')){
  #    stop("ccit not found. Please install the ccit package in your python environment. You can find help regarding the installion in `vignette(Installation)`.",
  #         call. = FALSE
  #    )
  #  }
  #  if(is.null(params_cit$colsample_bytrees)){
  #    params_cit$colsample_bytrees <- list(0.8)
  #  }
  #  updated_parameters <- update_params_cits(ccit$CCIT$CCIT, X,Y,Z, params_cit)

  #  res <- list()
  #  start_time <- timestamp()
  #  res[['p']] <- do.call(ccit$CCIT$CCIT, updated_parameters)
  #  res[['Sta']] <- -1
  #  end_time <- timestamp()
  #  res$runtime <- difftime(end_time, start_time, units = "secs")
  }else if(cit=='comets'){
    if (!requireNamespace("comets", quietly = TRUE)) {
      stop(
        "Package \"comets\" must be installed to use this function.",
        call. = FALSE
      )
    }
    if('gcm' %in% params_cit){
      params_cit$method <- NULL
      # 1:11 since we remove optional arguments ..., otherwise do.call throws an error
      updated_parameters <- update_params_cits(comets::gcm, X,Y,Z, params_cit)[1:11]

      start_time <- timestamp()
      res <- do.call(comets::gcm, updated_parameters)
      res[['p']] <- res[['p.value']]
      res[['Sta']] <- res[['statistic']]
      res[['p.value']] <- NULL
      res[['statistic']] <- NULL
      res$reject <- NULL
      end_time <- timestamp()
      res$runtime <- difftime(end_time, start_time, units = "secs")
    }else{
      updated_parameters <- update_params_cits(comets::pcm, X,Y,Z, params_cit)

      start_time <- timestamp()
      res <- do.call(comets::pcm, updated_parameters)
      res[['p']] <- res[['p.value']]
      res[['Sta']] <- res[['statistic']]
      res[['p.value']] <- NULL
      res[['statistic']] <- NULL
      res$reject <- NULL
      end_time <- timestamp()
      res$runtime <- difftime(end_time, start_time, units = "secs")
    }
  }else if(cit=='wald'){
    if(!is.null(params_cit)){
      lm_formula <- params_cit$'lm_formula'
      lm_model <- stats::lm(lm_formula, as.data.frame(cbind(X, Y,Z)))
    }else if(is.null(params_cit$'lm_model')){
      colnames(X) <- paste0('X',1:ncol(X))
      colnames(Y) <- paste0('Y',1:ncol(Y))
      colnames(Z) <- paste0('Z',1:ncol(Z))
      lm_formula <- stats::as.formula(paste(colnames(Y), "~ ."))
      lm_model <- stats::lm(lm_formula, as.data.frame(cbind(X, Y,Z)))
    }else{
      lm_model <- params_cit$'lm_model'
    }

    start_time <- timestamp()
    wtest <- lmtest::waldtest(lm_model, colnames(X))
    end_time <- timestamp()
    res <- list(p = wtest$`Pr(>F)`[2], Sta = wtest$F[2])
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
