append_slash <- function(img_dir_path) {
  if (substr(img_dir_path, nchar(img_dir_path), nchar(img_dir_path)) %in% c("/", "\\")) {
    return(img_dir_path)
  } else {
    return(paste0(img_dir_path, "/"))
  }
}

update_params <- function(fct, new_params=list()){
  default_parameters <- as.list(formals(fct))

  # Initialize a list to store valid new parameters
  valid_new_parameters <- list()

  # Iterate through new parameters, remove invalid ones, and print a warning
  for (param_name in names(new_params)) {
    if (param_name %in% names(default_parameters)) {
      # Valid parameter, add to the list of valid parameters
      valid_new_parameters[[param_name]] <- new_params[[param_name]]
    } else {
      # Invalid parameter, print a warning
      warning(paste("Parameter", param_name, "is not a valid parameter of the", fct, "function."))
    }
  }

  updated_parameters <- utils::modifyList(default_parameters, valid_new_parameters)
  return(updated_parameters)
}


update_params_cits <- function(fct, X,Y,Z, new_params=list()){
  default_parameters <- as.list(formals(fct))

  # Initialize a list to store valid new parameters
  valid_new_parameters <- list()

  # Iterate through new parameters, remove invalid ones, and print a warning
  for (param_name in names(new_params)) {
    if (param_name %in% names(default_parameters)) {
      # Valid parameter, add to the list of valid parameters
      valid_new_parameters[[param_name]] <- new_params[[param_name]]
    } else {
      # Invalid parameter, print a warning
      warning(paste("Parameter", param_name, "is not a valid parameter of the", fct, "function."))
    }
  }

  updated_parameters <- utils::modifyList(default_parameters, valid_new_parameters)
  if ('RCoT' %in% as.character(substitute(fct))){
    updated_parameters$x <- X
    updated_parameters$y <- Y
    updated_parameters$z <- Z
  }else{
    updated_parameters$X <- X
    updated_parameters$Y <- Y
    updated_parameters$Z <- Z
  }
  return(updated_parameters)
}
