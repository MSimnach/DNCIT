## Python virtual environment setup with reticulate

CMIknn <- NULL
np <- NULL
fcit <- NULL
open_ai_clip <- NULL
PIL_image <- NULL
py_torch <- NULL

.onLoad <- function(libname, pkgname) {
  py_packages <- c("tigramite", "scikit_learn", "fcit", "open_clip_torch")

  if(!(reticulate::virtualenv_exists('r-cits') || reticulate::condaenv_exists('r-cits'))){
    tryCatch({
      reticulate::virtualenv_create(envname="r-cits", version="<=3.10", packages = py_packages)
    }, error = function(e) {
      # If virtualenv fails, attempt conda
      cat("virtualenv-create failed. Attempting conda_install...\n")
      reticulate::conda_create(envname="r-cits", version="<=3.10")
      reticulate::conda_install(packages = "tigramite", envname = 'r-cits', pip=TRUE)
      reticulate::conda_install(packages = "scikit_learn", envname = 'r-cits')
      reticulate::conda_install(packages = "fcit", envname = 'r-cits',pip=TRUE, pip_options=c("--no-deps"))
      reticulate::conda_install(packages = "open_clip_torch", envname = 'r-cits',pip=TRUE)
    })
  }

  # if(!reticulate::condaenv_exists('r-cits')){
  #   if(!reticulate::virtualenv_exists('r-cits')){
  #     reticulate::virtualenv_create(envname="r-cits", version="<=3.10")
  #     reticulate::py_install(packages = "tigramite", envname = 'r-cits', pip=TRUE)
  #     reticulate::py_install(packages = "scikit_learn", envname = 'r-cits')
  #     reticulate::py_install(packages = "fcit", envname = 'r-cits',pip=TRUE, pip_options=c("--no-deps"))
  #     reticulate::py_install(packages = "open_clip_torch", envname = 'r-cits',pip=TRUE)
  #     startup::restart_r(quiet=TRUE)
  #   }
  # }
  if(reticulate::virtualenv_exists('r-cits')){
    reticulate::use_virtualenv("r-cits", required = FALSE)
  }else if(reticulate::condaenv_exists('r-cits')){
    reticulate::use_condaenv("r-cits", required = FALSE)
  }else{
    stop("No python environment found. Please install r-cits environment.")
  }

  # use superassignment to update global reference to python modules/packages
  CMIknn <<- reticulate::import("tigramite.independence_tests.cmiknn", delay_load = TRUE)
  np <<- reticulate::import('numpy', delay_load = TRUE)
  fcit <<- reticulate::import('fcit', delay_load = TRUE)
  open_ai_clip <<- reticulate::import('open_clip', delay_load = TRUE)
  PIL_image <<- reticulate::import('PIL.Image', delay_load = TRUE)
  py_torch <<- reticulate::import('torch', delay_load = TRUE)
}
