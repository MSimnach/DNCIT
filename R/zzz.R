## Python virtual environment setup with reticulate

CMIknn <- NULL
np <- NULL
fcit <- NULL
open_ai_clip <- NULL
PIL_image <- NULL
py_torch <- NULL

.onLoad <- function(libname, pkgname) {
  if(!reticulate::virtualenv_exists('r-cits')){
    reticulate::virtualenv_create(envname="r-cits", version="<=3.10")
    if(!reticulate::py_module_available('virtualenv')){
      reticulate::py_install('virtualenv', envname='r-cits', pip=TRUE, python_version = "<=3.10")
    }
    reticulate::py_install(packages = "tigramite", envname = 'r-cits', pip=TRUE)
    reticulate::py_install(packages = "scikit_learn", envname = 'r-cits')
    reticulate::py_install(packages = "fcit", envname = 'r-cits',pip=TRUE, pip_options=c("--no-deps"))
    reticulate::py_install(packages = "open_clip_torch", envname = 'r-cits',pip=TRUE)
  }
  reticulate::use_virtualenv("r-cits", required = FALSE)

  # use superassignment to update global reference to python modules/packages
  CMIknn <<- reticulate::import("tigramite.independence_tests.cmiknn", delay_load = TRUE)
  np <<- reticulate::import('numpy', delay_load = TRUE)
  fcit <<- reticulate::import('fcit', delay_load = TRUE)
  open_ai_clip <<- reticulate::import('open_clip', delay_load = TRUE)
  PIL_image <<- reticulate::import('PIL.Image', delay_load = TRUE)
  py_torch <<- reticulate::import('torch', delay_load = TRUE)
}
