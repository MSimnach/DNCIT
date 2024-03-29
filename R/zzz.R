#' @importFrom reticulate import use_virtualenv

CMIknn <- NULL
np <- NULL

.onLoad <- function(libname, pkgname) {
  if(!reticulate::virtualenv_exists('r-cits')){
    reticulate::py_install(packages = "tigramite", envname = 'r-cits')
    reticulate::py_install(packages = "scikit_learn", envname = 'r-cits')
    #reticulate::py_install(packages = "fcit", envname = 'r-cits')
  }
  reticulate::use_virtualenv("r-cits", required = FALSE)

  # use superassignment to update global reference to scipy
  CMIknn <<- reticulate::import("tigramite.independence_tests.cmiknn", delay_load = TRUE)
  np <<- reticulate::import('numpy', delay_load = TRUE)
}
