#' @importFrom reticulate import use_virtualenv

CMIknn <- NULL
np <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::use_virtualenv("r-cits")
  # use superassignment to update global reference to scipy
  CMIknn <<- reticulate::import("tigramite.independence_tests.cmiknn", delay_load = TRUE)
  np <<- reticulate::import('numpy', delay_load = TRUE)
}
