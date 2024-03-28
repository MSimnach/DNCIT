#' @importFrom reticulate py_install virtualenv_exists virtualenv_remove
install_py_cits <-
  function(...,
           envname = "r-cits",
           new_env = identical(envname, "r-cits")) {

    if(new_env && virtualenv_exists(envname))
      virtualenv_remove(envname)

    py_install(packages = "tigramite", envname = envname, ...)
    py_install(packages = "scikit_learn", envname = envname, ...)
    py_install(packages = "fcit", envname = envname, ...)
}

.onLoad <- function(...) {
  use_virtualenv("r-cits", required = FALSE)
}
