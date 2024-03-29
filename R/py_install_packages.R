#' @importFrom reticulate py_install virtualenv_exists virtualenv_remove
install_py_cits <-
  function(...,
           envname = "r-cits",
           new_env = identical(envname, "r-cits")) {

    if(new_env && reticulate::virtualenv_exists(envname))
      reticulate::virtualenv_remove(envname)

    reticulate::py_install(packages = "tigramite", envname = envname, ...)
    reticulate::py_install(packages = "scikit_learn", envname = envname, ...)
    reticulate::py_install(packages = "fcit", envname = envname, ...)
  }



