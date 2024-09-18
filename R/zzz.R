## Python virtual environment setup with reticulate

CMIknn <- NULL
np <- NULL
fcit <- NULL
open_ai_clip <- NULL
PIL_image <- NULL
py_torch <- NULL

.onLoad <- function(libname, pkgname) {
  # Check if the R option "dncit_with_python_env" is set to TRUE
  setup_python_env <- getOption("dncit_with_python_env", FALSE)  # Default to FALSE

  if (setup_python_env) {
    if(!(reticulate::virtualenv_exists('r-cits') || reticulate::condaenv_exists('r-cits'))){
      tryCatch({
        reticulate::virtualenv_create(envname="r-cits", version="<=3.10", packages="numpy<1.24")
        reticulate::py_install(packages = c("llvmlite==0.39.1","numba==0.56.4"), envname = 'r-cits', pip=TRUE)
        reticulate::py_install(packages = "tigramite", envname = 'r-cits', pip=TRUE)
        reticulate::py_install(packages = "scikit_learn", envname = 'r-cits')
        reticulate::py_install(packages = "fcit", envname = 'r-cits',pip=TRUE, pip_options=c("--no-deps"))
        reticulate::py_install(packages = "open_clip_torch", envname = 'r-cits',pip=TRUE)
      }, error = function(e) {
        if(reticulate::virtualenv_exists('r-cits')){
          reticulate::virtualenv_remove('r-cits')
        }
        # If virtualenv fails, attempt conda
        packageStartupMessage("virtualenv-create failed. Attempting conda_install...\n")
        reticulate::conda_create(envname="r-cits", version="<=3.10", packages="numpy<1.24")
        reticulate::py_install(packages = c("llvmlite==0.39.1","numba==0.56.4"), envname = 'r-cits', pip=TRUE)
        reticulate::conda_install(packages = "tigramite", envname = 'r-cits', pip=TRUE)
        reticulate::conda_install(packages = "scikit_learn", envname = 'r-cits', pip=TRUE)
        reticulate::conda_install(packages = "fcit", envname = 'r-cits',pip=TRUE, pip_options=c("--no-deps"))
        reticulate::conda_install(packages = "open_clip_torch", envname = 'r-cits',pip=TRUE)
      })
    }
  }
  if(reticulate::virtualenv_exists('r-cits')){
    reticulate::use_virtualenv("r-cits", required = FALSE)
  }else if(reticulate::condaenv_exists('r-cits')){
    reticulate::use_condaenv("r-cits", required = FALSE)
  }else{
    packageStartupMessage("No python environment 'r-cits' found. Either install r-cits environment or be aware that you are using a different python environment.")
  }

  # use superassignment to update global reference to python modules/packages
  if(reticulate::py_module_available('tigramite')){
    CMIknn <<- reticulate::import("tigramite.independence_tests.cmiknn", delay_load = TRUE)
  }else{
    packageStartupMessage('tigramite not found. If you would like to use the CMIknn, please install the tigramite package.')
  }
  if(reticulate::py_module_available('numpy')){
    np <<- reticulate::import('numpy', delay_load = TRUE)
  }else{
    packageStartupMessage('numpy not found. If you would like to use the numpy, please install the numpy package.')
  }
  if(reticulate::py_module_available('fcit')){
    fcit <<- reticulate::import('fcit', delay_load = TRUE)
  }else{
    packageStartupMessage('fcit not found. If you would like to use the fcit, please install the fcit package.')
  }
  if(reticulate::py_module_available('open_clip')){
    open_ai_clip <<- reticulate::import('open_clip', delay_load = TRUE)
  }else{
    packageStartupMessage('open_clip not found. If you would like to use the open_clip, please install the open_clip package.')
  }
  if(reticulate::py_module_available('PIL.Image')){
    PIL_Image <<- reticulate::import('PIL.Image', delay_load = TRUE)
  }else{
    packageStartupMessage('PIL.Image not found. If you would like to use the PIL.Image, please install the PIL.Image or teh open_clip package.')
  }
  if(reticulate::py_module_available('PIL.Image')){
    py_torch <<- reticulate::import('torch', delay_load = TRUE)
  }else{
    packageStartupMessage('py_torch not found. If you would like to use the py_torch, please install the py_torch or teh open_clip package.')
  }
}
