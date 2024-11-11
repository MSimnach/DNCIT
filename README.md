
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DNCIT

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/MSimnach/DNCIT/branch/master/graph/badge.svg)](https://app.codecov.io/gh/MSimnach/DNCIT?branch=master)
[![R-CMD-check](https://github.com/MSimnach/DNCIT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MSimnach/DNCIT/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

DNCIT provides statistical tests for conditional associations between an
image $X$ and a scalar $Y$, given a vector-valued confounder $Z$, called
deep nonparametric conditional independence test (CIT) (DNCIT), which
were developed in the paper ‘Deep Nonparametric Conditional Independence
Tests for Images’. The DNCIT maps the image through a embedding map onto
a feature representation and applies existing nonparametric CITs to the
feature representation and $Y$, given the confounder $Z$. The package
consists of a wrapper for potential embedding maps and a selection of
existing nonparametric CITs.

## Installation

You can install the development version of DNCIT from
[GitHub](https://github.com/) with:

``` r
options(dncit_with_python_env = TRUE)
# install.packages("devtools")
devtools::install_github("MSimnach/DNCIT")
```

A more detailed instruction on the installation can be found in
`vignette("Installation")`. To incorporate CITs and embedding maps
originally implemented in python, the corresponding modules have to be
installed in a virtual environment. Therefore, a virtual environment for
the python-based CITs is automatically created when loading the package
for the first time (by options(dncit_with_python_env = TRUE)), if there
exists no virtual environment called ´r-cits´. The virtual environment
is created with the following packages: tigramite, scikit_learn, fcit,
open_clip. If you want to use other python-based CITs and embedding
maps, you can install them in the virtual environment with the following
command: reticulate::py_install(packages = “package”, envname =
‘r-cits’). If setting up a virtual environment is not possible, the
installation will try to set up a Conda environment. You can find more
to the environment setup in `zzz.R`.
