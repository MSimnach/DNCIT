
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DNCIT

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/MSimnach/DNCIT/branch/master/graph/badge.svg)](https://app.codecov.io/gh/MSimnach/DNCIT?branch=master)
[![R-CMD-check](https://github.com/MSimnach/DNCIT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MSimnach/DNCIT/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

DNCIT provides statistical tests for conditional dependence between an
image and a scalar, given a vector-valued confounder, called deep
nonparametric conditional independence test (CIT) (DNCIT) and developed
in the paper ‘Deep Nonparametric Conditional Independence Tests for
Images’. The package consists of a wrapper for potential embedding maps
and a selection of existing nonparametric CITs..

## Installation

You can install the development version of DNCIT from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MSimnach/DNCIT")
```

To run also CITs from python, the corresponding tests have to be in
installend in a virtual environment. Therefore, a virtual environment
for the python-based CITs is automatically created when loading the
package for the first time, if there exists no virtual environment
called ´r-cits´. The virtual environment is created with the following
python-based CITs: tigramite, scikit_learn, fcit. If you want to use
other python-based CITs, you can install them in the virtual environment
with the following command: reticulate::py_install(packages = “package”,
envname = ‘r-cits’).

## Example

This is a basic example which shows you how to solve a common problem:

``` r
#library(DNCIT)
## basic example code
n <- 1000; p <- 100; q <- 2
#feature representation of images
X <- matrix(rnorm(n*p),nrow=n,ncol=p)
#scalar
Y <- matrix(rnorm(n), nrow=n)
#confounder
Z <- matrix(rnorm(n*q),nrow=n,ncol=q)
#DNCIT(X, Y, Z, embedding_map_with_parameters = 'feature_representations')
```
