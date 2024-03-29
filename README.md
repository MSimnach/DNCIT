
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DNCIT

<!-- badges: start -->
<!-- badges: end -->

The goal of DNCIT is to provide a package for the deep nonparametric
conditional independence test (CIT) (DNCIT) developed in the paper ‘Deep
Nonparametric Conditional Independence Tests for Images’. The CIT
consists of a wrapper for potential embedding maps and a selection of
existing nonparametric CITs.

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
python packages: tigramite, scikit_learn. If you want to use other CITs,
you can install them in the virtual environment with the following
command:

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(DNCIT)
#> Lade nötiges Paket: momentchi2
## basic example code
n <- 1000; p <- 100; q <- 2
X_feature_representation <- matrix(rnorm(n*p),nrow=n,ncol=p)
Y <- matrix(rnorm(n), nrow=n)
Z_confounder <- matrix(rnorm(n*q),nrow=n,ncol=q)
DNCIT(X_feature_representation,Y,Z_confounder)
#> $p
#> [1] 0.789155
#> 
#> $Sta
#> [1] 12.99851
#> 
#> $runtime
#> Time difference of 0.09491205 secs
```
