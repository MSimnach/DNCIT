
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DNCIT

<!-- badges: start -->
<!-- badges: end -->

The goal of DNCIT is to provide a package for the deep nonparametric
conditional independence test (DNCIT) developed in the paper ‘Deep
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
#> [1] 0.2796225
#> 
#> $Sta
#> [1] 27.80437
```
