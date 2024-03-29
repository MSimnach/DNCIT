
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
#> Using github PAT from envvar GITHUB_PAT. Use `gitcreds::gitcreds_set()` and unset GITHUB_PAT in .Renviron (or elsewhere) if you want to use the more secure git credential store instead.
#> Downloading GitHub repo MSimnach/DNCIT@HEAD
#> RcppEnsma... (NA -> 0.2.21.1.1) [CRAN]
#> RcppArmad... (NA -> 0.12.8.1.0) [CRAN]
#> Installing 2 packages: RcppEnsmallen, RcppArmadillo
#> Installiere Pakete nach 'C:/Users/Marco/AppData/Local/R/win-library/4.3'
#> (da 'lib' nicht spezifiziert)
#> Paket 'RcppEnsmallen' erfolgreich ausgepackt und MD5 Summen abgeglichen
#> Paket 'RcppArmadillo' erfolgreich ausgepackt und MD5 Summen abgeglichen
#> 
#> Die heruntergeladenen Binärpakete sind in 
#>  C:\Users\Marco\AppData\Local\Temp\Rtmp4IpMnX\downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\Marco\AppData\Local\Temp\Rtmp4IpMnX\remotes4cb01c8f5c5a\MSimnach-DNCIT-fef2fd983cb25018597271905af0320ad49a734f/DESCRIPTION' ...  ✔  checking for file 'C:\Users\Marco\AppData\Local\Temp\Rtmp4IpMnX\remotes4cb01c8f5c5a\MSimnach-DNCIT-fef2fd983cb25018597271905af0320ad49a734f/DESCRIPTION'
#>       ─  preparing 'DNCIT':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>   ─  building 'DNCIT_0.0.0.9000.tar.gz'
#>      
#> 
#> Installiere Paket nach 'C:/Users/Marco/AppData/Local/R/win-library/4.3'
#> (da 'lib' nicht spezifiziert)
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
#> [1] 0.2566561
#> 
#> $Sta
#> [1] 29.3534
#> 
#> $runtime
#> Time difference of 0.1163619 secs
```
