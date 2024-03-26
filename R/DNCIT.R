### DNCIT
DNCIT <- function(X, Y, Z, embedding_map = NULL, cit = "RCOT", params_CIT = list()) {
  if (!(is.matrix(X) && is.matrix(Y) && is.matrix(Z))) {
    return("Please input variables as matrices")
  }
  ## feature representations of X
  if (is.null(embedding_map)) {
    X <- X
  } else {
    X <- embedding_map(X)
  }
  ## nonparametric CIT
  if (cit == "RCOT") {
    start_time <- timestamp()
    res <- RCoT(X, Y, Z)
    end_time <- timestamp()
    res$runtime <- difftime(end_time, start_time, units = "secs")
  }
  return(res)
}

timestamp <- function(time = Sys.time()) {
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  format(time, "%Y-%B-%d_%H-%M-%S")
}

n <- 200
X <- rnorm(n, 0, 1)
Y <- rnorm(n, 0, 1)
Z <- rnorm(n, 0, 1)
X <- as.matrix(X)
Y <- as.matrix(Y)
Z <- as.matrix(Z)
DNCIT(X, Y, Z)
