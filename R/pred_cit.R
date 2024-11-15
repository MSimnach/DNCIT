#' Prediction based CIT with mlr3 and random forest
#' Auto tuner for hyperparameter optimization
#' Nested resampling for performance estimation
#'
#' @param X A nxp-matrix of n p-dimensional vectors
#' @param Y A nx1-matrix
#' @param Z A nxq-matrix of n q-dimensional confounder (optional)
#' @param term_time The termination time for the auto tuner
#'
#' @return list of p-value and test statistic
#' @export
#'
#' @examples
#' library(mlr3learners)
#' n <- 200; p <- 1; q <- 1
#' X <- matrix(rnorm(n*p), nrow = n, ncol = p)
#' Y <- matrix(rnorm(n), nrow = n)
#' Z <- matrix(rnorm(n*q), nrow = n, ncol = q)
#' pred_cit(X, Y, Z, term_time = 1)
pred_cit <- function(X,Y,Z, term_time = round(exp(4)/3)){
  # termination time for auto tuner
  term_time_at <- term_time
  tuner <- mlr3tuning::tnr("random_search")

  ## learn y from z
  yz <- do.call(cbind, list(Y,Z))
  colnames(yz) <- paste0("V", 1:ncol(yz))
  measure <- mlr3::msr("regr.mse")
  learner <- mlr3tuningspaces::lts(mlr3::lrn("regr.ranger", verbose = FALSE))

  tsk_yz <-  mlr3::as_task_regr(yz, target = "V1")
  #same folds as for y on xz
  rsmp_inner <- mlr3::rsmp("cv", folds = 2)
  rsmp_outer <- mlr3::rsmp("cv", folds = 2)

  at_yz <- mlr3tuning::auto_tuner(
    tuner = tuner,
    learner = learner,
    resampling = rsmp_inner,
    measure = measure,
    term_time = term_time_at,
    store_models = TRUE
  )

  rr_yz <- mlr3::resample(tsk_yz, at_yz, rsmp_outer, store_models = TRUE)

  ## learn y_hat from x,z
  yxz <- do.call(cbind, list(Y,X,Z))
  colnames(yxz) <- paste0("V", 1:ncol(yxz))
  tsk_yxz <-  mlr3::as_task_regr(yxz, target = "V1")

  ## same outer resampling splits as for y on z
  rsmp_custom = mlr3::rsmp("custom")
  rr_resampling_yxz <- rr_yz$resampling$instance
  test_splits <- list()
  train_splits <- list()
  for (i in 1:rsmp_outer$iters) {
    test_splits[[i]] <- rr_resampling_yxz[rr_resampling_yxz$"fold"==i,]$row_id
    train_splits[[i]] <- rr_resampling_yxz[rr_resampling_yxz$"fold"!=i,]$row_id
  }
  rsmp_custom$instantiate(tsk_yxz,
                          train = train_splits,
                          test = test_splits
  )

  at_yxz <- mlr3tuning::auto_tuner(
    tuner = mlr3tuning::tnr("random_search"),
    learner = learner,
    resampling = rsmp_inner,
    measure = measure,
    term_time = term_time_at
  )

  rr_yxz <- mlr3::resample(tsk_yxz, at_yxz, rsmp_custom)


  ## residuals from predictions
  preds_yxz <- rr_yxz$prediction()
  res_yxz <- (preds_yxz$truth - preds_yxz$response)^2
  preds_yz <- rr_yz$prediction()
  res_yz <- (preds_yz$truth - preds_yz$response)^2

  ## Statistical test
  wilcox_test <- stats::wilcox.test(res_yxz, res_yz, paired = TRUE, alternative = "less")
  res <- c()
  res$p <- wilcox_test$p.value
  res$Sta <- wilcox_test$statistic
  return(res)
}
