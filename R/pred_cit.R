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
pred_cit <- function(X,Y,Z, term_time = round(exp(4)/3)){
  # termination time for auto tuner
  term_time_at <- term_time

  ## learn y from x,z
  yxz <- do.call(cbind, list(Y,X,Z))
  colnames(yxz) <- paste0("V", 1:ncol(yxz))

  tsk_yxz <-  as_task_regr(yxz, target = "V1")
  measure <- msr("regr.mse")
  rsmp_inner <- rsmp("cv", folds = 5)
  rsmp_outer <- rsmp("cv", folds = 3)
  learner <- mlr3tuningspaces::lts(lrn("regr.ranger"))

  at_yxz <- auto_tuner(
    tuner = tnr("random_search"),
    learner = learner,
    resampling = rsmp_inner,
    measure = measure,
    term_time = term_time_at
  )

  rr_yxz <- resample(tsk_yxz, at_yxz, rsmp_outer)


  gen_error_estimate_yxz <- rr_yxz$aggregate()

  ## learn y from z
  yz <- do.call(cbind, list(Y,Z))
  colnames(yz) <- paste0("V", 1:ncol(yz))

  tsk_yz <-  as_task_regr(yz, target = "V1")
  #same folds as for y on xz
  rsmp_custom = rsmp("custom")
  rr_resampling_yxz <- rr_yxz$resampling$instance
  test_splits <- list()
  train_splits <- list()
  for (i in 1:rsmp_outer$iters) {
    test_splits[[i]] <- rr_resampling_yxz[rr_resampling_yxz$"fold"==i,]$row_id
    train_splits[[i]] <- rr_resampling_yxz[rr_resampling_yxz$"fold"!=i,]$row_id
  }
  rsmp_custom$instantiate(tsk_yz,
                          train = train_splits,
                          test = test_splits
  )

  at_yz <- auto_tuner(
    tuner = tnr("random_search"),
    learner = learner,
    resampling = rsmp_inner,
    measure = measure,
    term_time = term_time_at
  )

  rr_yz <- resample(tsk_yz, at_yz, rsmp_custom)

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
