# Latency model helpers for LKT accuracy models.
# These functions are sourced before LKTfunctions.R by working-copy examples
# and collated before LKTfunctions.R during package installation.


lkt_latency_fit_mask <- function(data, usefolds = NA, question_type = NULL) {
  keep <- rep(TRUE, nrow(data))
  if (!is.na(usefolds)[1L]) {
    lkt_require_columns(data, "fold", "LKT latency fitting mask")
    keep <- data$fold %in% usefolds
  }
  if (!is.null(question_type)) {
    if (!is.character(question_type) || length(question_type) != 1L ||
        is.na(question_type) || !nzchar(question_type))
      stop("latency_question_type_filter must be NULL or one nonempty string.", call. = FALSE)
    lkt_require_columns(data, "CF..Question.Type.", "LKT latency question-type filter")
    keep <- keep & !is.na(data$CF..Question.Type.) & data$CF..Question.Type. == question_type
  }
  if (!any(keep)) stop("No observations match the latency fitting mask.", call. = FALSE)
  keep
}

lkt_prepare_latency_data <- function(data, usefolds = NA, question_type = NULL) {
  lkt_require_columns(data, c("CF..ansbin.", "Duration..sec.", "pred", "Anon.Student.Id"), "LKT latency model")
  keep <- lkt_latency_fit_mask(data, usefolds, question_type)
  correct_idx <- which(keep & data$CF..ansbin. == 1 &
                         is.finite(data$Duration..sec.) & data$Duration..sec. > 0)
  if (!length(correct_idx)) stop("No usable correct-response latencies in the fitting mask.", call. = FALSE)
  correct_rt <- data$Duration..sec.[correct_idx]
  correct_pred <- data$pred[correct_idx]
  correct_subject_id <- data$Anon.Student.Id[correct_idx]
  if (any(!is.finite(correct_pred))) stop("Nonfinite accuracy predictions in latency fitting observations.", call. = FALSE)
  q1 <- stats::quantile(correct_rt, .25); q3 <- stats::quantile(correct_rt, .75)
  outlier_idx <- which(correct_rt < q1 - 1.5 * (q3-q1) | correct_rt > q3 + 1.5 * (q3-q1))
  clean <- setdiff(seq_along(correct_rt), outlier_idx)
  if (!length(clean)) stop("No usable correct-response latencies after IQR screening.", call. = FALSE)
  list(correct_rt=correct_rt, correct_pred=correct_pred, correct_subject_id=correct_subject_id,
       outlier_idx=outlier_idx, clean_rt=correct_rt[clean], clean_pred=correct_pred[clean],
       clean_subject_id=correct_subject_id[clean], fit_mask=keep)
}

lkt_fit_latency_model <- function(latency_data, distribution='Wald', verbose=FALSE,
    wald_calibration=NULL, wald_contamination_weight=.02,
    wald_t0_fixed=NULL, wald_control=list()) {
  if(!identical(distribution,'Wald'))
    stop('Only Wald latency is supported. Exponential and Weibull latency models are retired.',call.=FALSE)
  clean_rt<-latency_data$clean_rt;clean_pred<-latency_data$clean_pred
  clean_subject_id<-latency_data$clean_subject_id
  if(is.null(wald_calibration)) {
    if(verbose)cat('Fitting centered Wald likelihood; 2-component fit, engaged-only generation.\n')
    model<-fit_wald_activation(rt=clean_rt,p=clean_pred,subject_id=clean_subject_id,
      contamination_weight=wald_contamination_weight,t0_fixed=wald_t0_fixed,
      control=if(verbose)utils::modifyList(list(trace=1L),wald_control) else wald_control)
  } else {
    model<-import_wald_calibration(wald_calibration,clean_rt,clean_pred,clean_subject_id)
  }
  predicted<-model$predict(clean_pred,model$subject_effects[as.character(clean_subject_id)],
    add_noise=TRUE,truncate=FALSE)
  fitstat<-cor(clean_rt,predicted,use='complete.obs')^2
  if(verbose) {
    cat('\nWALD NORMAL-SCORE LATENCY: IQR +',100*model$contamination_weight,'% fit-only allowance.\n')
    print(model[c('k','lambda0','beta1','t0','sigma_subject','n_obs','n_subjects',
      'negative_log_likelihood','max_rt')])
    cat('Generation: engaged only; one normal trial draw; unchanged successful-RT cap.\n')
  }
  list(model=model,distribution='Wald',fitstat=fitstat,predicted=predicted)
}

lkt_failure_latency <- function(data, usefolds=NA, question_type=NULL) {
  keep <- lkt_latency_fit_mask(data,usefolds,question_type)
  rt <- data$Duration..sec.[keep & data$CF..ansbin.==0 & is.finite(data$Duration..sec.) & data$Duration..sec.>0]
  if (!length(rt)) stop("No usable failed-response latencies in the fitting mask.",call.=FALSE)
  mean(rt)
}

lkt_latency_model_result <- function(distribution,latency_model=NULL,failure_latency) {
  if(!identical(distribution,'Wald'))stop('Only Wald latency is supported.',call.=FALSE)
  list(latency_model,failure_latency)
}

lkt_latency_model_specification <- function(distribution,latency_model=NULL,failure_latency) {
  if(!identical(distribution,'Wald'))stop('Only Wald latency is supported.',call.=FALSE)
  c(list(distribution='Wald',failure_latency=failure_latency,
    latency_model_equation='RT=t0+QWald(Phi(-beta*logit(p)+u+epsilon)); epsilon~N(0,1)'),latency_model)
}
#' @import stats
NULL
