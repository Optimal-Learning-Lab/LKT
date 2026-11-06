# Model output and prediction helpers for LKT.
# These functions are sourced before LKTfunctions.R by working-copy examples
# and collated before LKTfunctions.R during package installation.

build_model_specification <- function(coef_vec,
                                      features,
                                      components,
                                      param_tracker) {
  coefficient <- coefficient_name <- feature <- component <- NULL

  if (length(coef_vec) == 0L) {
    return(data.table(
      coefficient_name = character(),
      feature = character(),
      component = character(),
      component_level = character(),
      coefficient = numeric()
    ))
  }

  spec <- data.table(
    coefficient_name = names(coef_vec),
    feature          = NA_character_,
    component        = "global",
    component_level  = NA_character_
  )

  if (length(features) != length(components)) {
    stop("Feature metadata requires paired features and components.", call. = FALSE)
  }
  pairs <- unique(data.frame(
    feature = gsub("[@$]", "", features),
    component = components, stringsAsFactors = FALSE))
  prefixes <- paste0(pairs$feature, gsub("%", "", pairs$component))

  # Match the full feature/component prefix, with the longest match first.
  # Prefix-only feature matching confuses base with base2 and KC with KC_long.
  for (i in seq_len(nrow(spec))) {
    cn <- spec$coefficient_name[i]
    if (cn %in% c("(Intercept)", "intercept")) {
      spec$feature[i] <- "intercept"
      next
    }
    hits <- which(vapply(prefixes, function(prefix) startsWith(cn, prefix),
                         logical(1)))
    if (!length(hits)) next
    hits <- hits[nchar(prefixes[hits]) == max(nchar(prefixes[hits]))]
    if (length(hits) != 1L) {
      stop("Ambiguous feature/component coefficient name: ", cn, call. = FALSE)
    }
    hit <- hits[[1L]]
    level <- substring(cn, nchar(prefixes[hit]) + 1L)
    spec[i, `:=`(feature = pairs$feature[hit],
                 component = pairs$component[hit],
                 component_level = if (nzchar(level)) level else NA_character_)]
  }

  #- add numeric coefficient ----
  spec[, coefficient := coef_vec[coefficient_name]]

  #- add nonlinear parameters (para through pare) ----
  spec[, `:=`(para = NA_real_, parb = NA_real_,
              parc = NA_real_, pard = NA_real_,
              pare = NA_real_)]

  for (i in seq_along(param_tracker)) {
    tracker <- param_tracker[[i]]
    if (length(tracker) == 0) {
      next
    }
    if (is.list(tracker) && all(c("feature", "component", "pars") %in% names(tracker))) {
      tracker_feature <- tracker$feature
      tracker_component <- tracker$component
      pars <- tracker$pars
      if (length(pars) == 0) pars <- rep(NA_real_, 5)
      spec[feature == tracker_feature & component == tracker_component,
           `:=`(para = pars[1],
                parb = pars[2],
                parc = pars[3],
                pard = pars[4],
                pare = pars[5])]
    } else {
      tracker_feature <- names(param_tracker)[i]
      pars <- tracker
      if (length(pars) == 0) pars <- rep(NA_real_, 5)
      spec[feature == tracker_feature,
           `:=`(para = pars[1],
                parb = pars[2],
                parc = pars[3],
                pard = pars[4],
                pare = pars[5])]
    }
  }


  spec[]
}




#' Predict with an LKT model object
#'
#' @description Generates predicted probabilities from a fitted `LKT()` object and, optionally, computes common evaluation statistics for selected folds. Stored design matrices and online predictions refer to the fitted rows in their original order; this helper does not reconstruct feature histories for new rows.
#'
#' @param modelob Fitted LKT model object containing coefficients and predictors.
#' @param data Data frame or data table aligned with the fitted rows. `CF..ansbin.` is required only for statistics; a `fold` column is required when selecting folds.
#' @param fold Optional vector of fold values to include in the returned predictions and statistics. `NULL` or an empty vector uses all rows.
#' @param return_stats Logical; if `TRUE`, return predictions plus log-likelihood, AUC, RMSE, and McFadden-style R-squared. AUC and R-squared are `NA` for single-class outcomes. If `FALSE`, return only predictions without evaluating outcomes or fitting diagnostics.
#' @param min_pred_limit Minimum probability used to clamp predictions.
#' @param max_pred_limit Maximum probability used to clamp predictions.
#' @param state Optional retained sequence-model or GRU student state.
#' @param return_state Include updated sequence state in the result.
#' @return If return_stats is FALSE, returns a list containing:
#' \itemize{
#'   \item \code{predictions}: The predicted probabilities for each observation in the specified fold(s).
#' } If return_stats is TRUE, returns a list containing:
#' \itemize{
#'   \item \code{predictions}: The predicted probabilities for each observation in the specified fold(s).
#'   \item \code{LL}: Log-Likelihood of the model given the actual outcomes.
#'   \item \code{AUC}: Area Under the ROC Curve.
#'   \item \code{RMSE}: Root Mean Squared Error.
#'   \item \code{R2}: R-squared value, indicating the proportion of variance explained by the model.
#' }
#' @examples
#' \donttest{
#' data(samplelkt)
#' samplelkt$CF..ansbin. <- as.integer(samplelkt$Outcome == "CORRECT")
#' model <- LKT(
#'   data = samplelkt,
#'   components = "KC..Default.",
#'   features = "intercept",
#'   interc = TRUE,
#'   verbose = FALSE
#' )
#' preds <- predict_lkt(model, samplelkt)
#' head(preds$predictions)
#' }
#' @export
predict_lkt <- function(modelob, data, fold = NULL, return_stats = FALSE,
                        min_pred_limit = .00001, max_pred_limit = .99999,
                        state = NULL, return_state = FALSE) {
  if (!is.numeric(min_pred_limit) || length(min_pred_limit) != 1L ||
      !is.numeric(max_pred_limit) || length(max_pred_limit) != 1L ||
      !is.finite(min_pred_limit) || !is.finite(max_pred_limit) ||
      min_pred_limit <= 0 || max_pred_limit >= 1 ||
      min_pred_limit >= max_pred_limit) {
    stop("Prediction bounds must satisfy 0 < min < max < 1.", call. = FALSE)
  }
  if (!is.null(fold) && length(fold) && !"fold" %in% names(data)) {
    stop("Fold selection requires a fold column.", call. = FALSE)
  }
  keep <- if (is.null(fold) || !length(fold)) rep(TRUE, nrow(data)) else
    data$fold %in% fold

  bayesian <- identical(modelob$model_name,'OnlineBayesian')
  gru <- modelob$model_name %in% c('GRU','Sequence')
  if (!is.logical(return_state) || length(return_state)!=1L || is.na(return_state))
    stop('return_state must be TRUE or FALSE.',call.=FALSE)
  if (!gru && (!is.null(state) || return_state))
    stop('Retained state is supported by predict_lkt only for sequence models.',call.=FALSE)
  if(bayesian&&!identical(as.data.frame(data),as.data.frame(modelob$newdata)))
    stop('OnlineBayesian predict_lkt requires the stored modelob$newdata; for new trials use LKTBayesianStep with causal features and retained state, or run a new LKT evaluation for new subjects.',call.=FALSE)
  sequential <- modelob$model_name %in% c("OnlineCalibration", "OnlineAdaptive", "OnlineBayesian")
  if (gru) {
    continued <- LKTSequenceStep(modelob,if(is.null(state))LKTSequenceState(modelob) else state,data)
    pred <- continued$predictions
  } else if (identical(modelob$model_name,'GLMM')) {
    policy <- modelob$prediction_policy
    if(is.null(policy))stop('GLMM prediction policy is missing.',call.=FALSE)
    pred <- lkt_glmm_prediction(modelob$model,data,policy$mode,
      policy$unseen_levels,policy$clip_epsilon)
  } else if (length(sequential) && sequential) {
    if (is.null(modelob$prediction) ||
        length(modelob$prediction) != nrow(data)) {
      stop("Online predictions are sequential and subject-stateful; ",
           "predict_lkt supports them only on the fitted rows in their ",
           "original order.", call. = FALSE)
    }
    pred <- modelob$prediction
  } else {
    if (is.null(modelob$predictors) || is.null(modelob$coefs)) {
      stop("modelob must include predictors and coefs from an LKT model fit.",
           call. = FALSE)
    }
    coef_vec <- if (is.matrix(modelob$coefs) || is.data.frame(modelob$coefs)) {
      if ("coefficient" %in% colnames(modelob$coefs)) {
        modelob$coefs[, "coefficient"]
      } else {
        modelob$coefs[, 1L]
      }
    } else modelob$coefs
    predictor_matrix <- if (is.character(modelob$predictors)) {
      as.matrix(as.data.frame(data)[, modelob$predictors, drop = FALSE])
    } else modelob$predictors
    if (nrow(predictor_matrix) != nrow(data)) {
      stop("predictor row count must match data row count.", call. = FALSE)
    }
    if (ncol(predictor_matrix) != length(coef_vec)) {
      stop("predictor column count must match coefficient count.", call. = FALSE)
    }
    pred <- as.numeric(plogis(predictor_matrix %*% as.numeric(coef_vec)))
  }
  if (!is.numeric(pred) || any(!is.finite(pred)) || any(pred < 0 | pred > 1)) {
    stop("Model predictions must be finite probabilities.", call. = FALSE)
  }
  pred <- if(bayesian)as.numeric(pred)[keep] else
    as.numeric(pmin(pmax(pred, min_pred_limit), max_pred_limit))[keep]
  # Prediction-only requests do not require outcomes or fit diagnostic models.
  if (!return_stats) return(c(list(predictions = pred),if(return_state)list(state=continued$state)))

  if (!"CF..ansbin." %in% names(data)) {
    stop("Prediction statistics require CF..ansbin. outcomes.", call. = FALSE)
  }
  actuals <- data$CF..ansbin.[keep]
  if (!length(actuals) || anyNA(actuals) || any(!actuals %in% c(0, 1))) {
    stop("Prediction statistics require nonempty binary outcomes.", call. = FALSE)
  }
  LL <- if(bayesian)sum(modelob$model$diagnostics$log_score[keep]) else
    sum(log(ifelse(actuals == 1, pred, 1 - pred)))
  # AUC and null-relative R2 are undefined for a one-class sample.
  AUC <- if (length(unique(actuals)) == 2L)
    as.numeric(pROC::auc(actuals, pred, quiet = TRUE)) else NA_real_
  null_LL <- sum(stats::dbinom(actuals, 1L, mean(actuals), log = TRUE))
  R2 <- if (null_LL == 0) NA_real_ else round(1 - LL / null_LL, 6)
  c(list(predictions = pred, LL = LL, AUC = AUC,
       RMSE = sqrt(mean((actuals - pred)^2)), R2 = R2),if(return_state)list(state=continued$state))
}
