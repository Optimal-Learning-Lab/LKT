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

  # map coefficient names to feature / component ----
  for (i in seq_len(nrow(spec))) {
    cn <- spec$coefficient_name[i]

    # student-specific intercept
    if (grepl("^interceptAnon\\.Student\\.Id", cn)) {
      spec[i, `:=`(feature = "intercept",
                   component = "Anon.Student.Id",
                   component_level =
                     sub("^interceptAnon\\.Student\\.Id", "", cn))]
      next
    }

    # global intercept
    if (cn %in% c("(Intercept)", "intercept")) {
      spec$feature[i] <- "intercept"
      next
    }

    # everything else
    for (f in features) {
      plain_f <- gsub("[@$]", "", f)
      if (startsWith(cn, plain_f)) {
        spec$feature[i] <- plain_f
        for (cmp in components) {
          if (grepl(cmp, cn, fixed = TRUE)) {
            spec$component[i] <- cmp
            lv <- sub(paste0(".*", gsub("\\.", "\\\\.", cmp)), "", cn)
            lv <- sub("^\\.*", "", lv)
            spec$component_level[i] <- ifelse(nchar(lv), lv, NA)
            break
          }
        }
        break
      }
    }
  }

  #- add numeric coefficient ----
  spec[, coefficient := coef_vec[coefficient_name]]

  #- add nonlinear parameters (para through pare) ----
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
#' @description Generates predicted probabilities from a fitted `LKT()` object and, optionally, computes common evaluation statistics for selected folds.
#'
#' @param modelob Fitted LKT model object containing coefficients and predictors.
#' @param data Data frame or data table containing `CF..ansbin.` and, when fold filtering is requested, a `fold` column.
#' @param fold Optional vector of fold values to include in the returned predictions and statistics. `NULL` or an empty vector uses all rows.
#' @param return_stats Logical; if `TRUE`, return predictions plus log-likelihood, AUC, RMSE, and McFadden-style R-squared. If `FALSE`, return only predictions.
#' @param min_pred_limit Minimum probability used to clamp predictions.
#' @param max_pred_limit Maximum probability used to clamp predictions.
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
                        min_pred_limit = .00001, max_pred_limit = .99999) {
  # Check if fold is NULL or empty, use all data if so
  if (is.null(fold) || length(fold) == 0) {
    fold <- if ("fold" %in% names(data)) unique(data$fold) else TRUE
  }

  if (is.null(modelob$predictors)) {
    stop("modelob must include predictors from an LKT model fit.", call. = FALSE)
  }

  if (is.null(modelob$coefs)) {
    stop("modelob must include coefs from an LKT model fit.", call. = FALSE)
  }

  if (identical(modelob$model_name, "OnlineCalibration")) {
    if (is.null(modelob$prediction) ||
        length(modelob$prediction) != nrow(data)) {
      stop("OnlineCalibration predictions are sequential and subject-stateful; ",
           "predict_lkt currently supports this model only on the fitted data ",
           "stored in the LKT result.", call. = FALSE)
    }
    keep <- if ("fold" %in% names(data)) data$fold %in% fold else rep(TRUE, nrow(data))
    pred <- modelob$prediction[keep]
    actuals <- data$CF..ansbin.[keep]
    LL <- sum(log(ifelse(actuals == 1, pred, 1 - pred)))
    AUC <- pROC::auc(actuals, pred)[1]
    nullmodel <- glm(actuals ~ 1, family = binomial(logit))
    R2 <- round(1 - LL / logLik(nullmodel)[1], 6)
    RMSE <- sqrt(mean((actuals - pred)^2))

    if (!return_stats) {
      return(list(predictions = pred))
    } else {
      return(list(predictions = pred, LL = LL, AUC = AUC, RMSE = RMSE, R2 = R2))
    }
  }

  coef_vec <- if (is.matrix(modelob$coefs) || is.data.frame(modelob$coefs)) {
    if ("coefficient" %in% colnames(modelob$coefs)) {
      modelob$coefs[, "coefficient"]
    } else {
      modelob$coefs[, 1]
    }
  } else {
    modelob$coefs
  }

  predictor_matrix <- if (is.character(modelob$predictors)) {
    as.matrix(as.data.frame(data)[, modelob$predictors, drop = FALSE])
  } else {
    modelob$predictors
  }

  if (nrow(predictor_matrix) != nrow(data)) {
    stop("predictor row count must match data row count.", call. = FALSE)
  }

  if (ncol(predictor_matrix) != length(coef_vec)) {
    stop("predictor column count must match coefficient count.", call. = FALSE)
  }

  # Current LKT objects store the fitted design matrix directly.
  predictionsMatrix <- predictor_matrix %*% as.numeric(coef_vec)

  # Apply configurable min and max limits
  pred <- as.numeric(pmin(pmax(plogis(predictionsMatrix), min_pred_limit), max_pred_limit))
  keep <- if ("fold" %in% names(data)) data$fold %in% fold else rep(TRUE, nrow(data))
  pred <- pred[keep]

  # Calculate Log-Likelihood, AUC, R2, and RMSE using actual values and predictions
  actuals <- data$CF..ansbin.[keep]
  LL <- sum(log(ifelse(actuals == 1, pred, 1 - pred)))
  AUC <- pROC::auc(actuals, pred)[1]
  nullmodel <- glm(actuals ~ 1, family = binomial(logit))
  R2 <- round(1 - LL / logLik(nullmodel)[1], 6)
  RMSE <- sqrt(mean((actuals - pred)^2))

  # Return predictions and optionally evaluation statistics
  if (!return_stats) {
    return(list(predictions = pred))
  } else {
    return(list(predictions = pred, LL = LL, AUC = AUC, RMSE = RMSE, R2 = R2))
  }
}
