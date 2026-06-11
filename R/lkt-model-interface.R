#' Define a custom LKT model
#'
#' @description Creates a model adapter that can be passed to `LKT(model = ...)`. Model `fit` functions receive one argument named `request` and may also accept `verbose`. The request contains the shared feature input (`formula`, `data`, `response`, `feature_spec`, and sparse design matrices) plus fit controls such as `usefolds` and model options.
#'
#' @param name Single model name used in returned LKT objects and errors.
#' @param fit Function with signature like `function(request, verbose = FALSE)`.
#' @return A model adapter suitable for `LKT(model = ...)`.
#' @examples
#' adapter <- LKTCustomModel(
#'   "constant",
#'   function(request, verbose = FALSE) {
#'     LKTModelFit(
#'       model = list(name = "constant"),
#'       pred = rep(0.5, length(request$response))
#'     )
#'   }
#' )
#' inherits(adapter, "LKTModel")
#' @export
LKTCustomModel <- function(name, fit) {
  if (!is.character(name) || length(name) != 1L || is.na(name) || name == "") {
    stop("LKTCustomModel name must be a non-empty character scalar.",
         call. = FALSE)
  }
  if (!is.function(fit)) {
    stop("LKTCustomModel fit must be a function.", call. = FALSE)
  }
  structure(
    list(name = name, fit = fit),
    class = c("LKTModel", "list")
  )
}

#' Create an LKT model fit result
#'
#' @description Standardizes the fit-result object returned by LKT model adapters. `model` and `pred` are required. `pred` must be one finite probability per row in the shared feature input, strictly between 0 and 1. If `modelvs`, `fitstat`, or predictor matrices are omitted, LKT fills common defaults where possible.
#'
#' @param model Fitted model object.
#' @param pred Numeric probability vector, one value per input row.
#' @param modelvs Optional coefficient matrix/data frame with a `coefficient` column.
#' @param fitstat Optional log likelihood or comparable fit statistic.
#' @param predictset Optional sparse model matrix.
#' @param predictset2 Optional CSR model matrix used by existing LKT outputs.
#' @return A model fit-result list.
#' @examples
#' fit_result <- LKTModelFit(
#'   model = list(name = "constant"),
#'   pred = c(0.5, 0.5)
#' )
#' names(fit_result)
#' @export
LKTModelFit <- function(model, pred, modelvs = NULL, fitstat = NULL,
                        predictset = NULL, predictset2 = NULL) {
  result <- list(
    model = model,
    pred = pred
  )
  if (!is.null(modelvs)) {
    result$modelvs <- modelvs
  }
  if (!is.null(fitstat)) {
    result$fitstat <- fitstat
  }
  if (!is.null(predictset)) {
    result$predictset <- predictset
  }
  if (!is.null(predictset2)) {
    result$predictset2 <- predictset2
  }
  result
}

lkt_model_fit_request <- function(feature_input, usefolds, options) {
  # Models receive the shared feature input plus fit controls. The duplicated
  # top-level fields keep simple model adapters easy to write.
  list(
    feature_input = feature_input,
    design_matrix = feature_input$design_matrix,
    data = feature_input$data,
    response = feature_input$response,
    usefolds = usefolds,
    options = options,
    formula = feature_input$formula,
    feature_spec = feature_input$feature_spec
  )
}

lkt_resolve_model <- function(model) {
  if (is.character(model) && length(model) == 1L) {
    if (identical(model, "liblinear")) {
      return(LibLinearModel())
    }
    if (identical(model, "online_calibration")) {
      return(OnlineCalibrationModel())
    }
    if (identical(model, "online_adaptive")) {
      return(OnlineAdaptiveModel())
    }
    if (identical(model, "fast_online_simple_adaptive")) {
      return(OnlineAdaptiveModel())
    }
    stop("Unknown LKT model: ", model, call. = FALSE)
  }

  if (is.function(model)) {
    return(LKTCustomModel(name = "custom", fit = model))
  }

  if (is.list(model) && is.function(model$fit)) {
    model_name <- if (is.null(model$name)) {
      "custom"
    } else {
      model$name
    }
    return(LKTCustomModel(name = model_name, fit = model$fit))
  }

  stop("model must be LibLinearModel(), OnlineCalibrationModel(), OnlineAdaptiveModel(), a model fit function, or an LKTCustomModel().",
       call. = FALSE)
}

lkt_model_options <- function(bias, cost, epsilon, type, extra_options = list()) {
  options <- list(
    bias = bias,
    cost = cost,
    epsilon = epsilon,
    type = type
  )
  utils::modifyList(options, extra_options)
}

lkt_model_log_likelihood <- function(response, pred, usefolds, data) {
  if (is.na(usefolds)[1]) {
    keep <- rep(TRUE, length(response))
  } else {
    keep <- data$fold %in% usefolds
  }
  sum(log(ifelse(response[keep] == 1, pred[keep], 1 - pred[keep])))
}

lkt_normalize_model_fit <- function(fit_result, request) {
  if (!("predictset2" %in% names(fit_result)) ||
      is.null(fit_result$predictset2)) {
    fit_result$predictset2 <- request$design_matrix$csr
  }
  if (!("predictset" %in% names(fit_result)) ||
      is.null(fit_result$predictset)) {
    fit_result$predictset <- request$design_matrix$sparse
  }
  if (!("fitstat" %in% names(fit_result)) ||
      is.null(fit_result$fitstat)) {
    fit_result$fitstat <- lkt_model_log_likelihood(
      response = request$response,
      pred = fit_result$pred,
      usefolds = request$usefolds,
      data = request$data)
  }
  if (!("modelvs" %in% names(fit_result)) ||
      is.null(fit_result$modelvs)) {
    fit_result$modelvs <- matrix(numeric(0), nrow = 0, ncol = 1,
                                 dimnames = list(character(), "coefficient"))
  }
  fit_result
}

lkt_validate_model_fit <- function(fit_result, model_name) {
  required <- c("model", "pred", "fitstat", "predictset2")
  missing <- required[!(required %in% names(fit_result))]
  if (length(missing) > 0) {
    stop("LKT model '", model_name,
         "' returned an incomplete fit result. Missing: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (length(fit_result$pred) != nrow(fit_result$predictset2)) {
    stop("LKT model '", model_name,
         "' returned predictions with length ", length(fit_result$pred),
         " but expected ", nrow(fit_result$predictset2), ".",
         call. = FALSE)
  }
  if (!is.numeric(fit_result$pred) ||
      any(!is.finite(fit_result$pred)) ||
      any(fit_result$pred <= 0 | fit_result$pred >= 1)) {
    stop("LKT model '", model_name,
         "' must return pred as finite probabilities strictly between 0 and 1.",
         call. = FALSE)
  }
  fit_result
}

lkt_call_model_fit <- function(model, request, verbose = FALSE) {
  fit_result <- tryCatch(
    model$fit(request = request, verbose = verbose),
    error = function(err) {
      msg <- conditionMessage(err)
      if (!grepl("unused argument", msg, fixed = TRUE)) {
        stop(err)
      }
      model$fit(
        design_matrix = request$design_matrix,
        data = request$data,
        usefolds = request$usefolds,
        options = request$options,
        verbose = verbose)
    })
  fit_result
}

lkt_run_model <- function(model, request, verbose = FALSE) {
  resolved_model <- lkt_resolve_model(model)
  fit_result <- lkt_call_model_fit(resolved_model, request, verbose = verbose)
  fit_result <- lkt_normalize_model_fit(fit_result, request)
  fit_result <- lkt_validate_model_fit(fit_result, resolved_model$name)
  fit_result$model_name <- resolved_model$name
  fit_result
}
