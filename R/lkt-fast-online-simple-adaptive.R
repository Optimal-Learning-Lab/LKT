#' Fast online simple-adaptive objective input
#'
#' @description Builds compact numeric arrays for the fast compiled objective
#' used by the simple-adaptive online recency/logsuc model.
#'
#' @param modelob Fitted simple-adaptive LKT model with predictors and coefs.
#' @param data Data used for the fit.
#' @param subject_col Subject identifier column.
#' @param response_col Binary response column.
#' @param recency_coef Coefficient name for the recency feature.
#' @param logsuc_coef Coefficient name for the log-success feature.
#' @param logitdec_coef Coefficient name for the student logitdec feature.
#' @param spacing_col Raw spacing column used to recompute recency when the
#'   recency decay parameter is adapted online.
#' @param recency_decay_start Optional starting recency decay value. If
#'   `NULL`, the value is read from `modelob$model_specification`.
#' @return A list of compact arrays and starting parameters.
#' @export
LKTOnlineSimpleAdaptiveInput <- function(
    modelob,
    data,
    subject_col = "Anon.Student.Id",
    response_col = "CF..ansbin.",
    recency_coef = "recencyKC..Default.",
    logsuc_coef = "logsucKC..Default.",
    logitdec_coef = "logitdecAnon.Student.Id",
    spacing_col = "KC..Default.spacing",
    recency_decay_start = NULL) {
  if (is.null(modelob$predictors) || is.null(modelob$coefs)) {
    stop("modelob must include predictors and coefs.", call. = FALSE)
  }
  if (!subject_col %in% names(data)) {
    stop("data is missing subject_col '", subject_col, "'.", call. = FALSE)
  }
  if (!response_col %in% names(data)) {
    stop("data is missing response_col '", response_col, "'.", call. = FALSE)
  }
  if (!spacing_col %in% names(data)) {
    stop("data is missing spacing_col '", spacing_col, "'.", call. = FALSE)
  }

  coef_names <- rownames(modelob$coefs)
  required <- c("(Intercept)", recency_coef, logsuc_coef, logitdec_coef)
  missing <- setdiff(required, coef_names)
  if (length(missing) > 0L) {
    stop("modelob is missing required coefficients: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  predictors <- modelob$predictors
  if (!inherits(predictors, "matrix.csr")) {
    stop("fast simple-adaptive input currently requires SparseM matrix.csr predictors.",
         call. = FALSE)
  }
  predictor_names <- colnames(predictors)
  if (is.null(predictor_names)) {
    predictor_names <- coef_names
  }
  if (length(predictor_names) != ncol(predictors)) {
    stop("predictor column names do not match predictor column count.",
         call. = FALSE)
  }
  missing_predictors <- setdiff(required, predictor_names)
  if (length(missing_predictors) > 0L) {
    stop("predictors are missing required columns: ",
         paste(missing_predictors, collapse = ", "), call. = FALSE)
  }

  subjects <- data[[subject_col]]
  if (any(is.na(subjects))) {
    stop("subject_col contains NA values.", call. = FALSE)
  }
  subject_runs <- split(seq_along(subjects), subjects, drop = TRUE)
  subject_start <- vapply(subject_runs, function(idx) idx[1L], integer(1L))
  subject_end <- vapply(subject_runs, function(idx) idx[length(idx)], integer(1L))
  contiguous <- all(vapply(subject_runs, function(idx) {
    all(diff(idx) == 1L)
  }, logical(1L)))
  if (!contiguous) {
    stop("data must be sorted so each subject occupies one contiguous block.",
         call. = FALSE)
  }

  coef_vec <- as.numeric(modelob$coefs[, "coefficient"])
  names(coef_vec) <- coef_names
  if (is.null(recency_decay_start)) {
    recency_decay_start <- NA_real_
    spec <- modelob$model_specification
    if (is.list(spec) && length(spec) > 0L &&
        "coefficient_name" %in% names(spec[[1L]]) &&
        "para" %in% names(spec[[1L]])) {
      hit <- which(spec[[1L]]$coefficient_name == recency_coef)
      if (length(hit) > 0L) {
        recency_decay_start <- as.numeric(spec[[1L]]$para[hit[1L]])
      }
    }
    if (!is.finite(recency_decay_start)) {
      stop("recency_decay_start was not supplied and could not be read from ",
           "modelob$model_specification.", call. = FALSE)
    }
  }
  input_columns <- c(recency_coef, logsuc_coef, logitdec_coef)
  input_x <- lkt_online_adaptive_extract_csr_columns(
    predictors, match(input_columns, predictor_names))
  colnames(input_x) <- input_columns

  list(
    y = as.numeric(data[[response_col]]),
    x_recency = input_x[, recency_coef],
    x_logsuc = input_x[, logsuc_coef],
    x_logitdec = input_x[, logitdec_coef],
    x_spacing = as.numeric(data[[spacing_col]]),
    subject_start = as.integer(subject_start),
    subject_end = as.integer(subject_end),
    subject = names(subject_runs),
    recency_decay_start = as.numeric(recency_decay_start),
    beta_start = c(
      intercept = coef_vec["(Intercept)"],
      recency = coef_vec[recency_coef],
      logsuc = coef_vec[logsuc_coef],
      logitdec = coef_vec[logitdec_coef]
    ),
    base_loglike = modelob$loglike,
    base_r2 = modelob$r2,
    native_available = is.loaded("C_lkt_online_simple_adaptive_eval",
                                 PACKAGE = "LKT")
  )
}

#' Evaluate the fast online simple-adaptive objective
#'
#' @param par Numeric vector: intercept, recency beta, logsuc beta,
#'   logitdec beta, recency alpha, logsuc alpha.
#' @param input Output from \code{LKTOnlineSimpleAdaptiveInput()}.
#' @param clip_epsilon Probability clipping value.
#' @param denom_epsilon Denominator stabilizer.
#' @param return_details Logical; return predictions and final subject betas.
#' @param require_native Logical; stop if the compiled native evaluator is not
#'   loaded.
#' @return Negative log likelihood, or details when requested.
#' @export
LKTOnlineSimpleAdaptiveEval <- function(par, input, clip_epsilon = 1e-6,
                                        denom_epsilon = 1e-6,
                                        return_details = FALSE,
                                        require_native = FALSE) {
  if (length(par) != 6L || any(!is.finite(par))) {
    stop("par must be a finite numeric vector of length 6.", call. = FALSE)
  }
  if (!is.loaded("C_lkt_online_simple_adaptive_eval", PACKAGE = "LKT")) {
    if (isTRUE(require_native)) {
      stop("The compiled fast online simple-adaptive evaluator is not loaded. ",
           "Install/build LKT with native code before running with ",
           "require_native = TRUE.", call. = FALSE)
    }
    return(lkt_online_simple_adaptive_eval_r(
      par = par,
      input = input,
      clip_epsilon = clip_epsilon,
      denom_epsilon = denom_epsilon,
      return_details = return_details
    ))
  }
  result <- .Call(
    "C_lkt_online_simple_adaptive_eval",
    as.numeric(par),
    input$y,
    input$x_recency,
    input$x_logsuc,
    input$x_logitdec,
    input$subject_start,
    input$subject_end,
    as.numeric(clip_epsilon),
    as.numeric(denom_epsilon),
    as.logical(return_details),
    PACKAGE = "LKT"
  )
  if (return_details) {
    result$loglike <- -result$nll
    result$final_betas <- data.frame(
      subject = input$subject,
      final_recency = result$final_recency,
      final_logsuc = result$final_logsuc,
      stringsAsFactors = FALSE
    )
  }
  result
}

lkt_online_simple_adaptive_gradient_r <- function(par, input, clip_epsilon,
                                                  denom_epsilon) {
  beta_intercept <- par[1L]
  beta_recency_start <- par[2L]
  beta_logsuc_start <- par[3L]
  beta_logitdec <- par[4L]
  alpha_recency <- par[5L]
  alpha_logsuc <- par[6L]
  y <- input$y
  gradient <- numeric(6L)

  for (s in seq_along(input$subject_start)) {
    beta_recency <- beta_recency_start
    beta_logsuc <- beta_logsuc_start
    d_beta_recency <- c(0, 1, 0, 0, 0, 0)
    d_beta_logsuc <- c(0, 0, 1, 0, 0, 0)
    rows <- input$subject_start[s]:input$subject_end[s]

    for (row in rows) {
      xr <- input$x_recency[row]
      xl <- input$x_logsuc[row]
      xg <- input$x_logitdec[row]
      eta <- beta_intercept +
        beta_recency * xr +
        beta_logsuc * xl +
        beta_logitdec * xg
      raw_p <- stats::plogis(eta)
      p <- pmin(pmax(raw_p, clip_epsilon), 1 - clip_epsilon)

      d_eta <- xr * d_beta_recency + xl * d_beta_logsuc
      d_eta[1L] <- d_eta[1L] + 1
      d_eta[4L] <- d_eta[4L] + xg
      d_p <- if (raw_p > clip_epsilon && raw_p < 1 - clip_epsilon) {
        raw_p * (1 - raw_p) * d_eta
      } else {
        numeric(6L)
      }
      gradient <- gradient + (p - y[row]) * d_eta
      err <- y[row] - p
      d_err <- -d_p

      if (xr != 0 && alpha_recency != 0) {
        denom <- xr^2 + denom_epsilon
        d_beta_recency <- d_beta_recency +
          alpha_recency * xr * d_err / denom
        d_beta_recency[5L] <- d_beta_recency[5L] + xr * err / denom
        beta_recency <- beta_recency + alpha_recency * xr * err / denom
      } else if (xr != 0) {
        denom <- xr^2 + denom_epsilon
        d_beta_recency[5L] <- d_beta_recency[5L] + xr * err / denom
      }

      if (xl != 0 && alpha_logsuc != 0) {
        denom <- xl^2 + denom_epsilon
        d_beta_logsuc <- d_beta_logsuc +
          alpha_logsuc * xl * d_err / denom
        d_beta_logsuc[6L] <- d_beta_logsuc[6L] + xl * err / denom
        beta_logsuc <- beta_logsuc + alpha_logsuc * xl * err / denom
      } else if (xl != 0) {
        denom <- xl^2 + denom_epsilon
        d_beta_logsuc[6L] <- d_beta_logsuc[6L] + xl * err / denom
      }
    }
  }

  gradient
}

#' Gradient for the fast online simple-adaptive objective
#'
#' @param par Numeric vector: intercept, recency beta, logsuc beta,
#'   logitdec beta, recency alpha, logsuc alpha.
#' @param input Output from \code{LKTOnlineSimpleAdaptiveInput()}.
#' @param clip_epsilon Probability clipping value.
#' @param denom_epsilon Denominator stabilizer.
#' @param require_native Logical; stop if the compiled native evaluator is not
#'   loaded.
#' @return Gradient of the negative log likelihood.
#' @export
LKTOnlineSimpleAdaptiveGradient <- function(par, input, clip_epsilon = 1e-6,
                                            denom_epsilon = 1e-6,
                                            require_native = FALSE) {
  if (length(par) != 6L || any(!is.finite(par))) {
    stop("par must be a finite numeric vector of length 6.", call. = FALSE)
  }
  if (isTRUE(require_native) &&
      !is.loaded("C_lkt_online_simple_adaptive_gradient", PACKAGE = "LKT")) {
    stop("The compiled fast online simple-adaptive gradient is not loaded. ",
         "Install/build LKT with native code before running with ",
         "require_native = TRUE.", call. = FALSE)
  }
  if (is.loaded("C_lkt_online_simple_adaptive_gradient", PACKAGE = "LKT")) {
    return(.Call(
      "C_lkt_online_simple_adaptive_gradient",
      as.numeric(par),
      input$y,
      input$x_recency,
      input$x_logsuc,
      input$x_logitdec,
      input$subject_start,
      input$subject_end,
      as.numeric(clip_epsilon),
      as.numeric(denom_epsilon),
      PACKAGE = "LKT"
    ))
  }
  lkt_online_simple_adaptive_gradient_r(
    par = par,
    input = input,
    clip_epsilon = clip_epsilon,
    denom_epsilon = denom_epsilon
  )
}

lkt_online_simple_adaptive_value_gradient_native <- function(
    par, input, clip_epsilon = 1e-6, denom_epsilon = 1e-6,
    require_native = FALSE) {
  if (!is.loaded("C_lkt_online_simple_adaptive_value_gradient",
                 PACKAGE = "LKT")) {
    if (isTRUE(require_native)) {
      stop("The compiled fast online simple-adaptive value/gradient evaluator ",
           "is not loaded. Install/build LKT with native code before running ",
           "with require_native = TRUE.", call. = FALSE)
    }
    return(NULL)
  }
  .Call(
    "C_lkt_online_simple_adaptive_value_gradient",
    as.numeric(par),
    input$y,
    input$x_recency,
    input$x_logsuc,
    input$x_logitdec,
    input$subject_start,
    input$subject_end,
    as.numeric(clip_epsilon),
    as.numeric(denom_epsilon),
    PACKAGE = "LKT"
  )
}

lkt_online_simple_adaptive_eval_r <- function(par, input, clip_epsilon,
                                              denom_epsilon,
                                              return_details = FALSE) {
  beta_intercept <- par[1L]
  beta_recency_start <- par[2L]
  beta_logsuc_start <- par[3L]
  beta_logitdec <- par[4L]
  alpha_recency <- par[5L]
  alpha_logsuc <- par[6L]
  y <- input$y
  pred <- if (isTRUE(return_details)) numeric(length(y)) else NULL
  final_recency <- numeric(length(input$subject_start))
  final_logsuc <- numeric(length(input$subject_start))
  loglike <- 0

  for (s in seq_along(input$subject_start)) {
    beta_recency <- beta_recency_start
    beta_logsuc <- beta_logsuc_start
    rows <- input$subject_start[s]:input$subject_end[s]
    for (row in rows) {
      eta <- beta_intercept +
        beta_recency * input$x_recency[row] +
        beta_logsuc * input$x_logsuc[row] +
        beta_logitdec * input$x_logitdec[row]
      p <- pmin(pmax(stats::plogis(eta), clip_epsilon), 1 - clip_epsilon)
      err <- y[row] - p
      if (isTRUE(return_details)) {
        pred[row] <- p
      }
      loglike <- loglike + log(ifelse(y[row] == 1, p, 1 - p))
      if (input$x_recency[row] != 0 && alpha_recency != 0) {
        beta_recency <- beta_recency +
          alpha_recency * input$x_recency[row] * err /
          (input$x_recency[row]^2 + denom_epsilon)
      }
      if (input$x_logsuc[row] != 0 && alpha_logsuc != 0) {
        beta_logsuc <- beta_logsuc +
          alpha_logsuc * input$x_logsuc[row] * err /
          (input$x_logsuc[row]^2 + denom_epsilon)
      }
    }
    final_recency[s] <- beta_recency
    final_logsuc[s] <- beta_logsuc
  }

  if (!isTRUE(return_details)) {
    return(-loglike)
  }
  list(
    nll = -loglike,
    pred = pred,
    final_recency = final_recency,
    final_logsuc = final_logsuc,
    loglike = loglike,
    final_betas = data.frame(
      subject = input$subject,
      final_recency = final_recency,
      final_logsuc = final_logsuc,
      stringsAsFactors = FALSE
    )
  )
}

LKTOnlineSimpleAdaptiveDecayEval <- function(
    par, input, clip_epsilon = 1e-6, denom_epsilon = 1e-6,
    decay_lower = 1e-5, decay_upper = 0.99999, return_details = FALSE,
    require_native = FALSE) {
  if (length(par) != 7L || any(!is.finite(par))) {
    stop("par must be a finite numeric vector of length 7.", call. = FALSE)
  }
  if (!is.loaded("C_lkt_online_simple_adaptive_decay_eval", PACKAGE = "LKT")) {
    if (isTRUE(require_native)) {
      stop("The compiled fast online simple-adaptive decay evaluator is not ",
           "loaded. Install/build LKT with native code before running with ",
           "require_native = TRUE.", call. = FALSE)
    }
    return(lkt_online_simple_adaptive_decay_eval_r(
      par = par,
      input = input,
      clip_epsilon = clip_epsilon,
      denom_epsilon = denom_epsilon,
      decay_lower = decay_lower,
      decay_upper = decay_upper,
      return_details = return_details
    ))
  }
  result <- .Call(
    "C_lkt_online_simple_adaptive_decay_eval",
    as.numeric(par),
    input$y,
    input$x_spacing,
    input$x_logsuc,
    input$x_logitdec,
    input$subject_start,
    input$subject_end,
    as.numeric(input$recency_decay_start),
    as.numeric(decay_lower),
    as.numeric(decay_upper),
    as.numeric(clip_epsilon),
    as.numeric(denom_epsilon),
    as.logical(return_details),
    PACKAGE = "LKT"
  )
  if (return_details) {
    result$loglike <- -result$nll
    result$final_betas <- data.frame(
      subject = input$subject,
      final_recency = result$final_recency,
      final_logsuc = result$final_logsuc,
      final_decay = result$final_decay,
      stringsAsFactors = FALSE
    )
  }
  result
}

lkt_online_simple_adaptive_decay_eval_r <- function(
    par, input, clip_epsilon, denom_epsilon, decay_lower, decay_upper,
    return_details = FALSE) {
  beta_intercept <- par[1L]
  beta_recency_start <- par[2L]
  beta_logsuc_start <- par[3L]
  beta_logitdec <- par[4L]
  alpha_recency <- par[5L]
  alpha_logsuc <- par[6L]
  alpha_decay <- par[7L]
  y <- input$y
  pred <- if (isTRUE(return_details)) numeric(length(y)) else NULL
  final_recency <- numeric(length(input$subject_start))
  final_logsuc <- numeric(length(input$subject_start))
  final_decay <- numeric(length(input$subject_start))
  loglike <- 0

  for (s in seq_along(input$subject_start)) {
    beta_recency <- beta_recency_start
    beta_logsuc <- beta_logsuc_start
    recency_decay <- input$recency_decay_start
    rows <- input$subject_start[s]:input$subject_end[s]
    for (row in rows) {
      spacing <- input$x_spacing[row]
      xr <- if (spacing > 0) spacing^(-recency_decay) else 0
      xl <- input$x_logsuc[row]
      eta <- beta_intercept +
        beta_recency * xr +
        beta_logsuc * xl +
        beta_logitdec * input$x_logitdec[row]
      p <- pmin(pmax(stats::plogis(eta), clip_epsilon), 1 - clip_epsilon)
      err <- y[row] - p
      if (isTRUE(return_details)) {
        pred[row] <- p
      }
      loglike <- loglike + log(ifelse(y[row] == 1, p, 1 - p))
      if (xr != 0 && alpha_recency != 0) {
        beta_recency <- beta_recency +
          alpha_recency * xr * err / (xr^2 + denom_epsilon)
      }
      if (xl != 0 && alpha_logsuc != 0) {
        beta_logsuc <- beta_logsuc +
          alpha_logsuc * xl * err / (xl^2 + denom_epsilon)
      }
      if (alpha_decay != 0) {
        recency_decay <- pmin(
          pmax(recency_decay - alpha_decay * err, decay_lower),
          decay_upper
        )
      }
    }
    final_recency[s] <- beta_recency
    final_logsuc[s] <- beta_logsuc
    final_decay[s] <- recency_decay
  }

  if (!isTRUE(return_details)) {
    return(-loglike)
  }
  list(
    nll = -loglike,
    pred = pred,
    final_recency = final_recency,
    final_logsuc = final_logsuc,
    final_decay = final_decay,
    loglike = loglike,
    final_betas = data.frame(
      subject = input$subject,
      final_recency = final_recency,
      final_logsuc = final_logsuc,
      final_decay = final_decay,
      stringsAsFactors = FALSE
    )
  )
}

#' Optimize the fast online simple-adaptive model
#'
#' @param input Output from \code{LKTOnlineSimpleAdaptiveInput()}.
#' @param start Optional six-parameter starting vector.
#' @param lower Lower bounds for L-BFGS-B.
#' @param upper Upper bounds for L-BFGS-B.
#' @param control Optimizer control list.
#' @param require_native Logical; stop if the compiled native evaluator is not
#'   loaded.
#' @param use_gradient Logical; pass the propagated gradient to the optimizer.
#' @return A list containing the optimizer result and fit details.
#' @export
LKTOptimizeOnlineSimpleAdaptive <- function(
    input,
    start = NULL,
    lower = c(-Inf, -Inf, -Inf, -Inf, 0, 0),
    upper = rep(Inf, 6),
    control = list(maxit = 100),
    require_native = FALSE,
    use_gradient = TRUE) {
  if (is.null(start)) {
    start <- c(input$beta_start, alpha_recency = 0.03, alpha_logsuc = 0.05)
  }
  parameter_names <- c("intercept", "recency", "logsuc", "logitdec",
                       "alpha_recency", "alpha_logsuc")
  names(start) <- parameter_names
  native_value_gradient <- isTRUE(use_gradient) &&
    is.loaded("C_lkt_online_simple_adaptive_value_gradient", PACKAGE = "LKT")
  cache <- new.env(parent = emptyenv())
  cache$par <- NULL
  cache$result <- NULL
  value_gradient <- function(par) {
    if (!is.null(cache$par) && isTRUE(all(par == cache$par))) {
      return(cache$result)
    }
    result <- lkt_online_simple_adaptive_value_gradient_native(
      par,
      input,
      require_native = require_native
    )
    cache$par <- par
    cache$result <- result
    result
  }
  objective <- function(par) {
    if (native_value_gradient) {
      return(as.numeric(value_gradient(par)$nll))
    }
    as.numeric(LKTOnlineSimpleAdaptiveEval(
      par,
      input,
      return_details = FALSE,
      require_native = require_native
    ))
  }
  gradient <- function(par) {
    if (native_value_gradient) {
      return(value_gradient(par)$gradient)
    }
    LKTOnlineSimpleAdaptiveGradient(
      par,
      input,
      require_native = require_native
    )
  }
  optim_result <- stats::optim(
    par = as.numeric(start),
    fn = objective,
    gr = if (isTRUE(use_gradient)) gradient else NULL,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper,
    control = control
  )
  details <- LKTOnlineSimpleAdaptiveEval(
    optim_result$par,
    input,
    return_details = TRUE,
    require_native = require_native
  )
  names(optim_result$par) <- parameter_names
  null_loglike <- input$base_loglike / (1 - input$base_r2)
  r2 <- 1 - details$loglike / null_loglike
  list(
    par = optim_result$par,
    loglike = details$loglike,
    delta_loglike = details$loglike - input$base_loglike,
    r2 = r2,
    delta_r2 = r2 - input$base_r2,
    optimizer = optim_result,
    details = details
  )
}

#' Optimize only the online learning rates
#'
#' @description Optimizes the recency and logsuc online learning rates while
#' holding the global simple-adaptive coefficients fixed at their LibLinear
#' optimum.
#'
#' @param input Output from \code{LKTOnlineSimpleAdaptiveInput()}.
#' @param start Optional two-parameter starting vector: recency alpha and
#'   logsuc alpha.
#' @param lower Lower bounds for L-BFGS-B.
#' @param upper Upper bounds for L-BFGS-B.
#' @param control Optimizer control list.
#' @param require_native Logical; stop if the compiled native evaluator is not
#'   loaded.
#' @param use_gradient Logical; pass the propagated alpha gradient to the
#'   optimizer.
#' @return A list containing the optimizer result and fit details.
#' @export
LKTOptimizeOnlineSimpleAdaptiveAlpha <- function(
    input,
    start = c(alpha_recency = 0.03, alpha_logsuc = 0.05),
    lower = c(0, 0),
    upper = rep(Inf, 2),
    control = list(maxit = 100),
    require_native = FALSE,
    use_gradient = TRUE) {
  if (length(start) != 2L || any(!is.finite(start))) {
    stop("start must be a finite numeric vector of length 2.", call. = FALSE)
  }
  parameter_names <- c("alpha_recency", "alpha_logsuc")
  names(start) <- parameter_names
  full_parameter <- function(alpha) {
    c(input$beta_start,
      alpha_recency = alpha[1L],
      alpha_logsuc = alpha[2L])
  }
  cache <- new.env(parent = emptyenv())
  cache$alpha <- NULL
  cache$result <- NULL
  native_value_gradient <- isTRUE(use_gradient) &&
    is.loaded("C_lkt_online_simple_adaptive_value_gradient", PACKAGE = "LKT")
  value_gradient <- function(alpha) {
    if (!is.null(cache$alpha) && isTRUE(all(alpha == cache$alpha))) {
      return(cache$result)
    }
    par <- full_parameter(alpha)
    result <- if (native_value_gradient) {
      lkt_online_simple_adaptive_value_gradient_native(
        par,
        input,
        require_native = require_native
      )
    } else {
      list(
        nll = LKTOnlineSimpleAdaptiveEval(
          par,
          input,
          return_details = FALSE,
          require_native = require_native
        ),
        gradient = LKTOnlineSimpleAdaptiveGradient(
          par,
          input,
          require_native = require_native
        )
      )
    }
    cache$alpha <- alpha
    cache$result <- result
    result
  }
  objective <- function(alpha) {
    as.numeric(value_gradient(alpha)$nll)
  }
  gradient <- function(alpha) {
    as.numeric(value_gradient(alpha)$gradient[5:6])
  }
  optim_result <- stats::optim(
    par = as.numeric(start),
    fn = objective,
    gr = if (isTRUE(use_gradient)) gradient else NULL,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper,
    control = control
  )
  names(optim_result$par) <- parameter_names
  par <- full_parameter(optim_result$par)
  details <- LKTOnlineSimpleAdaptiveEval(
    par,
    input,
    return_details = TRUE,
    require_native = require_native
  )
  names(par) <- c("intercept", "recency", "logsuc", "logitdec",
                  "alpha_recency", "alpha_logsuc")
  null_loglike <- input$base_loglike / (1 - input$base_r2)
  r2 <- 1 - details$loglike / null_loglike
  list(
    par = par,
    alpha = optim_result$par,
    loglike = details$loglike,
    delta_loglike = details$loglike - input$base_loglike,
    r2 = r2,
    delta_r2 = r2 - input$base_r2,
    optimizer = optim_result,
    details = details
  )
}

LKTOptimizeOnlineSimpleAdaptiveDecayAlpha <- function(
    input,
    start = c(alpha_recency = 0.03, alpha_logsuc = 0.05,
              alpha_decay = 0.01),
    lower = c(0, 0, 0),
    upper = rep(Inf, 3),
    control = list(maxit = 100),
    require_native = FALSE,
    use_gradient = FALSE,
    decay_lower = 1e-5,
    decay_upper = 0.99999) {
  if (length(start) != 3L || any(!is.finite(start))) {
    stop("start must be a finite numeric vector of length 3.", call. = FALSE)
  }
  parameter_names <- c("alpha_recency", "alpha_logsuc", "alpha_decay")
  names(start) <- parameter_names
  full_parameter <- function(alpha) {
    c(input$beta_start,
      alpha_recency = alpha[1L],
      alpha_logsuc = alpha[2L],
      alpha_decay = alpha[3L])
  }
  objective <- function(alpha) {
    as.numeric(LKTOnlineSimpleAdaptiveDecayEval(
      full_parameter(alpha),
      input,
      decay_lower = decay_lower,
      decay_upper = decay_upper,
      require_native = require_native
    ))
  }
  optim_result <- stats::optim(
    par = as.numeric(start),
    fn = objective,
    gr = NULL,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper,
    control = control
  )
  names(optim_result$par) <- parameter_names
  par <- full_parameter(optim_result$par)
  details <- LKTOnlineSimpleAdaptiveDecayEval(
    par,
    input,
    decay_lower = decay_lower,
    decay_upper = decay_upper,
    return_details = TRUE,
    require_native = require_native
  )
  names(par) <- c("intercept", "recency", "logsuc", "logitdec",
                  "alpha_recency", "alpha_logsuc", "alpha_decay")
  null_loglike <- input$base_loglike / (1 - input$base_r2)
  r2 <- 1 - details$loglike / null_loglike
  list(
    par = par,
    alpha = optim_result$par,
    loglike = details$loglike,
    delta_loglike = details$loglike - input$base_loglike,
    r2 = r2,
    delta_r2 = r2 - input$base_r2,
    optimizer = optim_result,
    details = details
  )
}

lkt_online_adaptive_normalize_vector <- function(value, n, names, label) {
  if (n == 0L) {
    return(numeric(0))
  }
  if (is.null(value)) {
    stop(label, " must not be NULL.", call. = FALSE)
  }
  if (length(value) == 1L) {
    value <- rep(as.numeric(value), n)
  }
  if (length(value) != n) {
    stop(label, " must have length 1 or ", n, "; got ", length(value), ".",
         call. = FALSE)
  }
  value <- as.numeric(value)
  if (any(!is.finite(value))) {
    stop(label, " must contain finite numeric values.", call. = FALSE)
  }
  stats::setNames(value, names)
}

lkt_online_adaptive_extract_csr_column <- function(matrix_csr, column_index) {
  lkt_online_adaptive_extract_csr_columns(matrix_csr, column_index)[, 1L]
}

lkt_online_adaptive_extract_csr_columns <- function(matrix_csr, column_indices) {
  values <- matrix(0, nrow = nrow(matrix_csr), ncol = length(column_indices))
  if (length(column_indices) == 0L) {
    return(values)
  }
  for (row in seq_len(nrow(matrix_csr))) {
    start <- matrix_csr@ia[row]
    end <- matrix_csr@ia[row + 1L] - 1L
    if (start <= end) {
      ptr <- start:end
      hit <- match(column_indices, matrix_csr@ja[ptr])
      present <- !is.na(hit)
      if (any(present)) {
        values[row, present] <- matrix_csr@ra[ptr[hit[present]]]
      }
    }
  }
  values
}

lkt_online_adaptive_subject_ranges <- function(data, subject_col) {
  if (!subject_col %in% names(data)) {
    stop("data is missing subject_col '", subject_col, "'.", call. = FALSE)
  }
  subjects <- data[[subject_col]]
  if (any(is.na(subjects))) {
    stop("subject_col contains NA values.", call. = FALSE)
  }
  subject_runs <- split(seq_along(subjects), subjects, drop = TRUE)
  contiguous <- all(vapply(subject_runs, function(idx) {
    all(diff(idx) == 1L)
  }, logical(1L)))
  if (!contiguous) {
    stop("data must be sorted so each subject occupies one contiguous block.",
         call. = FALSE)
  }
  list(
    subject_start = vapply(subject_runs, function(idx) idx[1L], integer(1L)),
    subject_end = vapply(subject_runs, function(idx) idx[length(idx)], integer(1L)),
    subject = names(subject_runs)
  )
}

lkt_online_adaptive_parse_nonlinear_key <- function(key) {
  parts <- strsplit(key, "\\|", fixed = FALSE)[[1L]]
  if (length(parts) != 3L) {
    stop("Invalid nonlinear_alpha_terms entry '", key,
         "'. Use feature|component|slot, for example ",
         "recency|KC..Default.|para.", call. = FALSE)
  }
  slot <- parts[3L]
  valid_slots <- c("para", "parb", "parc", "pard", "pare")
  if (!slot %in% valid_slots) {
    stop("Invalid nonlinear parameter slot '", slot,
         "'. Valid slots are: ", paste(valid_slots, collapse = ", "),
         call. = FALSE)
  }
  list(feature = parts[1L], component = parts[2L], slot = slot)
}

lkt_online_adaptive_find_feature_spec <- function(feature_spec, feature,
                                                 component) {
  if (!is.list(feature_spec) || length(feature_spec) == 0L) {
    stop("online_adaptive nonlinear adaptation needs feature metadata from ",
         "LKT feature construction.", call. = FALSE)
  }
  for (spec in feature_spec) {
    if (is.list(spec) &&
        identical(spec$feature, feature) &&
        identical(spec$component, component)) {
      return(spec)
    }
  }
  stop("No feature metadata found for nonlinear adaptive term ",
       feature, "|", component, ".", call. = FALSE)
}

lkt_online_adaptive_compute_feature_at_row <- function(prepared_data,
                                                       subject_start,
                                                       row, feature, component,
                                                       pars) {
  prefix <- data.table::copy(prepared_data[subject_start:row, ])
  values <- computefeatures(
    prefix,
    feature,
    pars[["para"]],
    pars[["parb"]],
    prefix$index,
    prefix$indexcomp,
    pars[["parc"]],
    pars[["pard"]],
    pars[["pare"]],
    component)
  as.numeric(values[length(values)])
}

lkt_online_adaptive_group_nonlinear_terms <- function(nonlinear_alpha_terms,
                                                      feature_spec,
                                                      coef_vec,
                                                      predictor_names,
                                                      predictors,
                                                      data) {
  if (length(nonlinear_alpha_terms) == 0L) {
    return(list(terms = list(), alpha_names = character(0)))
  }
  parsed <- lapply(nonlinear_alpha_terms, lkt_online_adaptive_parse_nonlinear_key)
  group_keys <- vapply(parsed, function(x) {
    paste(x$feature, x$component, sep = "|")
  }, character(1L))
  grouped <- split(seq_along(parsed), group_keys)
  valid_slots <- c("para", "parb", "parc", "pard", "pare")
  terms <- vector("list", length(grouped))
  alpha_names <- character(0)
  pos <- 1L
  for (key in names(grouped)) {
    first <- parsed[[grouped[[key]][1L]]]
    spec <- lkt_online_adaptive_find_feature_spec(
      feature_spec, first$feature, first$component)
    coefficient_name <- lkt_feature_column_name(first$feature, first$component)
    if (!coefficient_name %in% names(coef_vec)) {
      stop("No fitted coefficient named '", coefficient_name,
           "' for nonlinear adaptive term ", key, ".", call. = FALSE)
    }
    if (!coefficient_name %in% predictor_names) {
      stop("No predictor column named '", coefficient_name,
           "' for nonlinear adaptive term ", key, ".", call. = FALSE)
    }
    slots <- vapply(parsed[grouped[[key]]], `[[`, character(1L), "slot")
    duplicated_slots <- unique(slots[duplicated(slots)])
    if (length(duplicated_slots) > 0L) {
      stop("Duplicate nonlinear adaptive slot(s) for ", key, ": ",
           paste(duplicated_slots, collapse = ", "), call. = FALSE)
    }
    start_pars <- as.numeric(spec$pars[valid_slots])
    names(start_pars) <- valid_slots
    if (any(is.na(start_pars[slots]))) {
      stop("Requested nonlinear adaptive slot(s) not present for ", key, ": ",
           paste(slots[is.na(start_pars[slots])], collapse = ", "),
           call. = FALSE)
    }
    prepared_data <- lkt_prepare_component_context(
      data.table::copy(data), first$component, first$feature)
    alpha_for_slots <- paste(first$feature, first$component, slots, sep = "|")
    alpha_names <- c(alpha_names, paste0("nonlinear:", alpha_for_slots))
    terms[[pos]] <- list(
      key = key,
      feature = first$feature,
      component = first$component,
      coefficient_name = coefficient_name,
      coefficient = as.numeric(coef_vec[coefficient_name]),
      original_x = lkt_online_adaptive_extract_csr_column(
        predictors, match(coefficient_name, predictor_names)),
      prepared_data = prepared_data,
      pars_start = start_pars,
      slots = slots,
      slot_indices = match(slots, valid_slots),
      alpha_names = paste0("nonlinear:", alpha_for_slots)
    )
    pos <- pos + 1L
  }
  list(terms = terms, alpha_names = alpha_names)
}

lkt_online_adaptive_build_registry <- function(modelob, data, feature_spec,
                                               beta_alpha_terms,
                                               nonlinear_alpha_terms,
                                               alpha_start,
                                               alpha_lower,
                                               alpha_upper,
                                               subject_col = "Anon.Student.Id",
                                               response_col = "CF..ansbin.",
                                               clip_epsilon = 1e-6,
                                               denom_epsilon = 1e-6,
                                               nonlinear_lower = 1e-5,
                                               nonlinear_upper = 0.99999) {
  if (is.null(modelob$predictors) || is.null(modelob$coefs)) {
    stop("modelob must include predictors and coefs.", call. = FALSE)
  }
  if (!response_col %in% names(data)) {
    stop("data is missing response_col '", response_col, "'.", call. = FALSE)
  }
  predictors <- modelob$predictors
  if (!inherits(predictors, "matrix.csr")) {
    stop("online_adaptive requires SparseM matrix.csr predictors.",
         call. = FALSE)
  }
  predictor_names <- colnames(predictors)
  if (is.null(predictor_names)) {
    predictor_names <- rownames(modelob$coefs)
  }
  if (length(predictor_names) != ncol(predictors)) {
    stop("predictor column names do not match predictor column count.",
         call. = FALSE)
  }
  coef_names <- rownames(modelob$coefs)
  coef_vec <- as.numeric(modelob$coefs[, "coefficient"])
  names(coef_vec) <- coef_names

  beta_alpha_terms <- unique(beta_alpha_terms %||% character(0))
  nonlinear_alpha_terms <- unique(nonlinear_alpha_terms %||% character(0))
  missing_beta <- setdiff(beta_alpha_terms, coef_names)
  if (length(missing_beta) > 0L) {
    stop("Unknown beta_alpha_terms coefficient(s): ",
         paste(missing_beta, collapse = ", "), call. = FALSE)
  }
  missing_beta_predictors <- setdiff(beta_alpha_terms, predictor_names)
  if (length(missing_beta_predictors) > 0L) {
    stop("beta_alpha_terms missing predictor column(s): ",
         paste(missing_beta_predictors, collapse = ", "), call. = FALSE)
  }

  beta_x <- if (length(beta_alpha_terms) > 0L) {
    lkt_online_adaptive_extract_csr_columns(
      predictors, match(beta_alpha_terms, predictor_names))
  } else {
    matrix(numeric(0), nrow = nrow(predictors), ncol = 0L)
  }
  colnames(beta_x) <- beta_alpha_terms
  beta_terms <- lapply(seq_along(beta_alpha_terms), function(i) {
    term <- beta_alpha_terms[i]
    list(
      name = term,
      x = beta_x[, i],
      beta_start = as.numeric(coef_vec[term]),
      alpha_name = paste0("beta:", term)
    )
  })
  beta_start <- if (length(beta_terms) > 0L) {
    vapply(beta_terms, `[[`, numeric(1L), "beta_start")
  } else {
    numeric(0)
  }
  names(beta_start) <- beta_alpha_terms
  nonlinear_grouped <- lkt_online_adaptive_group_nonlinear_terms(
    nonlinear_alpha_terms = nonlinear_alpha_terms,
    feature_spec = feature_spec,
    coef_vec = coef_vec,
    predictor_names = predictor_names,
    predictors = predictors,
    data = data)
  alpha_names <- c(
    vapply(beta_terms, `[[`, character(1L), "alpha_name"),
    nonlinear_grouped$alpha_names)
  if (length(alpha_names) == 0L) {
    stop("online_adaptive needs at least one beta_alpha_terms or ",
         "nonlinear_alpha_terms entry.", call. = FALSE)
  }
  if (is.null(alpha_start)) {
    alpha_start <- rep(0.01, length(alpha_names))
  }
  alpha_start <- lkt_online_adaptive_normalize_vector(
    alpha_start, length(alpha_names), alpha_names, "alpha_start")
  alpha_lower <- lkt_online_adaptive_normalize_vector(
    alpha_lower, length(alpha_names), alpha_names, "alpha_lower")
  alpha_upper <- lkt_online_adaptive_normalize_vector(
    alpha_upper, length(alpha_names), alpha_names, "alpha_upper")
  if (any(alpha_lower > alpha_upper)) {
    stop("alpha_lower must be less than or equal to alpha_upper.",
         call. = FALSE)
  }
  ranges <- lkt_online_adaptive_subject_ranges(data, subject_col)
  eta_base <- stats::qlogis(pmin(pmax(as.numeric(modelob$pred),
                                      clip_epsilon), 1 - clip_epsilon))
  list(
    y = as.numeric(data[[response_col]]),
    data = data,
    eta_base = eta_base,
    beta_terms = beta_terms,
    beta_x = beta_x,
    beta_start = beta_start,
    nonlinear_terms = nonlinear_grouped$terms,
    alpha_names = alpha_names,
    alpha_start = alpha_start,
    alpha_lower = alpha_lower,
    alpha_upper = alpha_upper,
    subject_start = ranges$subject_start,
    subject_end = ranges$subject_end,
    subject = ranges$subject,
    clip_epsilon = clip_epsilon,
    denom_epsilon = denom_epsilon,
    nonlinear_lower = nonlinear_lower,
    nonlinear_upper = nonlinear_upper,
    base_loglike = modelob$loglike,
    base_r2 = modelob$r2,
    native_available = is.loaded("C_lkt_online_adaptive_beta_eval",
                                 PACKAGE = "LKT")
  )
}

lkt_online_adaptive_eval_beta_native <- function(alpha, registry,
                                                return_details = FALSE,
                                                require_native = FALSE) {
  if (!is.loaded("C_lkt_online_adaptive_beta_eval", PACKAGE = "LKT")) {
    if (isTRUE(require_native)) {
      stop("The compiled generalized online_adaptive beta evaluator is not ",
           "loaded. Install/build LKT with native code before running with ",
           "require_native = TRUE.", call. = FALSE)
    }
    return(lkt_online_adaptive_eval_general(
      alpha, registry, return_details = return_details))
  }
  result <- .Call(
    "C_lkt_online_adaptive_beta_eval",
    as.numeric(alpha),
    registry$y,
    registry$eta_base,
    registry$beta_x,
    registry$beta_start,
    as.integer(registry$subject_start),
    as.integer(registry$subject_end),
    as.numeric(registry$clip_epsilon),
    as.numeric(registry$denom_epsilon),
    as.logical(return_details),
    PACKAGE = "LKT")
  if (!isTRUE(return_details)) {
    return(result)
  }
  colnames(result$final_beta) <- names(registry$beta_start)
  final_states <- data.frame(subject = registry$subject,
                             stringsAsFactors = FALSE)
  for (j in seq_along(registry$beta_start)) {
    final_states[[paste0("final_beta:", names(registry$beta_start)[j])]] <-
      result$final_beta[, j]
  }
  list(
    nll = result$nll,
    loglike = -result$nll,
    pred = result$pred,
    final_states = final_states
  )
}

lkt_online_adaptive_eval_dispatch <- function(alpha, registry,
                                             return_details = FALSE,
                                             require_native = FALSE) {
  if (length(registry$nonlinear_terms) == 0L) {
    return(lkt_online_adaptive_eval_beta_native(
      alpha, registry,
      return_details = return_details,
      require_native = require_native))
  }
  feature_decay_spec <- lkt_online_adaptive_feature_decay_native_spec(registry)
  if (!is.null(feature_decay_spec)) {
    return(lkt_online_adaptive_eval_feature_decay_native(
      alpha, registry,
      return_details = return_details,
      require_native = require_native))
  }
  if (isTRUE(require_native)) {
    stop("require_native = TRUE was requested, but generalized native ",
         "nonlinear online_adaptive evaluation is not available for the ",
         "requested nonlinear terms.", call. = FALSE)
  }
  lkt_online_adaptive_eval_general(
    alpha, registry, return_details = return_details)
}

lkt_online_adaptive_eval_recency_decay_native <- function(alpha, registry,
                                                         return_details = FALSE,
                                                         require_native = TRUE) {
  if (!is.loaded("C_lkt_online_adaptive_recency_decay_eval", PACKAGE = "LKT")) {
    if (isTRUE(require_native)) {
      stop("The compiled generalized online_adaptive recency-decay evaluator ",
           "is not loaded. Install/build LKT with compiled code before ",
           "running with require_native = TRUE.", call. = FALSE)
    }
    return(lkt_online_adaptive_eval_general(
      alpha, registry, return_details = return_details))
  }
  if (length(registry$nonlinear_terms) != 1L ||
      !identical(registry$nonlinear_terms[[1L]]$feature, "recency") ||
      !identical(registry$nonlinear_terms[[1L]]$component, "KC..Default.") ||
      !identical(registry$nonlinear_terms[[1L]]$slots, "para")) {
    stop("recency-decay native evaluator received unsupported nonlinear terms.",
         call. = FALSE)
  }
  term <- registry$nonlinear_terms[[1L]]
  spacing_col <- paste0(term$component, "spacing")
  if (!spacing_col %in% names(registry$data)) {
    stop("data is missing spacing column '", spacing_col,
         "' needed by compiled recency-decay evaluator.", call. = FALSE)
  }
  result <- .Call(
    "C_lkt_online_adaptive_recency_decay_eval",
    as.numeric(alpha),
    registry$y,
    registry$eta_base,
    registry$beta_x,
    registry$beta_start,
    as.numeric(registry$data[[spacing_col]]),
    term$original_x,
    as.numeric(term$coefficient),
    as.integer(registry$subject_start),
    as.integer(registry$subject_end),
    as.numeric(term$pars_start[["para"]]),
    as.numeric(registry$nonlinear_lower),
    as.numeric(registry$nonlinear_upper),
    as.numeric(registry$clip_epsilon),
    as.numeric(registry$denom_epsilon),
    as.logical(return_details),
    PACKAGE = "LKT")
  if (!isTRUE(return_details)) {
    return(result)
  }
  if (length(registry$beta_start) > 0L) {
    colnames(result$final_beta) <- names(registry$beta_start)
  }
  final_states <- data.frame(subject = registry$subject,
                             stringsAsFactors = FALSE)
  for (j in seq_along(registry$beta_start)) {
    final_states[[paste0("final_beta:", names(registry$beta_start)[j])]] <-
      result$final_beta[, j]
  }
  final_states[["final_nonlinear:recency|KC..Default.|para"]] <-
    result$final_decay
  list(
    nll = result$nll,
    loglike = -result$nll,
    pred = result$pred,
    final_states = final_states
  )
}

lkt_online_adaptive_feature_decay_native_spec <- function(registry) {
  if (length(registry$nonlinear_terms) == 0L) {
    return(NULL)
  }
  types <- integer(length(registry$nonlinear_terms))
  aux <- matrix(0, nrow = length(registry$y),
                ncol = length(registry$nonlinear_terms))
  original <- matrix(0, nrow = length(registry$y),
                     ncol = length(registry$nonlinear_terms))
  coefficient <- numeric(length(registry$nonlinear_terms))
  start <- numeric(length(registry$nonlinear_terms))
  keys <- character(length(registry$nonlinear_terms))

  for (j in seq_along(registry$nonlinear_terms)) {
    term <- registry$nonlinear_terms[[j]]
    if (!identical(term$slots, "para")) {
      return(NULL)
    }
    keys[j] <- paste(term$feature, term$component, "para", sep = "|")
    if (identical(term$feature, "recency") &&
        identical(term$component, "KC..Default.")) {
      spacing_col <- paste0(term$component, "spacing")
      if (!spacing_col %in% names(registry$data)) {
        return(NULL)
      }
      types[j] <- 1L
      aux[, j] <- as.numeric(registry$data[[spacing_col]])
    } else if (identical(term$feature, "logitdec") &&
               identical(term$component, "Anon.Student.Id")) {
      types[j] <- 2L
    } else {
      return(NULL)
    }
    original[, j] <- term$original_x
    coefficient[j] <- term$coefficient
    start[j] <- term$pars_start[["para"]]
  }

  list(
    types = types,
    aux = aux,
    original = original,
    coefficient = coefficient,
    start = start,
    keys = keys
  )
}

lkt_online_adaptive_eval_feature_decay_native <- function(alpha, registry,
                                                         return_details = FALSE,
                                                         require_native = FALSE) {
  spec <- lkt_online_adaptive_feature_decay_native_spec(registry)
  if (is.null(spec)) {
    if (isTRUE(require_native)) {
      stop("require_native = TRUE was requested, but compiled nonlinear ",
           "online_adaptive evaluation does not support the requested ",
           "nonlinear terms.", call. = FALSE)
    }
    return(lkt_online_adaptive_eval_general(
      alpha, registry, return_details = return_details))
  }
  if (!is.loaded("C_lkt_online_adaptive_feature_decay_eval", PACKAGE = "LKT")) {
    if (isTRUE(require_native)) {
      stop("The compiled generalized online_adaptive feature-decay evaluator ",
           "is not loaded. Install/build LKT with compiled code before ",
           "running with require_native = TRUE.", call. = FALSE)
    }
    return(lkt_online_adaptive_eval_general(
      alpha, registry, return_details = return_details))
  }

  result <- .Call(
    "C_lkt_online_adaptive_feature_decay_eval",
    as.numeric(alpha),
    registry$y,
    registry$eta_base,
    registry$beta_x,
    registry$beta_start,
    as.integer(spec$types),
    spec$aux,
    spec$original,
    as.numeric(spec$coefficient),
    as.numeric(spec$start),
    as.integer(registry$subject_start),
    as.integer(registry$subject_end),
    as.numeric(registry$nonlinear_lower),
    as.numeric(registry$nonlinear_upper),
    as.numeric(registry$clip_epsilon),
    as.numeric(registry$denom_epsilon),
    as.logical(return_details),
    PACKAGE = "LKT")
  if (!isTRUE(return_details)) {
    return(result)
  }

  if (length(registry$beta_start) > 0L) {
    colnames(result$final_beta) <- names(registry$beta_start)
  }
  colnames(result$final_nonlinear) <- spec$keys
  final_states <- data.frame(subject = registry$subject,
                             stringsAsFactors = FALSE)
  for (j in seq_along(registry$beta_start)) {
    final_states[[paste0("final_beta:", names(registry$beta_start)[j])]] <-
      result$final_beta[, j]
  }
  for (j in seq_along(spec$keys)) {
    final_states[[paste0("final_nonlinear:", spec$keys[j])]] <-
      result$final_nonlinear[, j]
  }
  list(
    nll = result$nll,
    loglike = -result$nll,
    pred = result$pred,
    final_states = final_states
  )
}

lkt_online_adaptive_eval_general <- function(alpha, registry,
                                             return_details = FALSE) {
  alpha <- as.numeric(alpha)
  names(alpha) <- registry$alpha_names
  if (length(alpha) != length(registry$alpha_names) ||
      any(!is.finite(alpha))) {
    stop("alpha must be a finite numeric vector matching the adaptive terms.",
         call. = FALSE)
  }
  y <- registry$y
  pred <- numeric(length(y))
  nll <- 0
  final_states <- vector("list", length(registry$subject_start))
  valid_slots <- c("para", "parb", "parc", "pard", "pare")

  for (s in seq_along(registry$subject_start)) {
    subject_start <- registry$subject_start[s]
    rows <- subject_start:registry$subject_end[s]
    beta_state <- vapply(registry$beta_terms, `[[`, numeric(1L),
                          "beta_start")
    names(beta_state) <- vapply(registry$beta_terms, `[[`, character(1L),
                                 "name")
    nonlinear_state <- lapply(registry$nonlinear_terms, function(term) {
      term$pars_start
    })

    for (row in rows) {
      eta <- registry$eta_base[row]
      if (length(registry$beta_terms) > 0L) {
        for (j in seq_along(registry$beta_terms)) {
          term <- registry$beta_terms[[j]]
          eta <- eta + (beta_state[j] - term$beta_start) * term$x[row]
        }
      }
      current_x <- numeric(length(registry$nonlinear_terms))
      if (length(registry$nonlinear_terms) > 0L) {
        for (j in seq_along(registry$nonlinear_terms)) {
          term <- registry$nonlinear_terms[[j]]
          if (all(alpha[term$alpha_names] == 0)) {
            current_x[j] <- term$original_x[row]
          } else {
            current_x[j] <- lkt_online_adaptive_compute_feature_at_row(
              prepared_data = term$prepared_data,
              subject_start = subject_start,
              row = row,
              feature = term$feature,
              component = term$component,
              pars = nonlinear_state[[j]])
          }
          eta <- eta + term$coefficient *
            (current_x[j] - term$original_x[row])
        }
      }
      p_raw <- stats::plogis(eta)
      p <- pmin(pmax(p_raw, registry$clip_epsilon),
                1 - registry$clip_epsilon)
      pred[row] <- p
      nll <- nll - (y[row] * log(p) + (1 - y[row]) * log(1 - p))
      error <- y[row] - p

      if (length(registry$beta_terms) > 0L) {
        for (j in seq_along(registry$beta_terms)) {
          term <- registry$beta_terms[[j]]
          x <- term$x[row]
          beta_state[j] <- beta_state[j] +
            alpha[term$alpha_name] * x * error / (x^2 + registry$denom_epsilon)
        }
      }
      if (length(registry$nonlinear_terms) > 0L) {
        for (j in seq_along(registry$nonlinear_terms)) {
          term <- registry$nonlinear_terms[[j]]
          for (slot in term$slots) {
            alpha_name <- paste0("nonlinear:", term$feature, "|",
                                 term$component, "|", slot)
            nonlinear_state[[j]][[slot]] <- pmin(
              pmax(nonlinear_state[[j]][[slot]] - alpha[alpha_name] * error,
                   registry$nonlinear_lower),
              registry$nonlinear_upper)
          }
        }
      }
    }

    state <- data.frame(subject = registry$subject[s],
                        stringsAsFactors = FALSE)
    if (length(beta_state) > 0L) {
      for (j in seq_along(beta_state)) {
        state[[paste0("final_beta:", names(beta_state)[j])]] <- beta_state[j]
      }
    }
    if (length(registry$nonlinear_terms) > 0L) {
      for (j in seq_along(registry$nonlinear_terms)) {
        term <- registry$nonlinear_terms[[j]]
        for (slot in term$slots) {
          state[[paste0("final_nonlinear:", term$feature, "|",
                        term$component, "|", slot)]] <-
            nonlinear_state[[j]][[slot]]
        }
      }
    }
    final_states[[s]] <- state
  }

  if (!isTRUE(return_details)) {
    return(nll)
  }
  list(
    nll = nll,
    loglike = -nll,
    pred = pred,
    final_states = do.call(rbind, final_states)
  )
}

lkt_online_adaptive_optimize_general <- function(registry,
                                                 control = list(maxit = 100),
                                                 require_native = FALSE) {
  if (all(registry$alpha_lower == registry$alpha_upper)) {
    fixed_alpha <- registry$alpha_lower
    names(fixed_alpha) <- registry$alpha_names
    details <- lkt_online_adaptive_eval_dispatch(
      fixed_alpha, registry, return_details = TRUE,
      require_native = require_native)
    null_loglike <- registry$base_loglike / (1 - registry$base_r2)
    r2 <- 1 - details$loglike / null_loglike
    return(list(
      par = fixed_alpha,
      alpha = fixed_alpha,
      loglike = details$loglike,
      delta_loglike = details$loglike - registry$base_loglike,
      r2 = r2,
      delta_r2 = r2 - registry$base_r2,
      optimizer = list(
        par = fixed_alpha,
        value = details$nll,
        counts = stats::setNames(c(1L, NA_integer_),
                                 c("function", "gradient")),
        convergence = 0L,
        message = "Fixed alpha bounds; optimization skipped."
      ),
      details = details
    ))
  }
  objective <- function(alpha) {
    as.numeric(lkt_online_adaptive_eval_dispatch(
      alpha, registry, return_details = FALSE,
      require_native = require_native))
  }
  optim_result <- stats::optim(
    par = as.numeric(registry$alpha_start),
    fn = objective,
    gr = NULL,
    method = "L-BFGS-B",
    lower = registry$alpha_lower,
    upper = registry$alpha_upper,
    control = control
  )
  names(optim_result$par) <- registry$alpha_names
  details <- lkt_online_adaptive_eval_dispatch(
    optim_result$par, registry, return_details = TRUE,
    require_native = require_native)
  null_loglike <- registry$base_loglike / (1 - registry$base_r2)
  r2 <- 1 - details$loglike / null_loglike
  list(
    par = optim_result$par,
    alpha = optim_result$par,
    loglike = details$loglike,
    delta_loglike = details$loglike - registry$base_loglike,
    r2 = r2,
    delta_r2 = r2 - registry$base_r2,
    optimizer = optim_result,
    details = details
  )
}

lkt_online_adaptive_is_specialized_alpha <- function(beta_terms,
                                                     nonlinear_terms) {
  identical(sort(beta_terms), sort(c("recencyKC..Default.",
                                     "logsucKC..Default."))) &&
    length(nonlinear_terms) == 0L
}

lkt_online_adaptive_is_specialized_decay <- function(beta_terms,
                                                     nonlinear_terms) {
  identical(nonlinear_terms, "recency|KC..Default.|para")
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Define the online adaptive LKT model
#'
#' @description Creates an LKT model adapter that first fits the standard
#' LibLinear model, then applies an online parameter-adaptation layer.
#'
#' @param alpha_recency_start Starting value for the recency learning rate.
#' @param alpha_logsuc_start Starting value for the logsuc learning rate.
#' @param maxit Maximum optimizer iterations.
#' @param factr Optimizer `factr` control value.
#' @param require_native Logical; stop if the compiled native evaluator is not
#'   loaded.
#' @param use_gradient Logical; pass the propagated gradient to the optimizer.
#' @return A model adapter suitable for `LKT(model = ...)`.
#' @export
OnlineAdaptiveModel <- function(alpha_recency_start = 0.03,
                                alpha_logsuc_start = 0.05,
                                alpha_decay_start = 0.01,
                                online_mode = "alpha_only",
                                beta_alpha_terms = NULL,
                                nonlinear_alpha_terms = character(0),
                                alpha_start = NULL,
                                alpha_lower = -1,
                                alpha_upper = 1,
                                nonlinear_lower = 1e-5,
                                nonlinear_upper = 0.99999,
                                maxit = 100,
                                factr = 1e7,
                                require_native = FALSE,
                                use_gradient = TRUE) {
  LKTCustomModel(
    name = "OnlineAdaptive",
    fit = function(request, verbose = FALSE) {
      options <- utils::modifyList(
        list(
          alpha_recency_start = alpha_recency_start,
          alpha_logsuc_start = alpha_logsuc_start,
          alpha_decay_start = alpha_decay_start,
          online_mode = online_mode,
          beta_alpha_terms = beta_alpha_terms,
          nonlinear_alpha_terms = nonlinear_alpha_terms,
          alpha_start = alpha_start,
          alpha_lower = alpha_lower,
          alpha_upper = alpha_upper,
          nonlinear_lower = nonlinear_lower,
          nonlinear_upper = nonlinear_upper,
          maxit = maxit,
          factr = factr,
          require_native = require_native,
          use_gradient = use_gradient
        ),
        request$options)
      lkt_fit_fast_online_simple_adaptive(
        design_matrix = request$design_matrix,
        data = request$data,
        response = request$response,
        usefolds = request$usefolds,
        feature_spec = request$feature_spec,
        options = options,
        liblinear_options = request$options,
        verbose = verbose)
    }
  )
}

#' @export
FastOnlineSimpleAdaptiveModel <- OnlineAdaptiveModel

lkt_fit_fast_online_simple_adaptive <- function(design_matrix, data, response,
                                                usefolds, feature_spec = NULL,
                                                options,
                                                liblinear_options,
                                                verbose = FALSE) {
  if (!is.na(usefolds)[1]) {
    stop("OnlineAdaptiveModel currently supports full-data fitting only.",
         call. = FALSE)
  }
  initial_fit <- lkt_fit_liblinear(
    design_matrix = design_matrix,
    data = data,
    usefolds = usefolds,
    bias = liblinear_options$bias,
    cost = liblinear_options$cost,
    epsilon = liblinear_options$epsilon,
    type = liblinear_options$type,
    verbose = verbose)

  null_fit <- stats::logLik(stats::glm(
    stats::as.formula("CF..ansbin.~ 1"),
    data = data,
    family = stats::binomial(link = "logit")
  ))[1]
  initial_r2 <- 1 - initial_fit$fitstat / null_fit
  base_model <- list(
    coefs = initial_fit$modelvs,
    predictors = initial_fit$predictset2,
    pred = initial_fit$pred,
    loglike = initial_fit$fitstat,
    r2 = initial_r2
  )
  recency_decay_start <- NA_real_
  if (is.list(feature_spec) && length(feature_spec) > 0L) {
    for (spec in feature_spec) {
      if (is.list(spec) &&
          identical(spec$feature, "recency") &&
          identical(spec$component, "KC..Default.") &&
          !is.null(spec$pars[["para"]])) {
        recency_decay_start <- as.numeric(spec$pars[["para"]])
        break
      }
    }
  }
  online_mode <- options$online_mode
  if (!online_mode %in% c("six_parameter", "alpha_only", "decay_alpha")) {
    stop("online_mode must be 'six_parameter', 'alpha_only', or 'decay_alpha'.",
         call. = FALSE)
  }
  input <- NULL
  registry <- NULL
  if (identical(online_mode, "six_parameter")) {
    input <- LKTOnlineSimpleAdaptiveInput(
      base_model,
      data,
      recency_decay_start = if (is.finite(recency_decay_start)) {
        recency_decay_start
      } else {
        NULL
      }
    )
    start <- c(
      input$beta_start,
      alpha_recency = options$alpha_recency_start,
      alpha_logsuc = options$alpha_logsuc_start
    )
    optimized <- LKTOptimizeOnlineSimpleAdaptive(
      input,
      start = start,
      control = list(maxit = options$maxit, factr = options$factr),
      require_native = options$require_native,
      use_gradient = options$use_gradient
    )
  } else if (identical(online_mode, "alpha_only")) {
    beta_alpha_terms <- options$beta_alpha_terms
    if (is.null(beta_alpha_terms)) {
      beta_alpha_terms <- intersect(
        c("recencyKC..Default.", "logsucKC..Default."),
        rownames(initial_fit$modelvs))
    }
    nonlinear_alpha_terms <- options$nonlinear_alpha_terms %||% character(0)

    if (lkt_online_adaptive_is_specialized_alpha(beta_alpha_terms,
                                                 nonlinear_alpha_terms)) {
      input <- LKTOnlineSimpleAdaptiveInput(
        base_model,
        data,
        recency_decay_start = if (is.finite(recency_decay_start)) {
          recency_decay_start
        } else {
          NULL
        }
      )
      start <- options$alpha_start %||% c(
        alpha_recency = options$alpha_recency_start,
        alpha_logsuc = options$alpha_logsuc_start)
      start <- lkt_online_adaptive_normalize_vector(
        start, 2L, c("alpha_recency", "alpha_logsuc"), "alpha_start")
      lower <- lkt_online_adaptive_normalize_vector(
        options$alpha_lower, 2L, names(start), "alpha_lower")
      upper <- lkt_online_adaptive_normalize_vector(
        options$alpha_upper, 2L, names(start), "alpha_upper")
      optimized <- LKTOptimizeOnlineSimpleAdaptiveAlpha(
        input,
        start = start,
        lower = lower,
        upper = upper,
        control = list(maxit = options$maxit, factr = options$factr),
        require_native = TRUE,
        use_gradient = options$use_gradient
      )
    } else if (lkt_online_adaptive_is_specialized_decay(
        beta_alpha_terms, nonlinear_alpha_terms)) {
      registry <- lkt_online_adaptive_build_registry(
        modelob = base_model,
        data = data,
        feature_spec = feature_spec,
        beta_alpha_terms = beta_alpha_terms,
        nonlinear_alpha_terms = nonlinear_alpha_terms,
        alpha_start = options$alpha_start,
        alpha_lower = options$alpha_lower,
        alpha_upper = options$alpha_upper,
        nonlinear_lower = options$nonlinear_lower,
        nonlinear_upper = options$nonlinear_upper)
      start <- options$alpha_start %||% registry$alpha_start
      start <- lkt_online_adaptive_normalize_vector(
        start, length(registry$alpha_names), registry$alpha_names,
        "alpha_start")
      lower <- lkt_online_adaptive_normalize_vector(
        options$alpha_lower, length(registry$alpha_names), names(start),
        "alpha_lower")
      upper <- lkt_online_adaptive_normalize_vector(
        options$alpha_upper, length(registry$alpha_names), names(start),
        "alpha_upper")
      registry$alpha_start <- start
      registry$alpha_lower <- lower
      registry$alpha_upper <- upper
      objective <- function(alpha) {
        as.numeric(lkt_online_adaptive_eval_recency_decay_native(
          alpha,
          registry,
          return_details = FALSE,
          require_native = TRUE))
      }
      if (all(lower == upper)) {
        fixed_alpha <- lower
        names(fixed_alpha) <- registry$alpha_names
        details <- lkt_online_adaptive_eval_recency_decay_native(
          fixed_alpha,
          registry,
          return_details = TRUE,
          require_native = TRUE)
        null_loglike <- registry$base_loglike / (1 - registry$base_r2)
        r2 <- 1 - details$loglike / null_loglike
        optimized <- list(
          par = fixed_alpha,
          alpha = fixed_alpha,
          loglike = details$loglike,
          delta_loglike = details$loglike - registry$base_loglike,
          r2 = r2,
          delta_r2 = r2 - registry$base_r2,
          optimizer = list(
            par = fixed_alpha,
            value = details$nll,
            convergence = 0L,
            message = "Fixed alpha bounds; optimization skipped."
          ),
          details = details)
      } else {
        optim_result <- stats::optim(
          par = as.numeric(start),
          fn = objective,
          gr = NULL,
          method = "L-BFGS-B",
          lower = lower,
          upper = upper,
          control = list(maxit = options$maxit, factr = options$factr))
        names(optim_result$par) <- registry$alpha_names
        details <- lkt_online_adaptive_eval_recency_decay_native(
          optim_result$par,
          registry,
          return_details = TRUE,
          require_native = TRUE)
        null_loglike <- registry$base_loglike / (1 - registry$base_r2)
        r2 <- 1 - details$loglike / null_loglike
        optimized <- list(
          par = optim_result$par,
          alpha = optim_result$par,
          loglike = details$loglike,
          delta_loglike = details$loglike - registry$base_loglike,
          r2 = r2,
          delta_r2 = r2 - registry$base_r2,
          optimizer = optim_result,
          details = details)
      }
    } else {
      if (isTRUE(options$require_native)) {
        registry <- lkt_online_adaptive_build_registry(
          modelob = base_model,
          data = data,
          feature_spec = feature_spec,
          beta_alpha_terms = beta_alpha_terms,
          nonlinear_alpha_terms = nonlinear_alpha_terms,
          alpha_start = options$alpha_start,
          alpha_lower = options$alpha_lower,
          alpha_upper = options$alpha_upper,
          nonlinear_lower = options$nonlinear_lower,
          nonlinear_upper = options$nonlinear_upper)
        optimized <- lkt_online_adaptive_optimize_general(
          registry,
          control = list(maxit = options$maxit, factr = options$factr),
          require_native = TRUE)
      } else {
        registry <- lkt_online_adaptive_build_registry(
          modelob = base_model,
          data = data,
          feature_spec = feature_spec,
          beta_alpha_terms = beta_alpha_terms,
          nonlinear_alpha_terms = nonlinear_alpha_terms,
          alpha_start = options$alpha_start,
          alpha_lower = options$alpha_lower,
          alpha_upper = options$alpha_upper,
          nonlinear_lower = options$nonlinear_lower,
          nonlinear_upper = options$nonlinear_upper)
        optimized <- lkt_online_adaptive_optimize_general(
          registry,
          control = list(maxit = options$maxit, factr = options$factr),
          require_native = FALSE)
      }
    }
  } else {
    input <- LKTOnlineSimpleAdaptiveInput(
      base_model,
      data,
      recency_decay_start = if (is.finite(recency_decay_start)) {
        recency_decay_start
      } else {
        NULL
      }
    )
    optimized <- LKTOptimizeOnlineSimpleAdaptiveDecayAlpha(
      input,
      start = c(
        alpha_recency = options$alpha_recency_start,
        alpha_logsuc = options$alpha_logsuc_start,
        alpha_decay = options$alpha_decay_start
      ),
      control = list(maxit = options$maxit, factr = options$factr),
      require_native = options$require_native
    )
  }

  modelvs <- initial_fit$modelvs
  if (all(c("intercept", "recency", "logsuc", "logitdec") %in%
          names(optimized$par))) {
    fitted_beta <- optimized$par[c("intercept", "recency", "logsuc", "logitdec")]
    row_map <- c(
      intercept = "(Intercept)",
      recency = "recencyKC..Default.",
      logsuc = "logsucKC..Default.",
      logitdec = "logitdecAnon.Student.Id"
    )
    for (name in names(row_map)) {
      modelvs[row_map[name], "coefficient"] <- fitted_beta[name]
    }
  }
  adaptive_beta_terms <- if (!is.null(registry)) {
    vapply(registry$beta_terms, `[[`, character(1L), "name")
  } else if (identical(online_mode, "alpha_only")) {
    options$beta_alpha_terms %||% intersect(
      c("recencyKC..Default.", "logsucKC..Default."),
      rownames(initial_fit$modelvs))
  } else {
    c("recencyKC..Default.", "logsucKC..Default.")
  }
  adaptive_nonlinear_terms <- if (!is.null(registry)) {
    unlist(lapply(registry$nonlinear_terms, function(term) {
      paste(term$feature, term$component, term$slots, sep = "|")
    }), use.names = FALSE)
  } else {
    options$nonlinear_alpha_terms %||% character(0)
  }
  final_states <- if (!is.null(optimized$details$final_states)) {
    optimized$details$final_states
  } else if (!is.null(optimized$details$final_betas)) {
    optimized$details$final_betas
  } else {
    NULL
  }

  model <- list(
    name = "OnlineAdaptive",
    par = optimized$par,
    online_mode = online_mode,
    alpha = optimized$alpha,
    alpha_recency = optimized$par["alpha_recency"],
    alpha_logsuc = optimized$par["alpha_logsuc"],
    adaptive_beta_terms = adaptive_beta_terms,
    adaptive_nonlinear_terms = adaptive_nonlinear_terms,
    initial_model = initial_fit$model,
    initial_modelvs = initial_fit$modelvs,
    input = input,
    registry = registry,
    optimizer = optimized$optimizer,
    final_betas = if (!is.null(optimized$details$final_betas)) {
      optimized$details$final_betas
    } else {
      NULL
    },
    final_states = final_states,
    native_available = if (!is.null(registry)) {
      registry$native_available
    } else if (!is.null(input)) {
      input$native_available
    } else {
      FALSE
    }
  )

  if (verbose) {
    cat("OnlineAdaptive loglike:", optimized$loglike, "\n")
    cat("OnlineAdaptive delta loglike:", optimized$delta_loglike, "\n")
    cat("OnlineAdaptive parameters:\n")
    print(optimized$par)
  }

  LKTModelFit(
    model = model,
    pred = optimized$details$pred,
    modelvs = modelvs,
    fitstat = optimized$loglike,
    predictset = design_matrix$sparse,
    predictset2 = design_matrix$csr
  )
}
