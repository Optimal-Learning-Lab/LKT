#' Define the online calibration LKT model
#'
#' @description Creates an LKT model adapter that first solves global feature
#' weights with the standard static LKT/LibLinear model, then uses those weights
#' as the starting beta vector for every subject. Within each subject, the full
#' beta vector is updated online after each observed response:
#' `beta[j, t + 1] = beta[j, t] + alpha * x[j, t] * (y[t] - p[t]) /
#' (x[j, t]^2 + denom_epsilon)` for each online-updated coefficient `j`. With
#' `alpha = 0`, this model reproduces the static global-beta predictions.
#'
#' @param alpha_start Initial learning-rate value.
#' @param alpha_lower Lower bound for the learning rate.
#' @param alpha_upper Upper bound for the learning rate.
#' @param beta_start Initial value for feature weights. Use `"liblinear"` to
#'   initialize from the standard static LKT fit. Numeric scalars are recycled.
#' @param beta_lower Lower bound for feature weights. A scalar is recycled.
#' @param beta_upper Upper bound for feature weights. A scalar is recycled.
#' @param optimize_alpha Logical; optimize the learning rate when `TRUE`.
#' @param optimize_beta Reserved for future joint optimization of the initial
#'   beta vector. The implemented model solves the initial beta vector globally
#'   before optimizing `alpha`.
#' @param clip_epsilon Probability clipping value used before log likelihoods.
#' @param denom_epsilon Small denominator stabilizer for online updates.
#' @param subject_col Subject identifier column used for within-subject updates.
#' @param fixed_online_coefficients Coefficients that remain fixed during
#'   within-subject online updates. By default, the global intercept is kept
#'   fixed when present.
#' @param update_strategy Online update rule. `"all"` updates every eligible
#'   active coefficient on the trial. `"best_loss"` tries each eligible
#'   one-coefficient update and applies only the update that most reduces that
#'   trial's immediate logistic loss.
#' @param maxit Maximum optimizer iterations.
#' @param factr Optimizer `factr` control value.
#' @return A model adapter suitable for `LKT(model = ...)`.
#' @examples
#' adapter <- OnlineCalibrationModel(maxit = 5)
#' adapter$name
#' @export
OnlineCalibrationModel <- function(alpha_start = 0,
                                   alpha_lower = 0,
                                   alpha_upper = 10,
                                   beta_start = "liblinear",
                                   beta_lower = -Inf,
                                   beta_upper = Inf,
                                   optimize_alpha = TRUE,
                                   optimize_beta = FALSE,
                                   clip_epsilon = 1e-6,
                                   denom_epsilon = clip_epsilon,
                                   subject_col = "Anon.Student.Id",
                                   fixed_online_coefficients = c("(Intercept)",
                                                                 "intercept"),
                                   update_strategy = c("all", "best_loss"),
                                   maxit = 100,
                                   factr = 1e7) {
  update_strategy <- match.arg(update_strategy)
  LKTCustomModel(
    name = "OnlineCalibration",
    fit = function(request, verbose = FALSE) {
      options <- utils::modifyList(
        list(
          alpha_start = alpha_start,
          alpha_lower = alpha_lower,
          alpha_upper = alpha_upper,
          beta_start = beta_start,
          beta_lower = beta_lower,
          beta_upper = beta_upper,
          optimize_alpha = optimize_alpha,
          optimize_beta = optimize_beta,
          clip_epsilon = clip_epsilon,
          denom_epsilon = denom_epsilon,
          subject_col = subject_col,
          fixed_online_coefficients = fixed_online_coefficients,
          update_strategy = update_strategy,
          maxit = maxit,
          factr = factr
        ),
        request$options)
      lkt_fit_online_calibration(
        design_matrix = request$design_matrix,
        data = request$data,
        response = request$response,
        usefolds = request$usefolds,
        options = options,
        verbose = verbose)
    }
  )
}

lkt_recycle_numeric_option <- function(value, n, name, allow_infinite = FALSE) {
  valid_values <- if (allow_infinite) {
    !is.na(value) & !is.nan(value)
  } else {
    is.finite(value)
  }
  if (!is.numeric(value) || length(value) < 1L || any(!valid_values)) {
    qualifier <- if (allow_infinite) "numeric and not NA/NaN" else "finite numeric"
    stop(name, " must be ", qualifier, ".", call. = FALSE)
  }
  if (length(value) == 1L) {
    rep(value, n)
  } else if (length(value) == n) {
    as.numeric(value)
  } else {
    stop(name, " must have length 1 or ", n, ".", call. = FALSE)
  }
}

lkt_online_liblinear_initial_fit <- function(design_matrix, data, usefolds,
                                             options, verbose = FALSE) {
  lkt_fit_liblinear(
    design_matrix = design_matrix,
    data = data,
    usefolds = usefolds,
    bias = options$bias,
    cost = options$cost,
    epsilon = options$epsilon,
    type = options$type,
    verbose = verbose)
}

lkt_online_initial_beta <- function(beta_start, n_coef, design_matrix, data,
                                    usefolds, options, verbose = FALSE) {
  if (is.character(beta_start) && length(beta_start) == 1L) {
    if (!identical(beta_start, "liblinear")) {
      stop("beta_start must be numeric or \"liblinear\".", call. = FALSE)
    }
    initial_fit <- lkt_online_liblinear_initial_fit(
      design_matrix = design_matrix,
      data = data,
      usefolds = usefolds,
      options = options,
      verbose = verbose)
    beta <- as.numeric(initial_fit$modelvs[, "coefficient"])
    names(beta) <- rownames(initial_fit$modelvs)
    return(list(beta = beta, model_fit = initial_fit))
  }

  list(
    beta = lkt_recycle_numeric_option(beta_start, n_coef, "beta_start"),
    model_fit = NULL)
}

lkt_online_probability_bounds <- function(x, eps) {
  pmin(pmax(x, eps), 1 - eps)
}

lkt_online_subject_indices <- function(subjects) {
  split(seq_along(subjects), subjects, drop = TRUE)
}

lkt_sparse_row_entries <- function(sparse_matrix) {
  entries <- Matrix::summary(sparse_matrix)
  rows <- vector("list", nrow(sparse_matrix))
  if (nrow(entries) == 0L) {
    return(lapply(rows, function(x) {
      list(index = integer(0), value = numeric(0))
    }))
  }
  split_entries <- split(entries, entries$i, drop = TRUE)
  for (row_name in names(split_entries)) {
    row_entries <- split_entries[[row_name]]
    rows[[as.integer(row_name)]] <- list(
      index = as.integer(row_entries$j),
      value = as.numeric(row_entries$x)
    )
  }
  for (i in seq_along(rows)) {
    if (is.null(rows[[i]])) {
      rows[[i]] <- list(index = integer(0), value = numeric(0))
    }
  }
  rows
}

lkt_online_update_entries <- function(entries, online_update_mask) {
  rows <- vector("list", length(entries))
  for (i in seq_along(entries)) {
    row_entries <- entries[[i]]
    keep <- online_update_mask[row_entries$index]
    rows[[i]] <- list(
      index = row_entries$index[keep],
      value = row_entries$value[keep]
    )
  }
  rows
}

lkt_online_row_loss <- function(response, probability) {
  if (response == 1) {
    -log(probability)
  } else {
    -log(1 - probability)
  }
}

lkt_online_best_loss_choice <- function(eta, y, update_values, alpha,
                                        denom_epsilon, clip_epsilon) {
  denom <- update_values^2 + denom_epsilon
  candidate_delta <- alpha * update_values^2 * (y - stats::plogis(eta)) / denom
  candidate_p <- lkt_online_probability_bounds(
    stats::plogis(eta + candidate_delta),
    clip_epsilon
  )
  which.min(lkt_online_row_loss(y, candidate_p))
}

lkt_online_beta_update_pass <- function(beta_start, row_entries, response,
                                        subject_indices, update_mask,
                                        clip_epsilon, denom_epsilon, alpha,
                                        update_strategy,
                                        store_subject_betas = FALSE) {
  pred <- numeric(length(response))
  final_state <- data.frame(
    subject = names(subject_indices),
    updates = rep(0L, length(subject_indices)),
    stringsAsFactors = FALSE
  )
  final_betas <- if (isTRUE(store_subject_betas)) {
    matrix(NA_real_, nrow = length(subject_indices), ncol = length(beta_start),
           dimnames = list(names(subject_indices), names(beta_start)))
  } else {
    NULL
  }

  for (s in seq_along(subject_indices)) {
    idx <- subject_indices[[s]]
    beta <- beta_start
    for (row in idx) {
      entries <- row_entries[[row]]
      eta <- if (length(entries$index) > 0L) {
        sum(beta[entries$index] * entries$value)
      } else {
        0
      }
      p <- lkt_online_probability_bounds(stats::plogis(eta), clip_epsilon)
      pred[row] <- p
      if (update_mask[row]) {
        update_entries <- entries$update
        err <- response[row] - p
        denom <- update_entries$value^2 + denom_epsilon
        if (length(update_entries$index) > 0L && all(denom > 0)) {
          chosen <- if (identical(update_strategy, "best_loss")) {
            lkt_online_best_loss_choice(
              eta = eta,
              y = response[row],
              update_values = update_entries$value,
              alpha = alpha,
              denom_epsilon = denom_epsilon,
              clip_epsilon = clip_epsilon
            )
          } else {
            seq_along(update_entries$index)
          }
          beta[update_entries$index[chosen]] <- beta[update_entries$index[chosen]] +
            alpha * update_entries$value[chosen] * err / denom[chosen]
        }
        final_state$updates[s] <- final_state$updates[s] + 1L
      }
    }
    if (isTRUE(store_subject_betas)) {
      final_betas[s, ] <- beta
    }
  }

  list(pred = pred, final_state = final_state, final_betas = final_betas)
}

lkt_online_objective_gradient <- function(par, unpack, predictset, row_entries,
                                          response,
                                          subject_indices, fit_mask,
                                          update_mask, clip_epsilon,
                                          denom_epsilon, optimize_alpha,
                                          update_strategy) {
  current <- unpack(par)
  n_par <- length(par)
  value <- 0
  gradient <- numeric(n_par)
  alpha_index <- if (isTRUE(optimize_alpha)) n_par else NA_integer_

  for (idx in subject_indices) {
    beta <- current$beta
    dbeta_dalpha <- numeric(length(current$beta))
    for (row in idx) {
      entries <- row_entries[[row]]
      eta <- if (length(entries$index) > 0L) {
        sum(beta[entries$index] * entries$value)
      } else {
        0
      }
      raw_p <- stats::plogis(eta)
      p <- lkt_online_probability_bounds(raw_p, clip_epsilon)
      deta_dalpha <- if (length(entries$index) > 0L) {
        sum(dbeta_dalpha[entries$index] * entries$value)
      } else {
        0
      }
      dp_dalpha <- if (raw_p > clip_epsilon && raw_p < 1 - clip_epsilon) {
        raw_p * (1 - raw_p) * deta_dalpha
      } else {
        0
      }

      if (fit_mask[row]) {
        value <- value - log(ifelse(response[row] == 1, p, 1 - p))
        if (isTRUE(optimize_alpha)) {
          gradient[alpha_index] <- gradient[alpha_index] +
            (p - response[row]) * deta_dalpha
        }
      }

      if (update_mask[row]) {
        update_entries <- entries$update
        err <- response[row] - p
        denom <- update_entries$value^2 + denom_epsilon
        if (length(update_entries$index) > 0L && all(denom > 0)) {
          chosen <- if (identical(update_strategy, "best_loss")) {
            lkt_online_best_loss_choice(
              eta = eta,
              y = response[row],
              update_values = update_entries$value,
              alpha = current$alpha,
              denom_epsilon = denom_epsilon,
              clip_epsilon = clip_epsilon
            )
          } else {
            seq_along(update_entries$index)
          }
          if (isTRUE(optimize_alpha)) {
            dbeta_dalpha[update_entries$index[chosen]] <-
              dbeta_dalpha[update_entries$index[chosen]] +
              update_entries$value[chosen] *
                (err - current$alpha * dp_dalpha) / denom[chosen]
          }
          beta[update_entries$index[chosen]] <-
            beta[update_entries$index[chosen]] +
            current$alpha * update_entries$value[chosen] * err / denom[chosen]
        }
      }
    }
  }

  attr(value, "gradient") <- gradient
  value
}

lkt_fit_online_calibration <- function(design_matrix, data, response, usefolds,
                                       options, verbose = FALSE) {
  predictset <- design_matrix$sparse
  predictset2 <- design_matrix$csr
  n_coef <- ncol(predictset)
  n_obs <- nrow(predictset)

  if (!options$subject_col %in% names(data)) {
    stop("OnlineCalibrationModel requires subject column '",
         options$subject_col, "'.", call. = FALSE)
  }
  if (length(response) != n_obs) {
    stop("response length must match design-matrix row count.", call. = FALSE)
  }

  fit_mask <- if (is.na(usefolds)[1]) {
    rep(TRUE, n_obs)
  } else {
    data$fold %in% usefolds
  }
  if (!any(fit_mask)) {
    stop("OnlineCalibrationModel has no rows selected for fitting.",
         call. = FALSE)
  }
  if (isTRUE(options$optimize_beta)) {
    stop("OnlineCalibrationModel solves the global beta vector first and ",
         "currently optimizes only alpha through the online per-subject ",
         "updates. Set optimize_beta = FALSE.", call. = FALSE)
  }
  update_mask <- fit_mask
  subject_indices <- lkt_online_subject_indices(data[[options$subject_col]])
  row_entries <- lkt_sparse_row_entries(predictset)
  online_update_mask <- !colnames(predictset) %in%
    options$fixed_online_coefficients
  update_entries <- lkt_online_update_entries(row_entries, online_update_mask)
  row_entries <- Map(function(full, update) {
    full$update <- update
    full
  }, row_entries, update_entries)

  initial_beta <- lkt_online_initial_beta(
    beta_start = options$beta_start,
    n_coef = n_coef,
    design_matrix = design_matrix,
    data = data,
    usefolds = usefolds,
    options = options,
    verbose = verbose)
  beta_start <- initial_beta$beta
  names(beta_start) <- colnames(predictset)
  beta_lower <- lkt_recycle_numeric_option(options$beta_lower, n_coef,
                                           "beta_lower",
                                           allow_infinite = TRUE)
  beta_upper <- lkt_recycle_numeric_option(options$beta_upper, n_coef,
                                           "beta_upper",
                                           allow_infinite = TRUE)
  if (any(beta_lower > beta_upper)) {
    stop("beta_lower must be less than or equal to beta_upper.",
         call. = FALSE)
  }
  if (!is.finite(options$alpha_start) ||
      !is.finite(options$alpha_lower) ||
      !is.finite(options$alpha_upper) ||
      options$alpha_lower > options$alpha_upper) {
    stop("alpha_start, alpha_lower, and alpha_upper must be finite bounds.",
         call. = FALSE)
  }
  if (options$alpha_start < options$alpha_lower ||
      options$alpha_start > options$alpha_upper) {
    stop("alpha_start must be inside alpha_lower and alpha_upper.",
         call. = FALSE)
  }
  if (options$clip_epsilon <= 0 || options$clip_epsilon >= 0.5) {
    stop("clip_epsilon must be greater than 0 and less than 0.5.",
         call. = FALSE)
  }
  if (options$denom_epsilon < 0) {
    stop("denom_epsilon must be non-negative.", call. = FALSE)
  }
  if (!identical(options$update_strategy, "all") &&
      !identical(options$update_strategy, "best_loss")) {
    stop("update_strategy must be \"all\" or \"best_loss\".", call. = FALSE)
  }

  pack <- function(alpha) {
    c(if (isTRUE(options$optimize_alpha)) alpha else numeric(0))
  }
  unpack <- function(par) {
    beta <- beta_start
    alpha <- options$alpha_start
    if (isTRUE(options$optimize_alpha)) {
      alpha <- par[1L]
    }
    list(beta = beta, alpha = alpha)
  }

  start <- pack(options$alpha_start)
  lower <- pack(options$alpha_lower)
  upper <- pack(options$alpha_upper)
  if (any(start < lower | start > upper)) {
    stop("OnlineCalibrationModel initial parameters are outside optimizer bounds.",
         call. = FALSE)
  }

  objective <- function(par) {
    lkt_online_objective_gradient(
      par = par,
      unpack = unpack,
      predictset = predictset,
      row_entries = row_entries,
      response = response,
      subject_indices = subject_indices,
      fit_mask = fit_mask,
      update_mask = update_mask,
      clip_epsilon = options$clip_epsilon,
      denom_epsilon = options$denom_epsilon,
      optimize_alpha = options$optimize_alpha,
      update_strategy = options$update_strategy)
  }
  gradient <- function(par) {
    attr(objective(par), "gradient")
  }

  baseline_par <- if (isTRUE(options$optimize_alpha) &&
                      options$alpha_lower <= 0 &&
                      options$alpha_upper >= 0) {
    pack(0)
  } else {
    start
  }
  baseline_value <- objective(baseline_par)

  if (length(start) > 0L) {
    optim_result <- stats::optim(
      par = start,
      fn = objective,
      gr = gradient,
      method = "L-BFGS-B",
      lower = lower,
      upper = upper,
      control = list(maxit = options$maxit, factr = options$factr))
    best_par <- if (optim_result$value <= baseline_value) {
      optim_result$par
    } else {
      baseline_par
    }
    fitted <- unpack(best_par)
  } else {
    optim_result <- NULL
    fitted <- unpack(numeric(0))
  }

  final_pass <- lkt_online_beta_update_pass(
    beta_start = fitted$beta,
    row_entries = row_entries,
    response = response,
    subject_indices = subject_indices,
    update_mask = update_mask,
    clip_epsilon = options$clip_epsilon,
    denom_epsilon = options$denom_epsilon,
    alpha = fitted$alpha,
    update_strategy = options$update_strategy)
  pred <- final_pass$pred
  fitstat <- sum(log(ifelse(response[fit_mask] == 1,
                            pred[fit_mask],
                            1 - pred[fit_mask])))

  modelvs <- matrix(fitted$beta, ncol = 1,
                    dimnames = list(colnames(predictset), "coefficient"))
  model <- list(
    name = "OnlineCalibration",
    beta = fitted$beta,
    alpha = fitted$alpha,
    clip_epsilon = options$clip_epsilon,
    denom_epsilon = options$denom_epsilon,
    subject_col = options$subject_col,
    fixed_online_coefficients = options$fixed_online_coefficients,
    update_strategy = options$update_strategy,
    online_update_coefficients = colnames(predictset)[online_update_mask],
    initial_model = if (is.null(initial_beta$model_fit)) {
      NULL
    } else {
      initial_beta$model_fit$model
    },
    baseline_value = baseline_value,
    final_state = final_pass$final_state,
    optimizer = optim_result)

  if (verbose) {
    cat("OnlineCalibration alpha:", fitted$alpha, "\n")
    if (!is.null(optim_result)) {
      cat("OnlineCalibration convergence:", optim_result$convergence, "\n")
    }
  }

  LKTModelFit(
    model = model,
    pred = pred,
    modelvs = modelvs,
    fitstat = fitstat,
    predictset = predictset,
    predictset2 = predictset2)
}
