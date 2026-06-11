# Latency model helpers for LKT accuracy models.
# These functions are sourced before LKTfunctions.R by working-copy examples
# and collated before LKTfunctions.R during package installation.


#' Fit a Weibull response-time model with activation effects
#'
#' @description Internal helper that fits a response-time distribution with a Weibull model whose scale parameter depends on LKT success probability. Uses a hybrid variance approach: subject effects are estimated once and scaled, and trial-level noise is estimated post hoc.
#'
#' Model structure: RT = t0 + Weibull(k, lambda) log(lambda) = log(lambda0) - beta1 * qlogis(p) + scale_subject * u_s_fixed + eps_trial where u_s_fixed are computed once before optimization, then scaled during fitting
#'
#' The function minimizes a weighted combination of: 1. Quantile MSE: matches overall RT distribution shape 2. Point-wise MSE: matches individual RT predictions (if mse_weight > 0)
#'
#' Hybrid variance approach prevents parameter instability: - subject effects are fixed up to a learned scale factor - sigma_trial: estimated post-hoc from log-scale residuals
#'
#' @param rt Numeric vector of response times (correct trials only, in seconds)
#' @param p Numeric vector of success probabilities (from LKT accuracy model)
#' @param subject_id Factor/character vector of subject identifiers (optional)
#' @param sigma_trial_init Numeric, initial within-subject SD (used as fixed trial noise; not optimized)
#' @param eps Numeric, small value to prevent log(0) errors (default 1e-6)
#' @param n_quantiles Integer, number of quantiles for distribution matching (default 20)
#' @param n_sim Integer, deprecated parameter kept for compatibility (default 1)
#' @param mse_weight Numeric in the range 0 to 1, weight for point-wise MSE vs quantile matching. 0 = pure quantile matching (default), 1 = pure MSE minimization. Higher weight = better point predictions, lower weight = better distribution shape
#' @param t0_fixed Numeric, fixed minimum RT in seconds (if NULL, will be optimized)
#'
#' @return A list containing:
#'   \item{k}{Weibull shape parameter}
#'   \item{lambda0}{Base scale parameter}
#'   \item{beta1}{Activation effect (negative = faster RT with higher activation)}
#'   \item{scale_subject}{Scaling factor for subject effects (adjusts magnitude of u_s_fixed)}
#'   \item{sigma_trial}{Post-hoc trial noise SD}
#'   \item{t0}{Minimum RT}
#'   \item{subject_effects}{Named vector of subject-specific deviations}
#'   \item{var_partition}{List with activation/subject/trial variance proportions}
#'   \item{predict}{Function(p, subject_effect, add_noise, truncate) for RT prediction}
#'   \item{sse}{Combined loss value}
#'   \item{r_squared}{R-squared fit quality metric}
#'   \item{conv}{Convergence code from optim (0 = success)}
#'   \item{n_obs}{Number of observations used}
#'   \item{n_subjects}{Number of unique subjects}
#'   \item{max_rt}{Maximum observed RT}
#' @keywords internal
fit_weibull_activation <- function(rt, p, subject_id = NULL,
                                   sigma_trial_init = NULL,  # Fixed trial noise (not optimized)
                                   eps = 1e-6,
                                   n_quantiles = 20, n_sim = 1, mse_weight = 0, t0_fixed = NULL) {

  ##----------------------------------------------------------------------------
  ## 1. DATA PREPARATION
  ##----------------------------------------------------------------------------
  # Clean data
  ok <- is.finite(rt) & is.finite(p) & rt > 0 & p > eps & p < (1 - eps)
  rt <- rt[ok]; p <- p[ok]
  if (length(rt) == 0L) {
    stop("fit_weibull_activation has no usable observations after filtering.",
         call. = FALSE)
  }

  if (!is.null(subject_id)) {
    subject_id <- subject_id[ok]
  }

  # Compute activation from probability
  A <- stats::qlogis(p)

  # Get unique subjects and create mapping
  if (!is.null(subject_id) && length(unique(subject_id)) > 1) {
    unique_subjects <- unique(subject_id)
    n_subjects <- length(unique_subjects)
    # Map subject IDs to indices
    subject_idx <- match(subject_id, unique_subjects)
  } else {
    n_subjects <- 1
    subject_idx <- rep(1, length(rt))
  }

  # Estimate subject effects ONCE before optimization
  # Subject effect = mean log(lambda) for each subject
  # These will be FIXED throughout optimization
  u_s_fixed <- numeric(n_subjects)
  if (n_subjects > 1) {
    t0_init <- if (!is.null(t0_fixed)) t0_fixed else min(rt)

    for (s in 1:n_subjects) {
      idx_s <- which(subject_idx == s)
      if (length(idx_s) > 0) {
        rt_s <- rt[idx_s]
        rt_adj <- pmax(rt_s - t0_init, eps)
        u_s_fixed[s] <- mean(log(rt_adj), na.rm = TRUE)
      }
    }
    u_s_fixed <- u_s_fixed - mean(u_s_fixed, na.rm = TRUE)
  }

  # Prefit sigma_trial from residual SD if not provided
  if (is.null(sigma_trial_init)) {
    t0_prefit <- if (!is.null(t0_fixed)) t0_fixed else min(rt)
    rt_adj_prefit <- pmax(rt - t0_prefit, eps)
    log_rt_adj_prefit <- log(rt_adj_prefit)
    if (n_subjects > 1) {
      prefit_df <- data.frame(log_rt_adj = log_rt_adj_prefit, A = A, subj = factor(subject_idx))
      prefit_fit <- stats::lm(log_rt_adj ~ A + subj, data = prefit_df)
    } else {
      prefit_df <- data.frame(log_rt_adj = log_rt_adj_prefit, A = A)
      prefit_fit <- stats::lm(log_rt_adj ~ A, data = prefit_df)
    }
    sigma_trial_init <- stats::sd(prefit_fit$residuals, na.rm = TRUE)
  }

  # Quantile matching setup (fixed draws for deterministic objective)
  prob_seq <- seq(0.05, 0.95, length.out = n_quantiles)
  obs_quantiles <- stats::quantile(rt, probs = prob_seq, na.rm = TRUE)
  u_fixed <- stats::runif(length(rt))
  z_trial_fixed <- stats::rnorm(length(rt))
  sigma_trial_fixed <- sigma_trial_init

  ##----------------------------------------------------------------------------
  ## 2. OBJECTIVE FUNCTION: Quantile Matching + Optional RT Prediction
  ##----------------------------------------------------------------------------
  # Fits Weibull parameters to minimize:
  #   - Quantile MSE: Match overall RT distribution shape
  #   - Point-wise MSE: Match individual trial predictions (optional, weighted)
  #
  # Hybrid variance approach:
  #   sigma_trial: estimated post-hoc (not optimized)
  objective <- function(par) {
    # Extract parameters
    k <- exp(par[1])
    lambda0 <- exp(par[2])
    beta1 <- par[3]
    scale_subject <- exp(par[4])  # NEW: scales the fixed subject effects
    t0 <- if (!is.null(t0_fixed)) t0_fixed else exp(par[5])

    # NOTE: u_s_fixed is from closure (estimated once before optimization)
    # NOTE: scale_subject is OPTIMIZED to adjust magnitude of u_s_fixed

    # 1. Construct per-trial scale using activation + scaled subject effects + fixed trial noise
    subj_effects <- scale_subject * u_s_fixed[subject_idx]
    log_lambda <- log(lambda0) - beta1 * A + subj_effects + sigma_trial_fixed * z_trial_fixed
    lambda_vec <- exp(log_lambda)

    # 2. Quantile matching component
    rt_sim <- t0 + stats::qweibull(u_fixed, shape = k, scale = lambda_vec)
    sim_quantiles <- stats::quantile(rt_sim, probs = prob_seq, na.rm = TRUE)
    quantile_mse <- mean((obs_quantiles - sim_quantiles)^2)

    # 3. Point-wise MSE component (if mse_weight > 0)
    if (mse_weight > 0) {
      # Compute deterministic predictions (marginal mean incl. trial noise)
      noise_bias <- exp(0.5 * sigma_trial_fixed^2)
      pred_rt <- numeric(length(rt))
      for (i in seq_along(rt)) {
        # Expected lambda at mean subject effect (0) + mean trial effect (0)

        subj_effect <- scale_subject * u_s_fixed[subject_idx[i]]
        lambda_i <- lambda0 * exp(-beta1 * A[i] + subj_effect)
        pred_rt[i] <- t0 + lambda_i * gamma(1 + 1/k) * noise_bias

      }
      rt_mse <- mean((log(rt) - log(pred_rt))^2)

      # Weighted combination (quantile MSE in seconds^2, RT MSE normalized by variance)
      combined_loss <- (1 - mse_weight) * quantile_mse + mse_weight * rt_mse
      return(combined_loss)
    } else {
      # Pure quantile matching (default behavior)
      return(quantile_mse)
    }
  }

  ##----------------------------------------------------------------------------
  ## 3. INITIAL PARAMETER VALUES
  ##----------------------------------------------------------------------------
  # Parameters to optimize:
  #   - k (Weibull shape, log-scale)
  #   - lambda0 (base scale, log-scale)
  #   - beta1 (activation weight)
  #   - scale_subject (scaling factor for u_s_fixed, log-scale) - NEW
  #   - t0 (minimum RT, log-scale if not fixed)
  # Note: sigma_trial is fixed for quantile matching; post-hoc sigma is reported

  if (!is.null(t0_fixed)) {
    # Fixed t0: optimize 4 parameters
    rt_adjusted <- stats::median(rt) - t0_fixed
    rt_adjusted <- max(rt_adjusted, 0.0005)  # Prevent negative/zero values
    init <- c(
      log(1.5),                      # k (shape)
      log(rt_adjusted),              # lambda0
      0.5,                           # beta1
      log(1.0)                       # scale_subject (neutral start)
    )
  } else {
    # Optimize 5 parameters including t0
    t0_init <- min(rt)
    rt_adjusted <- stats::median(rt) - t0_init
    rt_adjusted <- max(rt_adjusted, 0.0005)
    init <- c(
      log(1.5),                      # k (shape)
      log(rt_adjusted),    # lambda0
      0.5,                           # beta1
      log(1.0),                      # scale_subject (neutral start)
      log(1.0)                       # t0
    )
  }

  ##----------------------------------------------------------------------------
  ## 4. RUN OPTIMIZATION (TWO-PASS WITH UPDATED SUBJECT EFFECTS)
  ##----------------------------------------------------------------------------
  run_optim <- function(init_par) {
    if (!is.null(t0_fixed)) {
      cat("Fitting Weibull with FIXED t0 =", round(t0_fixed, 4), "sec")
    }
    if (mse_weight > 0) {
      cat(" | quantile matching + RT prediction (weight:", mse_weight, ")...\n")
    } else {
      cat(" | via quantile matching...\n")
    }
    opt_res <- stats::optim(init_par, objective, method = "Nelder-Mead",
                           control = list(maxit = 500, reltol = 1e-6))
    cat("  Optimization complete (Loss:", round(opt_res$value, 4), ")\n")
    opt_res
  }

  opt <- run_optim(init)

    beta1_pass1 <- opt$par[3]
    t0_pass1 <- if (!is.null(t0_fixed)) t0_fixed else exp(opt$par[5])
    rt_adj_all <- pmax(rt - t0_pass1, eps)
    log_rt_adj <- log(rt_adj_all)
    for (s in 1:n_subjects) {
      idx_s <- which(subject_idx == s)
      if (length(idx_s) > 0) {
        u_s_fixed[s] <- mean(log_rt_adj[idx_s] + beta1_pass1 * A[idx_s], na.rm = TRUE)
      }
    }
    u_s_fixed <- u_s_fixed - mean(u_s_fixed, na.rm = TRUE)
    opt <- run_optim(opt$par)


  ##----------------------------------------------------------------------------
  ## 5. REPORT LOSS BREAKDOWN (for weighted objective)
  ##----------------------------------------------------------------------------
  # Re-evaluate objective components with fitted parameters to show
  # quantile vs point-prediction trade-off
  if (mse_weight > 0) {
    # Extract final parameters
    k_final <- exp(opt$par[1])
    lambda0_final <- exp(opt$par[2])
    beta1_final <- opt$par[3]
    scale_subject_final <- exp(opt$par[4])
    t0_final <- if (!is.null(t0_fixed)) t0_fixed else exp(opt$par[5])

    # Use the fixed subject effects with fitted scaling (same as used in optimization)
    subj_effects_final <- scale_subject_final * u_s_fixed[subject_idx]
    log_lambda_final <- log(lambda0_final) - beta1_final * A + subj_effects_final +
                        sigma_trial_fixed * z_trial_fixed
    lambda_vec_final <- exp(log_lambda_final)
    rt_sim_final <- t0_final + stats::qweibull(u_fixed, shape = k_final, scale = lambda_vec_final)
    sim_quantiles_final <- stats::quantile(rt_sim_final, probs = prob_seq, na.rm = TRUE)
    quantile_mse_final <- mean((obs_quantiles - sim_quantiles_final)^2)

    # Compute the same log-scale RT loss used by the optimizer
    noise_bias_final <- exp(0.5 * sigma_trial_fixed^2)
    pred_rt_final <- numeric(length(rt))
    for (i in seq_along(rt)) {
      subj_effect_final <- scale_subject_final * u_s_fixed[subject_idx[i]]
      lambda_i_final <- lambda0_final * exp(-beta1_final * A[i] + subj_effect_final)
      pred_rt_final[i] <- t0_final + lambda_i_final * gamma(1 + 1/k_final) * noise_bias_final
    }

    rt_mse_final <- mean((log(rt) - log(pred_rt_final))^2)

    # Calculate traditional R-squared for reference
    ss_res_final <- sum((rt - pred_rt_final)^2)
    ss_tot_final <- sum((rt - mean(rt))^2)
    r2_actual_final <- 1 - (ss_res_final / ss_tot_final)

    cat("  Loss breakdown:\n")
    cat("    Quantile MSE:", round(quantile_mse_final, 4), "| weight:", round(1 - mse_weight, 4),
        "| contribution:", round((1 - mse_weight) * quantile_mse_final, 4), "\n")
    cat("    Log-RT MSE:", round(rt_mse_final, 4), "| R-squared:", round(r2_actual_final, 4),
        "| weight:", round(mse_weight, 4), "| contribution:", round(mse_weight * rt_mse_final, 4), "\n")
  }

  ##----------------------------------------------------------------------------
  ## 6. EXTRACT PARAMETERS AND ESTIMATE SUBJECT EFFECTS
  ##----------------------------------------------------------------------------
  # Estimate subject effects as residual deviations from population model
  # For subject s: u_s = mean(log(observed_lambda_s) - log(predicted_lambda_s))
  # where lambda = (RT - t0) / Weibull_mean_scale

  k <- exp(opt$par[1])
  lambda0 <- exp(opt$par[2])
  beta1 <- opt$par[3]
  scale_subject <- exp(opt$par[4])  # NEW: scaling factor for subject effects
  t0 <- if (!is.null(t0_fixed)) t0_fixed else exp(opt$par[5])

  # Use the scaled subject effects (u_s_fixed estimated once, then scaled by fitted parameter)
  if (n_subjects > 1) {
    subject_effect_map <- scale_subject * u_s_fixed
    names(subject_effect_map) <- unique_subjects
  } else {
    subject_effect_map <- NULL
  }
  sigma_subject <- if (n_subjects > 1) {
    stats::sd(as.numeric(u_s_fixed), na.rm = TRUE)
  } else {
    0
  }

  # Post-hoc estimate of trial noise from residuals in log(scale) space
  subj_effects_by_trial <- scale_subject * u_s_fixed[subject_idx]
  rt_adj_post <- pmax(rt - t0, eps)
  resid_log_lambda <- log(rt_adj_post) -
    (log(lambda0) - beta1 * A + subj_effects_by_trial)
  sigma_trial <- stats::sd(resid_log_lambda, na.rm = TRUE)

  ##----------------------------------------------------------------------------
  ## 7. VARIANCE PARTITIONING
  ##----------------------------------------------------------------------------
  # Decompose log(lambda) variance into three components:
  #   var(log_lambda) = beta1^2 * var(A) + var(subject_effects) + sigma_trial^2
  # Each component represents:
  #   - Activation: Effect of learning/success probability on RT
  #   - Subject: Stable individual differences (fast vs slow responders)
  #   - Trial: Trial-to-trial noise/randomness

  var_activation <- beta1^2 * stats::var(A, na.rm = TRUE)
  var_subject <- scale_subject^2 * stats::var(u_s_fixed, na.rm = TRUE)  # Actual variance of scaled subject effects
  var_trial <- sigma_trial^2
  var_total <- var_activation + var_subject + var_trial

  prop_activation <- var_activation / var_total
  prop_subject <- var_subject / var_total
  prop_trial <- var_trial / var_total

  # Store max RT for truncation
  max_rt_obs <- max(rt, na.rm = TRUE)

  # Calculate R-squared for the final model (marginal mean predictions with subject effects)
  noise_bias_post <- exp(0.5 * sigma_trial^2)
  pred_rt_final <- numeric(length(rt))
  for (i in seq_along(rt)) {
    # Get estimated subject effect using subject_idx
    if (!is.null(subject_effect_map) && n_subjects > 1) {
      subj_effect <- subject_effect_map[subject_idx[i]]
    } else {
      subj_effect <- 0
    }
    lambda_i_final <- lambda0 * exp(-beta1 * A[i] + subj_effect)
    pred_rt_final[i] <- t0 + lambda_i_final * gamma(1 + 1/k) * noise_bias_post
  }
  ss_res_final <- sum((rt - pred_rt_final)^2)
  ss_tot_final <- sum((rt - mean(rt))^2)
  r_squared_final <- 1 - (ss_res_final / ss_tot_final)

  cat("  Final R-squared (with total subject effects):", round(r_squared_final, 4), "\n")

  ##----------------------------------------------------------------------------
  ## 8. CREATE PREDICTION FUNCTION
  ##----------------------------------------------------------------------------
  # Returns closure that predicts RT from probability p
  # Modes:
  #   - add_noise=FALSE: Deterministic (marginal mean RT given p and subject effect)
  #   - add_noise=TRUE: Stochastic (sample with trial noise, matches real distribution)

  predict_fun <- function(pnew, subject_effect = 0, add_noise = FALSE,
                          truncate = TRUE, return_components = FALSE) {
    pnew <- as.numeric(pnew)
    pnew <- pmax(pmin(pnew, 1 - eps), eps)
    A_new <- stats::qlogis(pnew)
    lambda_base <- lambda0 * exp(-beta1 * A_new)

    if (add_noise) {
      # Vary scale parameter, then sample from Weibull
      # Add subject effect and trial noise to log(scale)
      trial_noise <- stats::rnorm(length(pnew), 0, sigma_trial)
      log_lambda_i <- log(lambda_base) + subject_effect + trial_noise
      lambda_i <- exp(log_lambda_i)

      # Draw from Weibull with noisy scale parameter and add t0.
      weibull_u <- stats::runif(length(pnew))
      rt_sim <- t0 + stats::qweibull(weibull_u, shape = k, scale = lambda_i)

      # Truncate at observed max if requested
      if (truncate) {
        rt_sim <- pmin(rt_sim, max_rt_obs)
      }
      if (return_components) {
        return(list(
          rt = rt_sim,
          trial_noise = trial_noise,
          log_lambda = log_lambda_i,
          lambda = lambda_i,
          weibull_u = weibull_u
        ))
      }
      rt_sim
    } else {
      # Deterministic prediction (marginal mean, with optional subject effect)
      noise_bias <- exp(0.5 * sigma_trial^2)
      rt_det <- t0 + lambda_base * gamma(1 + 1/k) * exp(subject_effect) * noise_bias
      if (return_components) {
        return(list(
          rt = rt_det,
          trial_noise = rep(0, length(pnew)),
          log_lambda = log(lambda_base) + subject_effect,
          lambda = lambda_base * exp(subject_effect),
          weibull_u = rep(NA_real_, length(pnew))
        ))
      }
      rt_det
    }
  }

  list(
    k = k,
    lambda0 = lambda0,
    beta1 = beta1,
    scale_subject = scale_subject,  # NEW: fitted scaling factor for subject effects
    sigma_trial = sigma_trial,
    t0 = t0,
    sse = opt$value,
    r_squared = r_squared_final,
    conv = opt$convergence,
    n_obs = length(rt),
    n_subjects = if (!is.null(subject_id)) n_subjects else NA,
    max_rt = max(rt, na.rm = TRUE),
    sigma_subject = sigma_subject,
    subject_effects = subject_effect_map,
    var_partition = list(
      activation = prop_activation,
      subject = prop_subject,
      trial = prop_trial
    ),
    predict = predict_fun
  )
}

lkt_prepare_latency_data <- function(data) {
  lkt_require_columns(
    data,
    c("CF..ansbin.", "Duration..sec.", "pred", "Anon.Student.Id"),
    "LKT latency model")

  correct_idx <- which(data$CF..ansbin. == 1)
  correct_rt <- data$Duration..sec.[correct_idx]
  correct_pred <- data$pred[correct_idx]
  correct_subject_id <- data$Anon.Student.Id[correct_idx]

  q1 <- stats::quantile(correct_rt, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(correct_rt, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  outlier_idx <- which(correct_rt < lower_bound | correct_rt > upper_bound)

  if (length(outlier_idx) > 0) {
    clean_rt <- correct_rt[-outlier_idx]
    clean_pred <- correct_pred[-outlier_idx]
    clean_subject_id <- correct_subject_id[-outlier_idx]
  } else {
    clean_rt <- correct_rt
    clean_pred <- correct_pred
    clean_subject_id <- correct_subject_id
  }

  list(
    correct_rt = correct_rt,
    correct_pred = correct_pred,
    correct_subject_id = correct_subject_id,
    outlier_idx = outlier_idx,
    clean_rt = clean_rt,
    clean_pred = clean_pred,
    clean_subject_id = clean_subject_id
  )
}

lkt_fit_exp_latency <- function(latency_data, verbose = FALSE) {
  rt_pred_base <- exp(-stats::qlogis(latency_data$clean_pred))
  model <- stats::lm(latency_data$clean_rt ~ as.numeric(rt_pred_base))
  rt_predicted <- stats::predict(model, type = "response")
  fitstat <- stats::cor(latency_data$clean_rt, rt_predicted)^2

  if (verbose) {
    latency_plot_file <- tempfile("lkt-latency-exp-", fileext = ".pdf")
    cat("=== LATENCY MODEL DEBUGGING (EXPONENTIAL) ===\n")
    cat("Original correct responses:", length(latency_data$correct_rt), "\n")
    cat("Outliers removed:", length(latency_data$outlier_idx), "\n")
    cat("Clean data points:", length(latency_data$clean_rt), "\n")
    coeffs <- stats::coef(model)
    cat("Latency Intercept:", round(coeffs[1], 6), "\n")
    cat("Latency Scalar (F):", round(coeffs[2], 6), "\n")
    cat(paste("R2 (cor squared) latency: ", fitstat, "\n", sep = ""))

    local({
      grDevices::pdf(latency_plot_file)
      on.exit(grDevices::dev.off(), add = TRUE)
      graphics::par(mfrow = c(2, 2))
      rt_range <- range(c(latency_data$clean_rt, rt_predicted), na.rm = TRUE)
      graphics::hist(latency_data$clean_rt, main = "Clean Response Times",
                     xlab = "Response Time (sec)", col = "lightblue",
                     xlim = rt_range, breaks = 40)
      graphics::hist(latency_data$clean_pred, main = "Clean Predictions (Probability)",
                     xlab = "Probability", col = "lightgreen")
      graphics::hist(rt_predicted, main = "Predicted Response Times",
                     xlab = "Response Time (sec)", col = "orange",
                     xlim = rt_range, breaks = 40)
      graphics::plot(rt_predicted, latency_data$clean_rt,
                     main = "rt.pred vs clean_rt",
                     xlab = "rt.pred", ylab = "clean_rt",
                     xlim = rt_range, ylim = rt_range)
      graphics::abline(model, col = "red", lwd = 2)
      graphics::par(mfrow = c(1, 1))
    })

    cat("=== END LATENCY DEBUGGING ===\n")
    cat("Latency diagnostic plot:", latency_plot_file, "\n")
  }

  list(
    model = model,
    fitstat = fitstat,
    predicted = rt_predicted,
    base_prediction = rt_pred_base
  )
}

lkt_fit_weibull_latency <- function(latency_data, weibull_mse_weight = 0,
                                    weibull_t0_fixed = NULL,
                                    verbose = FALSE) {
  correct_rt <- latency_data$correct_rt
  clean_rt <- latency_data$clean_rt
  clean_pred <- latency_data$clean_pred
  clean_subject_id <- latency_data$clean_subject_id

  t0_prefit <- if (!is.null(weibull_t0_fixed)) {
    weibull_t0_fixed
  } else {
    min(clean_rt, na.rm = TRUE)
  }
  if (verbose) {
    cat("Prefit t0 for noise estimate:", round(t0_prefit, 3), "sec\n")
  }

  log_rt_adjusted <- log(pmax(clean_rt - t0_prefit, 1e-6))
  A_prefit <- stats::qlogis(pmin(pmax(clean_pred, 1e-6), 1 - 1e-6))
  if (length(unique(clean_subject_id)) > 1) {
    prefit_df <- data.frame(log_rt_adjusted = log_rt_adjusted,
                            A = A_prefit,
                            subj = factor(clean_subject_id))
    prefit_fit <- stats::lm(log_rt_adjusted ~ A + subj, data = prefit_df)
  } else {
    prefit_df <- data.frame(log_rt_adjusted = log_rt_adjusted, A = A_prefit)
    prefit_fit <- stats::lm(log_rt_adjusted ~ A, data = prefit_df)
  }
  sigma_trial_init <- stats::sd(prefit_fit$residuals, na.rm = TRUE)
  if (verbose) {
    cat("Sigma_trial initial estimate:", round(sigma_trial_init, 4),
        "(prefit residual SD; fixed for quantile matching)\n")
  }

  weibull_fit <- fit_weibull_activation(
    rt = clean_rt,
    p = clean_pred,
    subject_id = clean_subject_id,
    sigma_trial_init = sigma_trial_init,
    mse_weight = weibull_mse_weight,
    t0_fixed = weibull_t0_fixed)

  clean_subject_effects <- if (is.null(weibull_fit$subject_effects)) {
    rep(0, length(clean_subject_id))
  } else {
    sapply(as.character(clean_subject_id), function(sid) {
      eff <- weibull_fit$subject_effects[sid]
      if (is.na(eff)) 0 else eff
    })
  }
  rt_predicted <- weibull_fit$predict(
    clean_pred,
    subject_effect = clean_subject_effects,
    add_noise = TRUE,
    truncate = FALSE)
  fitstat <- stats::cor(clean_rt, rt_predicted, use = "complete.obs")^2

  if (verbose) {
    if (weibull_mse_weight > 0) {
      cat("\n=== WEIBULL LATENCY MODEL (QUANTILE + RT PREDICTION) ===\n")
      cat("Objective weights: Quantile =", 1 - weibull_mse_weight,
          "| RT =", weibull_mse_weight, "\n")
    } else {
      cat("\n=== WEIBULL LATENCY MODEL (QUANTILE MATCHING) ===\n")
    }
    cat("Data:\n")
    cat("  Trials used for fitting:", length(clean_rt),
        "(IQR-filtered from", length(correct_rt), "correct trials)\n")
    cat("  Unique subjects:", weibull_fit$n_subjects, "\n")
    cat("\nFitted Parameters:\n")
    cat("  Shape (k):", round(weibull_fit$k, 4), "\n")
    cat("  Lambda0:", round(weibull_fit$lambda0, 4), "\n")
    cat("  Beta1 (activation):", round(weibull_fit$beta1, 4), "\n")
    cat("  Scale_subject (subject scaling):",
        round(weibull_fit$scale_subject, 4), "\n")
    cat("  t0 (min RT):", round(weibull_fit$t0, 4), "sec\n")
    cat("\nPost-hoc Parameters:\n")
    cat("  Sigma_trial:", round(weibull_fit$sigma_trial, 4), "\n")
    if (!is.null(weibull_fit$subject_effects) &&
        !is.null(clean_subject_id) &&
        length(clean_subject_id) == length(clean_pred)) {
      eps <- 1e-6
      p_clamped <- pmax(pmin(clean_pred, 1 - eps), eps)
      A_by_trial <- stats::qlogis(p_clamped)
      subject_effects <- weibull_fit$subject_effects
      subj_effect_by_trial <- subject_effects[as.character(clean_subject_id)]
      activation_corr <- stats::cor(
        A_by_trial,
        as.numeric(subj_effect_by_trial),
        use = "complete.obs")
      cat("  corr(activation, subject effect) per-trial:",
          round(activation_corr, 4), "\n")
    }
    cat("\nVariance Partitioning:\n")
    cat("  Activation (beta1*A):",
        round(weibull_fit$var_partition$activation * 100, 1), "%\n")
    cat("  Subject effects:",
        round(weibull_fit$var_partition$subject * 100, 1), "%\n")
    cat("  Trial noise:",
        round(weibull_fit$var_partition$trial * 100, 1), "%\n")
    cat("\nFit Quality:\n")
    cat("  Combined loss:", round(weibull_fit$sse, 4), "\n")
    cat("  R-squared (deterministic):",
        round(weibull_fit$r_squared, 4), "\n")
    cat("  Convergence:", weibull_fit$conv, "\n")
    cat("  R-squared (stochastic sim):", round(fitstat, 4), "\n")

    latency_plot_file <- tempfile("lkt-latency-weibull-", fileext = ".pdf")
    local({
      grDevices::pdf(latency_plot_file)
      on.exit(grDevices::dev.off(), add = TRUE)
      graphics::par(mfrow = c(2, 2))
      rt_plot_max <- 30
      rt_breaks <- seq(0, rt_plot_max, by = 1)
      rt_keep <- clean_rt <= rt_plot_max
      graphics::hist(clean_rt[rt_keep], main = "Clean Response Times",
                     xlab = "Response Time (sec)", col = "lightblue",
                     xlim = c(0, rt_plot_max), breaks = rt_breaks)
      graphics::hist(clean_pred, main = "Clean Predictions (Probability)",
                     xlab = "Probability", col = "lightgreen")
      pred_keep <- rt_predicted <= rt_plot_max
      graphics::hist(rt_predicted[pred_keep],
                     main = "Predicted Response Times (Weibull Quantile Fit)",
                     xlab = "Response Time (sec)", col = "orange",
                     xlim = c(0, rt_plot_max), breaks = rt_breaks)
      scatter_keep <- rt_predicted <= rt_plot_max & clean_rt <= rt_plot_max
      graphics::plot(rt_predicted[scatter_keep], clean_rt[scatter_keep],
                     main = "Stochastic sim vs clean_rt",
                     xlab = "Weibull pred (stochastic)", ylab = "clean_rt",
                     xlim = c(0, rt_plot_max), ylim = c(0, rt_plot_max))
      graphics::abline(0, 1, col = "red", lwd = 2)
      graphics::par(mfrow = c(1, 1))
    })
    cat("Latency diagnostic plot:", latency_plot_file, "\n")
  }

  list(
    model = weibull_fit,
    fitstat = fitstat,
    predicted = rt_predicted
  )
}

lkt_fit_latency_model <- function(latency_data, distribution,
                                  weibull_mse_weight = 0,
                                  weibull_t0_fixed = NULL,
                                  verbose = FALSE) {
  if (identical(distribution, "exp")) {
    fit <- lkt_fit_exp_latency(latency_data, verbose = verbose)
    fit$distribution <- "exp"
    return(fit)
  }
  if (identical(distribution, "Weibull")) {
    fit <- lkt_fit_weibull_latency(
      latency_data = latency_data,
      weibull_mse_weight = weibull_mse_weight,
      weibull_t0_fixed = weibull_t0_fixed,
      verbose = verbose)
    fit$distribution <- "Weibull"
    return(fit)
  }
  stop("Unknown distribution type: ", distribution, ". Must be 'exp' or 'Weibull'",
       call. = FALSE)
}

lkt_failure_latency <- function(data) {
  mean(data$Duration..sec.[data$CF..ansbin. == 0], na.rm = TRUE)
}

lkt_latency_model_result <- function(distribution, exp_model = NULL,
                                     weibull_model = NULL, failure_latency) {
  if (identical(distribution, "Weibull")) {
    list(weibull_model, failure_latency)
  } else {
    list(exp_model, failure_latency)
  }
}

lkt_weibull_sigma_subject <- function(weibull_model) {
  if (!is.null(weibull_model$sigma_subject)) {
    return(as.numeric(weibull_model$sigma_subject))
  }
  if (is.null(weibull_model$subject_effects)) {
    return(NA_real_)
  }
  scale_subject_value <- as.numeric(weibull_model$scale_subject)
  subject_sd <- stats::sd(as.numeric(weibull_model$subject_effects),
                          na.rm = TRUE)
  if (is.finite(scale_subject_value) &&
      !is.na(scale_subject_value) &&
      abs(scale_subject_value) > .Machine$double.eps) {
    subject_sd / abs(scale_subject_value)
  } else {
    subject_sd
  }
}

lkt_latency_model_specification <- function(distribution, exp_model = NULL,
                                            weibull_model = NULL,
                                            failure_latency) {
  if (identical(distribution, "exp")) {
    return(list(
      distribution = "exp",
      failure_latency = failure_latency,
      latency_scalar = stats::coef(exp_model)[2],
      latency_intercept = stats::coef(exp_model)[1]
    ))
  }
  if (identical(distribution, "Weibull")) {
    return(list(
      distribution = "Weibull",
      failure_latency = failure_latency,
      weibull_k = weibull_model$k,
      weibull_lambda0 = weibull_model$lambda0,
      weibull_beta1 = weibull_model$beta1,
      weibull_scale_subject = weibull_model$scale_subject,
      weibull_sigma_subject = lkt_weibull_sigma_subject(weibull_model),
      weibull_t0 = weibull_model$t0,
      weibull_sigma_trial = weibull_model$sigma_trial,
      weibull_var_activation = weibull_model$var_partition$activation,
      weibull_var_subject = weibull_model$var_partition$subject,
      weibull_var_trial = weibull_model$var_partition$trial,
      weibull_conv = weibull_model$conv,
      weibull_sse = weibull_model$sse,
      weibull_r_squared = weibull_model$r_squared,
      weibull_n_obs = weibull_model$n_obs,
      weibull_n_subjects = weibull_model$n_subjects,
      weibull_predict_function = weibull_model$predict,
      weibull_subject_effects = weibull_model$subject_effects
    ))
  }
  stop("Unknown distribution type: ", distribution, ". Must be 'exp' or 'Weibull'",
       call. = FALSE)
}
