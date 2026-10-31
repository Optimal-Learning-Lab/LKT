#' Define Bayesian online coefficient personalization
#'
#' @description Uses a fixed set of joint Gaussian prior draws and sequential
#' binary likelihood weights, independently reset for each subject. Predictions
#' average probabilities over draws before observing the current response.
#' No population fit, learning-rate optimization, resampling or jitter is done.
#' @param prior_mean Named finite vector covering every design-matrix coefficient,
#' including fixed coefficients. Supply values from independent training subjects
#' or prior knowledge, not a fit to the subjects being evaluated.
#' @param prior_covariance Named symmetric positive-definite covariance matrix for
#' the adapted coefficients. This is a prior for individual differences, not the
#' sampling covariance of a population mean estimate. Rows/columns are matched by
#' name, not position.
#' @param beta_alpha_terms Names of coefficients to personalize, as in
#' [OnlineAdaptiveModel()]. Defaults to covariance row names.
#' @param n_particles Number of fixed prior draws (at least two).
#' @param seed Nonnegative integer sampling seed. The caller's RNG state is restored.
#' @param subject_col Subject identifier column. Rows must be chronological within
#' each subject; LKT's existing history features supply the predictors.
#' @param ess_threshold Minimum effective sample size as a fraction of particles,
#' in (0,1]. Every row's ESS and threshold status are returned.
#' @param ess_action One of `"warn"` (once per subject), `"error"`, or `"ignore"`.
#' A low ESS means the fixed approximation needs review/refitting; it is never
#' repaired by silently resampling or adding noise.
#' @param store_trace Store posterior means and covariance after every response.
#' @return An LKT model adapter. The fitted `model` contains initial particles,
#' per-subject final states (particles, log weights, means, covariance, ESS),
#' row diagnostics and optional traces. `coefs` holds the supplied initial mean;
#' it is not a substitute for sequential posterior predictions.
#' @details Only linear coefficients are personalized. Nonlinear feature
#' parameters must be fixed in `LKT(fixedpars=...)`, with no automatic KC fitting
#' or dual latency fitting. Unknown model options are rejected. Fold masks are
#' rejected, as are `nosolve` and `@` random-effect features that bypass adapters.
#' Evaluate held-out subjects as a separate data set, whose responses
#' are observed sequentially. This is a static-parameter posterior approximation;
#' uncertainty usually narrows, unlike constant-rate alpha tracking.
#' @seealso [LKTBayesianState()], [LKTBayesianStep()]
#' @md
#' @export
OnlineBayesianModel <- function(prior_mean = NULL, prior_covariance = NULL,
                                beta_alpha_terms = NULL, n_particles = 2048L,
                                seed = 1L, subject_col = "Anon.Student.Id",
                                ess_threshold = 0.01,
                                ess_action = c("warn", "error", "ignore"),
                                store_trace = FALSE) {
  ess_action <- match.arg(ess_action)
  defaults <- list(prior_mean = prior_mean, prior_covariance = prior_covariance,
                   beta_alpha_terms = beta_alpha_terms, n_particles = n_particles,
                   seed = seed, subject_col = subject_col,
                   ess_threshold = ess_threshold, ess_action = ess_action,
                   store_trace = store_trace)
  LKTCustomModel("OnlineBayesian", function(request, verbose = FALSE) {
    unknown <- setdiff(names(request$options),
                       c(names(defaults), "bias", "cost", "epsilon", "type"))
    if (length(unknown)) stop("Unsupported OnlineBayesian option(s): ",
                              paste(unknown, collapse = ", "), call. = FALSE)
    options <- utils::modifyList(defaults, request$options)
    lkt_fit_online_bayesian(request, options)
  })
}

lkt_bayesian_names <- function(x, label) {
  if (is.null(x) || !length(x) || anyNA(x) || any(!nzchar(x)) ||
      anyDuplicated(x)) stop(label, " must have unique nonempty names.", call. = FALSE)
}

lkt_bayesian_logsumexp <- function(x) {
  m <- max(x)
  if (!is.finite(m)) stop("Bayesian log weights collapsed; refit with a suitable prior.",
                          call. = FALSE)
  m + log(sum(exp(x - m)))
}

lkt_bayesian_summary <- function(state) {
  w <- exp(state$log_weights)
  state$mean <- colSums(state$particles * w)
  centered <- sweep(state$particles, 2L, state$mean, "-")
  state$covariance <- crossprod(centered, centered * w)
  state$ess <- 1 / sum(w^2)
  state
}

#' Initialize a reusable Bayesian online state
#'
#' @param prior_mean Named mean vector for the personalized coefficients.
#' @param prior_covariance Named positive-definite joint prior covariance.
#' @param n_particles Number of independent Gaussian draws.
#' @param seed Nonnegative integer seed; caller RNG is restored, including absence
#' of an existing `.Random.seed`.
#' @return State containing joint draws, normalized log weights and posterior
#' moments. Keep the returned state between trials; create/reset it for each new
#' subject. It does not maintain feature history: callers must compute current
#' features using only previously observed responses.
#' @md
#' @export
LKTBayesianState <- function(prior_mean, prior_covariance,
                              n_particles = 2048L, seed = 1L) {
  lkt_bayesian_names(names(prior_mean), "prior_mean")
  if (!is.numeric(prior_mean) || any(!is.finite(prior_mean)))
    stop("prior_mean must be a finite named numeric vector.", call. = FALSE)
  v <- prior_covariance
  if (!is.matrix(v) || !is.numeric(v) || any(!is.finite(v)) ||
      nrow(v) != length(prior_mean) || ncol(v) != length(prior_mean))
    stop("prior_covariance must be a finite square numeric matrix matching prior_mean.",
         call. = FALSE)
  lkt_bayesian_names(rownames(v), "prior_covariance rows")
  lkt_bayesian_names(colnames(v), "prior_covariance columns")
  if (!setequal(rownames(v), names(prior_mean)) ||
      !setequal(colnames(v), names(prior_mean)))
    stop("prior_covariance names must match prior_mean.", call. = FALSE)
  v <- v[names(prior_mean), names(prior_mean), drop = FALSE]
  if (!isSymmetric(v, tol = 1e-12))
    stop("prior_covariance must be symmetric.", call. = FALSE)
  root <- tryCatch(chol(v), error = function(e)
    stop("prior_covariance must be positive definite; no automatic jitter is added.",
         call. = FALSE))
  if (!is.numeric(n_particles) || length(n_particles) != 1L ||
      !is.finite(n_particles) || n_particles < 2 || n_particles != floor(n_particles))
    stop("n_particles must be an integer at least two.", call. = FALSE)
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed < 0 || seed > .Machine$integer.max || seed != floor(seed))
    stop("seed must be a nonnegative integer.", call. = FALSE)
  had_rng <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_rng <- if (had_rng) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(if (had_rng) assign(".Random.seed", old_rng, envir = .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  # chol returns R with t(R) %*% R = covariance: row draws multiply R.
  draws <- matrix(stats::rnorm(n_particles * length(prior_mean)),
                  nrow = n_particles) %*% root
  draws <- sweep(draws, 2L, prior_mean, "+")
  colnames(draws) <- names(prior_mean)
  lkt_bayesian_summary(structure(list(
    particles = draws, log_weights = rep(-log(n_particles), n_particles),
    updates = 0L, ess_alerted = FALSE), class = "LKTBayesianState"))
}

#' Predict and optionally update one Bayesian online response
#'
#' @param state State returned by [LKTBayesianState()] or a previous step.
#' @param x Named finite feature vector matching the personalized coefficients.
#' @param y Observed binary response, or `NULL` to predict without updating.
#' @param offset Finite fixed contribution to the logit from other coefficients.
#' @param ess_threshold Minimum ESS fraction of particles.
#' @param ess_action Low ESS policy: `"warn"`, `"error"`, or `"ignore"`.
#' @return List with pre-response posterior predictive `prediction`, observed
#' response `log_score` (NULL when predicting), `low_ess`, and updated `state`.
#' The prediction is clipped only at machine epsilon for the LKT probability
#' contract. Log scores and likelihood weights use stable, unclipped logits.
#' @md
#' @export
LKTBayesianStep <- function(state, x, y = NULL, offset = 0,
                             ess_threshold = 0.01,
                             ess_action = c("warn", "error", "ignore")) {
  ess_action <- match.arg(ess_action)
  if (!inherits(state, "LKTBayesianState") || !is.matrix(state$particles) ||
      length(state$log_weights) != nrow(state$particles) ||
      any(!is.finite(state$particles)) || anyNA(state$log_weights) ||
      any(state$log_weights == Inf) || !any(is.finite(state$log_weights)))
    stop("Invalid Bayesian state.", call. = FALSE)
  lkt_bayesian_names(names(x), "x")
  if (!is.numeric(x) || any(!is.finite(x)) ||
      !setequal(names(x), colnames(state$particles)))
    stop("x must be finite and match the personalized coefficient names.", call. = FALSE)
  if (!is.numeric(offset) || length(offset) != 1L || !is.finite(offset))
    stop("offset must be a finite scalar.", call. = FALSE)
  if (!is.numeric(ess_threshold) || length(ess_threshold) != 1L ||
      !is.finite(ess_threshold) || ess_threshold <= 0 || ess_threshold > 1)
    stop("ess_threshold must be in (0,1].", call. = FALSE)
  if (!is.null(y) && (!is.numeric(y) || length(y) != 1L ||
                      !is.finite(y) || !y %in% c(0, 1)))
    stop("y must be a binary numeric response or NULL.", call. = FALSE)
  eta <- as.numeric(state$particles %*% x[colnames(state$particles)]) + offset
  if (any(!is.finite(eta))) stop("Nonfinite Bayesian logits.", call. = FALSE)
  state$log_weights <- state$log_weights - lkt_bayesian_logsumexp(state$log_weights)
  log_p <- -pmax(-eta, 0) - log1p(exp(-abs(eta)))
  log_q <- -pmax(eta, 0) - log1p(exp(-abs(eta)))
  prediction <- exp(lkt_bayesian_logsumexp(state$log_weights + log_p))
  prediction <- min(max(prediction, .Machine$double.eps), 1 - .Machine$double.eps)
  log_score <- NULL
  if (!is.null(y)) {
    next_weights <- state$log_weights + if (y == 1) log_p else log_q
    log_score <- lkt_bayesian_logsumexp(next_weights)
    state$log_weights <- next_weights - log_score
    state$updates <- state$updates + 1L
  }
  state <- lkt_bayesian_summary(state)
  low_ess <- state$ess < ess_threshold * nrow(state$particles)
  if (low_ess) {
    msg <- "Bayesian particle ESS below threshold; review/refit the prior or increase particles."
    if (ess_action == "error") stop(msg, call. = FALSE)
    if (ess_action == "warn" && !isTRUE(state$ess_alerted)) warning(msg, call. = FALSE)
    state$ess_alerted <- TRUE
  }
  list(prediction = prediction, log_score = log_score,
       low_ess = low_ess, state = state)
}

lkt_fit_online_bayesian <- function(request, options) {
  if (!is.na(request$usefolds)[1])
    stop("OnlineBayesian requires separate held-out subjects, not usefolds.", call. = FALSE)
  if (!is.logical(options$store_trace) || length(options$store_trace) != 1L ||
      is.na(options$store_trace)) stop("store_trace must be TRUE or FALSE.", call. = FALSE)
  x <- request$design_matrix$sparse
  nm <- colnames(x)
  mu <- options$prior_mean
  lkt_bayesian_names(nm, "Design matrix")
  lkt_bayesian_names(names(mu), "prior_mean")
  if (!is.numeric(mu) || any(!is.finite(mu)) || !setequal(names(mu), nm))
    stop("prior_mean must cover exactly all design-matrix coefficient names.", call. = FALSE)
  mu <- mu[nm]
  terms <- options$beta_alpha_terms
  if (is.null(terms)) terms <- rownames(options$prior_covariance)
  lkt_bayesian_names(terms, "beta_alpha_terms")
  if (!all(terms %in% nm)) stop("Unknown personalized coefficient names.", call. = FALSE)
  initial <- LKTBayesianState(mu[terms], options$prior_covariance,
                              options$n_particles, options$seed)
  subject_col <- options$subject_col
  if (!is.character(subject_col) || length(subject_col) != 1L ||
      !subject_col %in% names(request$data))
    stop("OnlineBayesian requires subject_col in data.", call. = FALSE)
  subjects <- as.character(request$data[[subject_col]])
  if (anyNA(subjects) || any(!nzchar(subjects))) stop("Missing subject identifiers.", call. = FALSE)
  if ("CF..Time." %in% names(request$data)) {
    times <- request$data[["CF..Time."]]
    if (!is.numeric(times) || any(!is.finite(times)) ||
        any(vapply(split(times, subjects), function(z) any(diff(z) < 0), logical(1L))))
      stop("OnlineBayesian requires finite chronological CF..Time. within subjects.", call. = FALSE)
  }
  y <- request$response
  if (!is.numeric(y) || length(y) != nrow(x) || anyNA(y) || any(!y %in% c(0, 1)))
    stop("OnlineBayesian requires complete binary responses.", call. = FALSE)
  fixed <- setdiff(nm, terms)
  offset <- if (length(fixed)) as.numeric(x[, fixed, drop = FALSE] %*% mu[fixed]) else
    rep(0, nrow(x))
  pred <- score <- ess <- numeric(nrow(x))
  low <- logical(nrow(x))
  trace <- if (options$store_trace) vector("list", nrow(x)) else NULL
  ids <- unique(subjects)
  states <- stats::setNames(vector("list", length(ids)), ids)
  for (id in ids) {
    state <- initial
    for (row in which(subjects == id)) {
      current <- as.numeric(x[row, terms, drop = FALSE])
      names(current) <- terms
      step <- LKTBayesianStep(state, current, y[row], offset[row],
                               options$ess_threshold, options$ess_action)
      state <- step$state
      pred[row] <- step$prediction
      score[row] <- step$log_score
      ess[row] <- state$ess
      low[row] <- step$low_ess
      if (options$store_trace) trace[[row]] <- list(mean = state$mean,
                                                   covariance = state$covariance)
    }
    states[[id]] <- state
  }
  LKTModelFit(model = list(name = "OnlineBayesian", prior_mean = mu,
    prior_covariance = options$prior_covariance, beta_alpha_terms = terms,
    subject_col = subject_col, initial_state = initial, final_states = states,
    diagnostics = data.frame(row = seq_along(y), ess = ess, low_ess = low,
                             log_score = score), trace = trace,
    ess_threshold = options$ess_threshold, ess_action = options$ess_action,
    seed = options$seed), pred = pred,
    modelvs = matrix(mu, ncol = 1L, dimnames = list(nm, "coefficient")),
    fitstat = sum(score), predictset = x, predictset2 = request$design_matrix$csr)
}
