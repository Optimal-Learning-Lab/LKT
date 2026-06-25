lkt_resolve_feature_parameters <- function(feature, fixedpars, seedparameters,
                                           fixed_index, optimized_index) {
  param_names <- c("para", "parb", "parc", "pard", "pare")
  param_count <- lkt_feature_param_counts(feature)
  pars <- stats::setNames(rep(NA_real_, length(param_names)), param_names)
  recompute_feature <- FALSE

  if (param_count == 0L) {
    return(list(
      pars = pars,
      param_count = param_count,
      fixed_index = fixed_index,
      optimized_index = optimized_index,
      recompute_feature = recompute_feature
    ))
  }

  for (slot in seq_len(param_count)) {
    fixed_value <- fixedpars[fixed_index]
    if (is.na(fixed_value)) {
      pars[slot] <- seedparameters[optimized_index]
      optimized_index <- optimized_index + 1L
      if (slot == 1L) {
        recompute_feature <- TRUE
      }
    } else if (fixed_value >= 1 && fixed_value %% 1 == 0) {
      pars[slot] <- seedparameters[fixed_value]
    } else {
      pars[slot] <- fixed_value
    }
    fixed_index <- fixed_index + 1L
  }

  list(
    pars = pars,
    param_count = param_count,
    fixed_index = fixed_index,
    optimized_index = optimized_index,
    recompute_feature = recompute_feature
  )
}

lkt_spacing_requirements <- function(data, components, features) {
  spacing_dependent_features <- c(
    "recency", "recencystudy", "recencytest", "recencysuc", "recencyfail",
    "ppe", "ppes", "ppef")
  features_needing_spacing <- character(0)
  missing_spacing_components <- character(0)

  for (i in seq_along(features)) {
    feature <- gsub("[$@]", "", features[i])
    component <- components[i]
    if (feature %in% spacing_dependent_features) {
      features_needing_spacing <- c(features_needing_spacing, feature)
      spacing_col <- paste0(component, "spacing")
      if (!spacing_col %in% colnames(data)) {
        missing_spacing_components <- c(missing_spacing_components, component)
      }
    }
  }

  list(
    features = unique(features_needing_spacing),
    components = unique(missing_spacing_components)
  )
}

lkt_fit_null_model <- function(data, usefolds) {
  formula <- stats::as.formula("CF..ansbin.~ 1")
  if (is.na(usefolds)[1]) {
    stats::glm(formula, data = data, family = stats::binomial("logit"))
  } else {
    stats::glm(formula, data = data[data$fold %in% usefolds, ],
               family = stats::binomial("logit"))
  }
}

lkt_fit_statistics <- function(data, fitstat, usefolds) {
  nullmodel <- lkt_fit_null_model(data, usefolds)
  nullfit <- stats::logLik(nullmodel)
  list(
    nullmodel = nullmodel,
    nullfit = nullfit,
    loglike = fitstat,
    mcfad = round(1 - fitstat[1] / nullfit[1], 6)
  )
}

lkt_student_rmse <- function(data) {
  if (!("pred" %in% colnames(data))) {
    return(NULL)
  }
  stats::aggregate(
    sqrt((data$pred - data$CF..ansbin.)^2),
    by = list(data$Anon.Student.Id),
    FUN = mean)
}

lkt_lkt_result <- function(model, coefs, model_specification, r2, data,
                           nullmodel, optimizedpars, predictors, loglike,
                           model_name, automat, dualfit = FALSE,
                           distribution = "exp", exp_model = NULL,
                           weibull_model = NULL, failure_latency = NULL) {
  result <- list(
    "model" = model,
    "coefs" = coefs,
    "model_specification" = list(model_specification),
    "r2" = r2,
    "prediction" = if ("pred" %in% colnames(data)) data$pred else NULL,
    "nullmodel" = nullmodel,
    "latencymodel" = if (dualfit) {
      lkt_latency_model_result(
        distribution = distribution,
        exp_model = exp_model,
        weibull_model = weibull_model,
        failure_latency = failure_latency)
    } else NULL,
    "optimizedpars" = optimizedpars,
    "studentRMSE" = lkt_student_rmse(data),
    "newdata" = data,
    "predictors" = predictors,
    "loglike" = loglike,
    "model_name" = model_name,
    "automat" = automat
  )
  if (dualfit) {
    result$model_specification[[2]] <- lkt_latency_model_specification(
      distribution = distribution,
      exp_model = exp_model,
      weibull_model = weibull_model,
      failure_latency = failure_latency)
  }
  result
}

lkt_apply_curve_feature <- function(data, curvefeat) {
  if (!is.na(curvefeat)) {
    lkt_require_columns(data, curvefeat, "LKT curvefeats")
    data$curvefeat <- as.numeric(as.character(data[[curvefeat]]))
  } else if ("pred" %in% colnames(data)) {
    data$curvefeat <- data$pred
  }
  data
}

lkt_materialize_feature <- function(data, feature, component, pars,
                                    param_count, nosolve,
                                    context_cache = NULL) {
  context_result <- lkt_prepare_component_context(
    data, component, feature, context_cache = context_cache)
  if (is.list(context_result) && all(c("data", "context_cache") %in%
                                     names(context_result))) {
    data <- context_result$data
    context_cache <- context_result$context_cache
  } else {
    data <- context_result
  }
  component <- gsub("\\s+", "", component)
  if (right(feature, 1) == "@") {
    data[[component]] <- computefeatures(
      data, feature, pars[["para"]], pars[["parb"]],
      data$index, data$indexcomp, pars[["parc"]],
      pars[["pard"]], pars[["pare"]], component)
  } else {
    feature_col <- lkt_feature_column_name(
      feature, component, if (param_count >= 1L) pars[["para"]] else NULL,
      nosolve)
    data[[feature_col]] <- computefeatures(
      data, feature, pars[["para"]], pars[["parb"]],
      data$index, data$indexcomp, pars[["parc"]],
      pars[["pard"]], pars[["pare"]], component)
  }
  list(data = data, component = component, context_cache = context_cache)
}

lkt_print_feature_progress <- function(feature, component, pars, param_count) {
  cat(paste(
    feature, component,
    if (param_count >= 1L) pars[["para"]],
    if (param_count >= 2L) pars[["parb"]],
    if (param_count >= 3L) pars[["parc"]],
    if (param_count >= 4L) pars[["pard"]],
    if (param_count >= 5L) pars[["pare"]],
    "\n"
  ))
}

lkt_prepare_lkt_data <- function(data, components, features, maketimes,
                                 verbose) {
  if (maketimes) {
    if (!("CF..reltime." %in% colnames(data))) {
      data$CF..reltime. <- practiceTime(data)
    }
    if (!("CF..Time." %in% colnames(data))) {
      data$CF..Time. <- data$CF..reltime.
    }
  }
  if (!("Outcome" %in% colnames(data))) {
    data$Outcome <- ifelse(data$CF..ansbin. == 1, "CORRECT", "INCORRECT")
  }
  if (!("CF..ansbin." %in% colnames(data))) {
    data$CF..ansbin. <- ifelse(data$Outcome == "CORRECT", 1, 0)
  }

  spacing_requirements <- lkt_spacing_requirements(data, components, features)
  features_needing_spacing <- spacing_requirements$features
  missing_spacing_components <- spacing_requirements$components
  if (length(missing_spacing_components) == 0L) {
    return(data)
  }

  if (verbose) {
    cat("LKT: Features", paste(features_needing_spacing, collapse = ", "),
        "require spacing predictors.\n")
    cat("LKT: Auto-computing spacing predictors for components:",
        paste(missing_spacing_components, collapse = ", "), "...\n")
  }
  for (comp in missing_spacing_components) {
    if (verbose) {
      cat("LKT: Computing spacing predictors for component '", comp,
          "'...\n", sep = "")
    }
    data <- suppressWarnings(computeSpacingPredictors(data, comp))
  }
  if (verbose) {
    cat("LKT: Spacing predictors computed successfully. Proceeding with model fitting.\n")
  }
  data
}

lkt_apply_autokc_components <- function(data, components, autoKC, autoKCcont) {
  automat <- list()
  if (!any(autoKC > 1)) {
    return(list(data = data, components = components, automat = automat))
  }
  for (k in seq_along(components)) {
    if (autoKC[k] > 1) {
      autokc_result <- lkt_apply_autokc(
        data = data,
        component = components[k],
        feature_index = k,
        cluster_count = autoKC[k],
        randomize = autoKCcont[k] == "rand")
      automat <- c(list(autokc_result$model), automat)
      data <- autokc_result$data
      components[k] <- autokc_result$component
    }
  }
  list(data = data, components = components, automat = automat)
}

lkt_fit_accuracy_step <- function(formula, data, features, feature_spec,
                                  terms_cache, model, model_options, usefolds,
                                  dualfit, nosolve, verbose) {
  if (any(grep("[@]", features)) && !dualfit) {
    cat("Using glmer, which uses lme4 package, which is not efficient for large complex data and has memory limitations.")
    fit <- glmer(formula, data = data, family = binomial(logit))
    return(list(
      model = fit,
      fitstat = logLik(fit),
      data = data
    ))
  }

  feature_input <- lkt_feature_input(
    formula = formula,
    data = data,
    feature_spec = feature_spec,
    terms_cache = terms_cache)
  if (nosolve) {
    design_matrix <- feature_input$design_matrix
    return(list(
      feature_input = feature_input,
      lassodata = list(colnames(design_matrix$sparse), design_matrix$csr),
      data = data
    ))
  }

  model_request <- lkt_model_fit_request(
    usefolds = usefolds,
    options = model_options,
    feature_input = feature_input)
  model_fit <- lkt_run_model(
    model = model,
    request = model_request,
    verbose = verbose)
  data$pred <- model_fit$pred
  list(
    model = model_fit$model,
    modelvs = model_fit$modelvs,
    fitstat = model_fit$fitstat,
    feature_input = feature_input,
    predictset2 = model_fit$predictset2,
    model_name = model_fit$model_name,
    data = data
  )
}

lkt_lkt_optimization_inputs <- function(fixedpars, seedpars,
                                        nonlinear_param_count) {
  fixed_slots <- if (nonlinear_param_count > 0L) {
    fixedpars[seq_len(nonlinear_param_count)]
  } else {
    numeric(0)
  }
  seeds <- seedpars[is.na(fixedpars)]
  seeds[is.na(seeds)] <- 0.5
  list(
    parameter_count = nonlinear_param_count - sum(!is.na(fixed_slots)),
    seeds = seeds
  )
}

lkt_optimize_lkt_parameters <- function(fixedpars, seedpars,
                                        nonlinear_param_count, objective,
                                        lowb, highb, maxitv, factrv) {
  opt_input <- lkt_lkt_optimization_inputs(
    fixedpars = fixedpars,
    seedpars = seedpars,
    nonlinear_param_count = nonlinear_param_count)
  if (opt_input$parameter_count > 0L) {
    return(stats::optim(
      opt_input$seeds,
      objective,
      method = "L-BFGS-B",
      lower = lowb,
      upper = highb,
      control = list(maxit = maxitv, factr = factrv)))
  }
  objective(numeric(0))
  NA
}

lkt_report_latency_fit <- function(data, distribution, exp_model, verbose) {
  failure_latency <- lkt_failure_latency(data)
  if (identical(distribution, "exp") && verbose) {
    latency_spec <- lkt_latency_model_specification(
      distribution = distribution,
      exp_model = exp_model,
      failure_latency = failure_latency)
    cat(paste("Failure latency: ", failure_latency, "\n"))
    cat(paste("Latency Scalar: ", latency_spec$latency_scalar, "\n",
              "Latency Intercept: ", latency_spec$latency_intercept, "\n",
              sep = ""))
  }
  failure_latency
}

lkt_build_accuracy_model_specification <- function(modelvs, features,
                                                   components, param_tracker) {
  build_model_specification(
    setNames(modelvs[, "coefficient"], rownames(modelvs)),
    features = gsub("[@$]", "", features),
    components = components,
    param_tracker = param_tracker)
}

lkt_build_feature_iteration <- function(data, components, features, curvefeats,
                                        fixedpars, seedparameters, connectors,
                                        interacts, interc, first_pass,
                                        recompute_feature, nosolve, verbose,
                                        context_cache = NULL) {
  formula_rhs <- if (interc) "1" else "0"
  fixed_index <- 1L
  optimized_index <- 1L
  param_tracker <- vector("list", length(features))

  for (k in seq_along(features)) {
    feature <- features[k]
    data <- lkt_apply_curve_feature(data, curvefeats[k])
    param_state <- lkt_resolve_feature_parameters(
      feature = feature,
      fixedpars = fixedpars,
      seedparameters = seedparameters,
      fixed_index = fixed_index,
      optimized_index = optimized_index)
    feature_param_count <- param_state$param_count
    feature_pars <- param_state$pars
    fixed_index <- param_state$fixed_index
    optimized_index <- param_state$optimized_index
    recompute_feature <- recompute_feature || param_state$recompute_feature

    if (recompute_feature || first_pass) {
      feature_result <- lkt_materialize_feature(
        data = data,
        feature = feature,
        component = components[k],
        pars = feature_pars,
        param_count = feature_param_count,
        nosolve = nosolve,
        context_cache = context_cache)
      data <- feature_result$data
      components[k] <- feature_result$component
      context_cache <- feature_result$context_cache
      recompute_feature <- FALSE
    }

    if (verbose) {
      lkt_print_feature_progress(feature, components[k], feature_pars,
                                 feature_param_count)
    }
    formula_rhs <- lkt_formula_term(
      feature = feature,
      component = components[k],
      connector = lkt_formula_connector(connectors[k]),
      current_formula = formula_rhs,
      interact = interacts[k],
      para = if (feature_param_count >= 1L) feature_pars[["para"]] else NULL,
      nosolve = nosolve)
    param_tracker[[k]] <- list(
      feature = lkt_clean_feature_name(feature),
      component = components[k],
      pars = feature_pars
    )
  }

  list(
    data = data,
    components = components,
    formula_rhs = formula_rhs,
    param_tracker = param_tracker,
    recompute_feature = recompute_feature,
    context_cache = context_cache
  )
}

#' Fit a logistic knowledge tracing model
#'
#' @description Computes LKT features from trial-level learning data and fits a logistic model that predicts `CF..ansbin.`. The default model adapter uses `LiblineaR`; custom adapters can be supplied with `LKTCustomModel()`.
#'
#' @param data Data frame or data table containing one row per trial. Must include `Anon.Student.Id`, `CF..ansbin.`, and every component column named in `components`. Some features also require timing columns such as `CF..Time.`, `CF..reltime.`, or `Duration..sec.`.
#' @param usefolds Numeric vector of fold values to use for fitting. Features are still computed over all rows so held-out rows can be evaluated later. `NA` fits on all rows.
#' @param components Character vector naming component columns or component specifications used to build feature histories.
#' @param features Character vector of LKT feature names, parallel to `components`.
#' @param fixedpars Numeric vector of nonlinear feature parameters. Use `NA` entries to optimize corresponding parameters.
#' @param seedpars Numeric vector of starting values for nonlinear parameter optimization. `NA` uses default starts.
#' @param interacts Optional vector of interaction terms added to component feature terms. Use `NA` for no interaction.
#' @param curvefeats Optional vector of columns used by legacy difficulty curve features.
#' @param dualfit Logical; if `TRUE`, also fit a response-time model. Requires `Duration..sec.`.
#' @param interc Logical; if `TRUE`, include a global intercept.
#' @param verbose Logical; if `TRUE`, print progress and fit diagnostics.
#' @param epsilon Numeric stopping tolerance passed to `LiblineaR`.
#' @param cost Numeric regularization cost passed to `LiblineaR`.
#' @param lowb Lower bound for nonlinear parameter optimization.
#' @param highb Upper bound for nonlinear parameter optimization.
#' @param type Integer LiblineaR solver type.
#' @param maketimes Logical; if `TRUE`, create missing time columns from `Duration..sec.` where possible.
#' @param bias Numeric bias term passed to `LiblineaR`.
#' @param maxitv Maximum iteration count for nonlinear optimization.
#' @param factrv Factr control value passed to `optim()` for nonlinear optimization.
#' @param nosolve Logical; if `TRUE`, return the constructed sparse design data instead of fitting a model. This is a legacy public interface used by LASSO search helpers.
#' @param autoKC Integer vector controlling automatic KC clustering. `0` disables clustering for the corresponding component; positive values are cluster counts.
#' @param autoKCcont Character vector controlling automatic KC randomization. Use `"rand"` to randomize cluster assignment for comparison runs.
#' @param connectors Character vector of formula operators (`"+"`, `":"`, or `"*"`) between model terms. Defaults to additive terms.
#' @param distribution Latency model distribution. Supported values are `"exp"` and `"Weibull"` when `dualfit = TRUE`.
#' @param weibull_mse_weight Numeric weight in `[0, 1]` for the point-prediction component of Weibull latency fitting.
#' @param weibull_t0_fixed Optional fixed minimum response time, in seconds, for Weibull latency fitting.
#' @param model LKT model adapter. Defaults to `LibLinearModel()`.
#' @param model_options Optional list of adapter-specific model options.
#' @return A list containing the fitted model, coefficients, predictions, fit statistics, processed data, feature metadata, and optional latency-model results. With `nosolve = TRUE`, returns the constructed sparse matrices and feature names for downstream legacy model-selection helpers.
#' @examples
#' \donttest{
#' data(samplelkt)
#' fit <- LKT(
#'   data = samplelkt,
#'   components = "KC..Default.",
#'   features = "intercept",
#'   interc = TRUE,
#'   verbose = FALSE
#' )
#' head(fit$prediction)
#' }
#' @export
LKT <- function(data,usefolds = NA,
                components,
                features,
                fixedpars = NA,
                seedpars = NA,
                interacts = NA,
                curvefeats = NA,
                dualfit = FALSE,
                interc = FALSE,
                verbose = TRUE,
                epsilon = 1e-4,
                cost = 512,
                lowb=.00001,
                highb=.99999,
                type = 0,
                maketimes = FALSE,
                bias = 0,
                maxitv=100,
                factrv=1e12,
                nosolve=FALSE,
                autoKC=rep(0,length(components)),
                autoKCcont = rep("NA",length(components)),
                connectors= rep("+",max(1,length(components)-1)),
                distribution = "exp",
                weibull_mse_weight = 0,
                weibull_t0_fixed = NULL,
                model = LibLinearModel(),
                model_options = list()) {
  lkt_validate_feature_spec(components, features)
  nonlinear_param_count <- sum(lkt_feature_param_counts(features))
  if (nosolve && !(length(fixedpars) == 1L && is.na(fixedpars))) {
    if (length(fixedpars) < nonlinear_param_count) {
      stop("fixedpars must have at least length ", nonlinear_param_count,
           " for nosolve matrix construction; got ", length(fixedpars), ".",
           call. = FALSE)
    }
    fixedpars <- as.numeric(fixedpars)
    seedpars <- lkt_normalize_parameter_vector(seedpars, nonlinear_param_count, "seedpars")
  } else {
    fixedpars <- lkt_normalize_parameter_vector(fixedpars, nonlinear_param_count, "fixedpars")
    seedpars <- lkt_normalize_parameter_vector(seedpars, nonlinear_param_count, "seedpars")
  }
  if (dualfit && !("Duration..sec." %in% colnames(data))) {
    stop("dualfit requires a Duration..sec. column.", call. = FALSE)
  }
  connectors<-c("+",connectors)
  data <- lkt_prepare_lkt_data(
    data = data,
    components = components,
    features = features,
    maketimes = maketimes,
    verbose = verbose)

  equation <- "CF..ansbin.~ "
  e <- new.env()

  e$data <- data
  e$fixedpars <- fixedpars
  e$seedpars <- seedpars
  e$counter <- 0
  e$recompute_feature <- FALSE
  e$df<-list()
  e$components <- components
  e$param_tracker <- vector("list", length(features))
  e$form_terms_cache <- new.env(parent = emptyenv())
  e$component_context_cache <- list()
  e$model <- model
  e$model_options <- lkt_model_options(
    bias = bias,
    cost = cost,
    epsilon = epsilon,
    type = type,
    extra_options = model_options)

  autokc_state <- lkt_apply_autokc_components(
    data = e$data,
    components = e$components,
    autoKC = autoKC,
    autoKCcont = autoKCcont)
  e$data <- autokc_state$data
  e$components <- autokc_state$components
  e$df <- autokc_state$automat

  modelfun <- function(seedparameters) {
    e$counter <- e$counter + 1
    feature_iteration <- lkt_build_feature_iteration(
      data = e$data,
      components = e$components,
      features = features,
      curvefeats = curvefeats,
      fixedpars = e$fixedpars,
      seedparameters = seedparameters,
      connectors = connectors,
      interacts = interacts,
      interc = interc,
      first_pass = e$counter < 2,
      recompute_feature = e$recompute_feature,
      nosolve = nosolve,
      verbose = verbose,
      context_cache = e$component_context_cache)
    e$data <- feature_iteration$data
    e$components <- feature_iteration$components
    e$param_tracker <- feature_iteration$param_tracker
    e$recompute_feature <- feature_iteration$recompute_feature
    e$component_context_cache <- feature_iteration$context_cache

    if (verbose) {
      cat(paste(feature_iteration$formula_rhs, "\n"))
    }
    e$form <- stats::as.formula(
      paste(equation, feature_iteration$formula_rhs, sep = ""))
    accuracy_fit <- lkt_fit_accuracy_step(
      formula = e$form,
      data = e$data,
      features = features,
      feature_spec = e$param_tracker,
      terms_cache = e$form_terms_cache,
      model = e$model,
      model_options = e$model_options,
      usefolds = usefolds,
      dualfit = dualfit,
      nosolve = nosolve,
      verbose = verbose)
    e$data <- accuracy_fit$data
    e$feature_input <- accuracy_fit$feature_input

    if (dualfit) {
      latency_data <- lkt_prepare_latency_data(e$data)
      latency_fit <- lkt_fit_latency_model(
        latency_data = latency_data,
        distribution = distribution,
        weibull_mse_weight = weibull_mse_weight,
        weibull_t0_fixed = weibull_t0_fixed,
        verbose = verbose)
      if (identical(latency_fit$distribution, "exp")) {
        e$lm.rt <- latency_fit$model
      } else {
        e$weibull.rt <- latency_fit$model
      }
    }

    if (!nosolve) {
      e$temp <- accuracy_fit$model
      e$modelvs <- accuracy_fit$modelvs
      e$predictset2 <- accuracy_fit$predictset2
      e$model_name <- accuracy_fit$model_name
      fitstat <- accuracy_fit$fitstat
      fit_stats <- lkt_fit_statistics(e$data, fitstat, usefolds)
      e$nullmodel <- fit_stats$nullmodel
      e$nullfit <- fit_stats$nullfit
      e$loglike <- fit_stats$loglike
      e$mcfad <- fit_stats$mcfad
      if (verbose) {
        cat(paste("McFadden's R2 logistic:", e$mcfad, "\n"))
        cat(paste("LogLike logistic:", round(fitstat, 8), "\n"))
      }
      if (length(seedparameters) > 0 & verbose) {
        cat(paste("step par values ="))
        cat(seedparameters, sep = ",")
        cat(paste("\n\n"))
      }
      -fitstat[1]
    } else {
      accuracy_fit$lassodata
    }
  }



  if (!nosolve) {
    optimizedpars <- lkt_optimize_lkt_parameters(
      fixedpars = e$fixedpars,
      seedpars = e$seedpars,
      nonlinear_param_count = nonlinear_param_count,
      objective = modelfun,
      lowb = lowb,
      highb = highb,
      maxitv = maxitv,
      factrv = factrv)
    failureLatency <- if (dualfit) {
      lkt_report_latency_fit(
        data = e$data,
        distribution = distribution,
        exp_model = e$lm.rt,
        verbose = verbose)
    } else NULL
    model_spec <- lkt_build_accuracy_model_specification(
      modelvs = e$modelvs,
      features = features,
      components = e$components,
      param_tracker = e$param_tracker)

    results <- lkt_lkt_result(
      model = e$temp,
      coefs = e$modelvs,
      model_specification = model_spec,
      r2 = e$mcfad,
      data = e$data,
      nullmodel = e$nullmodel,
      optimizedpars = optimizedpars,
      predictors = e$predictset2,
      loglike = e$loglike,
      model_name = if (!is.null(e$model_name)) e$model_name else NA,
      automat = e$df,
      dualfit = dualfit,
      distribution = distribution,
      exp_model = e$lm.rt,
      weibull_model = e$weibull.rt,
      failure_latency = failureLatency)
  } else {
    results <- list("lassodata" = modelfun(numeric(0)))
  }
  results
}




# Build a tidy model_specification from the fitted LKT object

# Boot function for LKT_HDI


#Given a par_reps matrix, computes HDI intervals for each column
# custom duration function, experimental

texteval <- function(stringv) {
  eval(parse(text = stringv))
}

#' Open a data frame in the system spreadsheet viewer
#'
#' @description Legacy exported convenience helper that writes a data frame to a temporary CSV file and opens it with the operating system's default application where supported. It is intended for interactive use, not package automation.
#'
#' @param df Data frame to write. Defaults to `.Last.value`.
#' @param file File path for the temporary CSV output.
#' @return The value returned by the operating-system file opener.
#' @examples
#' \dontrun{
#' ViewExcel(head(samplelkt))
#' }
#' @export
ViewExcel <-function(df = .Last.value, file = tempfile(fileext = ".csv")) {
  df <- try(as.data.frame(df))
  stopifnot(is.data.frame(df))
  utils::write.csv(df, file = file)
  if (.Platform$OS.type == "windows") {
    get("shell.exec", envir = asNamespace("utils"))(file)
  } else {
    utils::browseURL(file)
  }
}


LKTStartupMessage <- function()
{
  # > figlet -f doom LKT
  msg <- c(paste0(
    "  LL      KK  KK TTTTTTT
  LL      KK KK    TTT
  LL      KKKK     TTT
  LL      KK KK    TTT
  LLLLLLL KK  KK   TTT

  Join the mailing list: lkt@freelists.org
  Version ",
    packageVersion("LKT")),
    "\nType 'citation(\"LKT\")' for citing this R package in publications.")
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  # unlock .LKT variable allowing its modification
  #unlockBinding(".LKT", asNamespace("LKT"))
  # startup message
  msg <- LKTStartupMessage()
  if(!interactive())
    msg[1] <- paste("Package 'LKT' version", packageVersion("LKT"))
  packageStartupMessage(msg)
  invisible()
}
