script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
if (is.na(script_file) || !nzchar(script_file)) {
  script_file <- file.path("scripts", "compare-single-alpha-contributions.R")
}
root <- normalizePath(file.path(dirname(script_file), ".."),
                      winslash = "/", mustWork = TRUE)
vignette_dir <- file.path(root, "vignettes")
report_dir <- file.path(vignette_dir, "_example-reports")
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

Sys.setenv(LKT_SOURCE_WORKTREE = "true")

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(vignette_dir)
source("lkt-vignette-setup.R")

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) == 0L) {
    return(default)
  }
  sub(paste0("^--", name, "="), "", hit[1L])
}

maxit <- as.integer(get_arg("maxit", "20"))
require_native <- identical(tolower(get_arg("require-native", "true")), "true")
optim_trace <- as.integer(get_arg("optim-trace", "0"))
optim_report <- as.integer(get_arg("optim-report", "1"))
alpha_start_value <- as.numeric(get_arg("alpha-start", "0.01"))
alpha_lower <- as.numeric(get_arg("alpha-lower", "-1"))
alpha_upper <- as.numeric(get_arg("alpha-upper", "1"))

features <- c("linesuc", "recency", "linefail", "ppes")
components <- rep("KC..Default.", length(features))
fixedpars <- c(
  0.5781309,
  0.6731491, 0.1982123, 0.3366958, 0.7954989
)
beta_alpha_terms <- c(
  "linesucKC..Default.",
  "recencyKC..Default.",
  "linefailKC..Default.",
  "ppesKC..Default."
)

fit_online <- function(label, beta_terms, alpha_start, lower, upper) {
  cat("\n[fit]", label, "\n")
  LKT(
    data = val,
    interc = TRUE,
    dualfit = FALSE,
    factrv = 1e11,
    components = components,
    features = features,
    fixedpars = fixedpars,
    model = "online_adaptive",
    model_options = list(
      online_mode = "alpha_only",
      beta_alpha_terms = beta_terms,
      nonlinear_alpha_terms = character(0),
      alpha_start = alpha_start,
      alpha_lower = lower,
      alpha_upper = upper,
      require_native = require_native,
      maxit = maxit,
      factr = 1e7,
      optim_trace = optim_trace,
      optim_report = optim_report
    )
  )
}

auc_for <- function(fit) {
  as.numeric(pROC::auc(pROC::roc(
    response = val$CF..ansbin.,
    predictor = fit$prediction,
    quiet = TRUE
  )))
}

row_for <- function(label, adaptive_term, fit, baseline) {
  alpha <- fit$model$alpha
  fit_auc <- auc_for(fit)
  baseline_auc <- auc_for(baseline)
  data.frame(
    model = label,
    adaptive_term = adaptive_term,
    alpha_count = length(alpha),
    optimized_alpha = if (length(alpha) == 1L) as.numeric(alpha) else NA_real_,
    loglike = fit$loglike,
    delta_loglike_vs_no_alpha = fit$loglike - baseline$loglike,
    mcfadden_r2 = fit$r2,
    mcfadden_r2_gain_vs_no_alpha = fit$r2 - baseline$r2,
    auc = fit_auc,
    auc_gain_vs_no_alpha = fit_auc - baseline_auc,
    stringsAsFactors = FALSE
  )
}

cat("[1/5] Preparing largeraw sample\n")
val <- prepare_largeraw_sample()

cat("[2/5] Fitting fixed no-alpha baseline\n")
no_alpha_start <- stats::setNames(rep(0, length(beta_alpha_terms)),
                                  paste0("beta:", beta_alpha_terms))
no_alpha <- fit_online(
  label = "no_alpha_all_four_terms_fixed_at_zero",
  beta_terms = beta_alpha_terms,
  alpha_start = no_alpha_start,
  lower = no_alpha_start,
  upper = no_alpha_start
)
no_alpha_auc <- auc_for(no_alpha)
cat("No-alpha McFadden R2:", no_alpha$r2, "\n")
cat("No-alpha AUC:", no_alpha_auc, "\n")
cat("No-alpha loglike:", no_alpha$loglike, "\n")

cat("[3/5] Fitting one-alpha models\n")
single_fits <- list()
for (term in beta_alpha_terms) {
  alpha_name <- paste0("beta:", term)
  start <- stats::setNames(alpha_start_value, alpha_name)
  lower <- stats::setNames(alpha_lower, alpha_name)
  upper <- stats::setNames(alpha_upper, alpha_name)
  single_fits[[term]] <- fit_online(
    label = paste0("single_alpha_", term),
    beta_terms = term,
    alpha_start = start,
    lower = lower,
    upper = upper
  )
}

cat("[4/5] Fitting all-four-alpha model\n")
all_alpha_start <- stats::setNames(rep(alpha_start_value, length(beta_alpha_terms)),
                                   paste0("beta:", beta_alpha_terms))
all_alpha_lower <- stats::setNames(rep(alpha_lower, length(beta_alpha_terms)),
                                   paste0("beta:", beta_alpha_terms))
all_alpha_upper <- stats::setNames(rep(alpha_upper, length(beta_alpha_terms)),
                                   paste0("beta:", beta_alpha_terms))
all_four_fit <- fit_online(
  label = "all_four_alphas",
  beta_terms = beta_alpha_terms,
  alpha_start = all_alpha_start,
  lower = all_alpha_lower,
  upper = all_alpha_upper
)

cat("[5/5] Writing comparison outputs\n")
baseline_row <- data.frame(
  model = "no_alpha",
  adaptive_term = "none",
  alpha_count = length(no_alpha$model$alpha),
  optimized_alpha = NA_real_,
  loglike = no_alpha$loglike,
  delta_loglike_vs_no_alpha = 0,
  mcfadden_r2 = no_alpha$r2,
  mcfadden_r2_gain_vs_no_alpha = 0,
  auc = no_alpha_auc,
  auc_gain_vs_no_alpha = 0,
  stringsAsFactors = FALSE
)
comparison <- do.call(rbind, c(
  list(baseline_row),
  lapply(names(single_fits), function(term) {
    row_for(
      label = paste0("single_alpha_", sub("KC\\.\\.Default\\.$", "", term)),
      adaptive_term = term,
      fit = single_fits[[term]],
      baseline = no_alpha
    )
  }),
  list(row_for(
    label = "all_four_alphas",
    adaptive_term = paste(beta_alpha_terms, collapse = ";"),
    fit = all_four_fit,
    baseline = no_alpha
  ))
))

comparison_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-single-alpha-comparison.csv"
)
write.csv(comparison, comparison_path, row.names = FALSE)

alpha_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-single-alpha-values.csv"
)
alpha_values <- do.call(rbind, c(lapply(names(single_fits), function(term) {
  alpha <- single_fits[[term]]$model$alpha
  data.frame(
    model = paste0("single_alpha_", sub("KC\\.\\.Default\\.$", "", term)),
    adaptive_term = term,
    alpha_name = names(alpha),
    optimized_alpha = as.numeric(alpha),
    stringsAsFactors = FALSE
  )
}), list({
  alpha <- all_four_fit$model$alpha
  data.frame(
    model = "all_four_alphas",
    adaptive_term = sub("^beta:", "", names(alpha)),
    alpha_name = names(alpha),
    optimized_alpha = as.numeric(alpha),
    stringsAsFactors = FALSE
  )
})))
write.csv(alpha_values, alpha_path, row.names = FALSE)

plot_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-single-alpha-gains.png"
)
plot_rows <- comparison[comparison$model != "no_alpha", ]
plot_rows$term <- ifelse(
  plot_rows$model == "all_four_alphas",
  "all four",
  sub("KC\\.\\.Default\\.$", "", plot_rows$adaptive_term)
)
plot_long <- rbind(
  data.frame(term = plot_rows$term,
             metric = "McFadden R2 gain",
             value = plot_rows$mcfadden_r2_gain_vs_no_alpha,
             stringsAsFactors = FALSE),
  data.frame(term = plot_rows$term,
             metric = "AUC gain",
             value = plot_rows$auc_gain_vs_no_alpha,
             stringsAsFactors = FALSE),
  data.frame(term = plot_rows$term,
             metric = "Log-likelihood gain",
             value = plot_rows$delta_loglike_vs_no_alpha,
             stringsAsFactors = FALSE)
)
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package ggplot2 is required for the gains plot.", call. = FALSE)
}
gain_plot <- ggplot2::ggplot(
  plot_long,
  ggplot2::aes(x = stats::reorder(term, value), y = value)
) +
  ggplot2::geom_col(fill = "#4C78A8") +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~metric, scales = "free_x", ncol = 1) +
  ggplot2::labs(x = "Adaptive coefficient set", y = "Gain vs no-alpha") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
ggplot2::ggsave(plot_path, gain_plot, width = 8, height = 8, dpi = 150)

cat("\nSingle-alpha and all-four-alpha comparison:\n")
print(comparison)
cat("\nComparison CSV:", normalizePath(comparison_path, winslash = "/"), "\n")
cat("Alpha values CSV:", normalizePath(alpha_path, winslash = "/"), "\n")
cat("Gains PNG:", normalizePath(plot_path, winslash = "/"), "\n")
