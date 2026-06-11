script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
if (is.na(script_file) || !nzchar(script_file)) {
  script_file <- file.path("scripts", "validate-fast-online-simple-adaptive.R")
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
maxit <- as.integer(get_arg("maxit", "5"))
run_optim <- !identical(tolower(get_arg("optim", "true")), "false")
require_native <- identical(tolower(get_arg("require-native", "false")), "true")
gradient_check_subjects <- as.integer(get_arg("gradient-check-subjects", "0"))

val <- prepare_largeraw_sample()
base <- LKT(
  data = val,
  interc = TRUE,
  dualfit = FALSE,
  factrv = 1e11,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default."),
  features = c("logitdec", "logsuc", "recency"),
  fixedpars = c(0.98, 0.24)
)

input <- LKTOnlineSimpleAdaptiveInput(base, val)
par_zero <- c(input$beta_start, alpha_recency = 0, alpha_logsuc = 0)
par_grid <- c(input$beta_start, alpha_recency = 0.03, alpha_logsuc = 0.05)

zero <- LKTOnlineSimpleAdaptiveEval(
  par_zero,
  input,
  return_details = TRUE,
  require_native = require_native
)
grid <- LKTOnlineSimpleAdaptiveEval(
  par_grid,
  input,
  return_details = TRUE,
  require_native = require_native
)

zero_diff <- zero$loglike - base$loglike
zero_pred_diff <- max(abs(zero$pred - base$prediction))
if (abs(zero_diff) > 1e-6 || zero_pred_diff > 1e-12) {
  stop("alpha-zero validation failed.", call. = FALSE)
}

rows <- list(
  data.frame(
    model = "LibLinear",
    loglike = base$loglike,
    delta_loglike = 0,
    r2 = base$r2,
    delta_r2 = 0,
    stringsAsFactors = FALSE
  ),
  data.frame(
    model = "Fast online grid start",
    loglike = grid$loglike,
    delta_loglike = grid$loglike - base$loglike,
    r2 = 1 - grid$loglike / (base$loglike / (1 - base$r2)),
    delta_r2 = (1 - grid$loglike / (base$loglike / (1 - base$r2))) - base$r2,
    stringsAsFactors = FALSE
  )
)

optimized <- NULL
optimized_alpha <- NULL
optimized_decay <- NULL
elapsed <- NA_real_
elapsed_alpha <- NA_real_
elapsed_decay <- NA_real_
if (run_optim) {
  started_alpha <- proc.time()[["elapsed"]]
  optimized_alpha <- LKTOptimizeOnlineSimpleAdaptiveAlpha(
    input,
    start = c(alpha_recency = 0.03, alpha_logsuc = 0.05),
    control = list(maxit = maxit, factr = 1e7),
    require_native = require_native
  )
  elapsed_alpha <- proc.time()[["elapsed"]] - started_alpha
  rows[[length(rows) + 1L]] <- data.frame(
    model = paste0("Fast online alpha-only optimized maxit=", maxit),
    loglike = optimized_alpha$loglike,
    delta_loglike = optimized_alpha$delta_loglike,
    r2 = optimized_alpha$r2,
    delta_r2 = optimized_alpha$delta_r2,
    stringsAsFactors = FALSE
  )

  started_decay <- proc.time()[["elapsed"]]
  optimized_decay <- LKTOptimizeOnlineSimpleAdaptiveDecayAlpha(
    input,
    start = c(alpha_recency = 0.03, alpha_logsuc = 0.05,
              alpha_decay = 0.01),
    control = list(maxit = maxit, factr = 1e7),
    require_native = require_native
  )
  elapsed_decay <- proc.time()[["elapsed"]] - started_decay
  rows[[length(rows) + 1L]] <- data.frame(
    model = paste0("Fast online decay-alpha optimized maxit=", maxit),
    loglike = optimized_decay$loglike,
    delta_loglike = optimized_decay$delta_loglike,
    r2 = optimized_decay$r2,
    delta_r2 = optimized_decay$delta_r2,
    stringsAsFactors = FALSE
  )

  started <- proc.time()[["elapsed"]]
  optimized <- LKTOptimizeOnlineSimpleAdaptive(
    input,
    start = par_grid,
    control = list(maxit = maxit, factr = 1e7),
    require_native = require_native
  )
  elapsed <- proc.time()[["elapsed"]] - started
  rows[[length(rows) + 1L]] <- data.frame(
    model = paste0("Fast online optimized maxit=", maxit),
    loglike = optimized$loglike,
    delta_loglike = optimized$delta_loglike,
    r2 = optimized$r2,
    delta_r2 = optimized$delta_r2,
    stringsAsFactors = FALSE
  )
}

summary_table <- do.call(rbind, rows)
summary_path <- file.path(report_dir, "fast-online-simple-adaptive-validation.csv")
write.csv(summary_table, summary_path, row.names = FALSE)

cat("native loaded:",
    is.loaded("C_lkt_online_simple_adaptive_eval", PACKAGE = "LKT"), "\n")
cat("require native:", require_native, "\n")
cat("alpha-zero loglike diff:", zero_diff, "\n")
cat("alpha-zero max pred diff:", zero_pred_diff, "\n")
cat("\nValidation summary:\n")
print(summary_table, row.names = FALSE)

if (!is.null(optimized)) {
  cat("\nAlpha-only optimizer elapsed seconds:", elapsed_alpha, "\n")
  cat("Alpha-only optimizer convergence:", optimized_alpha$optimizer$convergence, "\n")
  cat("Alpha-only optimizer parameters:\n")
  print(optimized_alpha$par)
  cat("\nDecay-alpha optimizer elapsed seconds:", elapsed_decay, "\n")
  cat("Decay-alpha optimizer convergence:", optimized_decay$optimizer$convergence, "\n")
  cat("Decay-alpha optimizer parameters:\n")
  print(optimized_decay$par)
  cat("\nOptimizer elapsed seconds:", elapsed, "\n")
  cat("Optimizer convergence:", optimized$optimizer$convergence, "\n")
  cat("Optimizer parameters:\n")
  print(optimized$par)
}

cat("\nCSV:", normalizePath(summary_path, winslash = "/"), "\n")

if (gradient_check_subjects > 0L) {
  cat("\nGradient check using first", gradient_check_subjects, "subjects:\n")
  val_gradient <- prepare_largeraw_sample()
  keep_subjects <- unique(val_gradient$Anon.Student.Id)[seq_len(gradient_check_subjects)]
  val_gradient <- val_gradient[val_gradient$Anon.Student.Id %in% keep_subjects, ]
  base_gradient <- LKT(
    data = val_gradient,
    interc = TRUE,
    dualfit = FALSE,
    factrv = 1e11,
    components = c("Anon.Student.Id", "KC..Default.", "KC..Default."),
    features = c("logitdec", "logsuc", "recency"),
    fixedpars = c(0.98, 0.24)
  )
  input_gradient <- LKTOnlineSimpleAdaptiveInput(base_gradient, val_gradient)
  par_gradient <- c(
    input_gradient$beta_start,
    alpha_recency = 0.03,
    alpha_logsuc = 0.05
  )
  objective <- function(par) {
    as.numeric(LKTOnlineSimpleAdaptiveEval(par, input_gradient))
  }
  analytic <- LKTOnlineSimpleAdaptiveGradient(par_gradient, input_gradient)
  finite_difference <- numeric(length(par_gradient))
  step <- 1e-5
  for (i in seq_along(par_gradient)) {
    up <- par_gradient
    down <- par_gradient
    up[i] <- up[i] + step
    down[i] <- down[i] - step
    finite_difference[i] <- (objective(up) - objective(down)) / (2 * step)
  }
  max_abs_diff <- max(abs(analytic - finite_difference))
  max_relative_diff <- max(abs(analytic - finite_difference) /
                             pmax(1, abs(finite_difference)))
  cat("max abs diff:", max_abs_diff, "\n")
  cat("max relative diff:", max_relative_diff, "\n")
  if (max_abs_diff > 1e-4 || max_relative_diff > 1e-5) {
    stop("gradient validation failed.", call. = FALSE)
  }
}
