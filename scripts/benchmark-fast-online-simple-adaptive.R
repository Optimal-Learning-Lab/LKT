script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
if (is.na(script_file) || !nzchar(script_file)) {
  script_file <- file.path("scripts", "benchmark-fast-online-simple-adaptive.R")
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

eval_reps <- as.integer(get_arg("eval-reps", "200"))
grad_reps <- as.integer(get_arg("grad-reps", "100"))
pair_reps <- as.integer(get_arg("pair-reps", "100"))
optim_maxit <- as.integer(get_arg("optim-maxit", "2"))

if (!is.loaded("C_lkt_online_simple_adaptive_eval", PACKAGE = "LKT") ||
    !is.loaded("C_lkt_online_simple_adaptive_gradient", PACKAGE = "LKT") ||
    !is.loaded("C_lkt_online_simple_adaptive_value_gradient", PACKAGE = "LKT")) {
  stop(
    "Native fast online simple-adaptive symbols are not loaded. ",
    "Build and install LKT with compiled native code before benchmarking.",
    call. = FALSE
  )
}

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
par <- c(input$beta_start, alpha_recency = 0.03, alpha_logsuc = 0.05)

eval_once <- function() {
  LKTOnlineSimpleAdaptiveEval(par, input, require_native = TRUE)
}
grad_once <- function() {
  LKTOnlineSimpleAdaptiveGradient(par, input, require_native = TRUE)
}
pair_once <- function() {
  lkt_online_simple_adaptive_value_gradient_native(
    par,
    input,
    require_native = TRUE
  )
}

eval_time <- system.time({
  for (i in seq_len(eval_reps)) {
    eval_once()
  }
})[["elapsed"]]

grad_time <- system.time({
  for (i in seq_len(grad_reps)) {
    grad_once()
  }
})[["elapsed"]]

pair_time <- system.time({
  for (i in seq_len(pair_reps)) {
    pair_once()
  }
})[["elapsed"]]

optim_time <- NA_real_
optimized <- NULL
if (optim_maxit > 0L) {
  optim_time <- system.time({
    optimized <- LKTOptimizeOnlineSimpleAdaptive(
      input,
      start = par,
      control = list(maxit = optim_maxit, factr = 1e7),
      require_native = TRUE
    )
  })[["elapsed"]]
}

benchmark <- data.frame(
  metric = c("eval_total_seconds",
             "eval_seconds_per_call",
             "gradient_total_seconds",
             "gradient_seconds_per_call",
             "value_gradient_total_seconds",
             "value_gradient_seconds_per_call",
             "optimizer_total_seconds"),
  value = c(eval_time,
            eval_time / eval_reps,
            grad_time,
            grad_time / grad_reps,
            pair_time,
            pair_time / pair_reps,
            optim_time),
  stringsAsFactors = FALSE
)

summary <- data.frame(
  model = c("LibLinear", paste0("Fast online optimized maxit=", optim_maxit)),
  loglike = c(base$loglike, if (is.null(optimized)) NA_real_ else optimized$loglike),
  delta_loglike = c(0, if (is.null(optimized)) NA_real_ else optimized$delta_loglike),
  r2 = c(base$r2, if (is.null(optimized)) NA_real_ else optimized$r2),
  delta_r2 = c(0, if (is.null(optimized)) NA_real_ else optimized$delta_r2),
  stringsAsFactors = FALSE
)

benchmark_path <- file.path(report_dir, "fast-online-simple-adaptive-benchmark.csv")
summary_path <- file.path(report_dir, "fast-online-simple-adaptive-benchmark-summary.csv")
write.csv(benchmark, benchmark_path, row.names = FALSE)
write.csv(summary, summary_path, row.names = FALSE)

cat("Native benchmark:\n")
print(benchmark, row.names = FALSE)
cat("\nFit summary:\n")
print(summary, row.names = FALSE)
if (!is.null(optimized)) {
  cat("\nOptimizer parameters:\n")
  print(optimized$par)
}
cat("\nBenchmark CSV:", normalizePath(benchmark_path, winslash = "/"), "\n")
cat("Summary CSV:", normalizePath(summary_path, winslash = "/"), "\n")
