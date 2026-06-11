script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
if (is.na(script_file) || !nzchar(script_file)) {
  script_file <- file.path("scripts", "test-online-adaptive-generalized.R")
}
root <- normalizePath(file.path(dirname(script_file), ".."),
                      winslash = "/", mustWork = TRUE)
vignette_dir <- file.path(root, "vignettes")

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

n_rows <- as.integer(get_arg("rows", "220"))
maxit <- as.integer(get_arg("maxit", "2"))
tolerance <- as.numeric(get_arg("tolerance", "1e-8"))

val <- prepare_largeraw_sample()
val <- val[seq_len(min(n_rows, nrow(val))), ]

base <- LKT(
  data = val,
  interc = TRUE,
  dualfit = FALSE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default."),
  features = c("logitdec", "logsuc", "recency"),
  fixedpars = c(0.98, 0.24),
  model = "liblinear"
)

zero <- LKT(
  data = val,
  interc = TRUE,
  dualfit = FALSE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default."),
  features = c("logitdec", "logsuc", "recency"),
  fixedpars = c(0.98, 0.24),
  model = "online_adaptive",
  model_options = list(
    online_mode = "alpha_only",
    beta_alpha_terms = c("recencyKC..Default.", "logsucKC..Default."),
    nonlinear_alpha_terms = c("recency|KC..Default.|para"),
    alpha_start = c(0, 0, 0),
    alpha_lower = c(0, 0, 0),
    alpha_upper = c(0, 0, 0),
    require_native = FALSE,
    maxit = 1
  )
)

zero_diff <- zero$loglike - base$loglike
zero_pred_diff <- max(abs(zero$prediction - base$prediction))
if (abs(zero_diff) > tolerance || zero_pred_diff > tolerance) {
  stop("generalized alpha-zero invariant failed: loglike diff = ",
       zero_diff, ", prediction diff = ", zero_pred_diff, call. = FALSE)
}

parb_model <- LKT(
  data = val,
  interc = TRUE,
  dualfit = FALSE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default."),
  features = c("logitdec", "logsuc", "base2"),
  fixedpars = c(0.98, 0.2, 0.4),
  model = "online_adaptive",
  model_options = list(
    online_mode = "alpha_only",
    beta_alpha_terms = c("logsucKC..Default."),
    nonlinear_alpha_terms = c("base2|KC..Default.|parb"),
    alpha_start = c(0.01, 0.01),
    alpha_lower = -0.1,
    alpha_upper = 0.1,
    require_native = FALSE,
    maxit = maxit
  )
)

if (!"nonlinear:base2|KC..Default.|parb" %in% names(parb_model$model$alpha)) {
  stop("generalized nonlinear parb alpha was not returned.", call. = FALSE)
}
if (!"final_nonlinear:base2|KC..Default.|parb" %in%
    names(parb_model$model$final_states)) {
  stop("generalized nonlinear parb final states were not returned.",
       call. = FALSE)
}

multi_slot_model <- LKT(
  data = val,
  interc = TRUE,
  dualfit = FALSE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default."),
  features = c("logitdec", "logsuc", "base4"),
  fixedpars = c(0.98, 0.2, 0.4, 0.1, 0.3),
  model = "online_adaptive",
  model_options = list(
    online_mode = "alpha_only",
    beta_alpha_terms = character(0),
    nonlinear_alpha_terms = c(
      "base4|KC..Default.|para",
      "base4|KC..Default.|parb",
      "base4|KC..Default.|parc",
      "base4|KC..Default.|pard"
    ),
    alpha_start = rep(0.001, 4),
    alpha_lower = rep(0.001, 4),
    alpha_upper = rep(0.001, 4),
    require_native = FALSE,
    maxit = 1
  )
)

multi_slot_alpha_names <- paste0(
  "nonlinear:base4|KC..Default.|",
  c("para", "parb", "parc", "pard"))
missing_multi_slot_alpha <- setdiff(
  multi_slot_alpha_names, names(multi_slot_model$model$alpha))
if (length(missing_multi_slot_alpha) > 0L) {
  stop("generalized multi-slot nonlinear alphas were not returned: ",
       paste(missing_multi_slot_alpha, collapse = ", "), call. = FALSE)
}
missing_multi_slot_states <- setdiff(
  paste0("final_", sub("^nonlinear:", "nonlinear:", multi_slot_alpha_names)),
  names(multi_slot_model$model$final_states))
if (length(missing_multi_slot_states) > 0L) {
  stop("generalized multi-slot nonlinear final states were not returned: ",
       paste(missing_multi_slot_states, collapse = ", "), call. = FALSE)
}

native_error <- tryCatch({
  LKT(
    data = val,
    interc = TRUE,
    dualfit = FALSE,
    components = c("Anon.Student.Id", "KC..Default.", "KC..Default."),
    features = c("logitdec", "logsuc", "base2"),
    fixedpars = c(0.98, 0.2, 0.4),
    model = "online_adaptive",
    model_options = list(
      online_mode = "alpha_only",
      beta_alpha_terms = c("logitdecAnon.Student.Id"),
      nonlinear_alpha_terms = c("base2|KC..Default.|parb"),
      alpha_lower = -1,
      alpha_upper = 1,
      require_native = TRUE,
      maxit = 1
    )
  )
  ""
}, error = function(e) conditionMessage(e))

if (!grepl("require_native = TRUE was requested", native_error, fixed = TRUE)) {
  stop("unsupported generalized native request did not fail clearly.",
       call. = FALSE)
}

native_beta <- LKT(
  data = val,
  interc = TRUE,
  dualfit = FALSE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default."),
  features = c("logitdec", "logsuc", "recency"),
  fixedpars = c(0.98, 0.24),
  model = "online_adaptive",
  model_options = list(
    online_mode = "alpha_only",
    beta_alpha_terms = c("logitdecAnon.Student.Id"),
    alpha_start = c(0.01),
    alpha_lower = -0.1,
    alpha_upper = 0.1,
    require_native = TRUE,
    maxit = maxit
  )
)

if (!isTRUE(native_beta$model$native_available)) {
  stop("generalized beta-only native evaluator was not reported as available.",
       call. = FALSE)
}
if (!"beta:logitdecAnon.Student.Id" %in% names(native_beta$model$alpha)) {
  stop("generalized beta-only native alpha was not returned.", call. = FALSE)
}

cat("generalized online_adaptive short checks passed\n")
cat("zero loglike diff:", zero_diff, "\n")
cat("zero prediction diff:", zero_pred_diff, "\n")
cat("parb alpha:\n")
print(parb_model$model$alpha)
cat("multi-slot nonlinear alpha:\n")
print(multi_slot_model$model$alpha)
cat("native beta alpha:\n")
print(native_beta$model$alpha)
