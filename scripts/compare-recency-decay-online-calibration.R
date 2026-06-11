script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
if (is.na(script_file) || !nzchar(script_file)) {
  script_file <- file.path("scripts", "compare-recency-decay-online-calibration.R")
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

online_calibration_options <- list(maxit = 50, factr = 1e7)

val <- prepare_largeraw_sample()

model_specs <- list(
  rpfa = list(
    label = "Recent Performance Factors Analysis",
    args = list(
      data = val,
      interc = TRUE,
      components = c(
        "Anon.Student.Id",
        "KC..Default.",
        "KC..Default.",
        "KC..Default."
      ),
      features = c("intercept", "intercept", "propdec2", "linefail"),
      fixedpars = c(0.372666739924378)
    )
  ),
  logitdec_recency = list(
    label = "Recency tracing with logitdec",
    args = list(
      data = val,
      interc = TRUE,
      components = c(
        "Anon.Student.Id",
        "KC..Default.",
        "KC..Default.",
        "KC..Default."
      ),
      features = c("intercept", "intercept", "logitdec", "recency"),
      fixedpars = c(.9, .5)
    )
  ),
  logitdec_transfer = list(
    label = "Recency tracing with transfer from cluster",
    args = list(
      data = val,
      interc = TRUE,
      components = c(
        "Anon.Student.Id",
        "KC..Default.",
        "KC..Default.",
        "KC..Default.",
        "KC..Cluster."
      ),
      features = c("intercept", "intercept", "logitdec", "recency", "logitdec"),
      fixedpars = c(.9, .5, .5)
    )
  ),
  ppe = list(
    label = "Performance Prediction Equation",
    args = list(
      data = val,
      interc = TRUE,
      components = c(
        "Anon.Student.Id",
        "KC..Default.",
        "KC..Default.",
        "KC..Default."
      ),
      features = c("intercept", "intercept", "ppe", "logitdec"),
      fixedpars = c(0.3491901, 0.2045801, 1e-05, 0.9734477, 0.4443027)
    )
  ),
  base4 = list(
    label = "base4",
    args = list(
      data = val,
      interc = TRUE,
      components = c(
        "Anon.Student.Id",
        "KC..Default.",
        "KC..Default.",
        "KC..Default."
      ),
      features = c("intercept", "intercept", "base4", "logitdec"),
      fixedpars = c(0.1890747, 0.6309054, 0.05471752, .5, 0.2160748)
    )
  )
)

fit_one <- function(spec, model_name) {
  args <- spec$args
  args$verbose <- TRUE
  if (identical(model_name, "OnlineCalibration")) {
    if (!is.null(spec$new_args)) {
      for (name in names(spec$new_args)) {
        if (is.null(spec$new_args[[name]])) {
          args[[name]] <- NULL
        } else {
          args[[name]] <- spec$new_args[[name]]
        }
      }
    }
    args$model <- "online_calibration"
    args$model_options <- online_calibration_options
  }
  started <- proc.time()[["elapsed"]]
  fit <- do.call(LKT, args)
  elapsed <- proc.time()[["elapsed"]] - started
  list(fit = fit, elapsed = elapsed)
}

extract_nonlinear_parameters <- function(fit) {
  if (is.null(fit$optimizedpars) || length(fit$optimizedpars) == 1L &&
      is.na(fit$optimizedpars)) {
    return(data.frame(parameter = character(), value = numeric()))
  }
  if (!is.null(fit$optimizedpars$par)) {
    return(data.frame(
      parameter = names(fit$optimizedpars$par),
      value = as.numeric(fit$optimizedpars$par),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(parameter = character(), value = numeric())
}

online_parameters <- function(fit) {
  if (!identical(fit$model_name, "OnlineCalibration")) {
    return(data.frame(parameter = character(), value = character()))
  }
  data.frame(
    parameter = c("alpha", "optimizer_convergence", "optimizer_value"),
    value = c(
      as.character(fit$model$alpha),
      if (!is.null(fit$model$optimizer)) {
        as.character(fit$model$optimizer$convergence)
      } else {
        NA_character_
      },
      if (!is.null(fit$model$optimizer)) {
        as.character(fit$model$optimizer$value)
      } else {
        NA_character_
      }
    ),
    stringsAsFactors = FALSE
  )
}

format_number <- function(x, digits = 6) {
  ifelse(is.na(x), "", formatC(x, digits = digits, format = "f"))
}

markdown_table <- function(df) {
  if (nrow(df) == 0L) {
    return("_No rows._")
  }
  header <- paste(names(df), collapse = " | ")
  divider <- paste(rep("---", length(df)), collapse = " | ")
  body <- apply(df, 1, function(row) paste(row, collapse = " | "))
  paste(c(header, divider, body), collapse = "\n")
}

all_results <- list()
summary_rows <- list()
parameter_sections <- character()

for (spec_id in names(model_specs)) {
  spec <- model_specs[[spec_id]]
  cat("\n==============================\n")
  cat(spec$label, "\n")
  cat("==============================\n")

  cat("\nOLD MODEL PARAMETERS:", spec$label, "\n")
  old_result <- fit_one(spec, "LibLinear")
  print(old_result$fit$coefs)
  old_nonlinear <- extract_nonlinear_parameters(old_result$fit)
  if (nrow(old_nonlinear) > 0L) {
    cat("old nonlinear optimized parameters:\n")
    print(old_nonlinear)
  }

  cat("\nNEW ONLINE BETA UPDATE PARAMETERS:", spec$label, "\n")
  new_result <- fit_one(spec, "OnlineCalibration")
  print(online_parameters(new_result$fit))
  cat("new initial beta coefficients:\n")
  print(new_result$fit$coefs)
  new_nonlinear <- extract_nonlinear_parameters(new_result$fit)
  if (nrow(new_nonlinear) > 0L) {
    cat("new nonlinear optimized parameters:\n")
    print(new_nonlinear)
  }

  all_results[[spec_id]] <- list(old = old_result, new = new_result)
  summary_rows[[length(summary_rows) + 1L]] <- data.frame(
    model = spec$label,
    old_loglike = old_result$fit$loglike,
    new_loglike = new_result$fit$loglike,
    loglike_delta_new_minus_old = new_result$fit$loglike - old_result$fit$loglike,
    old_r2 = old_result$fit$r2,
    new_r2 = new_result$fit$r2,
    r2_delta_new_minus_old = new_result$fit$r2 - old_result$fit$r2,
    old_elapsed_sec = old_result$elapsed,
    new_elapsed_sec = new_result$elapsed,
    old_coef_count = nrow(old_result$fit$coefs),
    new_coef_count = nrow(new_result$fit$coefs),
    online_alpha = new_result$fit$model$alpha,
    online_optimizer_convergence = if (!is.null(new_result$fit$model$optimizer)) {
      new_result$fit$model$optimizer$convergence
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )

  parameter_sections <- c(
    parameter_sections,
    paste0("## ", spec$label),
    "",
    "### Old Model Coefficients",
    "",
    "```",
    paste(capture.output(print(old_result$fit$coefs)), collapse = "\n"),
    "```",
    "",
    "### Old Nonlinear Optimized Parameters",
    "",
    "```",
    paste(capture.output(print(old_nonlinear)), collapse = "\n"),
    "```",
    "",
    "### New Online Beta Update Parameters",
    "",
    "```",
    paste(capture.output(print(online_parameters(new_result$fit))), collapse = "\n"),
    "```",
    "",
    "### New Initial Beta Coefficients",
    "",
    "```",
    paste(capture.output(print(new_result$fit$coefs)), collapse = "\n"),
    "```",
    "",
    "### New Nonlinear Optimized Parameters",
    "",
    "```",
    paste(capture.output(print(new_nonlinear)), collapse = "\n"),
    "```",
    ""
  )
}

summary_table <- do.call(rbind, summary_rows)
numeric_cols <- c(
  "old_loglike",
  "new_loglike",
  "loglike_delta_new_minus_old",
  "old_r2",
  "new_r2",
  "r2_delta_new_minus_old",
  "old_elapsed_sec",
  "new_elapsed_sec",
  "online_alpha"
)
summary_for_md <- summary_table
for (col in numeric_cols) {
  summary_for_md[[col]] <- format_number(summary_for_md[[col]])
}
summary_for_md$online_optimizer_convergence <-
  as.character(summary_for_md$online_optimizer_convergence)

comparison_file <- file.path(
  report_dir,
  "examples-recency-decay-online-calibration-comparison.md"
)
writeLines(c(
  "# Recency and Decay: LibLinear vs Online Beta Update",
  "",
  paste("Generated:", as.character(Sys.time())),
  "",
  "Online calibration options:",
  "",
  "```",
  paste(capture.output(print(online_calibration_options)), collapse = "\n"),
  "```",
  "",
  "## Fit Comparison",
  "",
  markdown_table(summary_for_md),
  "",
  parameter_sections
), comparison_file)

cat("\nComparison Markdown:", normalizePath(comparison_file, winslash = "/"), "\n")
