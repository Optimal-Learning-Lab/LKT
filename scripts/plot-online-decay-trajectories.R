script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
if (is.na(script_file) || !nzchar(script_file)) {
  script_file <- file.path("scripts", "plot-online-decay-trajectories.R")
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
optim_trace <- as.integer(get_arg("optim-trace", "1"))
optim_report <- as.integer(get_arg("optim-report", "1"))
clip_epsilon <- as.numeric(get_arg("clip-epsilon", "0.000001"))
denom_epsilon <- as.numeric(get_arg("denom-epsilon", "0.000001"))
alpha_start_value <- as.numeric(get_arg("alpha-start", "0.01"))
alpha_lower <- as.numeric(get_arg("alpha-lower", "-1"))
alpha_upper <- as.numeric(get_arg("alpha-upper", "1"))
nonlinear_lower <- as.numeric(get_arg("nonlinear-lower", "0.00001"))
nonlinear_upper <- as.numeric(get_arg("nonlinear-upper", "0.99999"))
line_width <- as.numeric(get_arg("line-width", "0.08"))
line_alpha <- as.numeric(get_arg("line-alpha", "0.45"))
smooth <- identical(tolower(get_arg("smooth", "false")), "true")
smooth_degree <- as.integer(get_arg("smooth-degree", "2"))

beta_alpha_terms <- c(
  "linesucKC..Default.",
  "recencyKC..Default.",
  "linefailKC..Default.",
  "ppesKC..Default."
)
nonlinear_alpha_terms <- c(
  "recency|KC..Default.|para",
  "ppes|KC..Default.|para",
  "ppes|KC..Default.|parb",
  "ppes|KC..Default.|parc",
  "ppes|KC..Default.|pard"
)
alpha_names <- c(
  paste0("beta:", beta_alpha_terms),
  paste0("nonlinear:", nonlinear_alpha_terms)
)
alpha_start <- stats::setNames(rep(alpha_start_value, length(alpha_names)),
                               alpha_names)

cat("[1/6] Preparing largeraw sample\n")
val <- prepare_largeraw_sample()

cat("[2/6] Fitting fixed Default-KC base model:",
    paste(c("linesuc", "recency", "linefail", "ppes"), collapse = ", "),
    "\n")
base <- LKT(
  data = val,
  interc = TRUE,
  dualfit = FALSE,
  factrv = 1e11,
  components = c(
    "KC..Default.",
    "KC..Default.",
    "KC..Default.",
    "KC..Default."
  ),
  features = c("linesuc", "recency", "linefail", "ppes"),
  fixedpars = c(
    0.5781309,
    0.6731491, 0.1982123, 0.3366958, 0.7954989
  )
)

cat("[3/6] Optimizing four coefficient alphas plus five nonlinear alphas\n")
cat("      maxit =", maxit,
    "alpha_start =", alpha_start_value,
    "alpha_bounds = [", alpha_lower, ", ", alpha_upper, "]\n",
    sep = "")
cat("      nonlinear parameter bounds = [", nonlinear_lower, ", ",
    nonlinear_upper, "]\n", sep = "")
cat("      beta alpha terms:\n")
print(beta_alpha_terms)
cat("      nonlinear alpha terms:\n")
print(nonlinear_alpha_terms)
cat("      PPES nonlinear parameters included in alpha optimization\n")
optimized_online <- LKT(
  data = val,
  interc = TRUE,
  dualfit = FALSE,
  factrv = 1e11,
  components = c(
    "KC..Default.",
    "KC..Default.",
    "KC..Default.",
    "KC..Default."
  ),
  features = c("linesuc", "recency", "linefail", "ppes"),
  fixedpars = c(
    0.5781309,
    0.6731491, 0.1982123, 0.3366958, 0.7954989
  ),
  model = "online_adaptive",
  model_options = list(
    online_mode = "alpha_only",
    beta_alpha_terms = beta_alpha_terms,
    nonlinear_alpha_terms = nonlinear_alpha_terms,
    alpha_start = alpha_start,
    alpha_lower = alpha_lower,
    alpha_upper = alpha_upper,
    nonlinear_lower = nonlinear_lower,
    nonlinear_upper = nonlinear_upper,
    require_native = require_native,
    maxit = maxit,
    factr = 1e7,
    optim_trace = optim_trace,
    optim_report = optim_report
  )
)

registry <- optimized_online$model$registry
if (is.null(registry)) {
  stop("online_adaptive did not return a generalized registry.", call. = FALSE)
}
alpha <- optimized_online$model$alpha
cat("[4/6] Optimization complete; replaying participant trajectories\n")
base_auc <- as.numeric(pROC::auc(pROC::roc(
  response = val$CF..ansbin.,
  predictor = base$prediction,
  quiet = TRUE
)))
optimized_auc <- as.numeric(pROC::auc(pROC::roc(
  response = val$CF..ansbin.,
  predictor = optimized_online$prediction,
  quiet = TRUE
)))
cat("      optimized loglike =", optimized_online$loglike,
    "delta =", optimized_online$loglike - base$loglike, "\n")
cat("      base McFadden =", base$r2,
    "optimized McFadden =", optimized_online$r2,
    "delta =", optimized_online$r2 - base$r2, "\n")
cat("      base AUC =", base_auc,
    "optimized AUC =", optimized_auc,
    "delta =", optimized_auc - base_auc, "\n")
cat("      optimized alpha values:\n")
print(alpha)

make_state_row <- function(subject, trial, beta_state, nonlinear_state) {
  row <- c(subject = as.character(subject), trial = as.character(trial))
  if (length(beta_state) > 0L) {
    for (j in seq_along(beta_state)) {
      row[paste0("beta:", names(beta_state)[j])] <- as.character(beta_state[j])
    }
  }
  if (length(nonlinear_state) > 0L) {
    for (j in seq_along(nonlinear_state)) {
      row[paste0("nonlinear:", names(nonlinear_state)[j])] <-
        as.character(nonlinear_state[j])
    }
  }
  row
}

trajectory_rows <- vector("list", sum(registry$subject_end - registry$subject_start + 2L))
row_out <- 0L
replay_terms <- lapply(registry$nonlinear_terms, function(term) {
  if (identical(term$feature, "ppes")) {
    ppe_data <- lkt_prepare_ppe_static_context(
      term$prepared_data, term$prepared_data$index, term$component)
    ppe_context <- lkt_ppe_static_context_names(term$component)
    term$ppe_cor <- as.numeric(ppe_data$cor)
    term$ppe_tn <- as.numeric(ppe_data[[ppe_context[["tn"]]]])
    term$ppe_space <- as.numeric(ppe_data[[ppe_context[["space"]]]])
  }
  term
})

for (s in seq_along(registry$subject_start)) {
  subject_start <- registry$subject_start[s]
  rows <- subject_start:registry$subject_end[s]
  beta_state <- vapply(registry$beta_terms, `[[`, numeric(1L),
                        "beta_start")
  names(beta_state) <- vapply(registry$beta_terms, `[[`, character(1L),
                               "name")
  nonlinear_state <- unlist(lapply(replay_terms, function(term) {
    values <- as.numeric(term$pars_start[term$slots])
    names(values) <- paste(term$feature, term$component, term$slots, sep = "|")
    values
  }))

  row_out <- row_out + 1L
  trajectory_rows[[row_out]] <- make_state_row(
    registry$subject[s], 0L, beta_state, nonlinear_state)

  for (trial_i in seq_along(rows)) {
    row <- rows[trial_i]
    eta <- registry$eta_base[row]
    if (length(registry$beta_terms) > 0L) {
      for (j in seq_along(registry$beta_terms)) {
        term <- registry$beta_terms[[j]]
        eta <- eta + (beta_state[j] - term$beta_start) * term$x[row]
      }
    }
    if (length(registry$nonlinear_terms) > 0L) {
      for (j in seq_along(replay_terms)) {
        term <- replay_terms[[j]]
        if (identical(term$feature, "recency") &&
            identical(term$slots, "para")) {
          state_name <- paste(term$feature, term$component, "para", sep = "|")
          spacing_col <- paste0(term$component, "spacing")
          if (!spacing_col %in% names(registry$data)) {
            stop("Trajectory replay missing spacing column '", spacing_col, "'.",
                 call. = FALSE)
          }
          spacing <- registry$data[[spacing_col]][row]
          current_x <- if (spacing > 0) {
            spacing^(-nonlinear_state[state_name])
          } else {
            0
          }
        } else if (identical(term$feature, "ppes")) {
          para_name <- paste(term$feature, term$component, "para", sep = "|")
          parb_name <- paste(term$feature, term$component, "parb", sep = "|")
          parc_name <- paste(term$feature, term$component, "parc", sep = "|")
          pard_name <- paste(term$feature, term$component, "pard", sep = "|")
          tw <- ppetw(term$ppe_tn[subject_start:row],
                      nonlinear_state[pard_name])
          current_x <- term$ppe_cor[row]^nonlinear_state[para_name] *
            tw^-(nonlinear_state[parb_name] +
                   nonlinear_state[parc_name] * term$ppe_space[row])
        } else {
          stop("Trajectory replay does not support nonlinear feature '",
               term$feature, "'.", call. = FALSE)
        }
        eta <- eta + term$coefficient * (current_x - term$original_x[row])
      }
    }
    p <- pmin(pmax(stats::plogis(eta), clip_epsilon), 1 - clip_epsilon)
    err <- registry$y[row] - p

    if (length(registry$beta_terms) > 0L) {
      for (j in seq_along(registry$beta_terms)) {
        term <- registry$beta_terms[[j]]
        x <- term$x[row]
        beta_state[j] <- beta_state[j] +
          alpha[term$alpha_name] * x * err / (x^2 + denom_epsilon)
      }
    }
    if (length(registry$nonlinear_terms) > 0L) {
      for (j in seq_along(replay_terms)) {
        term <- replay_terms[[j]]
        for (slot in term$slots) {
          state_name <- paste(term$feature, term$component, slot, sep = "|")
          alpha_name <- paste0("nonlinear:", state_name)
          nonlinear_state[state_name] <- nonlinear_state[state_name] -
            alpha[alpha_name] * err
          nonlinear_state[state_name] <- min(
            max(nonlinear_state[state_name], nonlinear_lower),
            nonlinear_upper)
        }
      }
    }

    row_out <- row_out + 1L
    trajectory_rows[[row_out]] <- make_state_row(
      registry$subject[s], trial_i, beta_state, nonlinear_state)
  }
}

trajectory <- data.frame(do.call(rbind, trajectory_rows[seq_len(row_out)]),
                         stringsAsFactors = FALSE)
trajectory$trial <- as.integer(trajectory$trial)
parameter_columns <- setdiff(names(trajectory), c("subject", "trial"))
for (column in parameter_columns) {
  trajectory[[column]] <- as.numeric(trajectory[[column]])
}

trajectory_long <- do.call(rbind, lapply(parameter_columns, function(column) {
  data.frame(
    subject = trajectory$subject,
    trial = trajectory$trial,
    parameter = column,
    value = trajectory[[column]],
    stringsAsFactors = FALSE
  )
}))

if (isTRUE(smooth)) {
  cat("[5/6] Smoothing trajectories with polynomial degree", smooth_degree, "\n")
  smooth_one <- function(values, trials, degree) {
    if (length(unique(trials)) <= degree) {
      return(values)
    }
    fit <- stats::lm(values ~ stats::poly(trials, degree, raw = TRUE))
    as.numeric(stats::predict(fit, newdata = data.frame(trials = trials)))
  }
  split_key <- paste(trajectory_long$subject, trajectory_long$parameter, sep = "\r")
  trajectory_long$value_raw <- trajectory_long$value
  trajectory_long$value <- unsplit(
    lapply(split(trajectory_long, split_key), function(df) {
      smooth_one(df$value, df$trial, smooth_degree)
    }),
    split_key
  )
}

cat("[5/6] Writing trajectory CSVs and plot\n")
wide_path <- file.path(report_dir, "online-adaptive-default-kc-trajectories-wide.csv")
long_path <- file.path(report_dir, "online-adaptive-default-kc-trajectories-long.csv")
alpha_path <- file.path(report_dir, "online-adaptive-default-kc-alpha.csv")
final_values_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-final-parameter-values.csv"
)
final_cor_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-final-parameter-correlations.csv"
)
suffix <- if (isTRUE(smooth)) paste0("-poly", smooth_degree) else ""
combined_png <- file.path(
  report_dir,
  paste0("online-adaptive-default-kc-parameter-trajectories", suffix, ".png")
)
write.csv(trajectory, wide_path, row.names = FALSE)
write.csv(trajectory_long, long_path, row.names = FALSE)
write.csv(data.frame(alpha = names(alpha), value = as.numeric(alpha),
                     stringsAsFactors = FALSE),
          alpha_path, row.names = FALSE)

subject_counts <- aggregate(trial ~ subject, trajectory, max)
final_values <- merge(subject_counts, trajectory,
                      by = c("subject", "trial"), sort = FALSE)
final_parameter_values <- final_values[c("subject", parameter_columns)]
write.csv(final_parameter_values, final_values_path, row.names = FALSE)
final_parameter_correlations <- stats::cor(
  final_parameter_values[parameter_columns],
  use = "pairwise.complete.obs")
write.csv(final_parameter_correlations, final_cor_path)
names(subject_counts)[2L] <- "trials"

combined <- ggplot(
  trajectory_long,
  aes(x = trial, y = value, group = subject)
) +
  geom_line(alpha = line_alpha, linewidth = line_width,
            lineend = "round", linejoin = "round") +
  facet_wrap(~parameter, scales = "free_y") +
  labs(x = "Trial within participant", y = "Parameter value") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())
ggsave(combined_png, combined, width = 15, height = 10, dpi = 150)

cat("[6/6] Done\n")
cat("Default-KC online-adaptive loglike:", optimized_online$loglike, "\n")
cat("Default-KC online-adaptive delta loglike:",
    optimized_online$loglike - base$loglike, "\n")
cat("Default-KC online-adaptive delta McFadden:",
    optimized_online$r2 - base$r2, "\n")
cat("Default-KC online-adaptive delta AUC:",
    optimized_auc - base_auc, "\n")
cat("Default-KC base loglike:", base$loglike, "\n")
cat("Optimized coefficient and nonlinear alpha values:\n")
print(alpha)
cat("\nBase fixed nonlinear parameters:\n")
print(unique(as.data.frame(base$model_specification[[1]])[
  c("component", "feature", "para", "parb", "parc", "pard", "pare")
]))
cat("\nBase logistic coefficients:\n")
print(base$coefs)
cat("\nSubjects:", length(unique(trajectory$subject)), "\n")
cat("Rows including trial 0:", nrow(trajectory), "\n")
cat("Trial count summary:\n")
print(summary(subject_counts$trials))
cat("\nAlpha CSV:", normalizePath(alpha_path, winslash = "/"), "\n")
cat("Wide CSV:", normalizePath(wide_path, winslash = "/"), "\n")
cat("Long CSV:", normalizePath(long_path, winslash = "/"), "\n")
cat("Final parameter values CSV:",
    normalizePath(final_values_path, winslash = "/"), "\n")
cat("Final parameter correlations CSV:",
    normalizePath(final_cor_path, winslash = "/"), "\n")
cat("Combined PNG:", normalizePath(combined_png, winslash = "/"), "\n")
