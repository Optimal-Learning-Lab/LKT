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
decay_lower <- as.numeric(get_arg("decay-lower", "0.00001"))
decay_upper <- as.numeric(get_arg("decay-upper", "0.99999"))
clip_epsilon <- as.numeric(get_arg("clip-epsilon", "0.000001"))
denom_epsilon <- as.numeric(get_arg("denom-epsilon", "0.000001"))
line_width <- as.numeric(get_arg("line-width", "0.08"))
line_alpha <- as.numeric(get_arg("line-alpha", "0.45"))
smooth <- identical(tolower(get_arg("smooth", "false")), "true")
smooth_degree <- as.integer(get_arg("smooth-degree", "2"))

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
optimized_decay <- LKTOptimizeOnlineSimpleAdaptiveDecayAlpha(
  input,
  start = c(alpha_recency = 0.03, alpha_logsuc = 0.05, alpha_decay = 0.01),
  control = list(maxit = maxit, factr = 1e7),
  require_native = require_native,
  decay_lower = decay_lower,
  decay_upper = decay_upper
)

par <- optimized_decay$par
trajectory <- vector("list", length(input$subject_start))

for (s in seq_along(input$subject_start)) {
  rows <- input$subject_start[s]:input$subject_end[s]
  n <- length(rows)
  beta_recency <- unname(par["recency"])
  beta_logsuc <- unname(par["logsuc"])
  recency_decay <- input$recency_decay_start

  subject_trace <- data.frame(
    subject = rep(input$subject[s], n + 1L),
    trial = 0:n,
    recency_beta = NA_real_,
    logsuc_beta = NA_real_,
    recency_decay = NA_real_,
    stringsAsFactors = FALSE
  )
  subject_trace$recency_beta[1L] <- beta_recency
  subject_trace$logsuc_beta[1L] <- beta_logsuc
  subject_trace$recency_decay[1L] <- recency_decay

  for (trial_i in seq_along(rows)) {
    row <- rows[trial_i]
    spacing <- input$x_spacing[row]
    xr <- if (spacing > 0) spacing^(-recency_decay) else 0
    xl <- input$x_logsuc[row]
    eta <- unname(par["intercept"]) +
      beta_recency * xr +
      beta_logsuc * xl +
      unname(par["logitdec"]) * input$x_logitdec[row]
    p <- pmin(pmax(stats::plogis(eta), clip_epsilon), 1 - clip_epsilon)
    err <- input$y[row] - p

    if (xr != 0 && par["alpha_recency"] != 0) {
      beta_recency <- beta_recency +
        unname(par["alpha_recency"]) * xr * err / (xr^2 + denom_epsilon)
    }
    if (xl != 0 && par["alpha_logsuc"] != 0) {
      beta_logsuc <- beta_logsuc +
        unname(par["alpha_logsuc"]) * xl * err / (xl^2 + denom_epsilon)
    }
    if (par["alpha_decay"] != 0) {
      recency_decay <- pmin(
        pmax(recency_decay - unname(par["alpha_decay"]) * err, decay_lower),
        decay_upper
      )
    }

    subject_trace$recency_beta[trial_i + 1L] <- beta_recency
    subject_trace$logsuc_beta[trial_i + 1L] <- beta_logsuc
    subject_trace$recency_decay[trial_i + 1L] <- recency_decay
  }
  trajectory[[s]] <- subject_trace
}

trajectory <- do.call(rbind, trajectory)
trajectory_long <- rbind(
  data.frame(subject = trajectory$subject, trial = trajectory$trial,
             parameter = "Recency beta", value = trajectory$recency_beta,
             stringsAsFactors = FALSE),
  data.frame(subject = trajectory$subject, trial = trajectory$trial,
             parameter = "LogSuc beta", value = trajectory$logsuc_beta,
             stringsAsFactors = FALSE),
  data.frame(subject = trajectory$subject, trial = trajectory$trial,
             parameter = "Recency decay", value = trajectory$recency_decay,
             stringsAsFactors = FALSE)
)

if (isTRUE(smooth)) {
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

wide_path <- file.path(report_dir, "online-decay-parameter-trajectories-wide.csv")
long_path <- file.path(report_dir, "online-decay-parameter-trajectories-long.csv")
suffix <- if (isTRUE(smooth)) paste0("-poly", smooth_degree) else ""
combined_png <- file.path(
  report_dir,
  paste0("online-decay-parameter-trajectories", suffix, ".png")
)
write.csv(trajectory, wide_path, row.names = FALSE)
write.csv(trajectory_long, long_path, row.names = FALSE)

subject_counts <- aggregate(trial ~ subject, trajectory, max)
names(subject_counts)[2L] <- "trials"

plot_one <- function(parameter_name, color, output_file, y_label) {
  plot_data <- trajectory_long[trajectory_long$parameter == parameter_name, ]
  p <- ggplot(plot_data, aes(x = trial, y = value, group = subject)) +
    geom_line(alpha = line_alpha, linewidth = line_width, color = color,
              lineend = "round", linejoin = "round") +
    labs(x = "Trial within participant", y = y_label, title = parameter_name) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank())
  ggsave(output_file, p, width = 9, height = 5.2, dpi = 150)
  p
}

p_recency <- plot_one(
  "Recency beta",
  "#4C78A8",
  file.path(report_dir, paste0("online-decay-recency-beta-trajectories",
                               suffix, ".png")),
  "Recency beta"
)
p_logsuc <- plot_one(
  "LogSuc beta",
  "#59A14F",
  file.path(report_dir, paste0("online-decay-logsuc-beta-trajectories",
                               suffix, ".png")),
  "LogSuc beta"
)
p_decay <- plot_one(
  "Recency decay",
  "#E15759",
  file.path(report_dir, paste0("online-decay-recency-decay-trajectories",
                               suffix, ".png")),
  "Recency decay d"
)

combined <- ggplot(
  trajectory_long,
  aes(x = trial, y = value, group = subject, color = parameter)
) +
  geom_line(alpha = line_alpha, linewidth = line_width, show.legend = FALSE,
            lineend = "round", linejoin = "round") +
  facet_wrap(~parameter, scales = "free_y", nrow = 1) +
  scale_color_manual(values = c(
    "Recency beta" = "#4C78A8",
    "LogSuc beta" = "#59A14F",
    "Recency decay" = "#E15759"
  )) +
  labs(x = "Trial within participant", y = "Parameter value") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())
ggsave(combined_png, combined, width = 15, height = 5.2, dpi = 150)

cat("Decay-alpha loglike:", optimized_decay$loglike, "\n")
cat("Decay-alpha delta loglike:", optimized_decay$delta_loglike, "\n")
cat("Decay-alpha parameters:\n")
print(optimized_decay$par)
cat("\nSubjects:", length(unique(trajectory$subject)), "\n")
cat("Rows including trial 0:", nrow(trajectory), "\n")
cat("Trial count summary:\n")
print(summary(subject_counts$trials))
cat("\nWide CSV:", normalizePath(wide_path, winslash = "/"), "\n")
cat("Long CSV:", normalizePath(long_path, winslash = "/"), "\n")
cat("Combined PNG:", normalizePath(combined_png, winslash = "/"), "\n")
cat("Recency beta PNG:",
    normalizePath(file.path(report_dir, paste0("online-decay-recency-beta-trajectories",
                                               suffix, ".png")),
                  winslash = "/"), "\n")
cat("LogSuc beta PNG:",
    normalizePath(file.path(report_dir, paste0("online-decay-logsuc-beta-trajectories",
                                               suffix, ".png")),
                  winslash = "/"), "\n")
cat("Recency decay PNG:",
    normalizePath(file.path(report_dir, paste0("online-decay-recency-decay-trajectories",
                                               suffix, ".png")),
                  winslash = "/"), "\n")
