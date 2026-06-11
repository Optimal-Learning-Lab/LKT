script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
if (is.na(script_file) || !nzchar(script_file)) {
  script_file <- file.path("scripts", "plot-online-decay-final-states.R")
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
  require_native = require_native
)

final_states <- optimized_decay$details$final_betas
trial_counts <- as.data.frame(table(input$subject), stringsAsFactors = FALSE)
names(trial_counts) <- c("subject", "trials")
final_states <- merge(final_states, trial_counts, by = "subject", sort = FALSE)
final_states$recency_change <- final_states$final_recency -
  optimized_decay$par["recency"]
final_states$logsuc_change <- final_states$final_logsuc -
  optimized_decay$par["logsuc"]
final_states$decay_change <- final_states$final_decay -
  input$recency_decay_start

csv_path <- file.path(report_dir, "online-decay-final-states.csv")
png_path <- file.path(report_dir, "online-decay-final-state-histograms.png")
write.csv(final_states, csv_path, row.names = FALSE)

png(png_path, width = 1500, height = 550, res = 130)
old_par <- par(no.readonly = TRUE)
on.exit(par(old_par), add = TRUE)
par(mfrow = c(1, 3), mar = c(4.2, 4.2, 3.2, 1.2))
hist(final_states$final_recency,
     breaks = 30,
     main = "Final Recency Beta",
     xlab = "Final beta",
     col = "#4C78A8",
     border = "white")
abline(v = optimized_decay$par["recency"], col = "#222222", lwd = 2)
hist(final_states$final_logsuc,
     breaks = 30,
     main = "Final LogSuc Beta",
     xlab = "Final beta",
     col = "#59A14F",
     border = "white")
abline(v = optimized_decay$par["logsuc"], col = "#222222", lwd = 2)
hist(final_states$final_decay,
     breaks = 30,
     main = "Final Recency Decay",
     xlab = "Final d",
     col = "#E15759",
     border = "white")
abline(v = input$recency_decay_start, col = "#222222", lwd = 2)
dev.off()

summary_table <- data.frame(
  parameter = c("final_recency", "final_logsuc", "final_decay"),
  start_value = c(
    optimized_decay$par["recency"],
    optimized_decay$par["logsuc"],
    input$recency_decay_start
  ),
  mean = c(
    mean(final_states$final_recency),
    mean(final_states$final_logsuc),
    mean(final_states$final_decay)
  ),
  sd = c(
    stats::sd(final_states$final_recency),
    stats::sd(final_states$final_logsuc),
    stats::sd(final_states$final_decay)
  ),
  min = c(
    min(final_states$final_recency),
    min(final_states$final_logsuc),
    min(final_states$final_decay)
  ),
  max = c(
    max(final_states$final_recency),
    max(final_states$final_logsuc),
    max(final_states$final_decay)
  ),
  stringsAsFactors = FALSE
)

cat("Decay-alpha loglike:", optimized_decay$loglike, "\n")
cat("Decay-alpha delta loglike:", optimized_decay$delta_loglike, "\n")
cat("Decay-alpha parameters:\n")
print(optimized_decay$par)
cat("\nFinal-state summaries:\n")
print(summary_table, row.names = FALSE)
cat("\nCSV:", normalizePath(csv_path, winslash = "/"), "\n")
cat("PNG:", normalizePath(png_path, winslash = "/"), "\n")
