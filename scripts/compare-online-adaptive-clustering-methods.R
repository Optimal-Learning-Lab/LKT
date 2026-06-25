script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
if (is.na(script_file) || !nzchar(script_file)) {
  script_file <- file.path("scripts", "compare-online-adaptive-clustering-methods.R")
}
root <- normalizePath(file.path(dirname(script_file), ".."),
                      winslash = "/", mustWork = TRUE)
vignette_dir <- file.path(root, "vignettes")
report_dir <- file.path(vignette_dir, "_example-reports")
trajectory_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-trajectories-wide.csv"
)
if (!file.exists(trajectory_path)) {
  stop("Trajectory file not found: ", trajectory_path,
       ". Run scripts/plot-online-decay-trajectories.R first.",
       call. = FALSE)
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package ggplot2 is required to write comparison plots.", call. = FALSE)
}
if (!requireNamespace("mclust", quietly = TRUE)) {
  stop("Package mclust is required for Gaussian mixture comparison. ",
       "Install it with install.packages('mclust').", call. = FALSE)
}
suppressPackageStartupMessages(library(mclust))

Sys.setenv(LKT_SOURCE_WORKTREE = "true")
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(vignette_dir)
source("lkt-vignette-setup.R")

trajectory <- read.csv(trajectory_path, stringsAsFactors = FALSE,
                       check.names = FALSE)
coefficient_columns <- grep("^beta[.:]", names(trajectory), value = TRUE)
if (length(coefficient_columns) != 4L) {
  stop("Expected four beta columns; found ", length(coefficient_columns),
       ": ", paste(coefficient_columns, collapse = ", "), call. = FALSE)
}
coefficient_terms <- sub("^beta[.:]", "", coefficient_columns)

initial_rows <- do.call(rbind, lapply(split(trajectory, trajectory$subject),
                                      function(df) {
  df[which.min(df$trial), c("subject", "trial", coefficient_columns),
     drop = FALSE]
}))
final_rows <- do.call(rbind, lapply(split(trajectory, trajectory$subject),
                                    function(df) {
  df[which.max(df$trial), c("subject", "trial", coefficient_columns),
     drop = FALSE]
}))
row.names(initial_rows) <- NULL
row.names(final_rows) <- NULL
initial_rows <- initial_rows[order(initial_rows$subject), , drop = FALSE]
final_rows <- final_rows[order(final_rows$subject), , drop = FALSE]
if (!identical(initial_rows$subject, final_rows$subject)) {
  stop("Initial and final subject rows do not align.", call. = FALSE)
}

cat("[1/4] Refitting fixed base model for predictor standard deviations\n")
val <- prepare_largeraw_sample()
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
predictor_names <- colnames(base$predictors)
if (is.null(predictor_names)) {
  predictor_names <- rownames(base$coefs)
}
if (length(predictor_names) != ncol(base$predictors)) {
  stop("Could not determine predictor names for base predictor columns.",
       call. = FALSE)
}
missing_terms <- setdiff(coefficient_terms, predictor_names)
if (length(missing_terms) > 0L) {
  stop("Base predictors are missing coefficient terms: ",
       paste(missing_terms, collapse = ", "), call. = FALSE)
}
predictors <- as.matrix(base$predictors[, match(coefficient_terms, predictor_names)])
colnames(predictors) <- coefficient_terms
predictor_sd <- apply(predictors, 2L, stats::sd)
if (any(!is.finite(predictor_sd) | predictor_sd <= 0)) {
  stop("Predictor SDs must be finite and positive.", call. = FALSE)
}

raw_delta <- as.matrix(final_rows[, coefficient_columns, drop = FALSE]) -
  as.matrix(initial_rows[, coefficient_columns, drop = FALSE])
colnames(raw_delta) <- coefficient_terms
effect_change <- sweep(raw_delta, 2L, predictor_sd, `*`)
scaled_effect_change <- scale(effect_change)
if (any(!is.finite(scaled_effect_change))) {
  stop("Scaled effect-change matrix contains non-finite values.",
       call. = FALSE)
}

effect_change_rows <- data.frame(
  subject = final_rows$subject,
  raw_delta,
  check.names = FALSE
)
for (term in coefficient_terms) {
  effect_change_rows[[paste0("effect_change:", term)]] <- effect_change[, term]
}
effect_change_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-effect-change-features.csv"
)
write.csv(effect_change_rows, effect_change_path, row.names = FALSE)

write.csv(data.frame(term = coefficient_terms,
                     predictor_sd = as.numeric(predictor_sd),
                     stringsAsFactors = FALSE),
          file.path(report_dir,
                    "online-adaptive-default-kc-predictor-sds.csv"),
          row.names = FALSE)

cluster_quality <- function(cluster, distance_matrix) {
  sil <- cluster::silhouette(cluster, distance_matrix)
  mean(sil[, "sil_width"])
}

cat("[2/4] Running k-means, Ward hierarchical, and Gaussian mixtures\n")
set.seed(20260623)
distance_matrix <- stats::dist(scaled_effect_change)
pca <- stats::prcomp(scaled_effect_change, center = FALSE, scale. = FALSE)
pca_scores <- data.frame(
  subject = final_rows$subject,
  PC1 = pca$x[, 1L],
  PC2 = pca$x[, 2L],
  stringsAsFactors = FALSE
)
explained <- (pca$sdev^2) / sum(pca$sdev^2)
ward_tree <- stats::hclust(distance_matrix, method = "ward.D2")
mclust_fit <- mclust::Mclust(scaled_effect_change, G = 1:6)

method_rows <- list()
assignment_rows <- data.frame(subject = final_rows$subject,
                              stringsAsFactors = FALSE)
plot_rows <- list()
center_rows <- list()

for (k in 2:6) {
  km <- stats::kmeans(scaled_effect_change, centers = k, nstart = 50,
                      iter.max = 100)
  ward_cluster <- stats::cutree(ward_tree, k = k)
  mclust_k <- mclust::Mclust(scaled_effect_change, G = k,
                             modelNames = mclust_fit$modelName)
  methods <- list(
    kmeans = km$cluster,
    ward = ward_cluster,
    mclust = mclust_k$classification
  )
  for (method in names(methods)) {
    cluster <- methods[[method]]
    method_key <- paste(method, "k", k, sep = "_")
    assignment_rows[[method_key]] <- cluster
    sil <- cluster_quality(cluster, distance_matrix)
    method_rows[[method_key]] <- data.frame(
      method = method,
      k = k,
      mean_silhouette_width = sil,
      total_withinss = if (identical(method, "kmeans")) km$tot.withinss else NA_real_,
      mclust_bic = if (identical(method, "mclust")) mclust_k$bic else NA_real_,
      mclust_loglik = if (identical(method, "mclust")) mclust_k$loglik else NA_real_,
      mclust_model = if (identical(method, "mclust")) mclust_k$modelName else NA_character_,
      mclust_uncertainty_mean = if (identical(method, "mclust")) {
        mean(mclust_k$uncertainty)
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
    plot_rows[[method_key]] <- data.frame(
      pca_scores,
      method = method,
      k = paste0("k = ", k),
      cluster = factor(cluster),
      stringsAsFactors = FALSE
    )
    raw_centers <- stats::aggregate(
      as.data.frame(effect_change),
      by = list(cluster = cluster),
      FUN = mean
    )
    raw_centers$method <- method
    raw_centers$k <- k
    raw_centers$size <- as.integer(table(factor(cluster))[
      as.character(raw_centers$cluster)])
    center_rows[[method_key]] <- raw_centers
  }
}

method_summary <- do.call(rbind, method_rows)
method_assignments <- assignment_rows
method_plot <- do.call(rbind, plot_rows)
method_centers <- do.call(rbind, center_rows)

summary_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-method-comparison-summary.csv"
)
assignments_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-method-comparison-assignments.csv"
)
centers_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-method-comparison-centers-effect-change.csv"
)
write.csv(method_summary, summary_path, row.names = FALSE)
write.csv(method_assignments, assignments_path, row.names = FALSE)
write.csv(method_centers, centers_path, row.names = FALSE)
write.csv(data.frame(
  selected_G = mclust_fit$G,
  selected_model = mclust_fit$modelName,
  selected_bic = mclust_fit$bic,
  loglik = mclust_fit$loglik,
  mean_uncertainty = mean(mclust_fit$uncertainty),
  stringsAsFactors = FALSE
), file.path(report_dir,
             "online-adaptive-default-kc-mclust-selected-model.csv"),
row.names = FALSE)

cat("[3/4] Writing comparison plots\n")
comparison_plot_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-method-comparison-pca.png"
)
fit_plot_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-method-comparison-fit.png"
)
dendrogram_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-ward-dendrogram.png"
)
mclust_uncertainty_pca_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-mclust-uncertainty-pca.png"
)
mclust_pairs_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-mclust-effect-change-pairs.png"
)
mclust_pairs_readable_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-mclust-effect-change-pairs-readable.png"
)
mclust_pair_dir <- file.path(
  report_dir,
  "online-adaptive-default-kc-mclust-pair-plots"
)
dir.create(mclust_pair_dir, recursive = TRUE, showWarnings = FALSE)
mclust_uncertainty_hist_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-mclust-uncertainty-histogram.png"
)
mclust_dr_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-mclust-discriminant-projection.png"
)

comparison_plot <- ggplot2::ggplot(
  method_plot,
  ggplot2::aes(x = PC1, y = PC2, color = cluster)
) +
  ggplot2::geom_point(alpha = 0.82, size = 1.55) +
  ggplot2::facet_grid(method ~ k) +
  ggplot2::labs(
    x = sprintf("PC1 (%.1f%%)", 100 * explained[1L]),
    y = sprintf("PC2 (%.1f%%)", 100 * explained[2L]),
    color = "Cluster"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
ggplot2::ggsave(comparison_plot_path, comparison_plot, width = 16, height = 9,
                dpi = 150)

fit_long <- rbind(
  data.frame(method = method_summary$method, k = method_summary$k,
             metric = "Mean silhouette width",
             value = method_summary$mean_silhouette_width,
             stringsAsFactors = FALSE),
  data.frame(method = method_summary$method, k = method_summary$k,
             metric = "Mclust BIC (higher is better)",
             value = method_summary$mclust_bic,
             stringsAsFactors = FALSE),
  data.frame(method = method_summary$method, k = method_summary$k,
             metric = "K-means total within SS",
             value = method_summary$total_withinss,
             stringsAsFactors = FALSE)
)
fit_long <- fit_long[is.finite(fit_long$value), , drop = FALSE]
fit_plot <- ggplot2::ggplot(
  fit_long,
  ggplot2::aes(x = k, y = value, color = method)
) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~metric, scales = "free_y", ncol = 1) +
  ggplot2::scale_x_continuous(breaks = 2:6) +
  ggplot2::labs(x = "Number of clusters (k)", y = NULL, color = "Method") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
ggplot2::ggsave(fit_plot_path, fit_plot, width = 8.5, height = 9,
                dpi = 150)

mclust_plot_data <- data.frame(
  pca_scores,
  cluster = factor(mclust_fit$classification),
  uncertainty = mclust_fit$uncertainty,
  stringsAsFactors = FALSE
)
mclust_uncertainty_pca <- ggplot2::ggplot(
  mclust_plot_data,
  ggplot2::aes(x = PC1, y = PC2, color = cluster, size = uncertainty)
) +
  ggplot2::geom_point(alpha = 0.82) +
  ggplot2::scale_size_continuous(range = c(1.2, 5)) +
  ggplot2::labs(
    x = sprintf("PC1 (%.1f%%)", 100 * explained[1L]),
    y = sprintf("PC2 (%.1f%%)", 100 * explained[2L]),
    color = "Mclust class",
    size = "Uncertainty"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
ggplot2::ggsave(mclust_uncertainty_pca_path, mclust_uncertainty_pca,
                width = 8, height = 6, dpi = 150)

feature_display_names <- setNames(
  sub("KC\\.\\.Default\\.$", "", coefficient_terms),
  coefficient_terms
)
pair_terms <- utils::combn(coefficient_terms, 2, simplify = FALSE)
pair_plot_rows <- do.call(rbind, lapply(pair_terms, function(pair) {
  data.frame(
    subject = final_rows$subject,
    feature_x = feature_display_names[[pair[1L]]],
    feature_y = feature_display_names[[pair[2L]]],
    panel = paste0(
      feature_display_names[[pair[1L]]], " vs ",
      feature_display_names[[pair[2L]]]),
    value_x = effect_change[, pair[1L]],
    value_y = effect_change[, pair[2L]],
    cluster = factor(mclust_fit$classification),
    uncertainty = mclust_fit$uncertainty,
    stringsAsFactors = FALSE
  )
}))
pair_plot_paths <- character(length(pair_terms))
for (i in seq_along(pair_terms)) {
  pair <- pair_terms[[i]]
  x_name <- feature_display_names[[pair[1L]]]
  y_name <- feature_display_names[[pair[2L]]]
  pair_data <- pair_plot_rows[
    pair_plot_rows$feature_x == x_name & pair_plot_rows$feature_y == y_name,
    ,
    drop = FALSE
  ]
  pair_plot <- ggplot2::ggplot(
    pair_data,
    ggplot2::aes(x = value_x, y = value_y, color = cluster)
  ) +
    ggplot2::geom_point(ggplot2::aes(alpha = 1 - uncertainty), size = 2.4) +
    ggplot2::stat_ellipse(linewidth = 0.9, level = 0.68) +
    ggplot2::scale_alpha_continuous(range = c(0.25, 0.9), guide = "none") +
    ggplot2::labs(
      title = paste(x_name, "vs", y_name),
      x = paste0(x_name, " effect-change (delta beta * predictor SD)"),
      y = paste0(y_name, " effect-change (delta beta * predictor SD)"),
      color = "Mclust class"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 15, face = "bold"),
      axis.text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 11),
      legend.title = ggplot2::element_text(size = 12)
    )
  pair_plot_paths[i] <- file.path(
    mclust_pair_dir,
    paste0("mclust-pair-",
           gsub("[^A-Za-z0-9]+", "-", tolower(x_name)),
           "-vs-",
           gsub("[^A-Za-z0-9]+", "-", tolower(y_name)),
           ".png")
  )
  ggplot2::ggsave(pair_plot_paths[i], pair_plot, width = 8.5, height = 6,
                  dpi = 170)
}
mclust_pairs <- ggplot2::ggplot(
  pair_plot_rows,
  ggplot2::aes(x = value_x, y = value_y, color = cluster)
) +
  ggplot2::geom_point(ggplot2::aes(alpha = 1 - uncertainty), size = 1.7) +
  ggplot2::stat_ellipse(linewidth = 0.7, level = 0.68) +
  ggplot2::facet_wrap(~feature_x + feature_y, scales = "free", ncol = 3) +
  ggplot2::scale_alpha_continuous(range = c(0.25, 0.9), guide = "none") +
  ggplot2::labs(x = NULL, y = NULL, color = "Mclust class") +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = 8)
  )
ggplot2::ggsave(mclust_pairs_path, mclust_pairs, width = 13, height = 8,
                dpi = 150)

mclust_pairs_readable <- ggplot2::ggplot(
  pair_plot_rows,
  ggplot2::aes(x = value_x, y = value_y, color = cluster)
) +
  ggplot2::geom_point(ggplot2::aes(alpha = 1 - uncertainty), size = 2.1) +
  ggplot2::stat_ellipse(linewidth = 0.85, level = 0.68) +
  ggplot2::facet_wrap(
    ~panel,
    scales = "free",
    ncol = 2
  ) +
  ggplot2::scale_alpha_continuous(range = c(0.25, 0.9), guide = "none") +
  ggplot2::labs(
    x = "Effect-change value on x feature",
    y = "Effect-change value on y feature",
    color = "Mclust class"
  ) +
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = 12, face = "bold"),
    axis.text = ggplot2::element_text(size = 11),
    axis.title = ggplot2::element_text(size = 13),
    legend.text = ggplot2::element_text(size = 11),
    legend.title = ggplot2::element_text(size = 12)
  )
ggplot2::ggsave(mclust_pairs_readable_path, mclust_pairs_readable,
                width = 14, height = 12, dpi = 170)

mclust_uncertainty_hist <- ggplot2::ggplot(
  mclust_plot_data,
  ggplot2::aes(x = uncertainty, fill = cluster)
) +
  ggplot2::geom_histogram(bins = 30, alpha = 0.8, position = "identity") +
  ggplot2::labs(x = "Mclust classification uncertainty",
                y = "Participants", fill = "Mclust class") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
ggplot2::ggsave(mclust_uncertainty_hist_path, mclust_uncertainty_hist,
                width = 8, height = 5, dpi = 150)

mclust_dr <- mclust::MclustDR(mclust_fit)
mclust_dr_scores <- as.data.frame(mclust_dr$dir)
mclust_dr_scores$subject <- final_rows$subject
mclust_dr_scores$cluster <- factor(mclust_fit$classification)
mclust_dr_scores$uncertainty <- mclust_fit$uncertainty
if (ncol(mclust_dr$dir) >= 2L) {
  dr_x <- names(mclust_dr_scores)[1L]
  dr_y <- names(mclust_dr_scores)[2L]
  mclust_dr_plot <- ggplot2::ggplot(
    mclust_dr_scores,
    ggplot2::aes(x = .data[[dr_x]], y = .data[[dr_y]],
                 color = cluster, size = uncertainty)
  ) +
    ggplot2::geom_point(alpha = 0.82) +
    ggplot2::scale_size_continuous(range = c(1.2, 5)) +
    ggplot2::labs(x = dr_x, y = dr_y, color = "Mclust class",
                  size = "Uncertainty") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
} else {
  dr_x <- names(mclust_dr_scores)[1L]
  mclust_dr_plot <- ggplot2::ggplot(
    mclust_dr_scores,
    ggplot2::aes(x = .data[[dr_x]], y = 0,
                 color = cluster, size = uncertainty)
  ) +
    ggplot2::geom_jitter(height = 0.05, alpha = 0.82) +
    ggplot2::scale_size_continuous(range = c(1.2, 5)) +
    ggplot2::labs(x = dr_x, y = NULL, color = "Mclust class",
                  size = "Uncertainty") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )
}
ggplot2::ggsave(mclust_dr_path, mclust_dr_plot, width = 8, height = 5,
                dpi = 150)

png(dendrogram_path, width = 1500, height = 900, res = 150)
plot(ward_tree, labels = FALSE, hang = -1,
     main = "Ward hierarchical clustering on standardized effect changes",
     xlab = "Participants", sub = "")
rect.hclust(ward_tree, k = 2, border = "#E15759")
rect.hclust(ward_tree, k = 3, border = "#4C78A8")
dev.off()

cat("[4/4] Done\n")
cat("Effect-change features:", normalizePath(effect_change_path, winslash = "/"), "\n")
cat("Method summary:", normalizePath(summary_path, winslash = "/"), "\n")
cat("Assignments:", normalizePath(assignments_path, winslash = "/"), "\n")
cat("Centers:", normalizePath(centers_path, winslash = "/"), "\n")
cat("Method PCA plot:", normalizePath(comparison_plot_path, winslash = "/"), "\n")
cat("Method fit plot:", normalizePath(fit_plot_path, winslash = "/"), "\n")
cat("Ward dendrogram:", normalizePath(dendrogram_path, winslash = "/"), "\n")
cat("Mclust uncertainty PCA:", normalizePath(mclust_uncertainty_pca_path, winslash = "/"), "\n")
cat("Mclust pairwise feature plot:", normalizePath(mclust_pairs_path, winslash = "/"), "\n")
cat("Mclust readable pairwise feature plot:", normalizePath(mclust_pairs_readable_path, winslash = "/"), "\n")
cat("Mclust individually labeled pair plots:\n")
print(normalizePath(pair_plot_paths, winslash = "/"))
cat("Mclust uncertainty histogram:", normalizePath(mclust_uncertainty_hist_path, winslash = "/"), "\n")
cat("Mclust discriminant projection:", normalizePath(mclust_dr_path, winslash = "/"), "\n")
cat("Mclust selected G:", mclust_fit$G, "\n")
cat("Mclust selected model:", mclust_fit$modelName, "\n")
cat("Mclust selected BIC:", mclust_fit$bic, "\n")
cat("\nSummary:\n")
print(method_summary)
