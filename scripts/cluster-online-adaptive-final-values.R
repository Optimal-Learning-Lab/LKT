script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
if (is.na(script_file) || !nzchar(script_file)) {
  script_file <- file.path("scripts", "cluster-online-adaptive-final-values.R")
}
root <- normalizePath(file.path(dirname(script_file), ".."),
                      winslash = "/", mustWork = TRUE)
report_dir <- file.path(root, "vignettes", "_example-reports")
trajectory_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-trajectories-wide.csv"
)
if (!file.exists(trajectory_path)) {
  stop("Trajectory file not found: ", trajectory_path,
       ". Run scripts/plot-online-decay-trajectories.R first.",
       call. = FALSE)
}

trajectory <- read.csv(trajectory_path, stringsAsFactors = FALSE,
                       check.names = FALSE)
feature_columns <- grep("^beta[.:]", names(trajectory), value = TRUE)
if (length(feature_columns) != 4L) {
  stop("Expected four beta feature columns in trajectory file; found ",
       length(feature_columns), ": ",
       paste(feature_columns, collapse = ", "), call. = FALSE)
}

final_rows <- do.call(rbind, lapply(split(trajectory, trajectory$subject),
                                    function(df) {
  df[which.max(df$trial), c("subject", "trial", feature_columns),
     drop = FALSE]
}))
row.names(final_rows) <- NULL

feature_matrix <- as.matrix(final_rows[, feature_columns, drop = FALSE])
scaled_matrix <- scale(feature_matrix)
if (any(!is.finite(scaled_matrix))) {
  stop("Scaled final-value matrix contains non-finite values.", call. = FALSE)
}

set.seed(20260623)
pca <- stats::prcomp(scaled_matrix, center = FALSE, scale. = FALSE)
pca_scores <- data.frame(
  subject = final_rows$subject,
  PC1 = pca$x[, 1L],
  PC2 = pca$x[, 2L],
  stringsAsFactors = FALSE
)

final_values_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-final-coefficients.csv"
)
write.csv(final_rows, final_values_path, row.names = FALSE)

cluster_summaries <- list()
cluster_centers_raw <- list()
cluster_centers_scaled <- list()
cluster_assignments <- data.frame(subject = final_rows$subject,
                                  stringsAsFactors = FALSE)
plot_data <- list()
distance_matrix <- stats::dist(scaled_matrix)
silhouette_rows <- list()

for (k in 2:6) {
  km <- stats::kmeans(scaled_matrix, centers = k, nstart = 50, iter.max = 100)
  cluster_name <- paste0("k", k)
  cluster <- factor(km$cluster, levels = seq_len(k))
  cluster_assignments[[cluster_name]] <- km$cluster

  centers_scaled <- as.data.frame(km$centers)
  centers_scaled$cluster <- seq_len(k)
  centers_scaled$k <- k
  cluster_centers_scaled[[cluster_name]] <- centers_scaled

  centers_raw <- stats::aggregate(
    final_rows[, feature_columns, drop = FALSE],
    by = list(cluster = km$cluster),
    FUN = mean
  )
  centers_raw$k <- k
  centers_raw$size <- as.integer(table(cluster)[as.character(centers_raw$cluster)])
  cluster_centers_raw[[cluster_name]] <- centers_raw

  cluster_summaries[[cluster_name]] <- data.frame(
    k = k,
    cluster = seq_len(k),
    size = as.integer(table(cluster)),
    total_withinss = km$tot.withinss,
    between_over_total = km$betweenss / km$totss,
    stringsAsFactors = FALSE
  )
  silhouette <- cluster::silhouette(km$cluster, distance_matrix)
  silhouette_rows[[cluster_name]] <- data.frame(
    k = k,
    subject = final_rows$subject,
    cluster = silhouette[, "cluster"],
    neighbor = silhouette[, "neighbor"],
    silhouette_width = silhouette[, "sil_width"],
    stringsAsFactors = FALSE
  )

  plot_data[[cluster_name]] <- data.frame(
    pca_scores,
    k = paste0("k = ", k),
    cluster = cluster,
    stringsAsFactors = FALSE
  )
}

summary_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-kmeans-summary.csv"
)
fit_summary_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-kmeans-fit-summary.csv"
)
assignments_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-kmeans-assignments.csv"
)
centers_raw_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-kmeans-centers-raw.csv"
)
centers_scaled_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-kmeans-centers-scaled.csv"
)

cluster_summary <- do.call(rbind, cluster_summaries)
centers_raw_all <- do.call(rbind, cluster_centers_raw)
centers_scaled_all <- do.call(rbind, cluster_centers_scaled)
silhouette_all <- do.call(rbind, silhouette_rows)
silhouette_summary <- stats::aggregate(
  silhouette_width ~ k,
  silhouette_all,
  FUN = mean
)
names(silhouette_summary)[2L] <- "mean_silhouette_width"
cluster_summary <- merge(cluster_summary, silhouette_summary,
                         by = "k", all.x = TRUE)
write_optional_csv <- function(x, path) {
  tryCatch({
    write.csv(x, path, row.names = FALSE)
    invisible(TRUE)
  }, error = function(e) {
    warning("Could not write ", normalizePath(path, winslash = "/",
                                             mustWork = FALSE),
            ": ", conditionMessage(e), call. = FALSE)
    invisible(FALSE)
  })
}

write_optional_csv(cluster_summary, summary_path)
write.csv(cluster_summary, fit_summary_path, row.names = FALSE)
write_optional_csv(cluster_assignments, assignments_path)
write_optional_csv(centers_raw_all, centers_raw_path)
write_optional_csv(centers_scaled_all, centers_scaled_path)

silhouette_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-kmeans-silhouette-values.csv"
)
write.csv(silhouette_all, silhouette_path, row.names = FALSE)

correlation <- stats::cor(feature_matrix)
correlation_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-final-coefficient-correlations.csv"
)
write.csv(correlation, correlation_path)

plot_df <- do.call(rbind, plot_data)
combined_plot_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-kmeans-pca-k2-k6.png"
)
individual_plot_paths <- stats::setNames(
  file.path(report_dir,
            paste0("online-adaptive-default-kc-kmeans-pca-k",
                   2:6, ".png")),
  paste0("k = ", 2:6)
)
fit_plot_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-kmeans-fit-k2-k6.png"
)
silhouette_plot_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-kmeans-silhouette-k2-k6.png"
)
correlation_plot_path <- file.path(
  report_dir,
  "online-adaptive-default-kc-final-coefficient-correlations.png"
)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package ggplot2 is required to write the cluster plots.", call. = FALSE)
}

explained <- (pca$sdev^2) / sum(pca$sdev^2)
cluster_plot <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = PC1, y = PC2, color = cluster)
) +
  ggplot2::geom_point(alpha = 0.82, size = 1.8) +
  ggplot2::facet_wrap(~k, ncol = 3) +
  ggplot2::labs(
    x = sprintf("PC1 (%.1f%%)", 100 * explained[1L]),
    y = sprintf("PC2 (%.1f%%)", 100 * explained[2L]),
    color = "Cluster"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
ggplot2::ggsave(combined_plot_path, cluster_plot, width = 12, height = 8,
                dpi = 150)

fit_summary <- unique(cluster_summary[
  c("k", "total_withinss", "between_over_total", "mean_silhouette_width")
])
fit_long <- rbind(
  data.frame(k = fit_summary$k, metric = "Total within-cluster SS",
             value = fit_summary$total_withinss,
             stringsAsFactors = FALSE),
  data.frame(k = fit_summary$k, metric = "Between / total SS",
             value = fit_summary$between_over_total,
             stringsAsFactors = FALSE),
  data.frame(k = fit_summary$k, metric = "Mean silhouette width",
             value = fit_summary$mean_silhouette_width,
             stringsAsFactors = FALSE)
)
fit_plot <- ggplot2::ggplot(
  fit_long,
  ggplot2::aes(x = k, y = value)
) +
  ggplot2::geom_line(linewidth = 0.8, color = "#4C78A8") +
  ggplot2::geom_point(size = 2.4, color = "#4C78A8") +
  ggplot2::facet_wrap(~metric, scales = "free_y", ncol = 1) +
  ggplot2::scale_x_continuous(breaks = 2:6) +
  ggplot2::labs(x = "Number of clusters (k)", y = NULL) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
ggplot2::ggsave(fit_plot_path, fit_plot, width = 7.5, height = 8,
                dpi = 150)

silhouette_plot_data <- silhouette_all
silhouette_plot_data$k_label <- paste0("k = ", silhouette_plot_data$k)
silhouette_plot_data <- silhouette_plot_data[
  order(silhouette_plot_data$k, silhouette_plot_data$cluster,
        silhouette_plot_data$silhouette_width),
]
silhouette_plot_data$rank <- ave(
  silhouette_plot_data$silhouette_width,
  silhouette_plot_data$k,
  FUN = seq_along
)
silhouette_plot <- ggplot2::ggplot(
  silhouette_plot_data,
  ggplot2::aes(x = rank, y = silhouette_width, fill = factor(cluster))
) +
  ggplot2::geom_col(width = 1, show.legend = FALSE) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
  ggplot2::facet_wrap(~k_label, ncol = 1, scales = "free_x") +
  ggplot2::labs(x = "Participants sorted within k", y = "Silhouette width") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
ggplot2::ggsave(silhouette_plot_path, silhouette_plot, width = 9, height = 10,
                dpi = 150)

for (k_label in names(individual_plot_paths)) {
  one_plot <- ggplot2::ggplot(
    plot_df[plot_df$k == k_label, , drop = FALSE],
    ggplot2::aes(x = PC1, y = PC2, color = cluster)
  ) +
    ggplot2::geom_point(alpha = 0.82, size = 2) +
    ggplot2::labs(
      title = paste("K-means", k_label),
      x = sprintf("PC1 (%.1f%%)", 100 * explained[1L]),
      y = sprintf("PC2 (%.1f%%)", 100 * explained[2L]),
      color = "Cluster"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  ggplot2::ggsave(individual_plot_paths[[k_label]], one_plot,
                  width = 7.5, height = 5.5, dpi = 150)
}

cor_df <- as.data.frame(as.table(correlation), stringsAsFactors = FALSE)
names(cor_df) <- c("feature_x", "feature_y", "correlation")
cor_plot <- ggplot2::ggplot(
  cor_df,
  ggplot2::aes(x = feature_x, y = feature_y, fill = correlation)
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.4) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", correlation)),
                     size = 3) +
  ggplot2::scale_fill_gradient2(
    low = "#4C78A8",
    mid = "white",
    high = "#E15759",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  ggplot2::labs(x = NULL, y = NULL, fill = "r") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
    panel.grid = ggplot2::element_blank()
  )
ggplot2::ggsave(correlation_plot_path, cor_plot, width = 8, height = 6,
                dpi = 150)

cat("Final coefficient rows:", nrow(final_rows), "\n")
cat("Features:\n")
print(feature_columns)
cat("\nK-means summary:\n")
print(cluster_summary)
cat("\nCorrelation matrix:\n")
print(round(correlation, 4))
cat("\nFinal values CSV:", normalizePath(final_values_path, winslash = "/"), "\n")
cat("Assignments CSV:", normalizePath(assignments_path, winslash = "/"), "\n")
cat("Cluster summary CSV:", normalizePath(summary_path, winslash = "/"), "\n")
cat("Fit summary CSV:", normalizePath(fit_summary_path, winslash = "/"), "\n")
cat("Raw centers CSV:", normalizePath(centers_raw_path, winslash = "/"), "\n")
cat("Scaled centers CSV:", normalizePath(centers_scaled_path, winslash = "/"), "\n")
cat("Silhouette values CSV:", normalizePath(silhouette_path, winslash = "/"), "\n")
cat("Correlation CSV:", normalizePath(correlation_path, winslash = "/"), "\n")
cat("Cluster PCA PNG:", normalizePath(combined_plot_path, winslash = "/"), "\n")
cat("Individual cluster PCA PNGs:\n")
print(normalizePath(individual_plot_paths, winslash = "/"))
cat("Fit elbow/silhouette PNG:", normalizePath(fit_plot_path, winslash = "/"), "\n")
cat("Silhouette PNG:", normalizePath(silhouette_plot_path, winslash = "/"), "\n")
cat("Correlation PNG:", normalizePath(correlation_plot_path, winslash = "/"), "\n")
