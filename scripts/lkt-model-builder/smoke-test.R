helper_path <- file.path("scripts", "lkt-model-builder", "helpers.R")
if (!file.exists(helper_path)) {
  helper_path <- file.path("helpers.R")
}
source(helper_path)

load_lkt_namespace()

data(samplelkt, package = "LKT", envir = environment())
prepared <- prepare_response_column(samplelkt, "CF..ansbin.")$data
prepared_from_outcome <- prepare_response_column(samplelkt, "Outcome")$data
if (!"CF..ansbin." %in% names(prepared_from_outcome)) {
  stop("Outcome response selection did not derive CF..ansbin.", call. = FALSE)
}

components <- candidate_components(
  prepared,
  subject_col = "Anon.Student.Id",
  response_col = "CF..ansbin.",
  time_col = "CF..Time.",
  fold_col = character(0)
)
if (!"KC..Default." %in% components) {
  stop("Expected KC..Default. among detected component choices.", call. = FALSE)
}

kc_features <- feature_choices_for_component(prepared, "KC..Default.")
if ("numer" %in% kc_features) {
  stop("Text KC component should not offer numer feature.", call. = FALSE)
}
if (!"recency" %in% kc_features) {
  stop("KC component with Duration..sec. should offer recency feature.",
       call. = FALSE)
}
duration_features <- feature_choices_for_component(prepared, "Duration..sec.")
if (!"numer" %in% duration_features) {
  stop("Numeric component should offer numer feature.", call. = FALSE)
}
no_time <- prepared[, setdiff(names(prepared), "Duration..sec."), with = FALSE]
no_time_features <- feature_choices_for_component(no_time, "KC..Default.")
if ("recency" %in% no_time_features) {
  stop("Component without timing or spacing support should not offer recency.",
       call. = FALSE)
}

terms <- data.frame(
  on = TRUE,
  component = "KC..Default.",
  feature = "intercept",
  fixedpar = "NA",
  interaction = "(none)",
  connector = "+",
  stringsAsFactors = FALSE
)

fit <- fit_lkt_from_terms(prepared, terms, interc = TRUE)
if (is.null(fit$coefs) || nrow(as.data.frame(fit$coefs)) < 1L) {
  stop("Default GUI fit did not return coefficients.", call. = FALSE)
}
if (is.null(fit$prediction) || length(fit$prediction) != nrow(prepared)) {
  stop("Default GUI fit did not return one prediction per row.", call. = FALSE)
}

multi_terms <- data.frame(
  on = TRUE,
  component = c("Anon.Student.Id", "KC..Default.", "KC..Default."),
  feature = c("intercept", "lineafm", "logafm"),
  fixedpar = c("NA", "NA", "NA"),
  interaction = c("(none)", "(none)", "(none)"),
  connector = c("+", "+", "+"),
  stringsAsFactors = FALSE
)

multi_fit <- fit_lkt_from_terms(prepared, multi_terms, interc = TRUE)
multi_coefs <- as.data.frame(multi_fit$coefs)
if (!any(grepl("lineafmKC..Default.", rownames(multi_coefs), fixed = TRUE))) {
  stop("Multi-term GUI fit missing lineafm KC coefficient.", call. = FALSE)
}
if (!any(grepl("logafmKC..Default.", rownames(multi_coefs), fixed = TRUE))) {
  stop("Multi-term GUI fit missing logafm KC coefficient.", call. = FALSE)
}
if (is.null(multi_fit$prediction) ||
    length(multi_fit$prediction) != nrow(prepared)) {
  stop("Multi-term GUI fit did not return one prediction per row.",
       call. = FALSE)
}

call_text <- format_lkt_call(multi_terms, interc = TRUE)
if (!grepl('components = c\\("Anon.Student.Id", "KC..Default.", "KC..Default."\\)',
           call_text)) {
  stop("Formatted call missing multi-term components.", call. = FALSE)
}
if (!grepl('connectors = c\\("\\+", "\\+"\\)', call_text)) {
  stop("Formatted call missing multi-term connectors.", call. = FALSE)
}
if (!grepl("interacts = c\\(NA, NA, NA\\)", call_text)) {
  stop("Formatted call missing interaction vector.", call. = FALSE)
}

plot_data <- kc_trial_summary(
  prepared,
  subject_col = "Anon.Student.Id",
  kc_col = "KC..Default.",
  response_col = "CF..ansbin.",
  order_col = "CF..Time.",
  min_n = 1L,
  top_k = 12L
)
if (nrow(plot_data) < 1L) {
  stop("KC-by-trial summary returned no rows.", call. = FALSE)
}
if (any(plot_data$probability_correct < 0 | plot_data$probability_correct > 1)) {
  stop("KC-by-trial probability_correct outside [0, 1].", call. = FALSE)
}

overall_plot_data <- kc_trial_summary(
  prepared,
  subject_col = "Anon.Student.Id",
  kc_col = "KC..Default.",
  response_col = "CF..ansbin.",
  order_col = "CF..Time.",
  min_n = 1L,
  top_k = 12L,
  aggregate = "overall"
)
if (nrow(overall_plot_data) < 1L ||
    !"series" %in% names(overall_plot_data) ||
    !identical(unique(overall_plot_data$series), "All cases")) {
  stop("Overall mean-by-trial summary did not return the All cases series.",
       call. = FALSE)
}
if (any(overall_plot_data$probability_correct < 0 |
        overall_plot_data$probability_correct > 1)) {
  stop("Overall probability_correct outside [0, 1].", call. = FALSE)
}

plot_data_from_outcome <- kc_trial_summary(
  prepared_from_outcome,
  subject_col = "Anon.Student.Id",
  kc_col = "KC..Default.",
  response_col = "CF..ansbin.",
  order_col = "CF..Time.",
  min_n = 1L,
  top_k = 12L
)
if (nrow(plot_data_from_outcome) < 1L) {
  stop("KC-by-trial summary failed when response selection was Outcome.",
       call. = FALSE)
}

cat("LKT model builder smoke test passed\n")
cat("coefficients:", nrow(as.data.frame(fit$coefs)), "\n")
cat("multi_coefficients:", nrow(multi_coefs), "\n")
cat("predictions:", length(fit$prediction), "\n")
cat("kc_trial_rows:", nrow(plot_data), "\n")
