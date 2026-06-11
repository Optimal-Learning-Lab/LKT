app_env <- new.env(parent = globalenv())
source(file.path("scripts", "lkt-model-builder", "app.R"), local = app_env)

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install shiny before running the Shiny server test.", call. = FALSE)
}

shiny::testServer(app_env$server, {
  session$setInputs(
    data_source = "samplelkt",
    subject_col = "Anon.Student.Id",
    response_col = "Outcome",
    time_col = "CF..Time.",
    fold_col = "(none)",
    on_1 = TRUE,
    component_1 = "KC..Default.",
    feature_1 = "intercept",
    fixed_1 = "NA",
    interaction_1 = "(none)",
    connector_1 = "+",
    plot_aggregate = "kc",
    plot_kc_col = "KC..Default.",
    plot_min_n = 1,
    plot_top_k = 12
  )
  session$flushReact()

  session$setInputs(fit = 1)
  session$flushReact()

  if (is.null(state$fit)) {
    stop("Shiny server fit action did not store a fit.", call. = FALSE)
  }
  if (!grepl("^Fit complete", state$status)) {
    stop("Unexpected Shiny fit status: ", state$status, call. = FALSE)
  }
  if (length(state$fit$prediction) != nrow(prepared_data())) {
    stop("Shiny server fit did not return one prediction per row.",
         call. = FALSE)
  }
  if (!grepl("components = c\\(\"KC..Default\\.\"\\)", output$model_call)) {
    stop("Rendered model call does not include selected KC component.",
         call. = FALSE)
  }
  if (!grepl("interacts = c\\(NA\\)", output$model_call)) {
    stop("Rendered model call does not include interaction vector.",
         call. = FALSE)
  }
  if (!grepl("KC..Default.", output$term_preview, fixed = TRUE)) {
    stop("Rendered term preview does not include selected KC component.",
         call. = FALSE)
  }

  plot_data <- kc_plot_data()
  if (nrow(plot_data) < 1L) {
    stop("Shiny server KC-by-trial data is empty.", call. = FALSE)
  }
  if (any(plot_data$probability_correct < 0 |
          plot_data$probability_correct > 1)) {
    stop("Shiny server KC-by-trial probability outside [0, 1].",
         call. = FALSE)
  }

  session$setInputs(plot_aggregate = "overall")
  session$flushReact()
  overall_plot_data <- kc_plot_data()
  if (nrow(overall_plot_data) < 1L ||
      !"series" %in% names(overall_plot_data) ||
      !identical(unique(overall_plot_data$series), "All cases")) {
    stop("Shiny server overall mean-by-trial data is invalid.",
         call. = FALSE)
  }
})

cat("LKT model builder Shiny server test passed\n")
