required <- c("shiny", "data.table", "ggplot2")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0L) {
  stop("Install required package(s) before running this app: ",
       paste(missing, collapse = ", "), call. = FALSE)
}

helper_candidates <- c(
  file.path(getwd(), "helpers.R"),
  file.path(getwd(), "scripts", "lkt-model-builder", "helpers.R")
)
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
app_file <- if (length(file_arg)) sub("^--file=", "", file_arg[1L]) else NULL
if (!is.null(app_file) && nzchar(app_file)) {
  helper_candidates <- c(
    file.path(dirname(normalizePath(app_file, winslash = "/",
                                    mustWork = FALSE)), "helpers.R"),
    helper_candidates
  )
}
helper_path <- helper_candidates[file.exists(helper_candidates)][1L]
if (is.na(helper_path)) {
  stop("Could not locate helpers.R for the LKT model builder app.",
       call. = FALSE)
}
source(helper_path)

load_lkt_namespace()

compact_theme <- "
body { font-size: 13px; }
.container-fluid { padding: 8px 12px; }
.well { padding: 8px; margin-bottom: 8px; }
.form-group { margin-bottom: 6px; }
.control-label { margin-bottom: 2px; }
.btn { padding: 3px 8px; font-size: 12px; }
.nav-tabs > li > a { padding: 6px 10px; }
table { font-size: 12px; }
h3, h4 { margin-top: 6px; margin-bottom: 6px; }
.term-row { display: grid; grid-template-columns: 44px 1.2fr 1.1fr .75fr 1fr .7fr; gap: 6px; align-items: end; }
.status { white-space: pre-wrap; font-family: Consolas, monospace; font-size: 12px; }
"

ui <- shiny::fluidPage(
  shiny::tags$head(shiny::tags$style(shiny::HTML(compact_theme))),
  shiny::titlePanel("LKT Model Builder"),
  shiny::fluidRow(
    shiny::column(
      3,
      shiny::wellPanel(
        shiny::h4("Data"),
        shiny::selectInput("data_source", NULL,
                           c("samplelkt", "Upload file"), width = "100%"),
        shiny::conditionalPanel(
          "input.data_source == 'Upload file'",
          shiny::fileInput("data_file", NULL,
                           accept = c(".csv", ".rds", ".rda", ".RData"))
        ),
        shiny::uiOutput("column_controls"),
        shiny::checkboxInput("intercept", "Global intercept", TRUE),
        shiny::actionButton("compute_spacing", "Compute spacing"),
        shiny::actionButton("fit", "Fit model", class = "btn-primary"),
        shiny::div(class = "status", shiny::textOutput("status"))
      )
    ),
    shiny::column(
      9,
      shiny::tabsetPanel(
        shiny::tabPanel(
          "Model",
          shiny::wellPanel(
            shiny::actionButton("add_term", "Add term"),
            shiny::actionButton("remove_term", "Remove last"),
            shiny::uiOutput("term_controls")
          ),
          shiny::h4("Current Terms"),
          shiny::tableOutput("term_preview")
        ),
        shiny::tabPanel(
          "Results",
          shiny::fluidRow(
            shiny::column(5, shiny::h4("Fit"), shiny::tableOutput("fit_metrics")),
            shiny::column(7, shiny::h4("Call"), shiny::verbatimTextOutput("model_call"))
          ),
          shiny::h4("Coefficients"),
          shiny::tableOutput("coef_table"),
          shiny::h4("Prediction Preview"),
          shiny::tableOutput("prediction_preview")
        ),
        shiny::tabPanel(
          "KC By Trial",
          shiny::fluidRow(
            shiny::column(3, shiny::uiOutput("plot_controls")),
            shiny::column(9, shiny::plotOutput("kc_trial_plot", height = 440))
          ),
          shiny::tableOutput("kc_trial_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  state <- shiny::reactiveValues(
    n_terms = 1L,
    spacing_data = NULL,
    fit = NULL,
    fit_time = NULL,
    status = "Ready."
  )

  raw_data <- shiny::reactive({
    if (identical(input$data_source, "samplelkt")) {
      data(samplelkt, package = "LKT", envir = environment())
      return(data.table::as.data.table(samplelkt))
    }
    shiny::req(input$data_file)
    read_gui_file(input$data_file)
  })

  output$column_controls <- shiny::renderUI({
    dt <- raw_data()
    cols <- names(dt)
    subject <- default_col(cols, "Anon.Student.Id")
    response <- default_col(cols, c("CF..ansbin.", "Outcome"))
    time <- default_col(cols, c("CF..Time.", "CF..reltime.", "Duration..sec."))
    fold <- default_col(cols, "fold")
    shiny::tagList(
      shiny::selectInput("subject_col", "Subject", cols,
                         selected = first_or(subject, cols[1L]), width = "100%"),
      shiny::selectInput("response_col", "Response",
                         unique(c("CF..ansbin.", cols)),
                         selected = first_or(response, "CF..ansbin."),
                         width = "100%"),
      shiny::selectInput("time_col", "Order/time", c("(row order)", cols),
                         selected = if (length(time)) time else "(row order)",
                         width = "100%"),
      shiny::selectInput("fold_col", "Fold", c("(none)", cols),
                         selected = if (length(fold)) fold else "(none)",
                         width = "100%")
    )
  })

  prepared_data <- shiny::reactive({
    shiny::req(input$response_col)
    prepare_response_column(raw_data(), input$response_col)$data
  })

  shiny::observeEvent(raw_data(), {
    state$spacing_data <- NULL
    state$fit <- NULL
  }, ignoreInit = TRUE)

  analysis_data <- shiny::reactive({
    if (!is.null(state$spacing_data)) {
      return(state$spacing_data)
    }
    prepared_data()
  })

  component_choices <- shiny::reactive({
    dt <- prepared_data()
    choices <- candidate_components(
      dt,
      input$subject_col,
      "CF..ansbin.",
      if (identical(input$time_col, "(row order)")) character(0) else input$time_col,
      if (identical(input$fold_col, "(none)")) character(0) else input$fold_col
    )
    if (length(choices) == 0L) names(dt) else choices
  })

  output$term_controls <- shiny::renderUI({
    components <- component_choices()
    data_for_choices <- prepared_data()
    interactions <- c("(none)", names(data_for_choices))
    default_component <- if ("KC..Default." %in% components) "KC..Default." else components[1L]
    rows <- lapply(seq_len(state$n_terms), function(i) {
      selected_component <- input[[paste0("component_", i)]]
      if (is.null(selected_component) || !selected_component %in% components) {
        selected_component <- default_component
      }
      row_features <- feature_choices_for_component(data_for_choices, selected_component)
      default_feature <- if (i == 1L) "intercept" else "lineafm"
      selected_feature <- input[[paste0("feature_", i)]]
      if (is.null(selected_feature) || !selected_feature %in% row_features) {
        selected_feature <- if (default_feature %in% row_features) {
          default_feature
        } else {
          row_features[1L]
        }
      }
      shiny::div(
        class = "term-row",
        shiny::checkboxInput(paste0("on_", i), label = "On", value = TRUE),
        shiny::selectInput(paste0("component_", i), "Component",
                           components, selected = selected_component),
        shiny::selectInput(paste0("feature_", i), "Feature",
                           row_features, selected = selected_feature),
        shiny::textInput(paste0("fixed_", i), "Fixed par(s)", value = "NA"),
        shiny::selectInput(paste0("interaction_", i), "Interaction",
                           interactions, selected = "(none)"),
        shiny::selectInput(paste0("connector_", i), "Connector",
                           c("+", ":", "*"), selected = "+")
      )
    })
    shiny::tagList(rows)
  })

  shiny::observeEvent(input$add_term, {
    state$n_terms <- state$n_terms + 1L
  })

  shiny::observeEvent(input$remove_term, {
    state$n_terms <- max(1L, state$n_terms - 1L)
  })

  shiny::observeEvent(input$compute_spacing, {
    shiny::req(input$subject_col)
    terms <- term_table(input, state$n_terms)
    comps <- unique(terms$component[terms$feature %in% spacing_features])
    if (length(comps) < 1L) {
      state$status <- "No active spacing-dependent terms selected."
      return()
    }
    dt <- analysis_data()
    tryCatch({
      for (component in comps) {
        dt <- computeSpacingPredictors(dt, component)
      }
      state$spacing_data <- dt
      state$status <- paste("Spacing computed for:", paste(comps, collapse = ", "))
    }, error = function(err) {
      state$status <- conditionMessage(err)
    })
  })

  shiny::observeEvent(input$fit, {
    terms <- term_table(input, state$n_terms)
    dt <- analysis_data()
    spacing_needed <- unique(terms$component[terms$feature %in% spacing_features])
    tryCatch({
      if (length(spacing_needed) > 0L) {
        for (component in spacing_needed) {
          if (!paste0(component, "spacing") %in% names(dt)) {
            dt <- computeSpacingPredictors(dt, component)
          }
        }
      }
      elapsed <- system.time({
        state$fit <- fit_lkt_from_terms(dt, terms, input$intercept)
      })
      state$fit_time <- elapsed[["elapsed"]]
      state$status <- sprintf("Fit complete in %.2f seconds.", state$fit_time)
    }, error = function(err) {
      state$status <- conditionMessage(err)
    })
  })

  output$status <- shiny::renderText(state$status)

  output$fit_metrics <- shiny::renderTable({
    shiny::req(state$fit)
    data.frame(
      metric = c("model", "loglike", "r2", "elapsed_seconds"),
      value = c(
        state$fit$model_name %||% NA_character_,
        state$fit$loglike %||% NA_real_,
        state$fit$r2 %||% NA_real_,
        state$fit_time %||% NA_real_
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE)

  output$model_call <- shiny::renderText({
    terms <- term_table(input, state$n_terms)
    format_lkt_call(terms, input$intercept)
  })

  output$term_preview <- shiny::renderTable({
    terms <- term_table(input, state$n_terms)
    if (nrow(terms) < 1L) {
      return(data.frame(message = "No active terms."))
    }
    terms[, c("component", "feature", "fixedpar", "interaction", "connector"),
          drop = FALSE]
  }, striped = TRUE, bordered = TRUE)

  output$coef_table <- shiny::renderTable({
    shiny::req(state$fit)
    coefs <- as.data.frame(state$fit$coefs)
    coefs$term <- rownames(coefs)
    coefs[, c("term", setdiff(names(coefs), "term")), drop = FALSE]
  }, striped = TRUE, bordered = TRUE)

  output$prediction_preview <- shiny::renderTable({
    shiny::req(state$fit)
    dt <- data.table::as.data.table(state$fit$newdata)
    keep <- intersect(c(input$subject_col, "KC..Default.", "CF..ansbin.", "pred"),
                      names(dt))
    head(dt[, ..keep], 20)
  }, striped = TRUE, bordered = TRUE)

  output$plot_controls <- shiny::renderUI({
    components <- component_choices()
    default_component <- if ("KC..Default." %in% components) "KC..Default." else components[1L]
    shiny::tagList(
      shiny::selectInput("plot_aggregate", "Aggregate",
                         c("By KC" = "kc", "All cases" = "overall"),
                         selected = "kc"),
      shiny::selectInput("plot_kc_col", "KC/component", components,
                         selected = default_component),
      shiny::numericInput("plot_min_n", "Minimum N", value = 1, min = 1, step = 1),
      shiny::numericInput("plot_top_k", "Top K KCs", value = 12, min = 1, step = 1)
    )
  })

  kc_plot_data <- shiny::reactive({
    order_col <- if (identical(input$time_col, "(row order)")) NULL else input$time_col
    kc_trial_summary(
      prepared_data(),
      subject_col = input$subject_col,
      kc_col = input$plot_kc_col,
      response_col = "CF..ansbin.",
      order_col = order_col,
      min_n = input$plot_min_n,
      top_k = input$plot_top_k,
      aggregate = input$plot_aggregate %||% "kc"
    )
  })

  output$kc_trial_plot <- shiny::renderPlot({
    plot_data <- kc_plot_data()
    shiny::validate(shiny::need(nrow(plot_data) > 0L, "No KC-by-trial data to plot."))
    kc_col <- input$plot_kc_col
    if (identical(input$plot_aggregate, "overall")) {
      mapping <- ggplot2::aes(
        x = trial,
        y = probability_correct,
        color = series,
        group = series
      )
      color_label <- "Series"
    } else {
      mapping <- ggplot2::aes(
        x = trial,
        y = probability_correct,
        color = .data[[kc_col]],
        group = .data[[kc_col]]
      )
      color_label <- "KC"
    }
    ggplot2::ggplot(plot_data, mapping) +
      ggplot2::geom_line(linewidth = 0.6) +
      ggplot2::geom_point(ggplot2::aes(size = n), alpha = 0.75) +
      ggplot2::coord_cartesian(ylim = c(0, 1)) +
      ggplot2::labs(
        x = "Trial within KC",
        y = "Mean probability correct",
        color = color_label,
        size = "N"
      ) +
      ggplot2::theme_minimal(base_size = 11)
  })

  output$kc_trial_table <- shiny::renderTable({
    head(kc_plot_data(), 30)
  }, striped = TRUE, bordered = TRUE)
}

shiny::shinyApp(ui, server)
