load_lkt_namespace <- function(root = getwd()) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  if (basename(root) == "lkt-model-builder") {
    root <- normalizePath(file.path(root, "..", ".."),
                          winslash = "/", mustWork = FALSE)
  }
  if (file.exists(file.path(root, "DESCRIPTION")) &&
      requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(root, quiet = TRUE)
    return(invisible(TRUE))
  }
  if (requireNamespace("LKT", quietly = TRUE)) {
    library(LKT)
    return(invisible(TRUE))
  }
  stop("LKT is not loadable. Install LKT or install pkgload and run this app ",
       "from the package checkout.", call. = FALSE)
}

feature_choices <- c(
  "intercept", "numer", "lineafm", "logafm", "powafm",
  "linesuc", "linefail", "logsuc", "logfail",
  "recency", "recencysuc", "recencyfail", "recencystudy", "recencytest",
  "expdecafm", "expdecsuc", "expdecfail",
  "propdec", "propdec2", "base", "base2", "base4", "basesuc",
  "basefail", "base2suc", "base2fail",
  "ppe", "ppes", "ppef",
  "diffrelcor1", "diffrelcor2", "diffcor1", "diffcor2",
  "diffcorComp", "diffincorComp", "diffallComp",
  "diffincor1", "diffincor2", "diffall1", "diffall2",
  "dashafm", "dashsuc", "dashfail",
  "linecomp", "logit", "errordec", "logitdec", "logitdecevol",
  "baseratepropdec", "prop"
)

feature_param_counts <- c(
  intercept = 0L, numer = 0L, lineafm = 0L, logafm = 0L, powafm = 1L,
  recency = 1L, recencytest = 1L, recencystudy = 1L, expdecafm = 1L,
  base = 1L, base2 = 2L, base4 = 4L, ppe = 4L, ppes = 4L, ppef = 4L,
  base5suc = 5L, base5fail = 5L, dashafm = 1L, dashsuc = 1L,
  dashfail = 1L, diffrelcor1 = 0L, diffrelcor2 = 0L, diffcor1 = 0L,
  diffcor2 = 0L, diffcorComp = 0L, diffincorComp = 0L, diffallComp = 0L,
  diffincor1 = 0L, diffincor2 = 0L, diffall1 = 0L, diffall2 = 0L,
  logsuc = 0L, logsucadj = 1L, linesuc = 0L, logfail = 0L,
  linefail = 0L, recencyfail = 1L, recencysuc = 1L, expdecsuc = 1L,
  expdecfail = 1L, basesuc = 1L, basefail = 1L, base2fail = 2L,
  base2suc = 2L, linecomp = 0L, logit = 1L, errordec = 1L,
  propdec = 1L, propdec2 = 1L, logitdec = 1L, logitdecevol = 1L,
  baseratepropdec = 1L, prop = 0L
)

spacing_features <- c(
  "recency", "recencysuc", "recencyfail", "recencystudy", "recencytest",
  "ppe", "ppes", "ppef"
)

`%||%` <- function(x, y) if (is.null(x)) y else x

first_or <- function(x, y) {
  if (length(x) > 0L && !is.na(x[1L]) && nzchar(x[1L])) x[1L] else y
}

read_gui_file <- function(file_info) {
  path <- file_info$datapath
  ext <- tolower(tools::file_ext(file_info$name))
  if (identical(ext, "csv")) {
    return(data.table::fread(path))
  }
  if (identical(ext, "rds")) {
    return(readRDS(path))
  }
  if (identical(ext, "rdata") || identical(ext, "rda")) {
    env <- new.env(parent = emptyenv())
    loaded <- load(path, envir = env)
    objects <- mget(loaded, envir = env)
    data_objects <- objects[vapply(objects, function(x) {
      is.data.frame(x) || data.table::is.data.table(x)
    }, logical(1))]
    if (length(data_objects) != 1L) {
      stop("RData uploads must contain exactly one data.frame/data.table.",
           call. = FALSE)
    }
    return(data_objects[[1L]])
  }
  stop("Unsupported file type. Use .csv, .rds, .rda, or .RData.",
       call. = FALSE)
}

prepare_response_column <- function(data, response_col) {
  dt <- data.table::copy(data.table::as.data.table(data))
  if (identical(response_col, "Outcome") && "Outcome" %in% names(dt)) {
    dt[, CF..ansbin. := as.integer(Outcome == "CORRECT")]
    return(list(
      data = dt,
      response_col = "CF..ansbin.",
      note = "Derived CF..ansbin. from Outcome == \"CORRECT\"."
    ))
  }
  if (response_col %in% names(dt)) {
    return(list(data = dt, response_col = response_col, note = NULL))
  }
  if (identical(response_col, "CF..ansbin.") && "Outcome" %in% names(dt)) {
    dt[, CF..ansbin. := as.integer(Outcome == "CORRECT")]
    return(list(
      data = dt,
      response_col = "CF..ansbin.",
      note = "Derived CF..ansbin. from Outcome == \"CORRECT\"."
    ))
  }
  stop("Selected response column is not present.", call. = FALSE)
}

default_col <- function(names, preferred) {
  hit <- preferred[preferred %in% names]
  if (length(hit) > 0L) hit[1L] else character(0)
}

candidate_components <- function(data, subject_col, response_col,
                                 time_col, fold_col) {
  dt <- data.table::as.data.table(data)
  omit <- c(subject_col, response_col, "Outcome", time_col, fold_col)
  candidates <- setdiff(names(dt), omit)
  if (length(candidates) == 0L) {
    return(character(0))
  }
  candidates[vapply(dt[, ..candidates], function(x) {
    is.character(x) || is.factor(x) || is.integer(x) || length(unique(x)) <= 50L
  }, logical(1))]
}

component_supports_spacing <- function(data, component) {
  cols <- names(data)
  paste0(component, "spacing") %in% cols ||
    all(c("Anon.Student.Id", "CF..ansbin.") %in% cols) &&
      any(c("CF..reltime.", "Duration..sec.") %in% cols)
}

component_supports_numeric <- function(data, component) {
  component %in% names(data) && is.numeric(data[[component]])
}

feature_choices_for_component <- function(data, component) {
  choices <- feature_choices
  if (!component_supports_numeric(data, component)) {
    choices <- setdiff(choices, "numer")
  }
  if (!component_supports_spacing(data, component)) {
    choices <- setdiff(choices, spacing_features)
  }
  choices
}

term_table <- function(input, n_terms) {
  rows <- vector("list", n_terms)
  for (i in seq_len(n_terms)) {
    fixed <- input[[paste0("fixed_", i)]]
    rows[[i]] <- data.frame(
      on = isTRUE(input[[paste0("on_", i)]]),
      component = input[[paste0("component_", i)]] %||% NA_character_,
      feature = input[[paste0("feature_", i)]] %||% NA_character_,
      fixedpar = fixed %||% NA_character_,
      interaction = input[[paste0("interaction_", i)]] %||% "NA",
      connector = input[[paste0("connector_", i)]] %||% "+",
      stringsAsFactors = FALSE
    )
  }
  table <- do.call(rbind, rows)
  table[table$on, , drop = FALSE]
}

r_vector_text <- function(values, quote = TRUE, na_unquoted = FALSE) {
  values <- as.character(values)
  missing <- is.na(values)
  if (isTRUE(quote)) {
    values[!missing] <- paste0('"', gsub('"', '\\"', values[!missing],
                                        fixed = TRUE), '"')
  }
  if (isTRUE(na_unquoted)) {
    values[missing] <- "NA"
  }
  paste0("c(", paste(values, collapse = ", "), ")")
}

format_lkt_call <- function(terms, interc) {
  if (nrow(terms) < 1L) {
    return("LKT(\n  data = gui_data,\n  verbose = FALSE\n)")
  }
  fixedpars <- unlist(Map(fixedpars_for_feature, terms$fixedpar, terms$feature),
                      use.names = FALSE)
  fixed_text <- if (length(fixedpars) == 0L) "NA" else {
    paste(ifelse(is.na(fixedpars), "NA", as.character(fixedpars)),
          collapse = ", ")
  }
  interacts <- terms$interaction
  interacts[!nzchar(interacts) | is.na(interacts)] <- NA_character_
  interacts[interacts == "(none)" | interacts == "NA"] <- NA_character_
  connectors <- terms$connector[-1L]

  lines <- c(
    "LKT(",
    "  data = gui_data,",
    paste0("  components = ", r_vector_text(terms$component), ","),
    paste0("  features = ", r_vector_text(terms$feature), ","),
    paste0("  fixedpars = c(", fixed_text, "),"),
    paste0("  interacts = ", r_vector_text(interacts, na_unquoted = TRUE), ",")
  )
  if (length(connectors) > 0L) {
    lines <- c(lines, paste0("  connectors = ", r_vector_text(connectors), ","))
  }
  c(
    lines,
    paste0("  interc = ", if (isTRUE(interc)) "TRUE" else "FALSE", ","),
    "  verbose = FALSE",
    ")"
  ) |>
    paste(collapse = "\n")
}

fixedpars_for_feature <- function(value, feature) {
  feature <- gsub("[@$]", "", feature)
  count <- unname(feature_param_counts[[feature]])
  if (is.null(count) || is.na(count)) {
    stop("Unknown LKT feature: ", feature, call. = FALSE)
  }
  if (count == 0L) {
    return(numeric(0))
  }
  value <- trimws(value)
  if (!nzchar(value) || identical(toupper(value), "NA")) {
    return(rep(NA_real_, count))
  }
  pieces <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
  parsed <- as.numeric(pieces)
  if (any(is.na(parsed))) {
    stop("Fixed par(s) for ", feature, " must be numeric or NA.",
         call. = FALSE)
  }
  if (length(parsed) != count) {
    stop("Feature ", feature, " needs ", count,
         " fixed parameter value(s); provide comma-separated values or NA.",
         call. = FALSE)
  }
  parsed
}

kc_trial_summary <- function(data, subject_col, kc_col, response_col,
                             order_col = NULL, min_n = 1L, top_k = 12L,
                             aggregate = c("kc", "overall")) {
  aggregate <- match.arg(aggregate)
  dt <- data.table::copy(data.table::as.data.table(data))
  required <- c(subject_col, kc_col, response_col)
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0L) {
    stop("KC-by-trial plot missing required column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  dt <- dt[!is.na(get(subject_col)) &
             !is.na(get(kc_col)) &
             !is.na(get(response_col))]
  dt[, source_row := .I]
  if (!is.null(order_col) && nzchar(order_col) && order_col %in% names(dt)) {
    data.table::setorderv(dt, c(subject_col, kc_col, order_col, "source_row"))
  } else {
    data.table::setorderv(dt, c(subject_col, kc_col, "source_row"))
  }
  if (identical(aggregate, "overall")) {
    dt[, trial := seq_len(.N), by = c(subject_col, kc_col)]
    out <- dt[, .(
      probability_correct = mean(as.numeric(get(response_col)), na.rm = TRUE),
      n = .N,
      series = "All cases"
    ), by = "trial"]
    return(out[n >= min_n][order(trial)])
  }
  counts <- dt[, .N, by = kc_col][order(-N)]
  keep_kc <- counts[[kc_col]][seq_len(min(top_k, nrow(counts)))]
  dt <- dt[get(kc_col) %in% keep_kc]
  dt[, trial := seq_len(.N), by = c(subject_col, kc_col)]
  out <- dt[, .(
    probability_correct = mean(as.numeric(get(response_col)), na.rm = TRUE),
    n = .N
  ), by = c(kc_col, "trial")]
  out[n >= min_n][order(get(kc_col), trial)]
}

fit_lkt_from_terms <- function(data, terms, interc) {
  if (nrow(terms) < 1L) {
    stop("Add at least one active model term before fitting.", call. = FALSE)
  }
  fixedpars <- unlist(Map(fixedpars_for_feature, terms$fixedpar, terms$feature),
                      use.names = FALSE)
  if (length(fixedpars) == 0L) {
    fixedpars <- NA_real_
  }
  interacts <- terms$interaction
  interacts[!nzchar(interacts) | is.na(interacts)] <- NA_character_
  interacts[interacts == "(none)" | interacts == "NA"] <- NA_character_
  connectors <- terms$connector[-1L]
  clean_features <- gsub("[@$]", "", terms$feature)
  lkt_data <- if (all(clean_features == "intercept")) {
    as.data.frame(data)
  } else {
    data.table::copy(data.table::as.data.table(data))
  }
  if (length(connectors) > 0L) {
    return(LKT(
      data = lkt_data,
      components = terms$component,
      features = terms$feature,
      fixedpars = fixedpars,
      interacts = interacts,
      connectors = connectors,
      interc = isTRUE(interc),
      verbose = FALSE
    ))
  }
  LKT(
    data = lkt_data,
    components = terms$component,
    features = terms$feature,
    fixedpars = fixedpars,
    interacts = interacts,
    interc = isTRUE(interc),
    verbose = FALSE
  )
}
