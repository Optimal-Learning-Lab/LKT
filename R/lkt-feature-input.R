lkt_clean_feature_name <- function(features) {
  gsub("[@$]", "", features)
}

lkt_feature_registry <- function() {
  data.frame(
    feature = c(
      "intercept", "numer", "lineafm", "logafm", "powafm",
      "recency", "recencytest", "recencystudy", "expdecafm",
      "base", "base2", "base4", "ppe", "ppes", "ppef",
      "base5suc", "base5fail", "dashafm", "dashsuc", "dashfail",
      "diffrelcor1", "diffrelcor2", "diffcor1", "diffcor2",
      "diffcorComp", "diffincorComp", "diffallComp",
      "diffincor1", "diffincor2", "diffall1", "diffall2",
      "logsuc", "logsucadj", "linesuc", "logfail", "linefail",
      "recencyfail", "recencysuc", "expdecsuc", "expdecfail",
      "basesuc", "basefail", "base2fail", "base2suc",
      "linecomp", "logit", "errordec", "propdec", "propdec2",
      "logitdec", "logitdecevol", "baseratepropdec", "prop"
    ),
    param_count = c(
      0, 0, 0, 0, 1,
      1, 1, 1, 1,
      1, 2, 4, 4, 4, 4,
      5, 5, 1, 1, 1,
      0, 0, 0, 0,
      0, 0, 0,
      0, 0, 0, 0,
      0, 1, 0, 0, 0,
      1, 1, 1, 1,
      1, 1, 2, 2,
      0, 1, 1, 1, 1,
      1, 1, 1, 0
    ),
    stringsAsFactors = FALSE
  )
}

lkt_feature_param_counts <- function(features, allow_unknown = FALSE) {
  clean_features <- lkt_clean_feature_name(features)
  registry <- lkt_feature_registry()
  idx <- match(clean_features, registry$feature)
  unknown <- is.na(idx)
  if (any(unknown) && !allow_unknown) {
    stop("Unknown LKT feature(s): ",
         paste(unique(clean_features[unknown]), collapse = ", "), call. = FALSE)
  }
  counts <- rep(0L, length(clean_features))
  counts[!unknown] <- registry$param_count[idx[!unknown]]
  counts
}

lkt_validate_feature_spec <- function(components, features) {
  if (length(components) != length(features)) {
    stop("components and features must have the same length.", call. = FALSE)
  }
  lkt_feature_param_counts(features)
  invisible(TRUE)
}

lkt_normalize_parameter_vector <- function(values, required_count, label) {
  if (required_count == 0L) {
    return(numeric(0))
  }
  if (length(values) == 1L && is.na(values)) {
    return(rep(NA_real_, required_count))
  }
  if (length(values) != required_count) {
    stop(label, " must have length ", required_count,
         " for the requested nonlinear features; got ", length(values), ".",
         call. = FALSE)
  }
  as.numeric(values)
}

lkt_feature_column_name <- function(feature, component, para = NULL, nosolve = FALSE) {
  feature_base <- gsub("\\$", "", feature)
  component_base <- gsub("[%]", "", component)
  if (isTRUE(nosolve)) {
    paste0(feature_base, if (!is.null(para)) para else "", component_base)
  } else {
    paste0(feature_base, component_base)
  }
}

lkt_formula_connector <- function(connector) {
  if (connector == "*") {
    "*"
  } else if (connector == ":") {
    ":"
  } else {
    "+"
  }
}

lkt_formula_term <- function(feature, component, connector, current_formula,
                             interact = NA, para = NULL, nosolve = FALSE) {
  component_base <- gsub("[%]", "", component)
  para_text <- if (!is.null(para)) para else ""

  if (right(feature, 1) == "$") {
    clean_feature <- gsub("\\$", "", feature)
    if (is.na(interact)) {
      if (isTRUE(nosolve)) {
        paste(clean_feature, para_text, component, ":e$data$", component,
              connector, current_formula, sep = "")
      } else {
        paste(clean_feature, component, ":e$data$", component,
              connector, current_formula, sep = "")
      }
    } else {
      if (isTRUE(nosolve)) {
        paste(clean_feature, para_text, component, ":e$data$", component,
              ":", interact, connector, current_formula, sep = "")
      } else {
        paste(clean_feature, component, ":e$data$", component,
              ":", interact, connector, current_formula, sep = "")
      }
    }
  } else if (right(feature, 1) == "@") {
    paste("(1|", component, ")+", current_formula, sep = "")
  } else {
    if (is.na(interact)) {
      if (isTRUE(nosolve)) {
        paste(feature, para_text, component_base, connector, current_formula, sep = "")
      } else {
        paste(feature, component_base, connector, current_formula, sep = "")
      }
    } else {
      if (isTRUE(nosolve)) {
        paste(feature, para_text, component_base, ":", interact,
              connector, current_formula, sep = "")
      } else {
        paste(feature, component_base, ":", interact,
              connector, current_formula, sep = "")
      }
    }
  }
}

lkt_get_formula_terms <- function(formula, data, cache = NULL) {
  formula_key <- as.character(formula)
  formula_key <- paste(formula_key, collapse = "\n")
  if (!is.null(cache) && exists(formula_key, envir = cache, inherits = FALSE)) {
    return(get(formula_key, envir = cache, inherits = FALSE))
  }

  formula_terms <- stats::terms(formula, data = data)
  if (!is.null(cache)) {
    assign(formula_key, formula_terms, envir = cache)
  }
  formula_terms
}

lkt_sparse_model_matrix <- function(formula, data, terms_cache = NULL) {
  formula_terms <- lkt_get_formula_terms(formula, data, terms_cache)
  predictset <- sparse.model.matrix(formula_terms, data)
  predictset.csc <- new("matrix.csc",
                        ra = predictset@x,
                        ja = predictset@i + 1L,
                        ia = predictset@p + 1L,
                        dimension = predictset@Dim)
  list(
    sparse = predictset,
    csr = as.matrix.csr(predictset.csc)
  )
}

#' Create model-agnostic LKT feature input
#'
#' @description Builds the shared input consumed by LKT models. The feature input contains the fitted formula, processed data, response vector, feature specification, sparse design matrices, and design-matrix column names.
#'
#' @param formula Model formula after LKT feature construction.
#' @param data Processed trial-level data used for fitting.
#' @param feature_spec Per-feature metadata collected during feature construction.
#' @param terms_cache Optional environment used to cache formula terms.
#' @return An LKT feature input object consumed by model adapters.
#' @examples
#' data(samplelkt)
#' data.table::setDT(samplelkt)
#' samplelkt$CF..ansbin. <- as.integer(samplelkt$Outcome == "CORRECT")
#' input <- LKTFeatureInput(
#'   CF..ansbin. ~ KC..Default.,
#'   samplelkt,
#'   feature_spec = data.frame(feature = "intercept", component = "KC..Default.")
#' )
#' input$column_names
#' @export
LKTFeatureInput <- function(formula, data, feature_spec, terms_cache = NULL) {
  design_matrix <- lkt_sparse_model_matrix(formula, data, terms_cache)
  structure(list(
    formula = formula,
    data = data,
    response = data$CF..ansbin.,
    feature_spec = feature_spec,
    design_matrix = design_matrix,
    column_names = colnames(design_matrix$sparse)
  ), class = c("LKTFeatureInput", "list"))
}

lkt_feature_input <- function(formula, data, feature_spec, terms_cache = NULL) {
  LKTFeatureInput(
    formula = formula,
    data = data,
    feature_spec = feature_spec,
    terms_cache = terms_cache)
}
