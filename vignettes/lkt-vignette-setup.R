knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7.5,
  fig.path = "vigfig-"
)

library(LKT)
library(ggplot2)
library(pROC)
library(glmnet)
library(crayon)
library(dplyr)
library(boot)
library(cluster)
library(HDInterval)

if (identical(tolower(Sys.getenv("LKT_SOURCE_WORKTREE")), "true")) {
  lkt_source_files <- c(
    "lkt-feature-input.R",
    "lkt-feature-computation.R",
    "lkt-model-interface.R",
    "lkt-model-liblinear.R",
    "lkt-model-online-calibration.R",
    "lkt-fast-online-simple-adaptive.R",
    "lkt-latency.R",
    "lkt-prediction.R",
    "LKTfunctions.R",
    "lkt-hdi.R",
    "lkt-search.R"
  )
  for (lkt_source_file in lkt_source_files) {
    source(file.path("..", "R", lkt_source_file))
  }
}

prepare_largeraw_sample <- function(seed = 41) {
  set.seed(seed)
  val <- largerawsample

  val$KC..Default. <- val$Problem.Name
  val <- data.table::setDT(val)

  val$fold <- sample(1:5, length(val$Anon.Student.Id), replace = TRUE)

  unq <- sample(unique(val$Anon.Student.Id))
  sfold <- rep(1:5, length.out = length(unq))
  val$fold <- rep(0, length(val[, 1]))
  for (i in 1:5) {
    val$fold[which(val$Anon.Student.Id %in% unq[which(sfold == i)])] <- i
  }

  val$CF..Time. <- as.numeric(as.POSIXct(
    as.character(val$Time),
    format = "%Y-%m-%d %H:%M:%S"
  ))

  val <- val[order(val$Anon.Student.Id, val$CF..Time.), ]

  val$CF..ansbin. <- ifelse(
    tolower(val$Outcome) == "correct",
    1,
    ifelse(tolower(val$Outcome) == "incorrect", 0, -1)
  )
  val <- val[val$CF..ansbin. == 0 | val$CF..ansbin. == 1, ]

  val$Duration..sec. <- (
    val$CF..End.Latency. + val$CF..Review.Latency. + 500
  ) / 1000

  val <- computeSpacingPredictors(val, "KC..Default.")
  val <- computeSpacingPredictors(val, "KC..Cluster.")
  val <- computeSpacingPredictors(val, "Anon.Student.Id")
  val <- computeSpacingPredictors(val, "CF..Correct.Answer.")

  val
}

example_data_path <- function(filename) {
  file.path("C:", "Users", "ppavl", "OneDrive", "Active projects", filename)
}

prepare_mathia_sample <- function(seed = 41) {
  set.seed(seed)
  datafile <- example_data_path("ds4845_tx_All_Data_6977_2021_0723_141809.txt")
  check_true("MATHia data file exists", file.exists(datafile))

  val2 <- read.delim(
    colClasses = c("Anon.Student.Id" = "character"),
    datafile,
    sep = "\t",
    header = TRUE,
    quote = ""
  )
  val2 <- data.table::as.data.table(val2)
  val2$CF..Time. <- as.numeric(as.POSIXct(
    as.character(val2$Time),
    format = "%Y-%m-%d %H:%M:%S"
  ))
  val2 <- val2[order(val2$Anon.Student.Id, val2$CF..Time.), ]
  val2$Outcome <- ifelse(tolower(val2$Outcome) == "ok", "CORRECT", "INCORRECT")
  val2$CF..ansbin. <- ifelse(tolower(val2$Outcome) == "correct", 1, 0)
  val2 <- val2[val2$CF..ansbin. == 0 | val2$CF..ansbin. == 1, ]
  val2 <- val2[val2$Attempt.At.Step == 1, ]
  val2 <- val2[val2$KC..MATHia. != "", ]

  unq <- sample(unique(val2$Anon.Student.Id))
  sfold <- rep(1:5, length.out = length(unq))
  val2$fold <- rep(0, length(val2[, 1]))
  for (i in 1:5) {
    val2$fold[which(val2$Anon.Student.Id %in% unq[which(sfold == i)])] <- i
  }

  val2 <- suppressWarnings(computeSpacingPredictors(val2, "KC..MATHia."))
  val2 <- suppressWarnings(computeSpacingPredictors(val2, "Problem.Name"))
  val2 <- suppressWarnings(computeSpacingPredictors(val2, "Anon.Student.Id"))

  val2
}

prepare_assistments_sample <- function(seed = 42) {
  set.seed(seed)
  datafile <- example_data_path("2012-2013-data-with-predictions-4-final.csv")
  check_true("Assistments data file exists", file.exists(datafile))

  val3 <- data.table::fread(
    colClasses = c("user_id" = "character"),
    datafile,
    header = TRUE
  )
  val3$Anon.Student.Id <- val3$user_id

  selected_users <- sample(
    unique(val3$user_id),
    size = floor(.05 * length(unique(val3$user_id)))
  )
  val3 <- val3[val3$user_id %in% selected_users, ]
  val3 <- val3[val3$skill != "", ]
  val3 <- val3 %>% distinct(user_id, start_time, .keep_all = TRUE)

  val3$CF..Time. <- as.numeric(as.POSIXct(
    as.character(val3$start_time),
    format = "%Y-%m-%d %H:%M:%S"
  ))
  val3 <- val3[order(val3$Anon.Student.Id, val3$CF..Time.), ]
  val3$CF..ansbin. <- val3$correct
  val3 <- val3[val3$CF..ansbin. == 0 | val3$CF..ansbin. == 1, ]
  val3$Duration..sec. <- as.numeric(as.POSIXct(
    as.character(val3$end_time),
    format = "%Y-%m-%d %H:%M:%S"
  )) - as.numeric(as.POSIXct(
    as.character(val3$start_time),
    format = "%Y-%m-%d %H:%M:%S"
  ))
  val3$Outcome <- ifelse(val3$correct == 1, "CORRECT", "INCORRECT")

  val3 <- val3 %>%
    group_by(user_id) %>%
    filter(n() >= 20) %>%
    ungroup()

  unq <- sample(unique(val3$Anon.Student.Id))
  sfold <- rep(1:5, length.out = length(unq))
  val3$fold <- rep(0, length(val3[, 1]))
  for (i in 1:5) {
    val3$fold[which(val3$Anon.Student.Id %in% unq[which(sfold == i)])] <- i
  }

  val3 <- computeSpacingPredictors(val3, "skill")
  val3 <- computeSpacingPredictors(val3, "Anon.Student.Id")
  val3 <- computeSpacingPredictors(val3, "problem_type")
  val3 <- computeSpacingPredictors(val3, "type")
  data.table::setDT(val3)
}

check_close <- function(label, actual, expected, tolerance = 1e-4) {
  if (!isTRUE(is.finite(actual)) ||
      abs(as.numeric(actual) - as.numeric(expected)) > tolerance) {
    stop(
      sprintf(
        "%s expected %s +/- %s but got %s",
        label,
        expected,
        tolerance,
        actual
      ),
      call. = FALSE
    )
  }
  cat(sprintf("CHECK OK: %s = %.6f\n", label, as.numeric(actual)))
  invisible(TRUE)
}

check_true <- function(label, condition) {
  if (!isTRUE(condition)) {
    stop(sprintf("%s check failed", label), call. = FALSE)
  }
  cat(sprintf("CHECK OK: %s\n", label))
  invisible(TRUE)
}

check_has_coefficients <- function(model, expected_names) {
  missing <- setdiff(expected_names, rownames(model$coefs))
  if (length(missing) > 0) {
    stop(
      sprintf("missing expected coefficients: %s", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
  cat(sprintf(
    "CHECK OK: expected coefficients present: %s\n",
    paste(expected_names, collapse = ", ")
  ))
  invisible(TRUE)
}

check_has_coefficient_matching <- function(model, pattern) {
  hits <- grep(pattern, rownames(model$coefs), value = TRUE)
  if (length(hits) == 0) {
    stop(
      sprintf("no coefficient matched pattern: %s", pattern),
      call. = FALSE
    )
  }
  cat(sprintf("CHECK OK: coefficient pattern matched: %s\n", pattern))
  invisible(TRUE)
}

check_has_coefficient_containing <- function(model, text) {
  hits <- grep(text, rownames(model$coefs), value = TRUE, fixed = TRUE)
  if (length(hits) == 0) {
    stop(
      sprintf("no coefficient contained text: %s", text),
      call. = FALSE
    )
  }
  cat(sprintf("CHECK OK: coefficient text found: %s\n", text))
  invisible(TRUE)
}

check_lkt_fit <- function(model, expected_r2, expected_loglike,
                          r2_tolerance = 1e-4,
                          loglike_tolerance = 1e-2,
                          min_prediction_n = 1) {
  check_true("model has coefficients", !is.null(model$coefs) && nrow(model$coefs) > 0)
  check_close("McFadden R2", model$r2, expected_r2, r2_tolerance)
  check_close("log likelihood", model$loglike, expected_loglike, loglike_tolerance)
  check_true(
    "prediction length",
    length(model$prediction) >= min_prediction_n || is.null(model$prediction)
  )
  invisible(TRUE)
}
