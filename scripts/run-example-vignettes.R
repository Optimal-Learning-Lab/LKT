args <- commandArgs(trailingOnly = TRUE)

mode <- "report"
force <- FALSE
patterns <- character()
for (arg in args) {
  if (arg %in% c("--execute", "--render", "--report", "--stitch")) {
    mode <- sub("^--", "", arg)
  } else if (arg == "--force") {
    force <- TRUE
  } else {
    patterns <- c(patterns, arg)
  }
}

if (length(patterns) == 0) {
  patterns <- "^examples-.*[.]Rmd$"
}

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
vignette_dir <- file.path(root, "vignettes")
report_dir <- file.path(vignette_dir, "_example-reports")
dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
manifest_file <- file.path(vignette_dir, "example-section-manifest.csv")

files <- list.files(vignette_dir, pattern = paste(patterns, collapse = "|"),
                    full.names = TRUE)

if (length(files) == 0) {
  stop("No vignette files matched: ", paste(patterns, collapse = ", "))
}

Sys.setenv(LKT_SOURCE_WORKTREE = "true")

source_files_for_hash <- function(file) {
  candidates <- c(
    file,
    file.path(vignette_dir, "lkt-vignette-setup.R"),
    sort(list.files(file.path(root, "R"), pattern = "[.]R$",
                    full.names = TRUE)),
    file.path(root, "DESCRIPTION")
  )
  candidates[file.exists(candidates)]
}

source_fingerprint <- function(file) {
  hashes <- tools::md5sum(source_files_for_hash(file))
  paste(names(hashes), hashes, sep = "=", collapse = "\n")
}

report_paths <- function(file) {
  stem <- tools::file_path_sans_ext(basename(file))
  list(
    markdown = file.path(report_dir, paste0(stem, ".md")),
    metadata = file.path(report_dir, paste0(stem, ".rds"))
  )
}

metadata_matches <- function(file) {
  paths <- report_paths(file)
  if (!file.exists(paths$markdown) || !file.exists(paths$metadata)) {
    return(FALSE)
  }
  meta <- tryCatch(readRDS(paths$metadata), error = function(e) NULL)
  if (is.null(meta)) {
    return(FALSE)
  }
  if (!identical(meta$fingerprint, source_fingerprint(file)) ||
      !meta$status %in% c("OK", "FAIL")) {
    return(FALSE)
  }

  has_report_errors <- length(report_errors(paths$markdown)) > 0
  identical(meta$status, if (has_report_errors) "FAIL" else "OK")
}

report_errors <- function(markdown_file) {
  if (!file.exists(markdown_file)) {
    return("report file was not created")
  }

  lines <- readLines(markdown_file, warn = FALSE)
  error_hits <- grep(
    "^#> Error|^Error in|^#> ! .*check failed|Execution halted|Quitting from",
    lines,
    value = TRUE
  )

  if (length(error_hits) == 0) {
    return(character())
  }

  unique(error_hits)
}

extract_chunks <- function(file) {
  lines <- readLines(file, warn = FALSE)
  chunks <- list()
  in_chunk <- FALSE
  current <- character()
  start_line <- NA_integer_
  chunk_index <- 0L

  for (i in seq_along(lines)) {
    line <- lines[[i]]
    if (!in_chunk && grepl("^```\\s*\\{?r", line)) {
      in_chunk <- TRUE
      current <- character()
      start_line <- i + 1L
      next
    }
    if (in_chunk && grepl("^```\\s*$", line)) {
      chunk_index <- chunk_index + 1L
      chunks[[length(chunks) + 1L]] <- list(
        index = chunk_index,
        start = start_line,
        end = i - 1L,
        code = paste(current, collapse = "\n")
      )
      in_chunk <- FALSE
      next
    }
    if (in_chunk) {
      current <- c(current, line)
    }
  }

  chunks
}

execute_vignette <- function(file) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(dirname(file))

  env <- new.env(parent = globalenv())
  chunks <- extract_chunks(file)
  for (chunk in chunks) {
    if (!nzchar(trimws(chunk$code))) {
      next
    }
    eval(parse(text = chunk$code), envir = env)
  }
}

knit_report <- function(file) {
  paths <- report_paths(file)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(dirname(file))

  knitr::knit(
    input = basename(file),
    output = paths$markdown,
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
}

run_one <- function(file) {
  name <- basename(file)
  started <- proc.time()[["elapsed"]]
  error_message <- ""
  paths <- report_paths(file)

  if (mode %in% c("report", "stitch") && !force && metadata_matches(file)) {
    meta <- readRDS(report_paths(file)$metadata)
    message("CACHED ", name, " (", meta$status, ")")
    return(data.frame(
      file = name,
      status = meta$status,
      elapsed = 0,
      report = report_paths(file)$markdown,
      message = if (!is.null(meta$message)) meta$message else "",
      stringsAsFactors = FALSE
    ))
  }

  if (mode == "stitch") {
    if (file.exists(paths$markdown)) {
      errors <- report_errors(paths$markdown)
      status <- if (length(errors) == 0) "STALE" else "FAIL"
      message(status, " ", name, " (stitching existing report)")
      return(data.frame(
        file = name,
        status = status,
        elapsed = 0,
        report = paths$markdown,
        message = paste(errors, collapse = " | "),
        stringsAsFactors = FALSE
      ))
    }
    message("MISS ", name, " (no report to stitch)")
    return(data.frame(
      file = name,
      status = "MISS",
      elapsed = 0,
      report = paths$markdown,
      message = "no report to stitch",
      stringsAsFactors = FALSE
    ))
  }

  message("RUN  ", name)
  ok <- tryCatch({
    if (mode == "render") {
      if (!rmarkdown::pandoc_available()) {
        stop("Pandoc is not available; use --execute, --report, or install Pandoc")
      }
      rmarkdown::render(file, output_dir = tempdir(), quiet = TRUE,
                        envir = new.env(parent = globalenv()))
    } else if (mode == "report") {
      knit_report(file)
    } else {
      execute_vignette(file)
    }
    TRUE
  }, error = function(e) {
    elapsed <- proc.time()[["elapsed"]] - started
    error_message <<- conditionMessage(e)
    message("FAIL ", name, " after ", round(elapsed, 2), "s: ",
            conditionMessage(e))
    FALSE
  })

  elapsed <- proc.time()[["elapsed"]] - started
  if (ok && mode == "report") {
    errors <- report_errors(paths$markdown)
    if (length(errors) > 0) {
      ok <- FALSE
      error_message <- paste(errors, collapse = " | ")
      message("FAIL ", name, " after ", round(elapsed, 2),
              "s: embedded errors in report")
    }
  }
  if (ok) {
    message("OK   ", name, " after ", round(elapsed, 2), "s")
  }

  paths <- report_paths(file)
  if (mode == "report") {
    saveRDS(list(
      source = basename(file),
      status = if (ok) "OK" else "FAIL",
      generated_at = as.character(Sys.time()),
      elapsed = elapsed,
      message = error_message,
      fingerprint = source_fingerprint(file)
    ), paths$metadata)
  }

  data.frame(
    file = name,
    status = if (ok) "OK" else "FAIL",
    elapsed = elapsed,
    report = paths$markdown,
    message = error_message,
    stringsAsFactors = FALSE
  )
}

section_coverage <- function(results) {
  if (!file.exists(manifest_file)) {
    return(NULL)
  }

  manifest <- read.csv(manifest_file, stringsAsFactors = FALSE,
                       na.strings = c("", "NA"), check.names = FALSE)
  manifest$status <- "PENDING"
  manifest$result_file <- NA_character_

  for (i in seq_len(nrow(manifest))) {
    vignette <- manifest$vignette[[i]]
    if (is.na(vignette) || !nzchar(vignette)) {
      next
    }
    hit <- results[results$file == vignette, , drop = FALSE]
    if (nrow(hit) == 0) {
      next
    }
    manifest$result_file[[i]] <- hit$file[[1]]
    manifest$status[[i]] <- if (hit$status[[1]] %in% c("OK", "CACHED", "STALE")) {
      "OK"
    } else {
      "FAIL"
    }
  }

  manifest
}

stitch_reports <- function(results, coverage = NULL) {
  full_report <- file.path(report_dir, "examples-full-report.md")
  coverage_report <- file.path(report_dir, "section-coverage.csv")
  ok_reports <- results[results$status %in% c("OK", "CACHED", "STALE"), ]
  missing <- results[results$status %in% c("FAIL", "MISS"), ]

  if (!is.null(coverage)) {
    write.csv(coverage, coverage_report, row.names = FALSE)
  }

  lines <- c(
    "# LKT Example Vignette Report",
    "",
    paste("Generated:", as.character(Sys.time())),
    "",
    "## Run Summary",
    "",
    "Status definitions:",
    "",
    "- OK: the split vignette for the mapped original section executed successfully.",
    "- FAIL: the split vignette for the mapped original section errored.",
    "- CACHED: an earlier OK report was reused because the source fingerprint was unchanged.",
    "- PENDING: the original section is not yet mapped to an individual report.",
    "",
    paste(capture.output(print(results[, c("file", "status", "elapsed")],
                               row.names = FALSE)), collapse = "\n"),
    ""
  )

  if (!is.null(coverage)) {
    lines <- c(
      lines,
      "## Original Section Coverage",
      "",
      paste(capture.output(print(coverage[, c("section_id", "original_line", "status", "vignette")],
                                 row.names = FALSE)), collapse = "\n"),
      ""
    )
  }

  if (nrow(missing) > 0) {
    lines <- c(lines, "## Missing or Failed Reports", "")
    for (i in seq_len(nrow(missing))) {
      lines <- c(lines, paste0(
        "- ", missing$file[[i]], ": ", missing$status[[i]],
        if (nzchar(missing$message[[i]])) paste0(" - ", missing$message[[i]]) else ""
      ))
    }
    lines <- c(lines, "")
  }

  for (i in seq_len(nrow(ok_reports))) {
    report <- ok_reports$report[[i]]
    lines <- c(
      lines,
      "",
      paste0("\\newpage"),
      "",
      readLines(report, warn = FALSE)
    )
  }

  writeLines(lines, full_report)
  full_report
}

results <- do.call(rbind, lapply(files, run_one))
coverage <- section_coverage(results)
cat("\n")
print(results[, c("file", "status", "elapsed")], row.names = FALSE)
failed_results <- results[results$status %in% c("FAIL", "MISS"), , drop = FALSE]
if (nrow(failed_results) > 0) {
  cat("\nFailures:\n")
  print(failed_results[, c("file", "status", "message")], row.names = FALSE)
}

if (!is.null(coverage)) {
  cat("\nOriginal section coverage:\n")
  print(coverage[, c("section_id", "original_line", "status", "vignette")],
        row.names = FALSE)
}

if (mode %in% c("report", "stitch")) {
  full_report <- stitch_reports(results, coverage)
  cat("\nFull report:", normalizePath(full_report, winslash = "/"), "\n")
}

if (any(results$status %in% c("FAIL", "MISS"))) {
  quit(status = 1)
}
