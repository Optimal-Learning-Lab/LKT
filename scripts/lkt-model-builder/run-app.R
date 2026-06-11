args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1L && nzchar(args[1L])) as.integer(args[1L]) else 7865L
host <- if (length(args) >= 2L && nzchar(args[2L])) args[2L] else "127.0.0.1"

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install shiny before running the LKT model builder app.", call. = FALSE)
}

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
runner_file <- if (length(file_arg)) sub("^--file=", "", file_arg[1L]) else NULL
if (is.null(runner_file) || !nzchar(runner_file)) {
  runner_file <- "scripts/lkt-model-builder/run-app.R"
}
app_dir <- dirname(normalizePath(runner_file, winslash = "/",
                                mustWork = FALSE))
if (!file.exists(file.path(app_dir, "app.R"))) {
  app_dir <- file.path("scripts", "lkt-model-builder")
}

shiny::runApp(app_dir, port = port, host = host, launch.browser = FALSE)
