old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)

script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
if (is.na(script_file) || !nzchar(script_file)) {
  script_file <- file.path("scripts", "run-default-kc-feature-search.R")
}
vignette_dir <- normalizePath(
  file.path(dirname(script_file), "..", "vignettes"),
  winslash = "/",
  mustWork = TRUE
)
setwd(vignette_dir)

tmp <- tempfile(fileext = ".R")
knitr::purl("examples-build-search-default-kc-features.Rmd", output = tmp, quiet = TRUE)
source(tmp)
