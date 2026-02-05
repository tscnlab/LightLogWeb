#!/usr/bin/env Rscript

# Export a shinylive bundle of LightLogWeb into the ./live directory.
#
# Usage:
#   Rscript scripts/export-shinylive.R
#   Rscript scripts/export-shinylive.R path/to/output

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) == 0) "live" else args[[1]]

if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop("Package 'shinylive' is required. Please install it first.", call. = FALSE)
}

script_dir <- normalizePath(dirname(sys.frame(1)$ofile), winslash = "/", mustWork = TRUE)
repo_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
out_dir <- normalizePath(file.path(repo_dir, out_dir), winslash = "/", mustWork = FALSE)

app_dir <- file.path(tempdir(), "LightLogWeb-shinylive-app")
unlink(app_dir, recursive = TRUE, force = TRUE)
dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)

# Copy package R sources so the app can run in-browser without installing the package.
dir.create(file.path(app_dir, "R"), recursive = TRUE, showWarnings = FALSE)
file.copy(
  list.files(file.path(repo_dir, "R"), full.names = TRUE),
  file.path(app_dir, "R"),
  recursive = FALSE,
  overwrite = TRUE
)

# Copy static assets used via addResourcePath().
dir.create(file.path(app_dir, "app", "www"), recursive = TRUE, showWarnings = FALSE)
file.copy(
  list.files(file.path(repo_dir, "inst", "app", "www"), full.names = TRUE),
  file.path(app_dir, "app", "www"),
  recursive = TRUE,
  overwrite = TRUE
)

# Build a small shinylive entrypoint that sources all app functions.
entrypoint <- c(
  "r_files <- list.files('R', pattern = '\\.[Rr]$', full.names = TRUE)",
  "invisible(lapply(r_files, source, local = globalenv()))",
  "LightLogWeb()"
)
writeLines(entrypoint, file.path(app_dir, "app.R"))

unlink(out_dir, recursive = TRUE, force = TRUE)
shinylive::export(app_dir, out_dir)

message("Shinylive bundle exported to: ", out_dir)
