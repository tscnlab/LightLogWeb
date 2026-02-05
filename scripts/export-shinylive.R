#!/usr/bin/env Rscript

# Export a shinylive bundle of LightLogWeb into the ./live directory.
#
# Usage:
#   Rscript scripts/export-shinylive.R
#   Rscript scripts/export-shinylive.R path/to/output
#
# This file is designed to work both with Rscript and when sourced in RStudio.

find_script_path <- function() {
  file_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- file_args[startsWith(file_args, "--file=")][1]

  if (!is.na(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg), winslash = "/", mustWork = TRUE))
  }

  if (sys.nframe() > 0) {
    for (i in seq_len(sys.nframe())) {
      frame <- sys.frame(i)
      if (exists("ofile", envir = frame, inherits = FALSE)) {
        return(normalizePath(get("ofile", envir = frame), winslash = "/", mustWork = TRUE))
      }
    }
  }

  NA_character_
}

resolve_repo_dir <- function(script_path = find_script_path(), working_dir = getwd()) {
  candidates <- c(
    if (!is.na(script_path)) dirname(dirname(script_path)),
    normalizePath(working_dir, winslash = "/", mustWork = TRUE)
  )

  repo_dir <- candidates[vapply(
    candidates,
    function(path) {
      file.exists(file.path(path, "DESCRIPTION")) &&
        dir.exists(file.path(path, "R")) &&
        dir.exists(file.path(path, "inst", "app", "www"))
    },
    logical(1)
  )][1]

  if (is.na(repo_dir)) {
    stop(
      paste(
        "Could not determine the package root directory.",
        "Run this script from the LightLogWeb repository root or via Rscript scripts/export-shinylive.R."
      ),
      call. = FALSE
    )
  }

  normalizePath(repo_dir, winslash = "/", mustWork = TRUE)
}

parse_description_field <- function(field_value) {
  if (is.null(field_value) || is.na(field_value) || !nzchar(field_value)) {
    return(character())
  }

  cleaned <- field_value |>
    strsplit(",") |>
    unlist(use.names = FALSE) |>
    trimws() |>
    gsub("\\s*\\(.*\\)$", "", x =_, perl = TRUE)

  cleaned[nzchar(cleaned)]
}

package_dependencies_for_entrypoint <- function(repo_dir) {
  desc <- read.dcf(file.path(repo_dir, "DESCRIPTION"), all = FALSE)

  pkgs <- unique(c(
    parse_description_field(desc[1, "Depends"]),
    parse_description_field(desc[1, "Imports"])
  ))

  pkgs[!pkgs %in% c("R", "LightLogWeb")]
}

build_shinylive_entrypoint <- function(dependencies) {
  dependency_literal <- paste(sprintf("'%s'", dependencies), collapse = ", ")

  c(
    sprintf("deps <- c(%s)", dependency_literal),
    "invisible(lapply(deps, library, character.only = TRUE))",
    "r_files <- list.files('R', pattern = '\\\\.[Rr]$', full.names = TRUE)",
    "invisible(lapply(r_files, source, local = globalenv()))",
    "LightLogWeb()"
  )
}

export_shinylive_bundle <- function(output_dir = "live", repo_dir = resolve_repo_dir()) {
  if (!requireNamespace("shinylive", quietly = TRUE)) {
    stop("Package 'shinylive' is required. Please install it first.", call. = FALSE)
  }

  out_dir <- normalizePath(file.path(repo_dir, output_dir), winslash = "/", mustWork = FALSE)
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

  # Build a small shinylive entrypoint that preloads imports and sources app functions.
  entrypoint <- build_shinylive_entrypoint(
    dependencies = package_dependencies_for_entrypoint(repo_dir)
  )
  writeLines(entrypoint, file.path(app_dir, "app.R"))

  unlink(out_dir, recursive = TRUE, force = TRUE)
  shinylive::export(app_dir, out_dir)

  message("Shinylive bundle exported to: ", out_dir)
  invisible(out_dir)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  output_dir <- if (length(args) == 0) "live" else args[[1]]

  export_shinylive_bundle(output_dir = output_dir)
}

if (sys.nframe() == 0) {
  main()
}
