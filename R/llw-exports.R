# Reproducible result and artifact exports ----------------------------------

llw_result_table <- function(x) {
  if (inherits(x, "llw_result")) {
    if (is.data.frame(x$value)) return(tibble::as_tibble(x$value))
    if (identical(x$type, "metrics")) {
      return(dplyr::bind_rows(lapply(names(x$value), function(metric) {
        dplyr::mutate(tibble::as_tibble(x$value[[metric]]), .metric = metric, .before = 1)
      })))
    }
    if (identical(x$type, "plot") && is.data.frame(x$data)) return(tibble::as_tibble(x$data))
    if (identical(x$type, "diagnostic") && is.data.frame(x$value$overview)) return(tibble::as_tibble(x$value$overview))
  }
  if (is.data.frame(x)) return(tibble::as_tibble(x))
  llw_abort("This object does not have a single tabular representation for CSV export.")
}

#' Build a machine-readable analysis manifest
#'
#' @param x An `llw_dataset`, `llw_analysis`, or `llw_result`.
#' @param recipe Optional recipe when `x` is a dataset.
#'
#' @return A named list containing versions, parameters, citations, source
#'   fingerprints, and session information.
#' @export
llw_manifest <- function(x, recipe = NULL) {
  if (inherits(x, "llw_analysis")) {
    dataset <- x$dataset
    recipe <- x$recipe
    outputs <- list(metrics = names(x$metrics), plots = names(x$plots), modules = names(x$modules))
  } else if (inherits(x, "llw_dataset")) {
    dataset <- x
    recipe <- recipe %||% llw_recipe()
    outputs <- list()
  } else if (inherits(x, "llw_result")) {
    dataset <- NULL
    outputs <- list(type = x$type, label = x$label, source = x$source)
  } else {
    llw_abort("`x` must be an `llw_dataset`, `llw_analysis`, or `llw_result`.")
  }
  list(
    format = "LightLogWeb manifest",
    schema_version = .llw_schema_version,
    generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    dataset = if (is.null(dataset)) NULL else list(
      name = dataset$name,
      metadata = dataset$metadata,
      rows_raw = nrow(dataset$raw_data),
      rows_prepared = nrow(dataset$prepared_data),
      source_fingerprints = dataset$provenance$files %||% list()
    ),
    recipe = if (is.null(recipe)) NULL else recipe$steps,
    outputs = outputs,
    result = if (inherits(x, "llw_result")) list(
      parameters = x$parameters,
      grouping = x$grouping,
      provenance = x$provenance
    ) else NULL,
    versions = list(
      LightLogWeb = tryCatch(as.character(utils::packageVersion("LightLogWeb")), error = function(e) "development"),
      LightLogR = as.character(utils::packageVersion("LightLogR")),
      R = as.character(getRversion())
    ),
    citations = list(
      LightLogR = "https://tscnlab.github.io/LightLogR/",
      LightLogWeb = "https://tscnlab.github.io/LightLogWeb/"
    ),
    session_info = utils::capture.output(utils::sessionInfo())
  )
}

#' Export a LightLogWeb object or result
#'
#' `llw_export()` is the console-level implementation used by application
#' downloads. CSV exports use the object's tabular result, RDS preserves the
#' complete object, plot formats require an `llw_result` returned by
#' `llw_plot()`, and JSON writes a manifest for analysis objects.
#'
#' @param x Object to export.
#' @param path Destination path.
#' @param format One of `csv`, `rds`, `png`, `svg`, `pdf`, or `json`.
#' @param width,height Figure dimensions in inches.
#' @param dpi PNG resolution.
#'
#' @return The normalized output path, invisibly.
#' @export
llw_export <- function(x,
                       path,
                       format = tolower(tools::file_ext(path)),
                       width = 10,
                       height = 6,
                       dpi = 180) {
  format <- tolower(format)
  allowed <- c("csv", "rds", "png", "svg", "pdf", "json")
  if (!format %in% allowed) llw_abort(paste0("`format` must be one of: ", paste(allowed, collapse = ", "), "."))
  if (!nzchar(path)) llw_abort("Provide an export `path`.")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (format == "csv") {
    utils::write.csv(llw_result_table(x), path, row.names = FALSE, na = "")
  } else if (format == "rds") {
    saveRDS(x, path, version = 3)
  } else if (format == "json") {
    value <- if (inherits(x, c("llw_dataset", "llw_analysis", "llw_result"))) llw_manifest(x) else x
    llw_write_json(value, path)
  } else {
    if (!inherits(x, "llw_result") || !identical(x$type, "plot") || !inherits(x$value, "ggplot")) {
      llw_abort("PNG, SVG, and PDF exports require a plot `llw_result` from `llw_plot()`.")
    }
    device <- switch(format, png = "png", svg = svglite::svglite, pdf = "pdf")
    ggplot2::ggsave(path, plot = x$value, device = device, width = width, height = height, dpi = if (format == "png") dpi else 300)
  }
  invisible(normalizePath(path, mustWork = TRUE))
}
