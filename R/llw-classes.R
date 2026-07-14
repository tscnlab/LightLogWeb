# Core value objects ---------------------------------------------------------

.llw_schema_version <- "1.0.0"

llw_abort <- function(message, class = "lightlogweb_error", ...) {
  rlang::abort(message, class = class, ...)
}

llw_primary_variable <- function(x) {
  stopifnot(inherits(x, "llw_dataset"))
  variable <- x$metadata$variable %||% NULL
  if (is.null(variable) || !nzchar(variable) || !variable %in% names(x$prepared_data)) {
    llw_abort(
      "The dataset has no valid primary measurement variable. Set `metadata$variable`.",
      "lightlogweb_missing_variable"
    )
  }
  variable
}

llw_validate_data <- function(data, variable = NULL) {
  if (!is.data.frame(data)) {
    llw_abort("`data` must be a data frame.", "lightlogweb_invalid_data")
  }
  missing_required <- setdiff(c("Id", "Datetime"), names(data))
  if (length(missing_required)) {
    llw_abort(
      paste0("The dataset is missing required columns: ", paste(missing_required, collapse = ", "), "."),
      "lightlogweb_invalid_data"
    )
  }
  if (!inherits(data$Datetime, "POSIXct")) {
    llw_abort("`Datetime` must be a POSIXct column.", "lightlogweb_invalid_datetime")
  }
  if (!is.null(variable)) {
    if (!variable %in% names(data)) {
      llw_abort(paste0("Primary variable `", variable, "` is not present in the data."), "lightlogweb_missing_variable")
    }
    if (!is.numeric(data[[variable]])) {
      llw_abort(paste0("Primary variable `", variable, "` must be numeric."), "lightlogweb_invalid_variable")
    }
  }
  invisible(data)
}

#' Create a LightLogWeb dataset
#'
#' `llw_dataset()` creates the plain, versioned dataset object used by both the
#' console API and the Shiny application. The original data are retained in
#' `raw_data`; transformations are written to `prepared_data`.
#'
#' @param data A data frame containing `Id`, POSIXct `Datetime`, and one or more
#'   measurement columns.
#' @param metadata Named list. `variable` identifies the primary numeric
#'   measurement column. Other commonly used fields are `variable_name`,
#'   `variable_unit`, `timezone`, `coordinates`, `device`, `site`, and `country`.
#' @param name Dataset name.
#' @param provenance Named list describing the source and import operation.
#' @param annotations Named list of auxiliary interval datasets.
#'
#' @return An object of class `llw_dataset`.
#' @export
llw_dataset <- function(data,
                        metadata = list(),
                        name = NULL,
                        provenance = list(),
                        annotations = list()) {
  if (!is.list(metadata) || !is.list(provenance) || !is.list(annotations)) {
    llw_abort("`metadata`, `provenance`, and `annotations` must be lists.")
  }

  variable <- metadata$variable %||% NULL
  if (is.null(variable)) {
    candidates <- names(data)[vapply(data, is.numeric, logical(1))]
    candidates <- setdiff(candidates, c("Id", "Datetime"))
    variable <- candidates[[1]] %||% NULL
  }
  llw_validate_data(data, variable)

  timezone <- metadata$timezone %||% lubridate::tz(data$Datetime)
  if (is.null(timezone) || !nzchar(timezone)) timezone <- "UTC"
  metadata <- utils::modifyList(
    list(
      variable = variable,
      variable_name = variable,
      variable_unit = NA_character_,
      timezone = timezone,
      coordinates = NULL,
      device = NA_character_,
      site = NA_character_,
      country = NA_character_,
      daily_missing_max = 0.2
    ),
    metadata
  )

  structure(
    list(
      schema_version = .llw_schema_version,
      name = name %||% "dataset",
      raw_data = tibble::as_tibble(data),
      prepared_data = tibble::as_tibble(data),
      metadata = metadata,
      provenance = provenance,
      annotations = annotations,
      groups = character(),
      preparation = list()
    ),
    class = "llw_dataset"
  )
}

#' @export
print.llw_dataset <- function(x, ...) {
  cat("<llw_dataset>", x$name, "\n")
  cat("  rows:", format(nrow(x$prepared_data), big.mark = ","), "\n")
  cat("  ids:", dplyr::n_distinct(x$prepared_data$Id), "\n")
  cat("  variable:", x$metadata$variable, "\n")
  cat("  groups:", if (length(x$groups)) paste(x$groups, collapse = ", ") else "none", "\n")
  invisible(x)
}

#' Create a typed LightLogWeb result
#'
#' @param type Result type: scalar, table, plot, data, metrics, or diagnostic.
#' @param value Result value.
#' @param label Human-readable label.
#' @param units Optional units.
#' @param parameters Named parameter list.
#' @param grouping Character vector of grouping columns.
#' @param source Name of the function or module that produced the result.
#' @param data Optional source/plotted data.
#' @param provenance Additional provenance.
#'
#' @return An object of class `llw_result`.
#' @export
llw_result <- function(type,
                       value,
                       label = type,
                       units = NULL,
                       parameters = list(),
                       grouping = character(),
                       source = NULL,
                       data = NULL,
                       provenance = list()) {
  allowed <- c("scalar", "table", "plot", "data", "metrics", "diagnostic")
  if (!length(type) || !type[[1]] %in% allowed) {
    llw_abort(paste0("`type` must be one of: ", paste(allowed, collapse = ", "), "."))
  }
  structure(
    list(
      schema_version = .llw_schema_version,
      type = type[[1]],
      value = value,
      label = label,
      units = units,
      parameters = parameters,
      grouping = grouping,
      source = source,
      data = data,
      provenance = provenance
    ),
    class = "llw_result"
  )
}

#' @export
print.llw_result <- function(x, ...) {
  cat("<llw_result:", x$type, ">", x$label, "\n")
  if (is.data.frame(x$value)) print(x$value, ...)
  if (identical(x$type, "metrics")) cat("  metrics:", paste(names(x$value), collapse = ", "), "\n")
  invisible(x)
}

llw_analysis <- function(dataset,
                         recipe,
                         quality = NULL,
                         metrics = list(),
                         plots = list(),
                         modules = list(),
                         messages = character()) {
  structure(
    list(
      schema_version = .llw_schema_version,
      dataset = dataset,
      recipe = recipe,
      quality = quality,
      metrics = metrics,
      plots = plots,
      modules = modules,
      messages = messages,
      generated_at = Sys.time(),
      versions = list(
        LightLogWeb = tryCatch(as.character(utils::packageVersion("LightLogWeb")), error = function(e) "development"),
        LightLogR = as.character(utils::packageVersion("LightLogR")),
        R = as.character(getRversion())
      )
    ),
    class = "llw_analysis"
  )
}

#' @export
print.llw_analysis <- function(x, ...) {
  cat("<llw_analysis>", x$dataset$name, "\n")
  cat("  recipe steps:", length(x$recipe$steps), "\n")
  cat("  metric results:", length(x$metrics), "\n")
  cat("  plot results:", length(x$plots), "\n")
  invisible(x)
}
