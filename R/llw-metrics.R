# Metric registry and deterministic metric execution ------------------------

llw_metric_row <- function(id, name, family, summary, requirement, units,
                           light_arg = "Light.vector", time_arg = "Time.vector",
                           defaults = list()) {
  tibble::tibble(
    id = id,
    name = name,
    family = family,
    function_name = id,
    summary = summary,
    requirement = requirement,
    units = units,
    grouping = "Calculated independently for every declared recipe group.",
    caution = "Interpret relative to the measurement quantity, sampling, missing-data handling, and study context; defaults are not health recommendations.",
    documentation = paste0("https://tscnlab.github.io/LightLogR/reference/", id, ".html"),
    light_arg = light_arg,
    time_arg = time_arg,
    defaults = list(defaults)
  )
}

#' LightLogWeb metric registry
#'
#' The registry contains the built-in LightLogR metric families that operate on
#' one selected measurement variable, optionally with `Datetime`. Functions
#' requiring additional measurement modalities or spectral structures are
#' intentionally left to custom modules.
#'
#' @return A tibble with function names, descriptions, input requirements,
#'   documentation links, and default parameter lists.
#' @export
llw_metric_registry <- function() {
  dplyr::bind_rows(
    llw_metric_row(
      "barroso_lighting_metrics", "Barroso lighting metrics", "Circadian pattern",
      "Seven timing and contrast descriptors from Barroso et al.", "Light + timestamps", "mixed",
      defaults = list(epoch = "dominant.epoch", loop = FALSE, na.rm = FALSE)
    ),
    llw_metric_row(
      "bright_dark_period", "Brightest / darkest period", "Timing",
      "Mean, onset, midpoint, and offset of a continuous bright or dark window.", "Light + timestamps", "value and local time",
      defaults = list(period = "brightest", timespan = "10 hours", epoch = "dominant.epoch", loop = FALSE, na.rm = FALSE)
    ),
    llw_metric_row(
      "centroidLE", "Centroid of light exposure", "Timing",
      "Temporal centroid of light exposure.", "Light + timestamps", "local date/time",
      defaults = list(bin.size = NULL, na.rm = FALSE)
    ),
    llw_metric_row(
      "disparity_index", "Disparity index", "Distribution",
      "Disparity of exposure values across the selected analysis unit.", "Light", "unitless",
      time_arg = NA_character_, defaults = list(na.rm = FALSE)
    ),
    llw_metric_row(
      "dose", "Dose", "Level & dose",
      "Time-integrated exposure.", "Light + timestamps", "value-hours",
      defaults = list(epoch = "dominant.epoch", na.rm = FALSE)
    ),
    llw_metric_row(
      "duration_above_threshold", "Duration above / below threshold", "Duration & threshold",
      "Total duration above, below, or within declared thresholds.", "Light + timestamps", "duration",
      defaults = list(comparison = "above", threshold = 250, epoch = "dominant.epoch", na.rm = FALSE)
    ),
    llw_metric_row(
      "exponential_moving_average", "Exponential moving average", "Prior history",
      "Time-varying exposure history weighted by a decay half-life.", "Regular light + timestamps", "same as light",
      defaults = list(decay = "90 min", epoch = "dominant.epoch")
    ),
    llw_metric_row(
      "frequency_crossing_threshold", "Threshold crossing frequency", "Duration & threshold",
      "Number of crossings of a declared exposure threshold.", "Light", "count",
      time_arg = NA_character_, defaults = list(threshold = 250, na.rm = FALSE)
    ),
    llw_metric_row(
      "intradaily_variability", "Intradaily variability", "Rhythmicity",
      "Fragmentation of the within-day exposure rhythm.", "Regular light + timestamps", "unitless",
      time_arg = "Datetime.vector", defaults = list(use.samplevar = FALSE, na.rm = FALSE)
    ),
    llw_metric_row(
      "interdaily_stability", "Interdaily stability", "Rhythmicity",
      "Stability of the exposure pattern across consecutive days.", "Light + timestamps", "unitless",
      time_arg = "Datetime.vector", defaults = list(use.samplevar = FALSE, na.rm = FALSE)
    ),
    llw_metric_row(
      "midpointCE", "Midpoint of cumulative exposure", "Timing",
      "Time at which half of cumulative exposure has occurred.", "Light + timestamps", "local date/time",
      defaults = list(na.rm = FALSE)
    ),
    llw_metric_row(
      "period_above_threshold", "Longest threshold period", "Duration & threshold",
      "Longest continuous period above, below, or within thresholds.", "Light + timestamps", "duration",
      defaults = list(comparison = "above", threshold = 250, epoch = "dominant.epoch", loop = FALSE, na.replace = FALSE, na.rm = FALSE)
    ),
    llw_metric_row(
      "pulses_above_threshold", "Threshold pulses", "Duration & threshold",
      "Pulse count and timing with configurable interruptions.", "Light + timestamps", "mixed",
      defaults = list(comparison = "above", threshold = 250, min.length = "2 mins", max.interrupt = "8 mins", prop.interrupt = 0.25, epoch = "dominant.epoch", return.indices = FALSE, na.rm = FALSE)
    ),
    llw_metric_row(
      "threshold_for_duration", "Threshold for duration", "Duration & threshold",
      "Exposure threshold reached for a declared cumulative duration.", "Light + timestamps", "same as light",
      defaults = list(duration = "1 hour", comparison = "above", epoch = "dominant.epoch", na.rm = FALSE)
    ),
    llw_metric_row(
      "timing_above_threshold", "Timing above / below threshold", "Timing",
      "Mean, first, and last timing above, below, or within thresholds.", "Light + timestamps", "local date/time",
      defaults = list(comparison = "above", threshold = 250, na.rm = FALSE)
    )
  )
}

llw_metric_definition <- function(id) {
  registry <- llw_metric_registry()
  row <- registry[registry$id == id, , drop = FALSE]
  if (!nrow(row)) {
    llw_abort(
      paste0("Unknown metric `", id, "`. Choose one of: ", paste(registry$id, collapse = ", "), "."),
      "lightlogweb_unknown_metric"
    )
  }
  row
}

llw_metric_requests <- function(metrics) {
  if (is.character(metrics)) {
    return(lapply(metrics, function(id) list(id = id, parameters = list())))
  }
  if (!is.list(metrics) || !length(metrics)) llw_abort("`metrics` must contain at least one metric ID or specification.")
  if (!is.null(metrics$id)) metrics <- list(metrics)
  lapply(metrics, function(spec) {
    if (is.character(spec) && length(spec) == 1L) return(list(id = spec, parameters = list()))
    if (!is.list(spec) || is.null(spec$id)) llw_abort("Each metric specification needs an `id`.")
    list(id = spec$id, parameters = spec$parameters %||% spec$params %||% list())
  })
}

llw_metric_output_frame <- function(result, id, group, datetime = NULL) {
  if (is.data.frame(result)) {
    output <- tibble::as_tibble(result)
  } else if (is.list(result) && !inherits(result, c("Duration", "difftime", "POSIXct", "POSIXt"))) {
    output <- tibble::as_tibble(result)
  } else if (length(result) == 1L) {
    output <- tibble::tibble(!!id := result)
  } else {
    output <- tibble::tibble(Datetime = datetime, !!id := result)
  }
  if (ncol(group)) {
    key <- group[rep(1L, nrow(output)), , drop = FALSE]
    output <- dplyr::bind_cols(key, output)
  }
  output
}

llw_metric_groups <- function(data, groups) {
  if (!length(groups)) return(list(list(key = tibble::tibble(), data = data)))
  missing <- setdiff(groups, names(data))
  if (length(missing)) llw_abort(paste0("Grouping columns not found: ", paste(missing, collapse = ", "), "."))
  split_key <- interaction(data[groups], drop = TRUE, lex.order = TRUE)
  split_data <- split(data, split_key)
  lapply(split_data, function(group_data) {
    list(key = tibble::as_tibble(group_data[1, groups, drop = FALSE]), data = group_data)
  })
}

llw_calculate_metric <- function(data, variable, groups, id, parameters) {
  definition <- llw_metric_definition(id)
  fn <- getExportedValue("LightLogR", definition$function_name[[1]])
  defaults <- definition$defaults[[1]]
  parameters <- utils::modifyList(defaults, parameters)
  allowed <- names(formals(fn))
  unknown <- setdiff(names(parameters), allowed)
  if (length(unknown)) llw_abort(paste0("Unknown parameters for `", id, "`: ", paste(unknown, collapse = ", "), "."))

  pieces <- lapply(llw_metric_groups(data, groups), function(item) {
    args <- parameters
    args[[definition$light_arg[[1]]]] <- item$data[[variable]]
    time_arg <- definition$time_arg[[1]]
    if (!is.na(time_arg) && nzchar(time_arg)) args[[time_arg]] <- item$data$Datetime
    if ("as.df" %in% allowed) args$as.df <- TRUE
    result <- tryCatch(
      rlang::exec(fn, !!!args),
      error = function(error) {
        group_label <- if (ncol(item$key)) paste(names(item$key), item$key[1, ], sep = "=", collapse = ", ") else "all data"
        llw_abort(
          paste0("Metric `", id, "` failed for ", group_label, ": ", conditionMessage(error)),
          "lightlogweb_metric_error",
          parent = error
        )
      }
    )
    llw_metric_output_frame(result, id, item$key, item$data$Datetime)
  })
  dplyr::bind_rows(pieces)
}

#' Calculate one or more LightLogR metrics
#'
#' @param x An `llw_dataset` or compatible data frame.
#' @param metrics Character metric IDs or a list of specifications containing
#'   `id` and a named `parameters` list.
#' @param variable Primary measurement variable.
#' @param groups Grouping columns. Defaults to those declared with `llw_group()`.
#'
#' @return An `llw_result` of type `metrics`; `value` is a named list of result
#'   tables, one per requested metric.
#' @export
llw_metrics <- function(x, metrics, variable = NULL, groups = NULL) {
  if (!inherits(x, "llw_dataset")) x <- llw_dataset(x, metadata = list(variable = variable))
  variable <- variable %||% llw_primary_variable(x)
  data <- x$prepared_data
  groups <- groups %||% x$groups
  requests <- llw_metric_requests(metrics)
  output <- list()
  parameters <- list()
  for (request in requests) {
    id <- request$id
    if (id %in% names(output)) llw_abort(paste0("Metric `", id, "` was requested more than once; use separate recipe steps for different parameters."))
    output[[id]] <- llw_calculate_metric(data, variable, groups, id, request$parameters)
    parameters[[id]] <- utils::modifyList(llw_metric_definition(id)$defaults[[1]], request$parameters)
  }
  llw_result(
    "metrics",
    output,
    label = paste("Metrics for", x$name),
    parameters = parameters,
    grouping = groups,
    source = "LightLogR",
    data = data,
    provenance = list(variable = variable, documentation = llw_metric_registry()$documentation)
  )
}
