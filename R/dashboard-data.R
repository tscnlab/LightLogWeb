dashboard_plot_limits <- function(
  facets_per_page = 4L,
  detailed_max_participants = 4L,
  detailed_max_days = 7L,
  max_plot_rows = 12000L,
  max_overview_cells = 2000L
) {
  values <- c(
    facets_per_page = facets_per_page,
    detailed_max_participants = detailed_max_participants,
    detailed_max_days = detailed_max_days,
    max_plot_rows = max_plot_rows,
    max_overview_cells = max_overview_cells
  )
  if (
    !is.numeric(values) ||
      anyNA(values) ||
      any(!is.finite(values)) ||
      any(values < 1) ||
      any(values != floor(values))
  ) {
    abort_llw(
      "Dashboard plot limits must be positive whole numbers.",
      type = "validation"
    )
  }
  if (max_plot_rows < facets_per_page) {
    abort_llw(
      "`max_plot_rows` must allow at least one row per facet.",
      type = "validation"
    )
  }

  as.list(stats::setNames(as.integer(values), names(values)))
}

dashboard_table_contract <- function() {
  list(
    server = TRUE,
    page_length = 10L,
    search = TRUE,
    sort = TRUE,
    column_visibility = TRUE
  )
}

dashboard_assert_canonical_data <- function(data, arg) {
  if (!is.data.frame(data)) {
    abort_llw(
      paste0("`", arg, "` must be a data frame."),
      type = "validation"
    )
  }
  missing <- setdiff(c("Id", "Datetime"), names(data))
  if (length(missing) > 0L) {
    abort_llw(
      paste0(
        "`",
        arg,
        "` is missing required source column(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      type = "validation",
      public_message = paste0(
        "The selected dataset is missing required source column(s): ",
        paste(missing, collapse = ", "),
        ". No dashboard result was calculated."
      )
    )
  }
  if (!inherits(data$Datetime, "POSIXct") || anyNA(data$Datetime)) {
    abort_llw(
      paste0("`", arg, "$Datetime` must contain valid POSIXct instants."),
      type = "validation",
      public_message = paste(
        "The selected dataset does not contain valid absolute timestamps.",
        "No dashboard result was calculated."
      )
    )
  }
  ids <- as.character(data$Id)
  if (anyNA(ids) || any(!nzchar(trimws(ids)))) {
    abort_llw(
      paste0("`", arg, "$Id` contains missing or empty participant IDs."),
      type = "validation"
    )
  }
  invisible(data)
}

dashboard_valid_timezone <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value) &&
    value %in% OlsonNames()
}

dashboard_display_timezone <- function(record, data) {
  candidates <- c(
    record$analysis_settings$analysis_timezone %||% NA_character_,
    record$source_manifest$source_timezone %||% NA_character_,
    lubridate::tz(data$Datetime)
  )
  valid <- vapply(candidates, dashboard_valid_timezone, logical(1))
  if (!any(valid)) {
    abort_llw(
      "No valid IANA time zone is available for dashboard display.",
      type = "validation",
      public_message = paste(
        "The selected dataset has no valid time zone for local-date display.",
        "Review its import provenance before continuing."
      )
    )
  }
  candidates[which(valid)[[1L]]]
}

dashboard_quality_summary <- function(record, raw_data, timezone) {
  quality <- record$provenance$raw_import_quality
  if (inherits(quality, "llw_raw_import_quality")) {
    return(list(
      quality = quality,
      origin = "Stored import-quality provenance"
    ))
  }

  list(
    quality = summarize_raw_import_quality(raw_data, timezone),
    origin = paste(
      "Recomputed from the immutable source data because stored",
      "quality provenance was unavailable"
    )
  )
}

dashboard_recipe_summary <- function(record) {
  recipe <- validate_recipe(record$recipe)
  step_count <- length(recipe$steps)
  enabled_count <- if (step_count == 0L) {
    0L
  } else {
    sum(vapply(recipe$steps, `[[`, logical(1), "enabled"))
  }
  unchanged <- identical(record$raw_payload, record$prepared_payload)
  empty <- step_count == 0L
  label <- if (empty && unchanged) {
    "Empty recipe - source data unchanged"
  } else if (empty) {
    "Empty recipe - pre-processed data differ; review required"
  } else {
    paste(enabled_count, "of", step_count, "recipe step(s) enabled")
  }
  detail <- if (empty && unchanged) {
    paste(
      "No pre-processing step is committed. The active pre-processed result",
      "matches the immutable source data."
    )
  } else if (empty) {
    paste(
      "No recipe step explains the pre-processed-data difference.",
      "The pre-processed result should not be used until this is resolved."
    )
  } else {
    paste(
      "Pre-processed data were materialized from the source data using the",
      "committed recipe."
    )
  }

  list(
    state = if (empty && unchanged) "empty_unchanged" else "active",
    label = label,
    detail = detail,
    schema_version = recipe$version,
    dataset_revision = record$revision,
    step_count = step_count,
    enabled_count = enabled_count,
    unchanged = unchanged
  )
}

dashboard_grouping_summary <- function(record) {
  grouping <- record$analysis_settings$active_grouping %||%
    record$analysis_settings$grouping %||%
    NULL
  empty <- is.null(grouping) || length(grouping) == 0L
  if (empty) {
    return(list(
      state = "empty",
      label = "No active grouping",
      detail = "Pre-processed rows are currently ungrouped."
    ))
  }

  values <- if (is.atomic(grouping)) {
    as.character(grouping)
  } else if (!is.null(names(grouping)) && any(nzchar(names(grouping)))) {
    names(grouping)[nzchar(names(grouping))]
  } else {
    paste(length(grouping), "grouping item(s)")
  }
  list(
    state = "active",
    label = "Grouping active",
    detail = paste(values, collapse = ", ")
  )
}

dashboard_variable_metadata <- function(record, variable) {
  variables <- record$factual_metadata$variables %||% list()
  metadata <- variables[[variable]] %||% list()
  units <- record$source_manifest$details$units %||% list()
  unit <- metadata$unit %||% units[[variable]] %||% NA_character_
  calibration <- metadata$calibration %||%
    (record$source_manifest$details$calibration %||% list())[[variable]] %||%
    NA_character_
  list(
    unit = if (
      is.character(unit) && length(unit) == 1L && !is.na(unit) && nzchar(unit)
    ) {
      unit
    } else {
      "Unit not specified"
    },
    calibration = if (
      is.character(calibration) &&
        length(calibration) == 1L &&
        !is.na(calibration) &&
        nzchar(calibration)
    ) {
      calibration
    } else {
      "Not available"
    }
  )
}

dashboard_variable_role <- function(variable, primary_variable) {
  if (identical(variable, "Id")) return("Participant identity")
  if (identical(variable, "Datetime")) return("Absolute timestamp")
  if (identical(variable, primary_variable)) return("Primary measurement")
  if (identical(variable, "is.implicit")) return("Implicit-gap flag")
  if (startsWith(variable, "llw_source_")) return("Source provenance")
  "Available variable"
}

dashboard_variable_inventory <- function(data, record, primary_variable) {
  rows <- nrow(data)
  inventory <- lapply(names(data), function(variable) {
    value <- data[[variable]]
    missing <- sum(is.na(value))
    metadata <- dashboard_variable_metadata(record, variable)
    exact_zero <- if (
      is.numeric(value) &&
        !inherits(value, c("POSIXct", "Date", "difftime"))
    ) {
      sum(value == 0, na.rm = TRUE)
    } else {
      NA_integer_
    }
    data.frame(
      Variable = variable,
      Role = dashboard_variable_role(variable, primary_variable),
      Type = paste(class(value), collapse = "/"),
      Unit = metadata$unit,
      Missing = missing,
      `Missing (%)` = if (rows == 0L) NA_real_ else 100 * missing / rows,
      `Exact zeros` = exact_zero,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, inventory)
  rownames(result) <- NULL
  result
}

dashboard_numeric_focus_variables <- function(data, eligibility = NULL) {
  dashboard_assert_canonical_data(data, "data")
  numeric <- names(data)[vapply(
    data,
    function(value) {
      is.numeric(value) &&
        !inherits(value, c("POSIXct", "Date", "difftime"))
    },
    logical(1)
  )]
  if (inherits(eligibility, "llw_variable_eligibility")) {
    eligible <- eligibility$variable[eligibility$eligible]
    numeric <- intersect(eligible, numeric)
  }
  numeric
}

dashboard_day_boundaries <- function(dates, timezone) {
  labels <- format(dates, "%Y-%m-%d")
  next_labels <- format(dates + 1, "%Y-%m-%d")
  starts <- as.POSIXct(
    paste(labels, "00:00:00"),
    tz = timezone,
    format = "%Y-%m-%d %H:%M:%S"
  )
  ends <- as.POSIXct(
    paste(next_labels, "00:00:00"),
    tz = timezone,
    format = "%Y-%m-%d %H:%M:%S"
  )
  if (anyNA(starts) || anyNA(ends)) {
    abort_llw(
      paste0(
        "Local-day boundaries could not be resolved in `",
        timezone,
        "`."
      ),
      type = "validation"
    )
  }
  list(start = starts, end = ends)
}

dashboard_allocate_epoch_positions <- function(
  anchor,
  first_position,
  last_position,
  epoch_seconds,
  dates,
  timezone,
  tolerance
) {
  result <- integer(length(dates))
  if (
    !is.finite(first_position) ||
      !is.finite(last_position) ||
      first_position > last_position
  ) {
    return(result)
  }
  first_instant <- as.POSIXct(
    anchor + first_position * epoch_seconds,
    origin = "1970-01-01",
    tz = "UTC"
  )
  last_instant <- as.POSIXct(
    anchor + last_position * epoch_seconds,
    origin = "1970-01-01",
    tz = "UTC"
  )
  first_date <- as.Date(
    lubridate::with_tz(first_instant, tzone = timezone),
    tz = timezone
  )
  last_date <- as.Date(
    lubridate::with_tz(last_instant, tzone = timezone),
    tz = timezone
  )
  affected_dates <- seq(first_date, last_date, by = "day")
  boundaries <- dashboard_day_boundaries(affected_dates, timezone)
  lower <- ceiling(
    (as.numeric(boundaries$start) - anchor - tolerance) / epoch_seconds
  )
  upper <- ceiling(
    (as.numeric(boundaries$end) - anchor - tolerance) / epoch_seconds
  ) -
    1
  lower <- pmax(first_position, lower)
  upper <- pmin(last_position, upper)
  counts <- pmax(0, upper - lower + 1)
  result[match(affected_dates, dates)] <- as.integer(counts)
  result
}

dashboard_inferred_gap_epochs <- function(
  lag_seconds,
  epoch_seconds,
  tolerance = max(1e-6, epoch_seconds * 1e-7)
) {
  pmax(
    0,
    floor(lag_seconds / epoch_seconds + tolerance / epoch_seconds) - 1
  )
}

# Consecutive-time accounting deliberately does not require every timestamp to
# remain on one globally anchored modulo grid. This keeps ordinary 1 s / 3 s
# phase shifts in a nominal 2 s series from masquerading as missing epochs.
dashboard_participant_epoch_accounting <- function(
  datetimes,
  focus_observed,
  epoch_seconds,
  timezone
) {
  instants <- as.numeric(datetimes)
  order_index <- order(instants)
  sorted_instants <- instants[order_index]
  sorted_observed <- focus_observed[order_index]
  instant_runs <- rle(sorted_instants)
  run_end <- cumsum(instant_runs$lengths)
  run_start <- run_end - instant_runs$lengths + 1L
  cumulative_observed <- c(0L, cumsum(as.integer(sorted_observed)))
  observed_per_instant <- cumulative_observed[run_end + 1L] -
    cumulative_observed[run_start]
  unique_instants <- instant_runs$values
  unique_focus_observed <- observed_per_instant > 0L

  local_dates <- as.Date(
    lubridate::with_tz(datetimes, tzone = timezone),
    tz = timezone
  )
  unique_dates <- as.Date(
    lubridate::with_tz(
      as.POSIXct(unique_instants, origin = "1970-01-01", tz = "UTC"),
      tzone = timezone
    ),
    tz = timezone
  )
  dates <- seq(min(local_dates), max(local_dates), by = "day")
  date_levels <- format(dates, "%Y-%m-%d")
  row_counts <- as.integer(table(factor(
    format(local_dates, "%Y-%m-%d"),
    levels = date_levels
  )))
  unique_counts <- as.integer(table(factor(
    format(unique_dates, "%Y-%m-%d"),
    levels = date_levels
  )))
  focus_counts <- as.integer(table(factor(
    format(unique_dates[unique_focus_observed], "%Y-%m-%d"),
    levels = date_levels
  )))
  boundaries <- dashboard_day_boundaries(dates, timezone)
  day_seconds <- as.numeric(boundaries$end) - as.numeric(boundaries$start)

  if (!is.finite(epoch_seconds) || epoch_seconds <= 0) {
    return(list(
      dates = dates,
      day_seconds = day_seconds,
      row_counts = row_counts,
      unique_counts = unique_counts,
      focus_counts = focus_counts,
      explicit_missing = unique_counts - focus_counts,
      internal_gap_counts = rep(NA_integer_, length(dates)),
      boundary_gap_counts = rep(NA_integer_, length(dates)),
      full_gap_counts = rep(NA_integer_, length(dates)),
      jitter_counts = rep(NA_integer_, length(dates)),
      expected_full = rep(NA_integer_, length(dates)),
      unique_instants = unique_instants,
      unique_focus_observed = unique_focus_observed,
      regular_expected = NA_real_,
      regular_implicit = NA_real_,
      jitter_total = NA_real_
    ))
  }

  tolerance <- max(1e-6, epoch_seconds * 1e-7)
  internal_gap_counts <- integer(length(dates))
  jitter_counts <- integer(length(dates))
  regular_implicit <- 0
  if (length(unique_instants) > 1L) {
    lag_seconds <- diff(unique_instants)
    gap_counts <- dashboard_inferred_gap_epochs(
      lag_seconds,
      epoch_seconds,
      tolerance
    )
    regular_implicit <- sum(gap_counts)
    gap_index <- which(gap_counts > 0)
    for (index in gap_index) {
      internal_gap_counts <- internal_gap_counts +
        dashboard_allocate_epoch_positions(
          anchor = unique_instants[[index]],
          first_position = 1,
          last_position = gap_counts[[index]],
          epoch_seconds = epoch_seconds,
          dates = dates,
          timezone = timezone,
          tolerance = tolerance
        )
    }

    nearest_intervals <- round(lag_seconds / epoch_seconds)
    jitter <- abs(
      lag_seconds - nearest_intervals * epoch_seconds
    ) > tolerance
    jitter_counts <- as.integer(table(factor(
      format(unique_dates[-1L][jitter], "%Y-%m-%d"),
      levels = date_levels
    )))
  }

  boundary_gap_counts <- integer(length(dates))
  first_padding <- floor(
    (
      unique_instants[[1L]] -
        as.numeric(boundaries$start[[1L]]) +
        tolerance
    ) /
      epoch_seconds
  )
  last_padding <- ceiling(
    (
      as.numeric(boundaries$end[[length(dates)]]) -
        unique_instants[[length(unique_instants)]] -
        tolerance
    ) /
      epoch_seconds
  ) -
    1
  boundary_gap_counts[[1L]] <- max(0, first_padding)
  boundary_gap_counts[[length(dates)]] <-
    boundary_gap_counts[[length(dates)]] + max(0, last_padding)
  full_gap_counts <- internal_gap_counts + boundary_gap_counts
  expected_full <- unique_counts + full_gap_counts

  list(
    dates = dates,
    day_seconds = day_seconds,
    row_counts = row_counts,
    unique_counts = unique_counts,
    focus_counts = focus_counts,
    explicit_missing = unique_counts - focus_counts,
    internal_gap_counts = internal_gap_counts,
    boundary_gap_counts = boundary_gap_counts,
    full_gap_counts = full_gap_counts,
    jitter_counts = jitter_counts,
    expected_full = expected_full,
    unique_instants = unique_instants,
    unique_focus_observed = unique_focus_observed,
    regular_expected = length(unique_instants) + regular_implicit,
    regular_implicit = regular_implicit,
    jitter_total = sum(jitter_counts)
  )
}

dashboard_daily_coverage <- function(
  data,
  quality,
  timezone,
  focus_variable = NULL
) {
  dashboard_assert_canonical_data(data, "data")
  if (!inherits(quality, "llw_raw_import_quality")) {
    abort_llw(
      "`quality` must be a raw-import quality summary.",
      type = "validation"
    )
  }
  if (!dashboard_valid_timezone(timezone)) {
    abort_llw("`timezone` must be a valid IANA time zone.", type = "validation")
  }
  if (is.null(focus_variable)) {
    candidates <- dashboard_numeric_focus_variables(data, quality$eligibility)
    focus_variable <- if ("MEDI" %in% candidates) {
      "MEDI"
    } else if (length(candidates) > 0L) {
      candidates[[1L]]
    } else {
      NA_character_
    }
  }
  if (
    !is.character(focus_variable) ||
      length(focus_variable) != 1L ||
      is.na(focus_variable) ||
      !focus_variable %in% names(data) ||
      !is.numeric(data[[focus_variable]])
  ) {
    abort_llw(
      "`focus_variable` must name one numeric measurement column.",
      type = "validation"
    )
  }

  participant_ids <- sort(unique(as.character(data$Id)))
  summaries <- vector("list", length(participant_ids))
  for (participant_index in seq_along(participant_ids)) {
    participant_id <- participant_ids[[participant_index]]
    rows <- which(as.character(data$Id) == participant_id)
    datetimes <- data$Datetime[rows]
    focus_observed <- !is.na(data[[focus_variable]][rows])

    quality_row <- quality$participants[
      as.character(quality$participants$Id) == participant_id,
      ,
      drop = FALSE
    ]
    epoch_seconds <- if (
      nrow(quality_row) == 1L &&
        is.finite(quality_row$dominant_epoch_seconds[[1L]]) &&
        quality_row$dominant_epoch_seconds[[1L]] > 0
    ) {
      quality_row$dominant_epoch_seconds[[1L]]
    } else {
      NA_real_
    }
    accounting <- dashboard_participant_epoch_accounting(
      datetimes,
      focus_observed,
      epoch_seconds,
      timezone
    )

    if (is.finite(epoch_seconds)) {
      observed_regular <- accounting$unique_counts
      observed_focus <- accounting$focus_counts
      explicit_missing_focus <- accounting$explicit_missing
      irregular <- accounting$jitter_counts
      expected <- accounting$expected_full
      implicit_gap_epochs <- accounting$full_gap_counts
      coverage_percent <- ifelse(
        expected > 0,
        100 * observed_focus / expected,
        NA_real_
      )
      coverage_state <- rep(
        paste(
          "Focus coverage estimated from consecutive dominant-epoch",
          "intervals and participant-local day boundaries"
        ),
        length(accounting$dates)
      )
    } else {
      observed_regular <- rep(NA_integer_, length(accounting$dates))
      observed_focus <- rep(NA_integer_, length(accounting$dates))
      explicit_missing_focus <- rep(NA_integer_, length(accounting$dates))
      irregular <- rep(NA_integer_, length(accounting$dates))
      expected <- rep(NA_integer_, length(accounting$dates))
      implicit_gap_epochs <- rep(NA_integer_, length(accounting$dates))
      coverage_percent <- rep(NA_real_, length(accounting$dates))
      coverage_state <- rep(
        "Dominant epoch unavailable",
        length(accounting$dates)
      )
    }

    summaries[[participant_index]] <- data.frame(
      Id = participant_id,
      Date = accounting$dates,
      `Day length (h)` = accounting$day_seconds / 3600,
      `Epoch (s)` = epoch_seconds,
      `Expected epochs` = as.integer(expected),
      `Observed timestamp epochs` = observed_regular,
      `Observed focus epochs` = observed_focus,
      `Explicit missing focus epochs` = explicit_missing_focus,
      `Implicit gap epochs` = as.integer(implicit_gap_epochs),
      `Irregular timestamps` = irregular,
      `Duplicate rows` = accounting$row_counts - accounting$unique_counts,
      `Focus coverage (%)` = coverage_percent,
      `Focus variable` = focus_variable,
      Status = coverage_state,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(rbind, summaries)
  rownames(result) <- NULL
  structure(result, class = c("llw_dashboard_coverage", "data.frame"))
}

#' Summarize focus-variable missingness under explicit time denominators
#'
#' This internal helper keeps three scientifically distinct denominators
#' separate. "recorded" uses unique timestamps that are actually present;
#' duplicate rows count as observed when any value at that timestamp is
#' non-missing. "regular" counts complete dominant-epoch intervals strictly
#' between consecutive timestamps, so ordinary phase shifts are diagnostics
#' rather than missing epochs. "full_days" adds participant-local boundary
#' padding to that same interval accounting.
dashboard_focus_missingness <- function(
  data,
  quality,
  timezone,
  focus_variable,
  coverage = NULL
) {
  dashboard_assert_canonical_data(data, "data")
  if (!inherits(quality, "llw_raw_import_quality")) {
    abort_llw(
      "`quality` must be a raw-import quality summary.",
      type = "validation"
    )
  }
  if (!dashboard_valid_timezone(timezone)) {
    abort_llw("`timezone` must be a valid IANA time zone.", type = "validation")
  }
  if (
    !is.character(focus_variable) ||
      length(focus_variable) != 1L ||
      is.na(focus_variable) ||
      !focus_variable %in% names(data) ||
      !is.numeric(data[[focus_variable]])
  ) {
    abort_llw(
      "`focus_variable` must name one numeric measurement column.",
      type = "validation"
    )
  }
  if (is.null(coverage)) {
    coverage <- dashboard_daily_coverage(
      data,
      quality,
      timezone,
      focus_variable = focus_variable
    )
  }
  if (!inherits(coverage, "llw_dashboard_coverage")) {
    abort_llw(
      "`coverage` must be a dashboard coverage summary.",
      type = "validation"
    )
  }
  coverage_variables <- unique(as.character(coverage[["Focus variable"]]))
  coverage_variables <- coverage_variables[!is.na(coverage_variables)]
  if (
    length(coverage_variables) > 0L &&
      (
        length(coverage_variables) != 1L ||
          !identical(coverage_variables[[1L]], focus_variable)
      )
  ) {
    abort_llw(
      "`coverage` does not describe the requested focus variable.",
      type = "validation"
    )
  }

  participant_ids <- sort(unique(as.character(data$Id)))
  recorded_expected <- 0
  recorded_observed <- 0
  regular_expected <- 0
  regular_recorded <- 0
  regular_observed <- 0
  regular_off_grid <- 0
  regular_estimable <- 0L

  for (participant_id in participant_ids) {
    rows <- which(as.character(data$Id) == participant_id)
    focus_observed <- !is.na(data[[focus_variable]][rows])
    quality_row <- quality$participants[
      as.character(quality$participants$Id) == participant_id,
      ,
      drop = FALSE
    ]
    epoch_seconds <- if (
      nrow(quality_row) == 1L &&
        is.finite(quality_row$dominant_epoch_seconds[[1L]]) &&
        quality_row$dominant_epoch_seconds[[1L]] > 0
    ) {
      quality_row$dominant_epoch_seconds[[1L]]
    } else {
      NA_real_
    }
    accounting <- dashboard_participant_epoch_accounting(
      data$Datetime[rows],
      focus_observed,
      epoch_seconds,
      timezone
    )
    recorded_expected <- recorded_expected +
      length(accounting$unique_instants)
    recorded_observed <- recorded_observed +
      sum(accounting$unique_focus_observed)
    if (!is.finite(epoch_seconds)) next

    regular_estimable <- regular_estimable + 1L
    regular_expected <- regular_expected + accounting$regular_expected
    regular_recorded <- regular_recorded +
      length(accounting$unique_instants)
    regular_observed <- regular_observed +
      sum(accounting$unique_focus_observed)
    regular_off_grid <- regular_off_grid + accounting$jitter_total
  }

  make_row <- function(
    scope,
    label,
    expected,
    recorded,
    observed,
    explicit_missing,
    implicit_gap,
    off_grid,
    estimable,
    denominator
  ) {
    estimable <- as.integer(estimable)
    can_estimate <- estimable > 0L && is.finite(expected)
    expected <- if (can_estimate) expected else NA_real_
    recorded <- if (can_estimate) recorded else NA_real_
    observed <- if (can_estimate) observed else NA_real_
    explicit_missing <- if (can_estimate) explicit_missing else NA_real_
    implicit_gap <- if (can_estimate) implicit_gap else NA_real_
    off_grid <- if (can_estimate) off_grid else NA_real_
    missing <- if (can_estimate) max(0, expected - observed) else NA_real_
    data.frame(
      scope = scope,
      label = label,
      expected_count = expected,
      recorded_count = recorded,
      observed_count = observed,
      missing_count = missing,
      missing_percent = if (can_estimate && expected > 0) {
        100 * missing / expected
      } else {
        NA_real_
      },
      explicit_missing_count = explicit_missing,
      implicit_gap_count = implicit_gap,
      off_grid_count = off_grid,
      off_grid_percent = if (
        can_estimate &&
          recorded_expected > 0 &&
          is.finite(off_grid)
      ) {
        100 * off_grid / recorded_expected
      } else {
        NA_real_
      },
      participants_estimable = estimable,
      participants_total = length(participant_ids),
      denominator = denominator,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  recorded_missing <- max(0, recorded_expected - recorded_observed)
  recorded_row <- make_row(
    scope = "recorded",
    label = "Within recorded time series",
    expected = recorded_expected,
    recorded = recorded_expected,
    observed = recorded_observed,
    explicit_missing = recorded_missing,
    implicit_gap = 0,
    off_grid = NA_real_,
    estimable = length(participant_ids),
    denominator = paste(
      "Unique recorded timestamps. At duplicate timestamps, the focus value",
      "is observed when any duplicate row is non-missing."
    )
  )

  regular_explicit <- max(0, regular_recorded - regular_observed)
  regular_implicit <- max(0, regular_expected - regular_recorded)
  regular_row <- make_row(
    scope = "regular",
    label = "Within regular series",
    expected = regular_expected,
    recorded = regular_recorded,
    observed = regular_observed,
    explicit_missing = regular_explicit,
    implicit_gap = regular_implicit,
    off_grid = regular_off_grid,
    estimable = regular_estimable,
    denominator = paste(
      "Unique recorded timestamps plus complete dominant-epoch intervals",
      "strictly between consecutive timestamps. Phase/jitter transitions are",
      "reported separately, remain credited as observations, and do not by",
      "themselves create missing epochs."
    )
  )

  coverage_estimable_ids <- unique(as.character(
    coverage$Id[is.finite(coverage[["Expected epochs"]])]
  ))
  coverage_estimable <- length(coverage_estimable_ids)
  full_sum <- function(variable) {
    values <- coverage[[variable]]
    if (coverage_estimable == 0L) return(NA_real_)
    sum(values[is.finite(values)], na.rm = TRUE)
  }
  full_expected <- full_sum("Expected epochs")
  full_recorded <- full_sum("Observed timestamp epochs")
  full_observed <- full_sum("Observed focus epochs")
  full_explicit <- full_sum("Explicit missing focus epochs")
  full_implicit <- full_sum("Implicit gap epochs")
  full_off_grid <- full_sum("Irregular timestamps")
  full_row <- make_row(
    scope = "full_days",
    label = "Within full local days",
    expected = full_expected,
    recorded = full_recorded,
    observed = full_observed,
    explicit_missing = full_explicit,
    implicit_gap = full_implicit,
    off_grid = full_off_grid,
    estimable = coverage_estimable,
    denominator = paste(
      "Consecutive-interval denominator plus dominant-epoch padding from",
      "participant-local midnight to the first timestamp and from the last",
      "timestamp to the end of the final local day."
    )
  )

  result <- rbind(recorded_row, regular_row, full_row)
  rownames(result) <- NULL
  structure(
    result,
    class = c("llw_dashboard_focus_missingness", "data.frame")
  )
}

dashboard_dataset_snapshot <- function(record) {
  record <- validate_dataset_record(record)
  raw_data <- unserialize(record$raw_payload)
  prepared_data <- unserialize(record$prepared_payload)
  dashboard_assert_canonical_data(raw_data, "raw_data")
  dashboard_assert_canonical_data(prepared_data, "prepared_data")

  primary_variable <- record$analysis_settings$primary_variable %||%
    NA_character_
  if (
    !is.character(primary_variable) ||
      length(primary_variable) != 1L ||
      is.na(primary_variable) ||
      !primary_variable %in% names(prepared_data) ||
      !is.numeric(prepared_data[[primary_variable]])
  ) {
    abort_llw(
      "The active primary variable is missing or non-numeric.",
      type = "validation",
      public_message = paste(
        "Choose an available numeric primary measurement before opening the",
        "dashboard. No data were changed."
      )
    )
  }
  timezone <- dashboard_display_timezone(record, prepared_data)
  quality_info <- dashboard_quality_summary(record, raw_data, timezone)
  raw_local_dates <- as.Date(
    lubridate::with_tz(raw_data$Datetime, tzone = timezone),
    tz = timezone
  )
  prepared_local_dates <- as.Date(
    lubridate::with_tz(prepared_data$Datetime, tzone = timezone),
    tz = timezone
  )
  eligibility <- record$provenance$primary_variable_eligibility %||%
    quality_info$quality$eligibility
  focus_variables <- dashboard_numeric_focus_variables(
    prepared_data,
    eligibility
  )
  if (length(focus_variables) == 0L) {
    focus_variables <- dashboard_numeric_focus_variables(prepared_data)
  }
  if (!primary_variable %in% focus_variables) {
    focus_variables <- unique(c(primary_variable, focus_variables))
  }
  variable_metadata <- dashboard_variable_metadata(record, primary_variable)
  inventory <- dashboard_variable_inventory(
    prepared_data,
    record,
    primary_variable
  )
  coverage <- dashboard_daily_coverage(
    prepared_data,
    quality_info$quality,
    timezone,
    focus_variable = primary_variable
  )
  participant_ranges <- dashboard_participant_ranges(
    prepared_data,
    prepared_local_dates,
    focus_variable = primary_variable
  )

  structure(
    list(
      record = record,
      raw_data = raw_data,
      prepared_data = prepared_data,
      source_local_date = raw_local_dates,
      preprocessed_local_date = prepared_local_dates,
      prepared_local_date = prepared_local_dates,
      participants = sort(unique(as.character(prepared_data$Id))),
      participant_ranges = participant_ranges,
      date_start = min(prepared_local_dates),
      date_end = max(prepared_local_dates),
      display_timezone = timezone,
      source_timezone = record$source_manifest$source_timezone,
      primary_variable = primary_variable,
      focus_variables = focus_variables,
      primary_unit = variable_metadata$unit,
      calibration = variable_metadata$calibration,
      variable_inventory = inventory,
      quality = quality_info$quality,
      quality_origin = quality_info$origin,
      coverage = coverage,
      recipe = dashboard_recipe_summary(record),
      grouping = dashboard_grouping_summary(record)
    ),
    class = c("llw_dashboard_snapshot", "list")
  )
}

dashboard_focus_view <- function(
  snapshot,
  focus_variable = snapshot$primary_variable,
  data_stage = c("preprocessed", "source")
) {
  if (!inherits(snapshot, "llw_dashboard_snapshot")) {
    abort_llw("`snapshot` must be a dashboard snapshot.", type = "validation")
  }
  data_stage <- match.arg(data_stage)
  data <- if (identical(data_stage, "source")) {
    snapshot$raw_data
  } else {
    snapshot$prepared_data
  }
  local_dates <- if (identical(data_stage, "source")) {
    snapshot$source_local_date
  } else {
    snapshot$preprocessed_local_date
  }
  if (
    !is.character(focus_variable) ||
      length(focus_variable) != 1L ||
      is.na(focus_variable) ||
      !focus_variable %in% names(data) ||
      !is.numeric(data[[focus_variable]])
  ) {
    abort_llw(
      paste0(
        "Focus variable `",
        focus_variable,
        "` is unavailable in the selected data stage."
      ),
      type = "validation",
      public_message = paste(
        "Choose a numeric focus metric that exists in the selected source or",
        "pre-processed data."
      )
    )
  }
  quality <- if (
    identical(data_stage, "source") ||
      identical(snapshot$record$raw_payload, snapshot$record$prepared_payload)
  ) {
    snapshot$quality
  } else {
    summarize_raw_import_quality(data, snapshot$display_timezone)
  }
  metadata <- dashboard_variable_metadata(snapshot$record, focus_variable)
  missing_count <- sum(is.na(data[[focus_variable]]))
  coverage <- if (
    identical(data_stage, "preprocessed") &&
      identical(focus_variable, snapshot$primary_variable)
  ) {
    snapshot$coverage
  } else {
    dashboard_daily_coverage(
      data,
      quality,
      snapshot$display_timezone,
      focus_variable = focus_variable
    )
  }
  inventory <- if (
    identical(data_stage, "preprocessed") &&
      identical(focus_variable, snapshot$primary_variable)
  ) {
    snapshot$variable_inventory
  } else {
    dashboard_variable_inventory(data, snapshot$record, focus_variable)
  }
  participant_ranges <- dashboard_participant_ranges(
    data,
    local_dates,
    focus_variable = focus_variable
  )
  missingness <- dashboard_focus_missingness(
    data,
    quality,
    snapshot$display_timezone,
    focus_variable,
    coverage = coverage
  )
  structure(
    list(
      snapshot = snapshot,
      data = data,
      local_date = local_dates,
      data_stage = data_stage,
      stage_label = if (identical(data_stage, "source")) {
        "Source data"
      } else {
        "Pre-processed data"
      },
      focus_variable = focus_variable,
      focus_unit = metadata$unit,
      calibration = metadata$calibration,
      missing_count = missing_count,
      missing_percent = if (nrow(data) == 0L) {
        NA_real_
      } else {
        100 * missing_count / nrow(data)
      },
      missingness = missingness,
      quality = quality,
      coverage = coverage,
      variable_inventory = inventory,
      participant_ranges = participant_ranges
    ),
    class = c("llw_dashboard_focus_view", "list")
  )
}

dashboard_default_date_window <- function(
  snapshot,
  max_days = dashboard_plot_limits()$detailed_max_days
) {
  if (!inherits(snapshot, "llw_dashboard_snapshot")) {
    abort_llw("`snapshot` must be a dashboard snapshot.", type = "validation")
  }
  max_days <- suppressWarnings(as.integer(max_days))
  if (length(max_days) != 1L || is.na(max_days) || max_days < 1L) {
    abort_llw(
      "`max_days` must be a positive whole number.",
      type = "validation"
    )
  }
  c(
    snapshot$date_start,
    min(snapshot$date_end, snapshot$date_start + max_days - 1L)
  )
}

dashboard_date_value <- function(value, fallback) {
  if (is.null(value) || length(value) == 0L || all(is.na(value))) {
    return(fallback)
  }
  converted <- tryCatch(as.Date(value), error = function(cnd) NA)
  if (length(converted) == 0L || is.na(converted[[1L]])) fallback else
    converted[[1L]]
}

dashboard_participant_ranges <- function(
  data,
  local_date,
  focus_variable = NULL
) {
  dashboard_assert_canonical_data(data, "data")
  if (
    length(local_date) != nrow(data) ||
      !inherits(local_date, "Date") ||
      anyNA(local_date)
  ) {
    abort_llw(
      "`local_date` must provide one valid Date per data row.",
      type = "validation"
    )
  }
  if (
    !is.null(focus_variable) &&
      (
        !is.character(focus_variable) ||
          length(focus_variable) != 1L ||
          is.na(focus_variable) ||
          !focus_variable %in% names(data) ||
          !is.numeric(data[[focus_variable]])
      )
  ) {
    abort_llw(
      "`focus_variable` must name one numeric data column.",
      type = "validation"
    )
  }

  ids <- as.character(data$Id)
  participant_ids <- sort(unique(ids))
  rows <- lapply(participant_ids, function(participant_id) {
    index <- which(ids == participant_id)
    participant_dates <- local_date[index]
    focus_observed <- if (is.null(focus_variable)) {
      rep(TRUE, length(index))
    } else {
      !is.na(data[[focus_variable]][index])
    }
    focus_dates <- participant_dates[focus_observed]
    focus_start <- if (length(focus_dates) == 0L) {
      as.Date(NA)
    } else {
      min(focus_dates)
    }
    focus_end <- if (length(focus_dates) == 0L) {
      as.Date(NA)
    } else {
      max(focus_dates)
    }
    data.frame(
      Id = participant_id,
      `Date start` = min(participant_dates),
      `Date end` = max(participant_dates),
      `Measurement days` = as.integer(
        max(participant_dates) - min(participant_dates)
      ) + 1L,
      `Observed dates` = length(unique(participant_dates)),
      `Focus start` = focus_start,
      `Focus end` = focus_end,
      `Focus span days` = if (is.na(focus_start)) {
        0L
      } else {
        as.integer(focus_end - focus_start) + 1L
      },
      `Focus observed dates` = length(unique(focus_dates)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  ranges <- do.call(rbind, rows)
  rownames(ranges) <- NULL
  ranges
}

dashboard_requested_participants <- function(
  snapshot,
  participants = character(),
  show_all = FALSE,
  limits = dashboard_plot_limits(),
  fallback_count = limits$facets_per_page
) {
  if (!inherits(snapshot, "llw_dashboard_snapshot")) {
    abort_llw("`snapshot` must be a dashboard snapshot.", type = "validation")
  }
  requested <- intersect(
    snapshot$participants,
    as.character(participants %||% character())
  )
  show_all <- is.logical(show_all) && length(show_all) == 1L && isTRUE(show_all)
  if (show_all) {
    snapshot$participants
  } else if (length(requested) == 0L) {
    utils::head(
      snapshot$participants,
      dashboard_participants_per_page(
        fallback_count,
        fallback = limits$facets_per_page
      )
    )
  } else {
    requested
  }
}

dashboard_participant_date_window <- function(
  snapshot,
  participants = character()
) {
  if (!inherits(snapshot, "llw_dashboard_snapshot")) {
    abort_llw("`snapshot` must be a dashboard snapshot.", type = "validation")
  }
  participants <- intersect(snapshot$participants, as.character(participants))
  if (length(participants) == 0L) {
    return(c(snapshot$date_start, snapshot$date_end))
  }
  ranges <- snapshot$participant_ranges[
    snapshot$participant_ranges$Id %in% participants,
    ,
    drop = FALSE
  ]
  c(min(ranges[["Date start"]]), max(ranges[["Date end"]]))
}

dashboard_page_number <- function(value, page_count) {
  page <- suppressWarnings(as.integer(value %||% 1L))
  if (length(page) != 1L || is.na(page)) page <- 1L
  as.integer(max(1L, min(page, page_count)))
}

dashboard_days_per_page <- function(value) {
  days <- suppressWarnings(as.integer(value %||% 7L))
  if (
    length(days) != 1L ||
      is.na(days) ||
      !is.finite(days) ||
      days < 1L
  ) {
    7L
  } else {
    days
  }
}

dashboard_participants_per_page <- function(value, fallback = 4L) {
  fallback <- suppressWarnings(as.integer(fallback %||% 4L))
  if (
    length(fallback) != 1L ||
      is.na(fallback) ||
      fallback < 1L
  ) {
    fallback <- 4L
  }
  participants <- suppressWarnings(as.integer(value %||% fallback))
  if (
    length(participants) != 1L ||
      is.na(participants) ||
      !is.finite(participants) ||
      participants < 1L
  ) {
    fallback
  } else {
    participants
  }
}

dashboard_facet_slots <- function(
  page_participants,
  participant_pages,
  participants_per_page
) {
  page_participants <- unique(as.character(page_participants))
  participant_pages <- suppressWarnings(as.integer(participant_pages %||% 1L))
  if (
    length(participant_pages) != 1L ||
      is.na(participant_pages) ||
      !is.finite(participant_pages) ||
      participant_pages < 1L
  ) {
    participant_pages <- 1L
  }
  participants_per_page <- dashboard_participants_per_page(
    participants_per_page,
    fallback = max(1L, length(page_participants))
  )
  slot_count <- if (participant_pages > 1L) {
    max(length(page_participants), participants_per_page)
  } else {
    length(page_participants)
  }
  empty_count <- max(0L, slot_count - length(page_participants))
  empty_ids <- if (empty_count > 0L) {
    paste0(".llw-empty-facet-slot-", seq_len(empty_count))
  } else {
    character()
  }
  data.frame(
    Id = c(page_participants, empty_ids),
    Label = c(page_participants, rep.int("", empty_count)),
    Empty = c(
      rep.int(FALSE, length(page_participants)),
      rep.int(TRUE, empty_count)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

dashboard_measurement_chunks <- function(start_day, end_day, days_per_page) {
  start_day <- max(1L, as.integer(start_day))
  end_day <- max(start_day, as.integer(end_day))
  available_days <- end_day - start_day + 1L
  days_per_page <- min(
    dashboard_days_per_page(days_per_page),
    available_days
  )
  starts <- seq.int(start_day, end_day, by = days_per_page)
  ends <- starts + days_per_page - 1L
  data.frame(
    Page = seq_along(starts),
    Start = starts,
    End = ends,
    Label = ifelse(
      starts == ends,
      paste("Measurement day", starts),
      paste("Measurement days", paste0(starts, "\u2013", ends))
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

dashboard_date_chunks <- function(
  start,
  end,
  time_basis = c("calendar", "week", "month", "year"),
  days_per_page = 7L
) {
  time_basis <- match.arg(time_basis)
  start <- as.Date(start)
  end <- as.Date(end)
  if (start > end) {
    abort_llw("The date scope must start before it ends.", type = "validation")
  }
  available_days <- as.integer(end - start) + 1L
  days_per_page <- min(
    dashboard_days_per_page(days_per_page),
    available_days
  )

  cycle_starts <- switch(
    time_basis,
    calendar = start,
    week = seq(
      lubridate::floor_date(start, unit = "week", week_start = 1),
      lubridate::floor_date(end, unit = "week", week_start = 1),
      by = "1 week"
    ),
    month = seq(
      lubridate::floor_date(start, unit = "month"),
      lubridate::floor_date(end, unit = "month"),
      by = "1 month"
    ),
    year = seq(
      lubridate::floor_date(start, unit = "year"),
      lubridate::floor_date(end, unit = "year"),
      by = "1 year"
    )
  )
  cycle_ends <- switch(
    time_basis,
    calendar = end,
    week = cycle_starts + 6L,
    month = lubridate::ceiling_date(cycle_starts, unit = "month") - 1L,
    year = lubridate::ceiling_date(cycle_starts, unit = "year") - 1L
  )
  cycle_starts <- pmax(as.Date(cycle_starts), start)
  cycle_ends <- pmin(as.Date(cycle_ends), end)

  rows <- unlist(lapply(seq_along(cycle_starts), function(index) {
    chunk_starts <- seq(
      cycle_starts[[index]],
      cycle_ends[[index]],
      by = days_per_page
    )
    lapply(chunk_starts, function(chunk_start) {
      data.frame(
        Start = as.Date(chunk_start),
        End = if (identical(time_basis, "calendar")) {
          as.Date(chunk_start) + days_per_page - 1L
        } else {
          min(
            as.Date(chunk_start) + days_per_page - 1L,
            cycle_ends[[index]]
          )
        },
        stringsAsFactors = FALSE
      )
    })
  }), recursive = FALSE)
  chunks <- do.call(rbind, rows)
  rownames(chunks) <- NULL
  span_label <- function(first, last) {
    if (identical(first, last)) {
      format(first, "%d %b %Y")
    } else if (format(first, "%Y") == format(last, "%Y")) {
      paste0(format(first, "%d %b"), "\u2013", format(last, "%d %b %Y"))
    } else {
      paste0(format(first, "%d %b %Y"), "\u2013", format(last, "%d %b %Y"))
    }
  }
  cycle_label <- vapply(seq_len(nrow(chunks)), function(index) {
    first <- chunks$Start[[index]]
    last <- chunks$End[[index]]
    switch(
      time_basis,
      calendar = span_label(first, last),
      week = paste0(
        "Week ",
        format(first, "%V"),
        " \u00b7 ",
        span_label(first, last)
      ),
      month = paste0(format(first, "%B %Y"), " \u00b7 ", span_label(first, last)),
      year = paste0(format(first, "%Y"), " \u00b7 ", span_label(first, last))
    )
  }, character(1))
  data.frame(
    Page = seq_len(nrow(chunks)),
    Start = chunks$Start,
    End = chunks$End,
    Label = cycle_label,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

dashboard_selection_windows <- function(
  focus_view,
  participants,
  time_basis,
  date_start,
  date_end,
  measurement_start,
  measurement_end
) {
  ranges <- focus_view$participant_ranges[
    focus_view$participant_ranges$Id %in% participants,
    ,
    drop = FALSE
  ]
  if (time_basis %in% c("participant", "elapsed")) {
    ranges$`Window start` <- ranges$`Date start` + measurement_start - 1L
    ranges$`Window end` <- pmin(
      ranges$`Date end`,
      ranges$`Date start` + measurement_end - 1L
    )
  } else {
    ranges$`Window start` <- rep(as.Date(date_start), nrow(ranges))
    ranges$`Window end` <- rep(as.Date(date_end), nrow(ranges))
  }
  ranges[, c(
    "Id",
    "Date start",
    "Date end",
    "Window start",
    "Window end"
  ), drop = FALSE]
}

dashboard_available_span <- function(focus_view, participant_windows) {
  if (nrow(participant_windows) == 0L) return(0L)
  ids <- as.character(focus_view$data$Id)
  focus_values <- focus_view$data[[focus_view$focus_variable]]
  observed <- !is.na(focus_values)
  if (is.numeric(focus_values)) {
    observed <- observed & is.finite(focus_values)
  }
  spans <- vapply(seq_len(nrow(participant_windows)), function(index) {
    participant_id <- participant_windows$Id[[index]]
    keep <- ids == participant_id &
      observed &
      focus_view$local_date >= participant_windows$`Window start`[[index]] &
      focus_view$local_date <= participant_windows$`Window end`[[index]]
    dates <- focus_view$local_date[keep]
    if (length(dates) == 0L) {
      0L
    } else {
      as.integer(max(dates) - min(dates)) + 1L
    }
  }, integer(1))
  as.integer(max(spans, 0L))
}

dashboard_plot_selection <- function(
  snapshot,
  participants = character(),
  show_all = FALSE,
  date_window = NULL,
  facet_page = 1L,
  limits = dashboard_plot_limits(),
  time_basis = c(
    "calendar",
    "participant",
    "elapsed",
    "week",
    "month",
    "year"
  ),
  measurement_window = NULL,
  view_mode = c("auto", "detailed", "availability"),
  time_page = 1L,
  days_per_page = 7L,
  focus_view = NULL,
  participants_per_page = limits$facets_per_page
) {
  if (!inherits(snapshot, "llw_dashboard_snapshot")) {
    abort_llw("`snapshot` must be a dashboard snapshot.", type = "validation")
  }
  time_basis <- match.arg(time_basis)
  view_mode <- match.arg(view_mode)
  days_per_page <- dashboard_days_per_page(days_per_page)
  participants_per_page <- dashboard_participants_per_page(
    participants_per_page,
    fallback = limits$facets_per_page
  )
  if (is.null(focus_view)) {
    focus_view <- dashboard_focus_view(
      snapshot,
      focus_variable = snapshot$primary_variable,
      data_stage = "preprocessed"
    )
  }
  if (!inherits(focus_view, "llw_dashboard_focus_view")) {
    abort_llw(
      "`focus_view` must be a dashboard focus view.",
      type = "validation"
    )
  }
  requested <- dashboard_requested_participants(
    snapshot,
    participants = participants,
    show_all = show_all,
    fallback_count = participants_per_page,
    limits = limits
  )
  show_all <- is.logical(show_all) && length(show_all) == 1L && isTRUE(show_all)

  start <- snapshot$date_start
  end <- snapshot$date_end
  if (!is.null(date_window) && length(date_window) >= 2L) {
    start <- dashboard_date_value(date_window[[1L]], start)
    end <- dashboard_date_value(date_window[[2L]], end)
  }
  start <- max(start, snapshot$date_start)
  end <- min(end, snapshot$date_end)
  if (start > end) {
    start <- snapshot$date_start
    end <- snapshot$date_end
  }

  participant_page_count <- as.integer(max(
    1L,
    ceiling(length(requested) / participants_per_page)
  ))
  participant_page <- dashboard_page_number(
    facet_page,
    participant_page_count
  )
  first <- (participant_page - 1L) * participants_per_page + 1L
  last <- min(
    length(requested),
    participant_page * participants_per_page
  )
  page_participants <- if (length(requested) == 0L || first > last) {
    character()
  } else {
    requested[seq.int(first, last)]
  }
  facet_slots <- dashboard_facet_slots(
    page_participants,
    participant_pages = participant_page_count,
    participants_per_page = participants_per_page
  )

  requested_ranges <- focus_view$participant_ranges[
    focus_view$participant_ranges$Id %in% requested,
    ,
    drop = FALSE
  ]
  max_measurement_day <- if (nrow(requested_ranges) == 0L) {
    1L
  } else {
    max(requested_ranges[["Measurement days"]])
  }
  if (is.null(measurement_window) || length(measurement_window) < 2L) {
    measurement_start <- 1L
    measurement_end <- max_measurement_day
  } else {
    measurement_start <- suppressWarnings(as.integer(measurement_window[[1L]]))
    measurement_end <- suppressWarnings(as.integer(measurement_window[[2L]]))
    if (is.na(measurement_start)) measurement_start <- 1L
    if (is.na(measurement_end)) measurement_end <- max_measurement_day
  }
  measurement_start <- max(1L, min(measurement_start, max_measurement_day))
  measurement_end <- max(
    measurement_start,
    min(measurement_end, max_measurement_day)
  )

  full_windows <- dashboard_selection_windows(
    focus_view,
    requested,
    time_basis,
    date_start = start,
    date_end = end,
    measurement_start = measurement_start,
    measurement_end = measurement_end
  )
  available_span_days <- dashboard_available_span(focus_view, full_windows)
  mode <- switch(
    view_mode,
    detailed = "detailed",
    availability = "availability",
    auto = if (available_span_days == 0L) {
      "availability"
    } else if (available_span_days <= limits$detailed_max_days) {
      "detailed"
    } else {
      "availability"
    }
  )

  time_chunks <- if (time_basis %in% c("participant", "elapsed")) {
    dashboard_measurement_chunks(
      measurement_start,
      measurement_end,
      days_per_page
    )
  } else {
    dashboard_date_chunks(
      start,
      end,
      time_basis = time_basis,
      days_per_page = days_per_page
    )
  }
  time_page_count <- nrow(time_chunks)
  time_page <- dashboard_page_number(time_page, time_page_count)
  chosen_time <- time_chunks[time_page, , drop = FALSE]
  page_windows <- dashboard_selection_windows(
    focus_view,
    page_participants,
    time_basis,
    date_start = if (inherits(chosen_time$Start, "Date")) {
      chosen_time$Start[[1L]]
    } else {
      start
    },
    date_end = if (inherits(chosen_time$End, "Date")) {
      chosen_time$End[[1L]]
    } else {
      end
    },
    measurement_start = if (time_basis %in% c("participant", "elapsed")) {
      chosen_time$Start[[1L]]
    } else {
      measurement_start
    },
    measurement_end = if (time_basis %in% c("participant", "elapsed")) {
      chosen_time$End[[1L]]
    } else {
      measurement_end
    }
  )
  page_date_start <- if (nrow(page_windows) == 0L) {
    start
  } else {
    min(page_windows$`Window start`)
  }
  page_date_end <- if (nrow(page_windows) == 0L) {
    end
  } else {
    max(page_windows$`Window end`)
  }
  day_count <- if (time_basis %in% c("participant", "elapsed")) {
    as.integer(chosen_time$End[[1L]] - chosen_time$Start[[1L]]) + 1L
  } else {
    as.integer(chosen_time$End[[1L]] - chosen_time$Start[[1L]]) + 1L
  }

  warning <- if (show_all && length(requested) > participants_per_page) {
    paste0(
      "Show all includes ",
      format(length(requested), big.mark = ","),
      " participants across ",
      participant_page_count,
      " participant page(s). Only one bounded row and time page is drawn at a time."
    )
  } else if (show_all) {
    paste0(
      "Show all includes every available participant (",
      length(requested),
      ") in this bounded view."
    )
  } else {
    NULL
  }

  structure(
    list(
      participants = requested,
      page_participants = page_participants,
      facet_slots = facet_slots,
      participant_windows = page_windows,
      show_all = show_all,
      show_all_warning = warning,
      scope_date_start = start,
      scope_date_end = end,
      date_start = page_date_start,
      date_end = page_date_end,
      measurement_start = measurement_start,
      measurement_end = measurement_end,
      day_count = day_count,
      facet_page = participant_page,
      facet_pages = participant_page_count,
      participant_page = participant_page,
      participant_pages = participant_page_count,
      participants_per_page = participants_per_page,
      time_page = time_page,
      time_pages = time_page_count,
      time_page_label = chosen_time$Label[[1L]],
      time_basis = time_basis,
      days_per_page = days_per_page,
      view_mode = view_mode,
      available_span_days = available_span_days,
      mode = mode,
      time_chunks = time_chunks
    ),
    class = c("llw_dashboard_selection", "list")
  )
}

dashboard_page_navigator <- function(focus_view, selection) {
  if (!inherits(focus_view, "llw_dashboard_focus_view")) {
    abort_llw(
      "`focus_view` must be a dashboard focus view.",
      type = "validation"
    )
  }
  if (!inherits(selection, "llw_dashboard_selection")) {
    abort_llw(
      "`selection` must be a dashboard selection.",
      type = "validation"
    )
  }

  participant_pages <- max(1L, as.integer(selection$participant_pages))
  time_pages <- max(1L, as.integer(selection$time_pages))
  navigator <- expand.grid(
    time_page = seq_len(time_pages),
    participant_page = seq_len(participant_pages),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  navigator$observed_focus_values <- 0L

  values <- focus_view$data[[focus_view$focus_variable]]
  observed <- !is.na(values)
  if (is.numeric(values)) {
    observed <- observed & is.finite(values)
  }
  participant_index <- match(
    as.character(focus_view$data$Id),
    as.character(selection$participants)
  )
  participants_per_page <- dashboard_participants_per_page(
    selection$participants_per_page,
    fallback = max(
      1L,
      ceiling(length(selection$participants) / participant_pages)
    )
  )
  participant_page <- ceiling(participant_index / participants_per_page)

  if (selection$time_basis %in% c("participant", "elapsed")) {
    measurement_day <- dashboard_measurement_day(
      focus_view$data$Id,
      focus_view$local_date,
      focus_view$participant_ranges
    )
    time_value <- as.integer(measurement_day)
    chunk_start <- as.integer(selection$time_chunks$Start)
    chunk_end <- as.integer(selection$time_chunks$End)
  } else {
    time_value <- as.numeric(focus_view$local_date)
    chunk_start <- as.numeric(selection$time_chunks$Start)
    chunk_end <- as.numeric(selection$time_chunks$End)
  }
  time_page <- findInterval(time_value, chunk_start)
  valid_time <- !is.na(time_page) &
    time_page >= 1L &
    time_page <= time_pages
  valid_index <- which(valid_time)
  valid_time[valid_index] <- time_value[valid_index] <=
    chunk_end[time_page[valid_index]]
  keep <- observed &
    !is.na(participant_page) &
    participant_page >= 1L &
    participant_page <= participant_pages &
    valid_time

  if (any(keep)) {
    key <- paste(participant_page[keep], time_page[keep], sep = ":")
    counts <- table(key)
    navigator_key <- paste(
      navigator$participant_page,
      navigator$time_page,
      sep = ":"
    )
    matched <- match(navigator_key, names(counts))
    has_count <- !is.na(matched)
    navigator$observed_focus_values[has_count] <- as.integer(
      counts[matched[has_count]]
    )
  }
  navigator$has_data <- navigator$observed_focus_values > 0L
  navigator
}

dashboard_plot_scale_value <- function(value, fallback = "symlog") {
  allowed <- c("symlog", "linear", "log")
  fallback <- as.character(fallback %||% "symlog")
  if (
    length(fallback) != 1L ||
      is.na(fallback) ||
      !fallback %in% allowed
  ) {
    fallback <- "symlog"
  }
  value <- as.character(value %||% fallback)
  if (
    length(value) != 1L ||
      is.na(value) ||
      !value %in% allowed
  ) {
    fallback
  } else {
    value
  }
}

dashboard_symlog_threshold_value <- function(value, fallback = 1) {
  allowed <- c(10, 1, 0.1, 0.01, 0.001)
  fallback <- suppressWarnings(as.numeric(fallback %||% 1))
  if (
    length(fallback) != 1L ||
      is.na(fallback) ||
      !is.finite(fallback) ||
      fallback <= 0
  ) {
    fallback <- 1
  }
  value <- suppressWarnings(as.numeric(value %||% fallback))
  if (
    length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value <= 0
  ) {
    value <- fallback
  }
  allowed[[which.min(abs(log10(allowed) - log10(value)))]]
}

dashboard_time_basis_label <- function(value) {
  switch(
    value,
    participant = "Own recording dates",
    calendar = "Shared calendar dates",
    elapsed = "Days from first measurement",
    week = "Calendar weeks",
    month = "Calendar months",
    year = "Calendar years",
    "Unknown time reference"
  )
}

dashboard_plot_scale_label <- function(value) {
  switch(
    value,
    symlog = "Symlog",
    linear = "Linear",
    log = "Logarithmic",
    "Unknown scale"
  )
}

dashboard_view_defaults <- function(limits = dashboard_plot_limits()) {
  structure(
    list(
      time_basis = "participant",
      view_mode = "auto",
      days_per_page = limits$detailed_max_days,
      participants_per_page = limits$facets_per_page,
      plot_scale = "symlog",
      symlog_threshold = 1
    ),
    class = c("llw_dashboard_view_defaults", "list")
  )
}

dashboard_view_recommendation <- function(
  snapshot,
  focus_view = NULL,
  participants = character(),
  show_all = FALSE,
  limits = dashboard_plot_limits()
) {
  if (!inherits(snapshot, "llw_dashboard_snapshot")) {
    abort_llw("`snapshot` must be a dashboard snapshot.", type = "validation")
  }
  if (is.null(focus_view)) {
    focus_view <- dashboard_focus_view(
      snapshot,
      focus_variable = snapshot$primary_variable,
      data_stage = "preprocessed"
    )
  }
  if (!inherits(focus_view, "llw_dashboard_focus_view")) {
    abort_llw(
      "`focus_view` must be a dashboard focus view.",
      type = "validation"
    )
  }
  defaults <- dashboard_view_defaults(limits)

  requested <- dashboard_requested_participants(
    snapshot,
    participants = participants,
    show_all = show_all,
    limits = limits
  )
  ranges <- focus_view$participant_ranges[
    focus_view$participant_ranges$Id %in% requested,
    ,
    drop = FALSE
  ]
  measurement_end <- if (nrow(ranges) == 0L) {
    1L
  } else {
    as.integer(max(ranges[["Measurement days"]]))
  }
  date_window <- if (nrow(ranges) == 0L) {
    c(snapshot$date_start, snapshot$date_end)
  } else {
    c(min(ranges[["Date start"]]), max(ranges[["Date end"]]))
  }

  preferred_scale <- dashboard_plot_scale_value(
    snapshot$record$analysis_settings$display_scale %||%
      defaults$plot_scale,
    fallback = defaults$plot_scale
  )
  preferred_threshold <- dashboard_symlog_threshold_value(
    snapshot$record$analysis_settings$symlog_threshold %||%
      defaults$symlog_threshold,
    fallback = defaults$symlog_threshold
  )
  recommended_selection <- dashboard_plot_selection(
    snapshot,
    participants = requested,
    show_all = FALSE,
    date_window = date_window,
    limits = limits,
    time_basis = defaults$time_basis,
    measurement_window = c(1L, measurement_end),
    view_mode = defaults$view_mode,
    time_page = 1L,
    days_per_page = defaults$days_per_page,
    participants_per_page = defaults$participants_per_page,
    focus_view = focus_view
  )
  resolved_label <- if (identical(recommended_selection$mode, "detailed")) {
    "Detailed timeline"
  } else {
    "Coverage overview"
  }
  page_days <- defaults$days_per_page
  summary <- paste(
    dashboard_time_basis_label(defaults$time_basis),
    paste0("full duration, up to ", page_days, " days/page"),
    paste0(defaults$participants_per_page, " participants/page"),
    paste0("Automatic \u2192 ", resolved_label),
    dashboard_plot_scale_label(preferred_scale),
    sep = " \u00b7 "
  )
  view_reason <- if (recommended_selection$available_span_days == 0L) {
    paste(
      "No non-missing focus measurements are available, so Automatic starts",
      "with the coverage overview and its explicit empty state."
    )
  } else if (identical(recommended_selection$mode, "detailed")) {
    paste0(
      "The longest available participant focus span is ",
      recommended_selection$available_span_days,
      " day(s), so Automatic starts with the detailed timeline."
    )
  } else {
    paste0(
      "At least one participant has ",
      recommended_selection$available_span_days,
      " available focus days, so Automatic starts with coverage and keeps ",
      "detailed view available as an override."
    )
  }
  scale_reason <- if (identical(preferred_scale, "symlog")) {
    paste(
      "Symlog is the recorded display preference and keeps exact zero",
      "and negative readings visible."
    )
  } else {
    paste0(
      dashboard_plot_scale_label(preferred_scale),
      " is the recorded display preference for this dataset."
    )
  }

  structure(
    list(
      version = 1L,
      participants = requested,
      focus_variable = focus_view$focus_variable,
      data_stage = focus_view$data_stage,
      time_basis = defaults$time_basis,
      date_window = as.Date(date_window),
      measurement_window = c(1L, measurement_end),
      view_mode = defaults$view_mode,
      resolved_mode = recommended_selection$mode,
      available_span_days = recommended_selection$available_span_days,
      days_per_page = page_days,
      participants_per_page = defaults$participants_per_page,
      plot_scale = preferred_scale,
      symlog_threshold = preferred_threshold,
      summary = summary,
      reasons = c(
        paste(
          "Own recording dates is the neutral starting point: it does not",
          "pretend that staggered participants shared a calendar or a common",
          "study-day origin."
        ),
        paste(
          "The complete selected recording duration remains in scope and is",
          "paged instead of truncated."
        ),
        view_reason,
        scale_reason
      )
    ),
    class = c("llw_dashboard_recommendation", "list")
  )
}

dashboard_recommendation_state <- function(
  selection,
  recommendation,
  plot_scale = recommendation$plot_scale,
  symlog_threshold = recommendation$symlog_threshold
) {
  if (!inherits(selection, "llw_dashboard_selection")) {
    abort_llw(
      "`selection` must be a dashboard selection.",
      type = "validation"
    )
  }
  if (!inherits(recommendation, "llw_dashboard_recommendation")) {
    abort_llw(
      "`recommendation` must be a dashboard recommendation.",
      type = "validation"
    )
  }
  differences <- character()
  if (!identical(selection$time_basis, recommendation$time_basis)) {
    differences <- c(differences, "time reference")
  } else if (
    !identical(
      c(selection$measurement_start, selection$measurement_end),
      as.integer(recommendation$measurement_window)
    )
  ) {
    differences <- c(differences, "measurement-duration window")
  }
  if (!identical(selection$view_mode, recommendation$view_mode)) {
    differences <- c(differences, "view choice")
  }
  if (!identical(selection$days_per_page, recommendation$days_per_page)) {
    differences <- c(differences, "days per time page")
  }
  if (
    !identical(
      selection$participants_per_page,
      recommendation$participants_per_page
    )
  ) {
    differences <- c(differences, "participants per page")
  }
  if (identical(recommendation$resolved_mode, "detailed")) {
    chosen_scale <- dashboard_plot_scale_value(plot_scale)
    if (!identical(chosen_scale, recommendation$plot_scale)) {
      differences <- c(differences, "display scale")
    } else if (
      identical(chosen_scale, "symlog") &&
        !isTRUE(all.equal(
          dashboard_symlog_threshold_value(symlog_threshold),
          recommendation$symlog_threshold
        ))
    ) {
      differences <- c(differences, "symlog linear range")
    }
  }
  differences <- unique(differences)
  structure(
    list(
      active = length(differences) == 0L,
      differences = differences
    ),
    class = c("llw_dashboard_recommendation_state", "list")
  )
}

dashboard_evenly_spaced_positions <- function(size, limit) {
  if (size <= limit) return(seq_len(size))
  unique(as.integer(round(seq(1, size, length.out = limit))))
}

dashboard_bound_timeline <- function(data, max_rows) {
  if (nrow(data) <= max_rows) {
    return(list(data = data, reduced = FALSE))
  }
  participant_ids <- unique(as.character(data$Id))
  per_participant <- max(1L, floor(max_rows / length(participant_ids)))
  selected <- unlist(
    lapply(participant_ids, function(participant_id) {
      rows <- which(as.character(data$Id) == participant_id)
      rows[dashboard_evenly_spaced_positions(length(rows), per_participant)]
    }),
    use.names = FALSE
  )
  selected <- sort(utils::head(selected, max_rows))
  list(data = data[selected, , drop = FALSE], reduced = TRUE)
}

# Segment identifiers are derived before display thinning. Retained rows can
# therefore never invent a new gap merely because intermediate rows were
# omitted from the bounded visual sample.
dashboard_timeline_segments <- function(data, quality) {
  dashboard_assert_canonical_data(data, "data")
  if (!inherits(quality, "llw_raw_import_quality")) {
    abort_llw(
      "`quality` must be a raw-import quality summary.",
      type = "validation"
    )
  }
  ids <- as.character(data$Id)
  segment_number <- integer(nrow(data))
  for (participant_id in unique(ids)) {
    rows <- which(ids == participant_id)
    ordered_rows <- rows[order(data$Datetime[rows])]
    quality_row <- quality$participants[
      as.character(quality$participants$Id) == participant_id,
      ,
      drop = FALSE
    ]
    epoch_seconds <- if (
      nrow(quality_row) == 1L &&
        is.finite(quality_row$dominant_epoch_seconds[[1L]]) &&
        quality_row$dominant_epoch_seconds[[1L]] > 0
    ) {
      quality_row$dominant_epoch_seconds[[1L]]
    } else {
      NA_real_
    }
    starts_new_segment <- rep(FALSE, length(ordered_rows))
    if (is.finite(epoch_seconds) && length(ordered_rows) > 1L) {
      lag_seconds <- diff(as.numeric(data$Datetime[ordered_rows]))
      starts_new_segment[-1L] <- dashboard_inferred_gap_epochs(
        lag_seconds,
        epoch_seconds
      ) > 0
    }
    segment_number[ordered_rows] <- cumsum(starts_new_segment) + 1L
  }
  paste0(ids, "::", segment_number)
}

dashboard_reduce_coverage <- function(coverage, max_cells) {
  if (nrow(coverage) <= max_cells) {
    coverage$`Period start` <- coverage$Date
    coverage$`Period end` <- coverage$Date
    coverage$`Bin days` <- rep.int(1L, nrow(coverage))
    return(list(data = coverage, reduced = FALSE, bin_days = 1L))
  }
  participant_ids <- unique(as.character(coverage$Id))
  available_bins <- max(1L, floor(max_cells / length(participant_ids)))
  first_date <- min(coverage$Date)
  day_span <- as.integer(max(coverage$Date) - first_date) + 1L
  bin_days <- max(1L, ceiling(day_span / available_bins))
  bin <- floor(as.integer(coverage$Date - first_date) / bin_days)
  groups <- split(
    seq_len(nrow(coverage)),
    interaction(
      coverage$Id,
      bin,
      drop = TRUE,
      lex.order = TRUE
    )
  )
  rows <- lapply(groups, function(index) {
    values <- coverage[index, , drop = FALSE]
    expected <- values[["Expected epochs"]]
    observed <- values[["Observed focus epochs"]]
    estimable <- any(!is.na(expected)) && sum(expected, na.rm = TRUE) > 0
    expected_sum <- if (estimable) sum(expected, na.rm = TRUE) else NA_real_
    observed_sum <- if (estimable) sum(observed, na.rm = TRUE) else NA_real_
    period_start <- min(values$Date)
    period_end <- max(values$Date)
    data.frame(
      Id = as.character(values$Id[[1L]]),
      Date = period_start + floor(as.integer(period_end - period_start) / 2),
      `Expected epochs` = expected_sum,
      `Observed focus epochs` = observed_sum,
      `Focus coverage (%)` = if (estimable) {
        100 * observed_sum / expected_sum
      } else {
        NA_real_
      },
      `Focus variable` = as.character(values[["Focus variable"]][[1L]]),
      `Period start` = period_start,
      `Period end` = period_end,
      `Bin days` = as.integer(period_end - period_start) + 1L,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  reduced <- do.call(rbind, rows)
  rownames(reduced) <- NULL
  list(data = reduced, reduced = TRUE, bin_days = bin_days)
}

dashboard_rows_in_windows <- function(ids, local_date, participant_windows) {
  if (length(ids) != length(local_date)) {
    abort_llw(
      "`ids` and `local_date` must have equal lengths.",
      type = "validation"
    )
  }
  if (nrow(participant_windows) == 0L) return(rep(FALSE, length(ids)))
  position <- match(as.character(ids), participant_windows$Id)
  keep <- !is.na(position)
  keep[keep] <- local_date[keep] >=
    participant_windows$`Window start`[position[keep]] &
    local_date[keep] <= participant_windows$`Window end`[position[keep]]
  keep
}

dashboard_measurement_day <- function(ids, local_date, participant_ranges) {
  position <- match(as.character(ids), participant_ranges$Id)
  result <- rep.int(NA_integer_, length(ids))
  matched <- !is.na(position)
  result[matched] <- as.integer(
    local_date[matched] -
      participant_ranges$`Date start`[position[matched]]
  ) + 1L
  result
}

dashboard_elapsed_plot_time <- function(
  datetimes,
  measurement_day,
  timezone
) {
  local <- lubridate::with_tz(datetimes, tzone = timezone)
  seconds <- lubridate::hour(local) * 3600 +
    lubridate::minute(local) * 60 +
    floor(lubridate::second(local))
  as.POSIXct("2000-01-01 00:00:00", tz = "UTC") +
    (measurement_day - 1L) * 86400 +
    seconds
}

dashboard_focus_domains <- function(focus_view, selection) {
  if (!inherits(focus_view, "llw_dashboard_focus_view")) {
    abort_llw(
      "`focus_view` must be a dashboard focus view.",
      type = "validation"
    )
  }
  if (!inherits(selection, "llw_dashboard_selection")) {
    abort_llw(
      "`selection` must be a dashboard selection.",
      type = "validation"
    )
  }
  full_windows <- dashboard_selection_windows(
    focus_view,
    selection$participants,
    selection$time_basis,
    date_start = selection$scope_date_start,
    date_end = selection$scope_date_end,
    measurement_start = selection$measurement_start,
    measurement_end = selection$measurement_end
  )
  keep <- dashboard_rows_in_windows(
    focus_view$data$Id,
    focus_view$local_date,
    full_windows
  )
  values <- focus_view$data[[focus_view$focus_variable]][keep]
  finite <- values[is.finite(values)]
  positive <- finite[finite > 0]
  list(
    finite = if (length(finite) == 0L) NULL else range(finite),
    positive = if (length(positive) == 0L) NULL else range(positive)
  )
}

dashboard_plot_preview <- function(
  snapshot,
  selection,
  limits = dashboard_plot_limits(),
  focus_variable = snapshot$primary_variable,
  data_stage = c("preprocessed", "source"),
  focus_view = NULL
) {
  if (
    !inherits(snapshot, "llw_dashboard_snapshot") ||
      !inherits(selection, "llw_dashboard_selection")
  ) {
    abort_llw(
      "Dashboard preview inputs have invalid classes.",
      type = "validation"
    )
  }
  data_stage <- match.arg(data_stage)
  if (is.null(focus_view)) {
    focus_view <- dashboard_focus_view(
      snapshot,
      focus_variable = focus_variable,
      data_stage = data_stage
    )
  }
  if (!inherits(focus_view, "llw_dashboard_focus_view")) {
    abort_llw(
      "`focus_view` must be a dashboard focus view.",
      type = "validation"
    )
  }
  focus_domains <- dashboard_focus_domains(focus_view, selection)
  page_label <- paste0(
    "Participant page ",
    selection$participant_page,
    " of ",
    selection$participant_pages,
    "; time page ",
    selection$time_page,
    " of ",
    selection$time_pages,
    " (",
    selection$time_page_label,
    "); ",
    length(selection$page_participants),
    " of ",
    length(selection$participants),
    " requested participant(s)."
  )

  if (identical(selection$mode, "detailed")) {
    keep <- dashboard_rows_in_windows(
      focus_view$data$Id,
      focus_view$local_date,
      selection$participant_windows
    )
    source <- focus_view$data[keep, , drop = FALSE]
    source <- source[order(source$Id, source$Datetime), , drop = FALSE]
    source$.llw_dashboard_segment <- dashboard_timeline_segments(
      source,
      focus_view$quality
    )
    bounded <- dashboard_bound_timeline(source, limits$max_plot_rows)
    display_local_date <- as.Date(
      lubridate::with_tz(
        bounded$data$Datetime,
        tzone = snapshot$display_timezone
      ),
      tz = snapshot$display_timezone
    )
    measurement_day <- dashboard_measurement_day(
      bounded$data$Id,
      display_local_date,
      focus_view$participant_ranges
    )
    plot_time <- if (identical(selection$time_basis, "elapsed")) {
      dashboard_elapsed_plot_time(
        bounded$data$Datetime,
        measurement_day,
        snapshot$display_timezone
      )
    } else {
      bounded$data$Datetime
    }
    display <- data.frame(
      Id = as.character(bounded$data$Id),
      Datetime = bounded$data$Datetime,
      `Plot time` = plot_time,
      `Local date` = display_local_date,
      `Measurement day` = measurement_day,
      Value = bounded$data[[focus_view$focus_variable]],
      Segment = bounded$data$.llw_dashboard_segment,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    reduction <- if (bounded$reduced) {
      paste0(
        "Evenly spaced display sample: ",
        format(nrow(display), big.mark = ","),
        " of ",
        format(nrow(source), big.mark = ","),
        " filtered rows. Missing measurements remain missing; no value was imputed."
      )
    } else {
      paste0(
        "All ",
        format(nrow(source), big.mark = ","),
        " filtered row(s) are shown; no aggregation or imputation was applied."
      )
    }
    return(structure(
      list(
        mode = "detailed",
        data = display,
        source_rows = nrow(source),
        displayed_rows = nrow(display),
        reduced = bounded$reduced,
        notice = paste(page_label, reduction),
        page_label = page_label,
        focus_variable = focus_view$focus_variable,
        focus_unit = focus_view$focus_unit,
        data_stage = focus_view$data_stage,
        stage_label = focus_view$stage_label,
        time_basis = selection$time_basis,
        time_page_label = selection$time_page_label,
        participant_windows = selection$participant_windows,
        focus_domain = focus_domains$finite,
        positive_focus_domain = focus_domains$positive,
        selection = selection
      ),
      class = c("llw_dashboard_plot_preview", "list")
    ))
  }

  keep <- dashboard_rows_in_windows(
    focus_view$coverage$Id,
    focus_view$coverage$Date,
    selection$participant_windows
  )
  source <- focus_view$coverage[keep, , drop = FALSE]
  bounded <- dashboard_reduce_coverage(source, limits$max_overview_cells)
  bounded$data$`Measurement day` <- dashboard_measurement_day(
    bounded$data$Id,
    bounded$data$Date,
    focus_view$participant_ranges
  )
  bounded$data$`Plot date` <- if (
    selection$time_basis %in% c("participant", "elapsed")
  ) {
    as.Date("2000-01-01") + bounded$data$`Measurement day` - 1L
  } else {
    bounded$data$Date
  }
  reduction <- if (nrow(source) == 0L) {
    paste(
      "No focus measurements fall in this date window for the participants",
      "on this page."
    )
  } else if (bounded$reduced) {
    paste0(
      "Focus coverage was aggregated into bins of up to ",
      bounded$bin_days,
      " local day(s): ",
      nrow(bounded$data),
      " displayed cells from ",
      nrow(source),
      " participant-day cells."
    )
  } else {
    paste0(
      "All ",
      nrow(source),
      " participant-day cells are shown without date aggregation."
    )
  }
  structure(
    list(
      mode = "availability",
      data = bounded$data,
      source_rows = nrow(source),
      displayed_rows = nrow(bounded$data),
      reduced = bounded$reduced,
      notice = paste(
        page_label,
        reduction,
        paste(
          "Coverage counts non-missing focus measurements at regular epochs",
          "against the full participant-local calendar day. Exact zero is an",
          "observed value; partial first and last days remain visible."
        )
      ),
      page_label = page_label,
      focus_variable = focus_view$focus_variable,
      focus_unit = focus_view$focus_unit,
      data_stage = focus_view$data_stage,
      stage_label = focus_view$stage_label,
      time_basis = selection$time_basis,
      time_page_label = selection$time_page_label,
      participant_windows = selection$participant_windows,
      focus_domain = focus_domains$finite,
      positive_focus_domain = focus_domains$positive,
      selection = selection
    ),
    class = c("llw_dashboard_plot_preview", "list")
  )
}

dashboard_focus_axis_label <- function(preview, scale = NULL) {
  unit <- preview$focus_unit
  omit_unit <- is.null(unit) ||
    length(unit) == 0L ||
    is.na(unit[[1L]]) ||
    !nzchar(trimws(unit[[1L]])) ||
    identical(tolower(trimws(unit[[1L]])), "unit not specified") ||
    identical(
      tolower(trimws(unit[[1L]])),
      tolower(trimws(preview$focus_variable))
    )
  label <- if (omit_unit) {
    preview$focus_variable
  } else {
    paste0(preview$focus_variable, " (", unit[[1L]], ")")
  }
  if (is.null(scale)) return(label)
  scale <- match.arg(scale, c("symlog", "linear", "log"))
  paste0(
    label,
    " \u00b7 ",
    switch(
      scale,
      symlog = "symlog",
      linear = "linear",
      log = "log10"
    )
  )
}

dashboard_axis_number <- function(value) {
  vapply(
    value,
    function(item) {
      if (!is.finite(item)) return(NA_character_)
      if (item == 0) return("0")
      magnitude <- abs(item)
      digits <- if (magnitude < 1) {
        min(8L, max(1L, as.integer(ceiling(-log10(magnitude))) + 1L))
      } else {
        8L
      }
      label <- trimws(formatC(
        item,
        format = "fg",
        digits = digits,
        big.mark = ","
      ))
      sub("-", "\u2212", label, fixed = TRUE)
    },
    character(1)
  )
}

dashboard_thin_breaks <- function(values, n) {
  if (length(values) <= n) return(values)
  indices <- unique(as.integer(round(seq(
    from = 1,
    to = length(values),
    length.out = n
  ))))
  values[indices]
}

dashboard_minor_breaks <- function(major_breaks, subdivisions = 10L) {
  major_breaks <- sort(unique(major_breaks[is.finite(major_breaks)]))
  subdivisions <- as.integer(subdivisions)
  if (
    length(subdivisions) != 1L ||
      is.na(subdivisions) ||
      subdivisions < 2L
  ) {
    abort_llw(
      "`subdivisions` must be an integer of at least two.",
      type = "validation"
    )
  }
  if (length(major_breaks) < 2L) return(numeric())
  unlist(lapply(
    seq_len(length(major_breaks) - 1L),
    function(index) {
      seq(
        major_breaks[[index]],
        major_breaks[[index + 1L]],
        length.out = subdivisions + 1L
      )[seq.int(2L, subdivisions)]
    }
  ), use.names = FALSE)
}

dashboard_symlog_axis <- function(
  values,
  threshold = 1,
  base = 10,
  max_breaks = 11L
) {
  if (!is.numeric(values)) {
    abort_llw("`values` must be numeric.", type = "validation")
  }
  if (
    !is.numeric(threshold) ||
      length(threshold) != 1L ||
      is.na(threshold) ||
      !is.finite(threshold) ||
      threshold <= 0
  ) {
    abort_llw(
      "`threshold` must be one positive finite number.",
      type = "validation"
    )
  }
  if (
    !is.numeric(base) ||
      length(base) != 1L ||
      is.na(base) ||
      !is.finite(base) ||
      base <= 1
  ) {
    abort_llw(
      "`base` must be one finite number greater than one.",
      type = "validation"
    )
  }
  max_breaks <- as.integer(max_breaks)
  if (length(max_breaks) != 1L || is.na(max_breaks) || max_breaks < 3L) {
    abort_llw("`max_breaks` must be at least three.", type = "validation")
  }

  finite <- values[is.finite(values)]
  if (length(finite) == 0L) {
    return(list(
      limits = NULL,
      breaks = numeric(),
      minor_breaks = numeric(),
      labels = character(),
      threshold = threshold,
      base = base
    ))
  }

  data_range <- range(finite)
  anchored_range <- c(min(data_range[[1L]], 0), max(data_range[[2L]], 0))
  has_outer_values <- any(abs(finite) > threshold)
  if (!has_outer_values) {
    breaks <- pretty(anchored_range, n = min(5L, max_breaks))
    breaks <- breaks[
      breaks >= anchored_range[[1L]] &
        breaks <= anchored_range[[2L]]
    ]
    if (length(breaks) == 0L) breaks <- 0
    breaks <- sort(unique(c(breaks, 0)))
    return(list(
      limits = anchored_range,
      breaks = breaks,
      minor_breaks = dashboard_minor_breaks(breaks),
      labels = dashboard_axis_number(breaks),
      threshold = threshold,
      base = base
    ))
  }

  lower <- if (data_range[[1L]] < 0) {
    min(data_range[[1L]], -threshold)
  } else {
    0
  }
  upper <- if (data_range[[2L]] > 0) {
    max(data_range[[2L]], threshold)
  } else {
    0
  }
  central <- c(
    if (lower < 0) -threshold else numeric(),
    0,
    if (upper > 0) threshold else numeric()
  )

  decade_breaks <- function(maximum) {
    if (!is.finite(maximum) || maximum <= threshold) return(numeric())
    exponent_max <- floor(
      log(maximum / threshold, base = base) +
        sqrt(.Machine$double.eps)
    )
    if (exponent_max < 1) return(numeric())
    threshold * base^seq_len(exponent_max)
  }
  negative_outer <- -rev(decade_breaks(abs(lower)))
  positive_outer <- decade_breaks(upper)
  available_slots <- max(2L, max_breaks - length(central))
  if (length(negative_outer) > 0L && length(positive_outer) > 0L) {
    negative_slots <- floor(available_slots / 2L)
    positive_slots <- available_slots - negative_slots
  } else if (length(negative_outer) > 0L) {
    negative_slots <- available_slots
    positive_slots <- 0L
  } else {
    negative_slots <- 0L
    positive_slots <- available_slots
  }
  negative_outer <- dashboard_thin_breaks(
    negative_outer,
    max(1L, negative_slots)
  )
  positive_outer <- dashboard_thin_breaks(
    positive_outer,
    max(1L, positive_slots)
  )
  breaks <- sort(unique(c(negative_outer, central, positive_outer)))
  breaks <- breaks[breaks >= lower & breaks <= upper]

  list(
    limits = c(lower, upper),
    breaks = breaks,
    minor_breaks = dashboard_minor_breaks(breaks),
    labels = dashboard_axis_number(breaks),
    threshold = threshold,
    base = base
  )
}

dashboard_elapsed_axis_labels <- function(value) {
  dates <- as.Date(value, tz = "UTC")
  day <- as.integer(dates - as.Date("2000-01-01")) + 1L
  time <- format(value, "%H:%M", tz = "UTC")
  ifelse(
    time == "00:00",
    paste("Day", day),
    paste0("Day ", day, "\n", time)
  )
}

#' Validate an optional manual dashboard y-axis range
#'
#' Returns a non-throwing validation state so Shiny can surface user-input
#' errors next to the controls before attempting to draw the plot.
dashboard_y_axis_limits <- function(
  minimum = NULL,
  maximum = NULL,
  scale = c("symlog", "linear", "log")
) {
  scale <- match.arg(scale)
  parse_endpoint <- function(value, label) {
    if (is.null(value) || length(value) == 0L || all(is.na(value))) {
      return(list(value = NA_real_, message = NULL))
    }
    if (
      is.character(value) &&
        length(value) == 1L &&
        !nzchar(trimws(value))
    ) {
      return(list(value = NA_real_, message = NULL))
    }
    if (length(value) != 1L) {
      return(list(
        value = NA_real_,
        message = paste0(label, " must be one number or left blank.")
      ))
    }
    parsed <- suppressWarnings(as.numeric(value))
    if (is.na(parsed) || !is.finite(parsed)) {
      return(list(
        value = NA_real_,
        message = paste0(label, " must be a finite number or left blank.")
      ))
    }
    list(value = parsed, message = NULL)
  }

  lower <- parse_endpoint(minimum, "Y-axis minimum")
  upper <- parse_endpoint(maximum, "Y-axis maximum")
  messages <- c(lower$message, upper$message)
  limits <- c(minimum = lower$value, maximum = upper$value)
  active <- any(is.finite(limits))
  if (
    length(messages) == 0L &&
      all(is.finite(limits)) &&
      limits[[1L]] >= limits[[2L]]
  ) {
    messages <- "Y-axis minimum must be smaller than the maximum."
  }
  if (
    length(messages) == 0L &&
      identical(scale, "log") &&
      any(limits[is.finite(limits)] <= 0)
  ) {
    messages <- paste(
      "Logarithmic y-axis limits must be greater than zero;",
      "leave an endpoint blank for automatic scaling."
    )
  }
  list(
    limits = unname(limits),
    active = active,
    message = if (length(messages) == 0L) NULL else paste(messages, collapse = " ")
  )
}

dashboard_plot_height <- function(
  preview,
  detailed_minimum = 620L,
  availability_minimum = 520L,
  maximum = 1500L
) {
  if (!inherits(preview, "llw_dashboard_plot_preview")) {
    abort_llw(
      "`preview` must be a dashboard plot preview.",
      type = "validation"
    )
  }
  facet_slots <- preview$selection$facet_slots
  facet_count <- if (is.null(facet_slots)) {
    length(unique(as.character(preview$data$Id)))
  } else {
    nrow(facet_slots)
  }
  facet_count <- max(1L, as.integer(facet_count))
  if (identical(preview$mode, "detailed")) {
    requested <- 120L + 105L * facet_count
    minimum <- as.integer(detailed_minimum)
  } else {
    requested <- 180L + 55L * facet_count
    minimum <- as.integer(availability_minimum)
  }
  as.integer(min(as.integer(maximum), max(minimum, requested)))
}

plot_dashboard_timeline <- function(
  snapshot,
  preview,
  mode = c("light", "dark"),
  scale = c("symlog", "linear", "log"),
  symlog_threshold = 1,
  y_limits = NULL,
  y_scope = c("shared", "page")
) {
  mode <- match.arg(mode)
  scale <- match.arg(scale)
  y_scope <- match.arg(y_scope)
  if (!identical(preview$mode, "detailed")) {
    abort_llw("`preview` is not a detailed timeline.", type = "validation")
  }
  colors <- lightlogweb_plot_colors(mode)
  plot_data <- preview$data
  y_limit_state <- if (is.null(y_limits)) {
    dashboard_y_axis_limits(scale = scale)
  } else if (is.list(y_limits) && !is.null(y_limits$limits)) {
    if (!is.null(y_limits$message)) {
      y_limits
    } else {
      dashboard_y_axis_limits(
        y_limits$limits[[1L]],
        y_limits$limits[[2L]],
        scale = scale
      )
    }
  } else if (is.atomic(y_limits) && length(y_limits) == 2L) {
    dashboard_y_axis_limits(
      y_limits[[1L]],
      y_limits[[2L]],
      scale = scale
    )
  } else {
    list(
      limits = c(NA_real_, NA_real_),
      active = FALSE,
      message = "`y_limits` must be NULL or a numeric length-two range."
    )
  }
  if (!is.null(y_limit_state$message)) {
    abort_llw(y_limit_state$message, type = "validation")
  }
  manual_y_limits <- if (isTRUE(y_limit_state$active)) {
    y_limit_state$limits
  } else {
    NULL
  }
  log_zero_excluded <- 0L
  log_negative_excluded <- 0L
  if (identical(scale, "log")) {
    finite_value <- is.finite(plot_data$Value)
    log_zero_excluded <- sum(finite_value & plot_data$Value == 0)
    log_negative_excluded <- sum(finite_value & plot_data$Value < 0)
    nonpositive <- finite_value & plot_data$Value <= 0
    plot_data$Value[nonpositive] <- NA_real_
  }
  log_excluded <- log_zero_excluded + log_negative_excluded
  page_finite_values <- plot_data$Value[is.finite(plot_data$Value)]
  page_positive_values <- page_finite_values[page_finite_values > 0]
  y_domain <- if (identical(y_scope, "shared")) {
    preview$focus_domain
  } else if (length(page_finite_values) > 0L) {
    range(page_finite_values)
  } else {
    NULL
  }
  positive_y_domain <- if (identical(y_scope, "shared")) {
    preview$positive_focus_domain
  } else if (length(page_positive_values) > 0L) {
    range(page_positive_values)
  } else {
    NULL
  }
  facet_slots <- preview$selection$facet_slots
  if (is.null(facet_slots) || nrow(facet_slots) == 0L) {
    facet_slots <- data.frame(
      Id = unique(as.character(plot_data$Id)),
      Label = unique(as.character(plot_data$Id)),
      Empty = FALSE,
      stringsAsFactors = FALSE
    )
  }
  plot_data$Id <- factor(
    as.character(plot_data$Id),
    levels = facet_slots$Id
  )
  facet_labels <- stats::setNames(facet_slots$Label, facet_slots$Id)
  grouped_plot_data <- dplyr::group_by(plot_data, Id, .drop = TRUE)
  participant_timeline <- identical(preview$time_basis, "participant")
  x_scales <- if (participant_timeline) "free_x" else "fixed"
  facet_anchor_data <- NULL
  if (participant_timeline) {
    valid_plot_time <- plot_data[["Plot time"]]
    valid_plot_time <- valid_plot_time[!is.na(valid_plot_time)]
    observed_facets <- unique(as.character(plot_data$Id[
      !is.na(plot_data[["Plot time"]])
    ]))
    missing_facets <- setdiff(facet_slots$Id, observed_facets)
    if (
      length(missing_facets) > 0L &&
        length(valid_plot_time) > 0L
    ) {
      anchor_range <- range(valid_plot_time)
      facet_anchor_data <- data.frame(
        Id = factor(
          rep(missing_facets, each = 2L),
          levels = facet_slots$Id
        ),
        `Plot time` = rep(
          anchor_range,
          times = length(missing_facets)
        ),
        check.names = FALSE
      )
    }
  }
  participant_limit <- function(value) {
    value <- value[!is.na(value)]
    if (length(value) == 0L) return(NULL)
    first_value <- min(value)
    day_start <- lubridate::floor_date(first_value, unit = "1 day")
    LightLogR::Datetime_limits(
      value,
      start = as.numeric(
        difftime(day_start, first_value, units = "secs")
      ),
      length = lubridate::days(preview$selection$day_count),
      unit = "1 day",
      midnight.rollover = FALSE
    )
  }
  plot <- rlang::inject(
    LightLogR::gg_days(
      grouped_plot_data,
      y.axis = !!rlang::sym("Value"),
      geom = "line",
      x.axis = !!rlang::sym("Plot time"),
      group = !!rlang::sym("Segment"),
      scales = x_scales,
      x.axis.breaks = ggplot2::waiver(),
      y.axis.breaks = ggplot2::waiver(),
      y.scale = "identity",
      x.axis.limits = if (participant_timeline) participant_limit else
        LightLogR::Datetime_limits,
      x.axis.format = "%d %b\n%H:%M",
      x.axis.label = "Time",
      y.axis.label = dashboard_focus_axis_label(preview, scale),
      jco_color = FALSE,
      color = unname(colors[["primary"]]),
      linewidth = 0.45,
      na.rm = TRUE
    )
  ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = !!rlang::sym("Plot time"),
        y = !!rlang::sym("Value")
      ),
      color = unname(colors[["primary"]]),
      alpha = 0.5,
      size = 0.65,
      na.rm = TRUE,
      inherit.aes = FALSE
    )
  if (!is.null(facet_anchor_data)) {
    plot <- plot +
      ggplot2::geom_blank(
        data = facet_anchor_data,
        ggplot2::aes(x = !!rlang::sym("Plot time")),
        inherit.aes = FALSE
      )
  }
  plot <- plot +
    ggplot2::facet_wrap(
      ggplot2::vars(Id),
      ncol = 1,
      scales = x_scales,
      drop = FALSE,
      strip.position = "left",
      labeller = ggplot2::labeller(
        Id = ggplot2::as_labeller(facet_labels)
      )
    )
  chosen_time <- preview$selection$time_chunks[
    preview$selection$time_page,
    ,
    drop = FALSE
  ]
  elapsed_basis <- identical(preview$time_basis, "elapsed")
  absolute_basis <- preview$time_basis %in%
    c("calendar", "week", "month", "year")
  x_limits <- if (elapsed_basis) {
    anchor <- as.POSIXct("2000-01-01 00:00:00", tz = "UTC")
    c(
      anchor + (chosen_time$Start[[1L]] - 1L) * 86400,
      anchor + chosen_time$End[[1L]] * 86400
    )
  } else if (absolute_basis) {
    c(
      as.POSIXct(
        paste(chosen_time$Start[[1L]], "00:00:00"),
        tz = snapshot$display_timezone
      ),
      as.POSIXct(
        paste(chosen_time$End[[1L]] + 1L, "00:00:00"),
        tz = snapshot$display_timezone
      )
    )
  } else {
    NULL
  }
  break_width <- if (preview$selection$day_count <= 2L) {
    "6 hours"
  } else {
    "1 day"
  }
  x_label_pattern <- switch(
    preview$time_basis,
    elapsed = NA_character_,
    week = "%a\n%d %b",
    month = "%d %b",
    year = "%d %b",
    "%d %b\n%H:%M"
  )
  x_labeler <- if (elapsed_basis) {
    dashboard_elapsed_axis_labels
  } else {
    function(value) {
      format(
        value,
        x_label_pattern,
        tz = snapshot$display_timezone
      )
    }
  }
  plot <- suppressMessages(
    plot +
      ggplot2::scale_x_datetime(
        timezone = if (elapsed_basis) "UTC" else snapshot$display_timezone,
        date_breaks = break_width,
        date_minor_breaks = if (preview$selection$day_count <= 7L) {
          "6 hours"
        } else {
          ggplot2::waiver()
        },
        labels = x_labeler,
        limits = if (participant_timeline) participant_limit else x_limits,
        expand = ggplot2::expansion(mult = c(0, 0))
      )
  )
  plot <- plot +
    ggplot2::labs(
      x = if (elapsed_basis) {
        "Days from first measurement"
      } else {
        paste0(
          "Local Date/Time (",
          snapshot$display_timezone,
          ")"
        )
      },
      y = dashboard_focus_axis_label(preview, scale)
    ) +
    lightlogweb_plot_theme(mode) +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(face = "bold"),
      strip.text.y.left = ggplot2::element_text(face = "bold"),
      panel.spacing.y = grid::unit(0.8, "lines"),
      plot.margin = ggplot2::margin(12, 32, 12, 12)
    )
  if (identical(scale, "symlog")) {
    symlog_axis <- dashboard_symlog_axis(
      c(
        y_domain,
        if (is.null(manual_y_limits)) numeric() else
          manual_y_limits[is.finite(manual_y_limits)]
      ),
      threshold = symlog_threshold
    )
    plot <- suppressMessages(
      plot +
        ggplot2::scale_y_continuous(
          transform = LightLogR::symlog_trans(
            base = symlog_axis$base,
            thr = symlog_threshold,
            scale = symlog_threshold
          ),
          limits = symlog_axis$limits,
          breaks = symlog_axis$breaks,
          labels = symlog_axis$labels,
          minor_breaks = symlog_axis$minor_breaks,
          guide = ggplot2::guide_axis(minor.ticks = TRUE),
          expand = ggplot2::expansion(mult = c(0.03, 0.06))
        )
    )
    plot <- plot +
      ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.ticks.y.left = ggplot2::element_line(
          color = unname(colors[["text"]]),
          linewidth = 0.35
        ),
        axis.minor.ticks.y.left = ggplot2::element_line(
          color = unname(colors[["text"]]),
          linewidth = 0.25
        ),
        axis.ticks.length.y.left = grid::unit(0.12, "cm"),
        axis.minor.ticks.length.y.left = grid::unit(0.07, "cm")
      )
  } else if (identical(scale, "log")) {
    plot <- suppressMessages(
      plot +
        ggplot2::scale_y_continuous(
          transform = "log10",
          labels = dashboard_axis_number,
          minor_breaks = NULL,
          limits = if (
            identical(y_scope, "shared") &&
              is.null(manual_y_limits)
          ) {
            positive_y_domain
          } else {
            NULL
          }
        )
    )
  } else {
    plot <- suppressMessages(
      plot +
        ggplot2::scale_y_continuous(
          labels = dashboard_axis_number,
          limits = if (
            identical(y_scope, "shared") &&
              is.null(manual_y_limits)
          ) {
            y_domain
          } else {
            NULL
          }
        )
    )
  }
  if (!is.null(manual_y_limits)) {
    plot <- suppressMessages(
      plot +
        ggplot2::coord_cartesian(ylim = manual_y_limits)
    )
  }
  attr(plot, "llw_dashboard_plot") <- "detailed_timeline"
  attr(plot, "llw_dashboard_scale") <- scale
  attr(plot, "llw_symlog_threshold") <- if (identical(scale, "symlog")) {
    symlog_threshold
  } else {
    NA_real_
  }
  attr(plot, "llw_log_excluded") <- as.integer(log_excluded)
  attr(plot, "llw_log_zero_excluded") <- as.integer(log_zero_excluded)
  attr(plot, "llw_log_negative_excluded") <- as.integer(
    log_negative_excluded
  )
  attr(plot, "llw_y_limits") <- if (is.null(manual_y_limits)) {
    c(NA_real_, NA_real_)
  } else {
    manual_y_limits
  }
  attr(plot, "llw_y_scope") <- y_scope
  attr(plot, "llw_facet_slots") <- facet_slots
  attr(plot, "llw_empty_facet_anchor_ids") <- if (
    is.null(facet_anchor_data)
  ) {
    character()
  } else {
    unique(as.character(facet_anchor_data$Id))
  }
  plot
}

plot_dashboard_availability <- function(
  snapshot,
  preview,
  mode = c("light", "dark")
) {
  mode <- match.arg(mode)
  if (!identical(preview$mode, "availability")) {
    abort_llw("`preview` is not an availability overview.", type = "validation")
  }
  colors <- lightlogweb_plot_colors(mode)
  plot_data <- preview$data
  if (nrow(plot_data) == 0L) {
    plot <- ggplot2::ggplot(
      data.frame(x = 0, y = 0),
      ggplot2::aes(x = !!rlang::sym("x"), y = !!rlang::sym("y"))
    ) +
      ggplot2::annotate(
        "text",
        x = 0,
        y = 0,
        label = paste(
          "No focus measurements in this time page",
          "for the participants on this page."
        ),
        color = unname(colors[["text"]]),
        size = 4.2
      ) +
      ggplot2::coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
      ggplot2::labs(x = NULL, y = NULL) +
      lightlogweb_plot_theme(mode) +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )
    attr(plot, "llw_dashboard_plot") <- "daily_availability"
    return(plot)
  }
  facet_slots <- preview$selection$facet_slots
  if (is.null(facet_slots) || nrow(facet_slots) == 0L) {
    facet_slots <- data.frame(
      Id = unique(as.character(plot_data$Id)),
      Label = unique(as.character(plot_data$Id)),
      Empty = FALSE,
      stringsAsFactors = FALSE
    )
  }
  facet_labels <- stats::setNames(facet_slots$Label, facet_slots$Id)
  plot_data$Id <- factor(
    as.character(plot_data$Id),
    levels = rev(facet_slots$Id)
  )
  chosen_time <- preview$selection$time_chunks[
    preview$selection$time_page,
    ,
    drop = FALSE
  ]
  x_limits <- if (
    preview$time_basis %in% c("participant", "elapsed")
  ) {
    as.Date("2000-01-01") + c(
      chosen_time$Start[[1L]] - 1L,
      chosen_time$End[[1L]] - 1L
    )
  } else {
    as.Date(c(
      chosen_time$Start[[1L]],
      chosen_time$End[[1L]]
    ))
  }
  tile_width <- pmax(0.9, plot_data[["Bin days"]] * 0.92)
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = !!rlang::sym("Plot date"),
      y = !!rlang::sym("Id"),
      fill = !!rlang::sym("Focus coverage (%)")
    )
  ) +
    ggplot2::geom_tile(
      width = tile_width,
      height = 0.82,
      color = unname(colors[["grid"]]),
      linewidth = 0.15
    ) +
    ggplot2::scale_fill_gradient(
      low = unname(colors[["surface"]]),
      high = unname(colors[["primary"]]),
      limits = c(0, 100),
      na.value = unname(colors[["reference"]]),
      name = "Focus coverage (%)"
    ) +
    ggplot2::scale_x_date(
      date_breaks = if (preview$selection$day_count <= 14L) {
        "1 day"
      } else {
        "1 week"
      },
      labels = switch(
        preview$time_basis,
        participant = function(value) {
          paste(
            "Day",
            as.integer(value - as.Date("2000-01-01")) + 1L
          )
        },
        elapsed = function(value) {
          paste(
            "Day",
            as.integer(value - as.Date("2000-01-01")) + 1L
          )
        },
        week = function(value) format(value, "%a"),
        month = function(value) format(value, "%d %b"),
        year = function(value) format(value, "%d %b"),
        function(value) format(value, "%d %b")
      ),
      limits = x_limits,
      expand = ggplot2::expansion(add = 0.55)
    ) +
    ggplot2::scale_y_discrete(
      drop = FALSE,
      labels = facet_labels
    ) +
    ggplot2::labs(
      x = if (preview$time_basis %in% c("participant", "elapsed")) {
        "Measurement day"
      } else {
        paste0(
          "Participant-local date (",
          snapshot$display_timezone,
          ")"
        )
      },
      y = "Participant ID"
    ) +
    lightlogweb_plot_theme(mode) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.y = ggplot2::element_text(face = "bold"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  cells_per_participant <- max(table(plot_data$Id))
  show_coverage_labels <- nrow(plot_data) <= 160L &&
    cells_per_participant <= 31L
  if (show_coverage_labels) {
    coverage_percent <- plot_data[["Focus coverage (%)"]]
    plot_data[["Coverage label"]] <- ifelse(
      is.finite(coverage_percent),
      paste0(round(coverage_percent), "%"),
      ""
    )
    plot_data[["Coverage label color"]] <- ifelse(
      is.finite(coverage_percent) & coverage_percent >= 55,
      unname(colors[["surface"]]),
      unname(colors[["text"]])
    )
    label_size <- if (cells_per_participant <= 14L) 3 else 2.4
    plot <- plot +
      ggplot2::geom_text(
        data = plot_data,
        ggplot2::aes(
          x = !!rlang::sym("Plot date"),
          y = !!rlang::sym("Id"),
          label = !!rlang::sym("Coverage label"),
          color = !!rlang::sym("Coverage label color")
        ),
        size = label_size,
        fontface = "bold",
        show.legend = FALSE,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_color_identity()
  }
  attr(plot, "llw_coverage_labels_shown") <- show_coverage_labels
  attr(plot, "llw_dashboard_plot") <- "daily_availability"
  attr(plot, "llw_facet_slots") <- facet_slots
  plot
}

dashboard_scale_notice <- function(preview, scale, symlog_threshold = 1) {
  scale <- match.arg(scale, c("symlog", "linear", "log"))
  if (!identical(preview$mode, "detailed")) return(NULL)
  if (identical(scale, "symlog")) {
    unit <- preview$focus_unit
    unit_suffix <- if (
      is.null(unit) ||
        length(unit) == 0L ||
        is.na(unit[[1L]]) ||
        !nzchar(trimws(unit[[1L]])) ||
        identical(tolower(trimws(unit[[1L]])), "unit not specified")
    ) {
      ""
    } else {
      paste0(" ", unit[[1L]])
    }
    threshold <- dashboard_axis_number(symlog_threshold)
    return(paste0(
      "Symlog axis: values from \u2212",
      threshold,
      " to +",
      threshold,
      unit_suffix,
      " use linear spacing. That central half-range and each outer \u00d710 ",
      "step use equal axis spacing. ",
      "Zero and negative readings, if present, remain visible for review."
    ))
  }
  if (identical(scale, "linear")) {
    return(paste(
      "Linear axis: equal vertical distances represent equal changes in the",
      "recorded metric; zero and negative readings remain visible."
    ))
  }
  finite_value <- is.finite(preview$data$Value)
  zero_excluded <- sum(finite_value & preview$data$Value == 0)
  negative_excluded <- sum(finite_value & preview$data$Value < 0)
  excluded <- zero_excluded + negative_excluded
  paste0(
    "Logarithmic scaling cannot display zero or negative values. ",
    format(zero_excluded, big.mark = ","),
    " zero and ",
    format(negative_excluded, big.mark = ","),
    " negative point(s) are omitted (",
    format(excluded, big.mark = ","),
    " non-positive point(s) in total) from this display only."
  )
}

dashboard_table_options <- function(data, column_visibility = TRUE) {
  contract <- dashboard_table_contract()
  column_visibility <- isTRUE(column_visibility)
  numeric_columns <- unname(
    which(vapply(
      data,
      function(value) {
        is.numeric(value) &&
          !inherits(value, c("POSIXct", "Date", "difftime"))
      },
      logical(1)
    )) -
      1L
  )
  temporal_columns <- unname(
    which(vapply(
      data,
      function(value) {
        inherits(value, c("POSIXct", "Date", "difftime"))
      },
      logical(1)
    )) -
      1L
  )
  logical_columns <- unname(
    which(vapply(data, is.logical, logical(1))) - 1L
  )
  column_defs <- list()
  if (length(numeric_columns) > 0L) {
    column_defs <- c(
      column_defs,
      list(list(
        targets = numeric_columns,
        className = "dt-body-right llw-tabular"
      ))
    )
  }
  if (length(temporal_columns) > 0L) {
    column_defs <- c(
      column_defs,
      list(list(
        targets = temporal_columns,
        className = "dt-body-nowrap llw-tabular"
      ))
    )
  }
  if (length(logical_columns) > 0L) {
    column_defs <- c(
      column_defs,
      list(list(
        targets = logical_columns,
        className = "dt-body-center"
      ))
    )
  }
  id_column <- match("Id", names(data), nomatch = 0L) - 1L
  if (id_column >= 0L) {
    column_defs <- c(
      column_defs,
      list(list(
        targets = id_column,
        width = "9rem",
        className = "dt-body-nowrap"
      ))
    )
  }
  datetime_column <- match("Datetime", names(data), nomatch = 0L) - 1L
  if (datetime_column >= 0L) {
    column_defs <- c(
      column_defs,
      list(list(
        targets = datetime_column,
        width = "13rem",
        className = "dt-body-nowrap llw-tabular"
      ))
    )
  }

  list(
    dom = if (column_visibility) "Bftip" else "ftip",
    buttons = if (column_visibility) {
      list(list(extend = "colvis", text = "Choose columns"))
    } else {
      NULL
    },
    pageLength = contract$page_length,
    processing = TRUE,
    deferRender = TRUE,
    searchDelay = 350,
    scrollX = TRUE,
    autoWidth = TRUE,
    ordering = contract$sort,
    searching = contract$search,
    paging = TRUE,
    stateSave = FALSE,
    columnDefs = column_defs,
    language = list(
      search = "Search all columns:",
      processing = "Preparing this page...",
      zeroRecords = "No rows match the current search."
    )
  )
}

dashboard_datatable <- function(
  data,
  caption,
  column_visibility = TRUE,
  display_timezone = "UTC"
) {
  if (!is.data.frame(data)) {
    abort_llw("`data` must be a data frame.", type = "validation")
  }
  assert_scalar_string(caption, "caption")
  assert_scalar_string(display_timezone, "display_timezone")
  if (!display_timezone %in% OlsonNames()) {
    abort_llw(
      "`display_timezone` must be a valid Olson time zone.",
      type = "validation"
    )
  }
  widget <- DT::datatable(
    data,
    rownames = FALSE,
    filter = "none",
    selection = "none",
    extensions = if (isTRUE(column_visibility)) "Buttons" else character(),
    escape = TRUE,
    caption = htmltools::tags$caption(
      class = "visually-hidden",
      caption
    ),
    options = dashboard_table_options(
      data,
      column_visibility = column_visibility
    ),
    class = "stripe hover row-border compact"
  )
  integer_columns <- names(data)[vapply(
    data,
    function(value) {
      is.integer(value) && !inherits(value, c("Date", "POSIXct", "difftime"))
    },
    logical(1)
  )]
  double_columns <- names(data)[vapply(
    data,
    function(value) {
      is.double(value) && !inherits(value, c("Date", "POSIXct", "difftime"))
    },
    logical(1)
  )]
  if (length(integer_columns) > 0L) {
    widget <- DT::formatRound(
      widget,
      columns = integer_columns,
      digits = 0,
      mark = ","
    )
  }
  if (length(double_columns) > 0L) {
    widget <- DT::formatSignif(
      widget,
      columns = double_columns,
      digits = 6,
      zero.print = "0"
    )
  }
  datetime_columns <- names(data)[vapply(
    data,
    function(value) inherits(value, "POSIXct"),
    logical(1)
  )]
  if (length(datetime_columns) > 0L) {
    widget <- DT::formatDate(
      widget,
      columns = datetime_columns,
      method = "toLocaleString",
      params = list(
        "en-CA",
        list(
          timeZone = display_timezone,
          year = "numeric",
          month = "2-digit",
          day = "2-digit",
          hour = "2-digit",
          minute = "2-digit",
          second = "2-digit",
          hourCycle = "h23"
        )
      )
    )
  }
  widget
}

dashboard_main_columns <- function(snapshot, focus_variable, data_stage) {
  data_stage <- match.arg(data_stage, c("preprocessed", "source"))
  data <- if (identical(data_stage, "source")) {
    snapshot$raw_data
  } else {
    snapshot$prepared_data
  }
  if (!focus_variable %in% names(data)) {
    abort_llw("The focus variable is unavailable.", type = "validation")
  }
  grouping <- snapshot$record$analysis_settings$active_grouping %||%
    snapshot$record$analysis_settings$grouping %||%
    character()
  grouping <- if (is.atomic(grouping)) as.character(grouping) else
    names(grouping)
  grouping <- intersect(grouping[nzchar(grouping)], names(data))
  if (length(grouping) == 0L) grouping <- "Id"
  unique(c(grouping, "Id", "Datetime", focus_variable))
}

dashboard_format_available <- function(value, empty = "Not available") {
  if (is.null(value) || length(value) == 0L || all(is.na(value))) return(empty)
  value <- as.character(value)
  value <- value[!is.na(value) & nzchar(value)]
  if (length(value) == 0L) empty else paste(value, collapse = ", ")
}

dashboard_format_bytes <- function(bytes) {
  if (!is.numeric(bytes) || length(bytes) != 1L || !is.finite(bytes)) {
    return("Not available")
  }
  if (bytes < 1024) return(paste0(format(bytes, big.mark = ","), " B"))
  if (bytes < 1024^2) return(sprintf("%.1f KiB", bytes / 1024))
  if (bytes < 1024^3) return(sprintf("%.1f MiB", bytes / 1024^2))
  sprintf("%.2f GiB", bytes / 1024^3)
}

dashboard_source_size <- function(record) {
  values <- suppressWarnings(as.numeric(
    record$source_manifest$details$size_bytes %||% NA_real_
  ))
  if (length(values) == 0L || !any(is.finite(values))) return(NA_real_)
  sum(values[is.finite(values)])
}

dashboard_provenance_table <- function(snapshot) {
  record <- snapshot$record
  manifest <- record$source_manifest
  device <- record$factual_metadata$device %||%
    manifest$details$device %||%
    NA_character_
  version <- manifest$details$version %||%
    manifest$details$package_version %||%
    NA_character_
  files <- manifest$original_filenames
  data.frame(
    Field = c(
      "Source type",
      "Original source files",
      "Source-file hashes",
      "Import instant",
      "Source timezone",
      "Datetime display timezone",
      "Device / importer",
      "Device or package version",
      "Available source-file size",
      "Source payload size",
      "Source checksum",
      "LightLogR version",
      "Quality provenance"
    ),
    Value = c(
      dashboard_format_available(manifest$source_type),
      dashboard_format_available(files, "No source filename was supplied"),
      if (length(manifest$hashes) == 0L) {
        "Not supplied"
      } else {
        paste(length(manifest$hashes), "SHA-256 hash(es) retained")
      },
      format(manifest$imported_at, tz = "UTC", usetz = TRUE),
      dashboard_format_available(
        manifest$source_timezone,
        "Not supplied in source provenance"
      ),
      snapshot$display_timezone,
      dashboard_format_available(device),
      dashboard_format_available(version),
      dashboard_format_bytes(dashboard_source_size(record)),
      dashboard_format_bytes(length(record$raw_payload)),
      record$raw_checksum,
      dashboard_format_available(record$provenance$LightLogR_version),
      snapshot$quality_origin
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

dashboard_state_table <- function(snapshot) {
  data.frame(
    Field = c(
      "Pre-processed-data state",
      "Recipe schema version",
      "Dataset / recipe revision",
      "Committed recipe steps",
      "Pre-processed equals source data",
      "Grouping state",
      "Grouping detail",
      "Primary measurement",
      "Primary unit",
      "Calibration evidence"
    ),
    Value = c(
      snapshot$recipe$label,
      snapshot$recipe$schema_version,
      snapshot$recipe$dataset_revision,
      snapshot$recipe$step_count,
      if (snapshot$recipe$unchanged) "Yes" else "No",
      snapshot$grouping$label,
      snapshot$grouping$detail,
      snapshot$primary_variable,
      snapshot$primary_unit,
      snapshot$calibration
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

dashboard_sampling_label <- function(quality) {
  values <- quality$participants$dominant_epoch_seconds
  values <- sort(unique(values[is.finite(values) & values > 0]))
  if (length(values) == 0L) return("Not estimable")
  paste(paste0(format(values, trim = TRUE), " s"), collapse = ", ")
}

dashboard_span_label <- function(snapshot) {
  paste0(
    format(snapshot$date_start, "%Y-%m-%d"),
    " to ",
    format(snapshot$date_end, "%Y-%m-%d"),
    " (",
    snapshot$display_timezone,
    ")"
  )
}

dashboard_compact_span_label <- function(snapshot) {
  start <- snapshot$date_start
  end <- snapshot$date_end
  if (identical(start, end)) return(format(start, "%d %b %Y"))
  if (format(start, "%Y") == format(end, "%Y")) {
    return(paste(
      format(start, "%d %b"),
      "\u2013",
      format(end, "%d %b %Y")
    ))
  }
  paste(format(start, "%d %b %Y"), "\u2013", format(end, "%d %b %Y"))
}

dashboard_coverage_display <- function(coverage) {
  result <- coverage[,
    c(
      "Id",
      "Date",
      "Focus variable",
      "Day length (h)",
      "Epoch (s)",
      "Expected epochs",
      "Observed timestamp epochs",
      "Observed focus epochs",
      "Explicit missing focus epochs",
      "Implicit gap epochs",
      "Irregular timestamps",
      "Duplicate rows",
      "Focus coverage (%)",
      "Status"
    ),
    drop = FALSE
  ]
  result
}
