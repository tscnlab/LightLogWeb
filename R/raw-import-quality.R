primary_variable_eligibility <- function(data) {
  if (!is.data.frame(data)) {
    abort_llw("`data` must be a data frame.", type = "validation")
  }
  structural <- c("Id", "Datetime", "file.name", "is.implicit")
  rows <- lapply(names(data), function(variable) {
    value <- data[[variable]]
    missing <- sum(is.na(value))
    finite <- if (is.numeric(value)) sum(is.finite(value)) else NA_integer_
    eligible <- TRUE
    reason <- "Eligible numeric column."

    if (variable %in% structural) {
      eligible <- FALSE
      reason <- "Structural import column, not a selectable analysis variable."
    } else if (inherits(value, c("POSIXct", "POSIXlt", "Date", "difftime"))) {
      eligible <- FALSE
      reason <- "Date or time column, not a selectable numeric variable."
    } else if (!is.null(dim(value))) {
      eligible <- FALSE
      reason <- paste(
        "Matrix or array column; select one scalar numeric variable",
        "per observation instead."
      )
    } else if (is.list(value)) {
      eligible <- FALSE
      reason <- "Nested/list column; select a scalar numeric variable instead."
    } else if (!is.numeric(value)) {
      eligible <- FALSE
      reason <- paste0(
        "Non-numeric column (",
        paste(class(value), collapse = "/"),
        ")."
      )
    } else if (length(value) == 0L || finite == 0L) {
      eligible <- FALSE
      reason <- "Numeric column has no finite observations."
    } else if (any(!is.finite(value) & !is.na(value))) {
      eligible <- FALSE
      reason <- "Numeric column contains infinite values."
    } else if (missing > 0L) {
      reason <- paste0(
        "Eligible numeric column; ",
        format(missing, big.mark = ",", scientific = FALSE),
        " missing value(s) retained."
      )
    }

    data.frame(
      variable = variable,
      type = paste(class(value), collapse = "/"),
      missing_values = missing,
      finite_values = finite,
      eligible = eligible,
      reason = reason,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  structure(result, class = c("llw_variable_eligibility", "data.frame"))
}

validate_imported_light_data <- function(data, source_timezone) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    abort_llw(
      "Imported data must be a non-empty data frame.",
      type = "import",
      public_message = "The import produced no usable observations. Review the files and settings, then retry.",
      diagnostics = list(phase = "normalization")
    )
  }
  if (anyDuplicated(names(data))) {
    duplicates <- unique(names(data)[
      duplicated(names(data)) | duplicated(names(data), fromLast = TRUE)
    ])
    abort_llw(
      paste0(
        "Imported data contain duplicate column name(s): ",
        paste(duplicates, collapse = ", "),
        "."
      ),
      type = "import",
      public_message = paste0(
        "The imported table contains repeated column name(s): ",
        paste(duplicates, collapse = ", "),
        ". Export the data with unique columns and retry."
      ),
      diagnostics = list(phase = "normalization", duplicate_names = duplicates)
    )
  }
  missing_columns <- setdiff(c("Id", "Datetime"), names(data))
  if (length(missing_columns) > 0L) {
    abort_llw(
      paste0(
        "Imported data are missing column(s): ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      type = "import",
      public_message = paste0(
        "LightLogR could not produce required column(s): ",
        paste(missing_columns, collapse = ", "),
        ". Check the selected device and file format."
      ),
      diagnostics = list(
        phase = "normalization",
        missing_columns = missing_columns
      )
    )
  }
  if (!inherits(data$Datetime, "POSIXct")) {
    abort_llw(
      paste0(
        "`Datetime` has class `",
        paste(class(data$Datetime), collapse = "/"),
        "`, not POSIXct."
      ),
      type = "import",
      public_message = paste(
        "LightLogR did not produce a POSIXct `Datetime` column.",
        "Check the device, format version, source time zone, and timestamp fields."
      ),
      diagnostics = list(
        phase = "normalization",
        Datetime_class = class(data$Datetime)
      )
    )
  }
  if (anyNA(data$Datetime)) {
    abort_llw(
      paste0(
        "Imported `Datetime` contains ",
        sum(is.na(data$Datetime)),
        " missing value(s)."
      ),
      type = "import",
      public_message = paste(
        "Some timestamps could not be resolved.",
        "Correct the timestamp format or source time zone before importing."
      ),
      diagnostics = list(
        phase = "normalization",
        missing_Datetime = sum(is.na(data$Datetime))
      )
    )
  }
  ids <- as.character(data$Id)
  invalid_ids <- is.na(ids) | !nzchar(trimws(ids)) | grepl("[[:cntrl:]]", ids)
  if (any(invalid_ids)) {
    abort_llw(
      paste0(
        "Imported `Id` contains ",
        sum(invalid_ids),
        " missing or empty value(s)."
      ),
      type = "import",
      public_message = paste(
        "Some observations have no usable participant ID.",
        "Review the filename mapping or embedded Id column before importing."
      ),
      diagnostics = list(phase = "normalization", invalid_Id = sum(invalid_ids))
    )
  }
  assert_scalar_string(source_timezone, "source_timezone")
  if (!source_timezone %in% OlsonNames()) {
    abort_llw(
      paste0("Unknown IANA source time zone `", source_timezone, "`."),
      type = "validation"
    )
  }
  imported_timezone <- lubridate::tz(data$Datetime)
  if (!identical(imported_timezone, source_timezone)) {
    abort_llw(
      paste0(
        "Imported `Datetime` uses `",
        imported_timezone,
        "`; selected source time zone is `",
        source_timezone,
        "`."
      ),
      type = "import",
      public_message = paste0(
        "The imported timestamps use `",
        imported_timezone,
        "`, not the selected source time zone `",
        source_timezone,
        "`. No dataset was created; review the device and time-zone settings."
      ),
      diagnostics = list(
        phase = "normalization",
        imported_timezone = imported_timezone,
        source_timezone = source_timezone
      )
    )
  }
  invisible(data)
}

lightlogr_dominant_epoch_seconds <- function(datetime) {
  datetime <- sort(unique(datetime))
  if (length(datetime) < 2L) {
    return(NA_real_)
  }
  epoch <- tryCatch(
    LightLogR::dominant_epoch(data.frame(Datetime = datetime)),
    error = function(cnd) NULL
  )
  if (
    is.null(epoch) || nrow(epoch) == 0L || !"dominant.epoch" %in% names(epoch)
  ) {
    return(NA_real_)
  }
  as.numeric(epoch$dominant.epoch[[1L]], units = "seconds")
}

participant_import_quality_details <- function(
  data,
  participant_id,
  source_timezone
) {
  rows <- which(as.character(data$Id) == participant_id)
  participant <- data[rows, , drop = FALSE]
  datetime_original <- participant$Datetime
  datetime <- sort(unique(datetime_original))
  absolute_seconds <- as.numeric(datetime)
  epoch_seconds <- lightlogr_dominant_epoch_seconds(datetime)
  duplicate_rows <- sum(
    duplicated(datetime_original) |
      duplicated(datetime_original, fromLast = TRUE)
  )
  ordered <- !is.unsorted(as.numeric(datetime_original), strictly = FALSE)
  measurement_names <- setdiff(
    names(participant),
    c("Id", "Datetime", "file.name", "is.implicit")
  )
  explicit_missing <- if (length(measurement_names) == 0L) {
    0L
  } else {
    sum(is.na(participant[, measurement_names, drop = FALSE]))
  }

  implicit_gaps <- NA_real_
  gap_episodes <- NA_integer_
  irregular_observations <- NA_integer_
  gap_intervals <- empty_raw_import_gaps(source_timezone)
  if (is.finite(epoch_seconds) && epoch_seconds > 0 && length(datetime) > 1L) {
    offsets <- absolute_seconds - min(absolute_seconds)
    nearest_positions <- round(offsets / epoch_seconds)
    tolerance <- max(1e-6, epoch_seconds * 1e-7)
    regular <- abs(offsets - nearest_positions * epoch_seconds) <= tolerance
    regular_positions <- sort(unique(nearest_positions[regular]))
    max_expected_position <- floor(max(offsets) / epoch_seconds + tolerance)
    expected_count <- max_expected_position + 1
    implicit_gaps <- max(0, expected_count - length(regular_positions))
    internal_episodes <- if (length(regular_positions) > 1L) {
      sum(diff(regular_positions) > 1)
    } else {
      0L
    }
    trailing_episode <- as.integer(
      length(regular_positions) > 0L &&
        max(regular_positions) < max_expected_position
    )
    gap_episodes <- as.integer(internal_episodes + trailing_episode)
    irregular_observations <- sum(!regular)

    lag_seconds <- diff(absolute_seconds)
    gap_index <- which(lag_seconds > epoch_seconds + tolerance)
    if (length(gap_index) > 0L) {
      gap_intervals <- data.frame(
        Id = rep(participant_id, length(gap_index)),
        start = datetime[gap_index],
        end = datetime[gap_index + 1L],
        lag_seconds = lag_seconds[gap_index],
        dominant_epoch_seconds = rep(epoch_seconds, length(gap_index)),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }

  local_datetime <- lubridate::with_tz(datetime, tzone = source_timezone)
  dst_values <- lubridate::dst(local_datetime)
  dst_transitions <- if (length(dst_values) > 1L) {
    sum(dst_values[-1L] != dst_values[-length(dst_values)])
  } else {
    0L
  }
  local_clock_seconds <- as.numeric(
    lubridate::force_tz(local_datetime, tzone = "UTC")
  )
  local_clock_anomalies <- if (length(datetime) > 1L) {
    absolute_diff <- diff(absolute_seconds)
    local_diff <- diff(local_clock_seconds)
    sum(abs(local_diff - absolute_diff) > 1e-6)
  } else {
    0L
  }

  list(
    summary = data.frame(
      Id = participant_id,
      observations = nrow(participant),
      start = min(datetime),
      end = max(datetime),
      dominant_epoch_seconds = epoch_seconds,
      duplicate_timestamp_rows = duplicate_rows,
      ordered = ordered,
      explicit_missing_values = explicit_missing,
      implicit_gap_epochs = implicit_gaps,
      gap_episodes = gap_episodes,
      irregular_observations = irregular_observations,
      dst_transitions = dst_transitions,
      local_clock_anomalies = local_clock_anomalies,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    gaps = gap_intervals
  )
}

participant_import_quality <- function(data, participant_id, source_timezone) {
  participant_import_quality_details(
    data,
    participant_id,
    source_timezone
  )$summary
}

raw_import_signal_profile <- function(data, eligibility) {
  variables <- eligibility$variable[eligibility$eligible]
  if (length(variables) == 0L) {
    return(data.frame(
      variable = character(),
      missing = integer(),
      exact_zero = integer(),
      negative = integer(),
      minimum = numeric(),
      q25 = numeric(),
      median = numeric(),
      mean = numeric(),
      q75 = numeric(),
      maximum = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(variables, function(variable) {
    value <- data[[variable]]
    finite <- value[is.finite(value)]
    quartiles <- stats::quantile(
      finite,
      probs = c(0.25, 0.5, 0.75),
      names = FALSE,
      type = 7
    )
    data.frame(
      variable = variable,
      missing = sum(is.na(value)),
      exact_zero = sum(value == 0, na.rm = TRUE),
      negative = sum(value < 0, na.rm = TRUE),
      minimum = min(finite),
      q25 = quartiles[[1L]],
      median = quartiles[[2L]],
      mean = mean(finite),
      q75 = quartiles[[3L]],
      maximum = max(finite),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

raw_import_variable_review <- function(eligibility, signal_profile) {
  eligibility_columns <- c(
    "variable",
    "type",
    "missing_values",
    "eligible",
    "reason"
  )
  profile_columns <- c(
    "variable",
    "exact_zero",
    "negative",
    "minimum",
    "q25",
    "median",
    "mean",
    "q75",
    "maximum"
  )
  if (
    !is.data.frame(eligibility) ||
      !all(eligibility_columns %in% names(eligibility))
  ) {
    abort_llw(
      "`eligibility` does not contain the required variable-review columns.",
      type = "validation"
    )
  }
  if (
    !is.data.frame(signal_profile) ||
      !all(profile_columns %in% names(signal_profile))
  ) {
    abort_llw(
      "`signal_profile` does not contain the required numeric summaries.",
      type = "validation"
    )
  }

  profile_index <- match(eligibility$variable, signal_profile$variable)
  review <- data.frame(
    variable = eligibility$variable,
    type = eligibility$type,
    eligible = eligibility$eligible,
    missing = eligibility$missing_values,
    exact_zero = signal_profile$exact_zero[profile_index],
    negative = signal_profile$negative[profile_index],
    minimum = signal_profile$minimum[profile_index],
    q25 = signal_profile$q25[profile_index],
    median = signal_profile$median[profile_index],
    mean = signal_profile$mean[profile_index],
    q75 = signal_profile$q75[profile_index],
    maximum = signal_profile$maximum[profile_index],
    reason = ifelse(eligibility$eligible, "", eligibility$reason),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  structure(review, class = c("llw_variable_review", "data.frame"))
}

empty_raw_import_gaps <- function(source_timezone) {
  data.frame(
    Id = character(),
    start = as.POSIXct(character(), tz = source_timezone),
    end = as.POSIXct(character(), tz = source_timezone),
    lag_seconds = numeric(),
    dominant_epoch_seconds = numeric(),
    stringsAsFactors = FALSE
  )
}

raw_import_diagnostic_row <- function(check, status, value, detail) {
  data.frame(
    check = check,
    status = status,
    value = as.character(value),
    detail = detail,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

summarize_raw_import_quality <- function(data, source_timezone) {
  validate_imported_light_data(data, source_timezone)
  ids <- unique(as.character(data$Id))
  participant_details <- lapply(ids, function(id) {
    participant_import_quality_details(data, id, source_timezone)
  })
  participants <- do.call(
    rbind,
    lapply(participant_details, function(details) details$summary)
  )
  rownames(participants) <- NULL
  participant_gaps <- lapply(
    participant_details,
    function(details) details$gaps
  )
  has_gap_intervals <- vapply(participant_gaps, nrow, integer(1)) > 0L
  gaps <- if (any(has_gap_intervals)) {
    do.call(rbind, participant_gaps[has_gap_intervals])
  } else {
    empty_raw_import_gaps(source_timezone)
  }
  rownames(gaps) <- NULL
  eligibility <- primary_variable_eligibility(data)
  signal_profile <- raw_import_signal_profile(data, eligibility)

  duplicate_rows <- sum(participants$duplicate_timestamp_rows)
  unsorted_ids <- sum(!participants$ordered)
  explicit_missing <- sum(participants$explicit_missing_values)
  implicit_gaps <- sum(participants$implicit_gap_epochs, na.rm = TRUE)
  unknown_gap_ids <- sum(is.na(participants$implicit_gap_epochs))
  irregular <- sum(participants$irregular_observations, na.rm = TRUE)
  dst_transitions <- sum(participants$dst_transitions)
  local_clock_anomalies <- sum(participants$local_clock_anomalies)
  epochs <- sort(unique(participants$dominant_epoch_seconds[
    is.finite(participants$dominant_epoch_seconds)
  ]))
  differing_epochs <- length(epochs) > 1L
  start <- min(data$Datetime)
  end <- max(data$Datetime)

  diagnostics <- do.call(
    rbind,
    list(
      raw_import_diagnostic_row(
        "Required identity",
        "pass",
        paste(length(ids), "participant(s)"),
        "Every observation has a non-missing `Id`."
      ),
      raw_import_diagnostic_row(
        "Timestamp contract",
        "pass",
        "POSIXct",
        "Every observation has a valid absolute timestamp."
      ),
      raw_import_diagnostic_row(
        "Names and types",
        "pass",
        paste(ncol(data), "column(s)"),
        "Column names are unique; types are retained from LightLogR."
      ),
      raw_import_diagnostic_row(
        "Ordering",
        if (unsorted_ids > 0L) "warning" else "pass",
        paste(unsorted_ids, "unsorted participant(s)"),
        if (unsorted_ids > 0L) {
          "Rows are not chronological within every participant."
        } else {
          "Rows are chronological within each participant."
        }
      ),
      raw_import_diagnostic_row(
        "Duplicate Id/Datetime",
        if (duplicate_rows > 0L) "warning" else "pass",
        paste(duplicate_rows, "affected row(s)"),
        "Duplicate timestamps are diagnosed separately from identical rows."
      ),
      raw_import_diagnostic_row(
        "Source time zone",
        "pass",
        source_timezone,
        "The imported POSIXct display zone matches the selected source zone."
      ),
      raw_import_diagnostic_row(
        "Date range",
        "information",
        paste(
          format(start, tz = source_timezone, usetz = TRUE),
          "to",
          format(end, tz = source_timezone, usetz = TRUE)
        ),
        "The range is shown in the selected source time zone."
      ),
      raw_import_diagnostic_row(
        "Dominant epoch",
        if (differing_epochs || unknown_gap_ids > 0L) "warning" else "pass",
        if (length(epochs) == 0L) {
          "Unavailable"
        } else {
          paste(paste0(format(epochs, trim = TRUE), " s"), collapse = ", ")
        },
        paste(
          if (differing_epochs)
            "Dominant epochs differ between participants." else
            "Dominant epochs are consistent where estimable.",
          if (unknown_gap_ids > 0L)
            paste(
              unknown_gap_ids,
              "participant(s) have fewer than two unique timestamps."
            ) else ""
        )
      ),
      raw_import_diagnostic_row(
        "Explicit missing values",
        if (explicit_missing > 0L) "warning" else "pass",
        paste(explicit_missing, "value(s)"),
        "Missing sensor values remain missing; they are not replaced with zero."
      ),
      raw_import_diagnostic_row(
        "Implicit gaps",
        if (implicit_gaps > 0L) "warning" else "pass",
        paste(format(implicit_gaps, scientific = FALSE), "missing epoch(s)"),
        "Expected timestamps are counted without expanding or imputing the time series."
      ),
      raw_import_diagnostic_row(
        "Irregular observations",
        if (irregular > 0L) "warning" else "pass",
        paste(irregular, "off-grid observation(s)"),
        "Off-grid observations remain distinct from implicit gaps."
      ),
      raw_import_diagnostic_row(
        "DST transitions",
        if (dst_transitions > 0L || local_clock_anomalies > 0L) "warning" else
          "pass",
        paste(dst_transitions, "transition(s)"),
        paste(
          local_clock_anomalies,
          "local-clock discontinuity/discontinuities were detected; absolute instants were not reinterpreted."
        )
      )
    )
  )
  rownames(diagnostics) <- NULL

  warnings <- character()
  if (unsorted_ids > 0L) {
    warnings <- c(
      warnings,
      "Rows are not chronological within every participant."
    )
  }
  if (duplicate_rows > 0L) {
    warnings <- c(
      warnings,
      "Duplicate Id/Datetime pairs are present and were not removed."
    )
  }
  if (explicit_missing > 0L) {
    warnings <- c(
      warnings,
      "Explicit missing sensor values are present and remain missing."
    )
  }
  if (implicit_gaps > 0L) {
    warnings <- c(
      warnings,
      "Implicit gaps are present; no timestamps or exposure values were imputed."
    )
  }
  if (irregular > 0L) {
    warnings <- c(
      warnings,
      "Off-grid observations are present and remain distinct from gaps."
    )
  }
  if (differing_epochs) {
    warnings <- c(
      warnings,
      "Dominant sampling epochs differ between participants."
    )
  }
  if (unknown_gap_ids > 0L) {
    warnings <- c(
      warnings,
      "Some participants have too few unique timestamps to estimate an epoch."
    )
  }
  if (dst_transitions > 0L || local_clock_anomalies > 0L) {
    warnings <- c(
      warnings,
      paste(
        "The recording crosses a daylight-saving transition; verify whether",
        "the device already adjusted its clock before applying any DST correction."
      )
    )
  }

  structure(
    list(
      summary = list(
        rows = nrow(data),
        columns = ncol(data),
        participants = length(ids),
        start = start,
        end = end,
        source_timezone = source_timezone,
        duplicate_timestamp_rows = duplicate_rows,
        explicit_missing_values = explicit_missing,
        implicit_gap_epochs = implicit_gaps,
        irregular_observations = irregular,
        dst_transitions = dst_transitions,
        eligible_variables = sum(eligibility$eligible)
      ),
      participants = participants,
      gaps = gaps,
      diagnostics = diagnostics,
      eligibility = eligibility,
      signal_profile = signal_profile,
      warnings = unique(warnings)
    ),
    class = c("llw_raw_import_quality", "list")
  )
}

raw_import_preview_indices <- function(rows, max_rows = 100L) {
  if (
    !is.numeric(rows) ||
      length(rows) != 1L ||
      is.na(rows) ||
      rows < 0 ||
      rows != floor(rows)
  ) {
    abort_llw(
      "`rows` must be one non-negative whole number.",
      type = "validation"
    )
  }
  if (
    !is.numeric(max_rows) ||
      length(max_rows) != 1L ||
      is.na(max_rows) ||
      max_rows < 2 ||
      max_rows != floor(max_rows)
  ) {
    abort_llw(
      "`max_rows` must be a whole number of at least 2.",
      type = "validation"
    )
  }
  rows <- as.integer(rows)
  max_rows <- as.integer(max_rows)
  if (rows <= max_rows) {
    return(list(
      indices = seq_len(rows),
      reduced = FALSE,
      notice = paste("All", rows, "row(s) are shown.")
    ))
  }
  top <- floor(max_rows / 2)
  bottom <- max_rows - top
  list(
    indices = c(seq_len(top), seq.int(rows - bottom + 1L, rows)),
    reduced = TRUE,
    notice = paste0(
      "Preview shows the first ",
      top,
      " and last ",
      bottom,
      " of ",
      format(rows, big.mark = ",", scientific = FALSE),
      " rows. Quality diagnostics use the complete imported dataset."
    )
  )
}

raw_import_preview_data <- function(data, indices) {
  if (!is.data.frame(data)) {
    abort_llw("`data` must be a data frame.", type = "validation")
  }
  if (
    !is.numeric(indices) ||
      anyNA(indices) ||
      any(!is.finite(indices)) ||
      any(indices != floor(indices)) ||
      any(indices < 1L) ||
      any(indices > nrow(data))
  ) {
    abort_llw(
      "`indices` must contain valid data-row positions.",
      type = "validation"
    )
  }
  preview <- data[as.integer(indices), , drop = FALSE]
  dplyr::ungroup(preview)
}

plot_raw_import_overview <- function(
  data,
  quality,
  mode = c("light", "dark"),
  max_ids = 80L
) {
  mode <- match.arg(mode)
  if (!is.data.frame(data) || !all(c("Id", "Datetime") %in% names(data))) {
    abort_llw(
      "`data` must contain imported Id and Datetime columns.",
      type = "validation"
    )
  }
  if (!inherits(quality, "llw_raw_import_quality")) {
    abort_llw(
      "`quality` must be a raw-import quality summary.",
      type = "validation"
    )
  }
  participant_ids <- sort(unique(as.character(quality$participants$Id)))
  reduced <- length(participant_ids) > max_ids
  if (reduced) {
    participant_ids <- participant_ids[seq_len(max_ids)]
  }
  display_data <- data[
    as.character(data$Id) %in% participant_ids,
    ,
    drop = FALSE
  ]
  display_data$Id <- factor(
    as.character(display_data$Id),
    levels = rev(participant_ids)
  )
  display_data <- display_data |>
    dplyr::ungroup() |>
    dplyr::group_by(Id)
  gaps <- quality$gaps
  gaps <- gaps[
    as.character(gaps$Id) %in% participant_ids,
    ,
    drop = FALSE
  ]
  gaps$Id <- factor(as.character(gaps$Id), levels = rev(participant_ids))
  coverage <- if (reduced) {
    paste0(
      "<br>Showing first ",
      max_ids,
      " of ",
      quality$summary$participants,
      " participant IDs."
    )
  } else {
    ""
  }
  colors <- lightlogweb_plot_colors(mode)
  caption <- paste0(
    paste0(
      "<span style='color:",
      colors[["start"]],
      ";font-weight:700'>Start</span> \u00b7 <span style='color:",
      colors[["end"]],
      ";font-weight:700'>End</span> \u00b7 <span style='color:",
      colors[["gap"]],
      ";font-weight:700'>Gap interval</span><br>"
    ),
    "Times shown in <b>",
    quality$summary$source_timezone,
    "</b>.<br>",
    "Gap intervals mark consecutive observations whose lag exceeds that ",
    "participant's dominant epoch.",
    coverage
  )
  plot <- suppressMessages(
    LightLogR::gg_overview(display_data, gap.data = gaps)
  )
  geom_classes <- vapply(
    plot$layers,
    function(layer) class(layer$geom)[[1L]],
    character(1)
  )
  interval_layers <- which(geom_classes == "GeomLinerange")
  point_layers <- which(geom_classes == "GeomPoint")
  if (length(interval_layers) >= 1L) {
    plot$layers[[interval_layers[[1L]]]]$aes_params$colour <-
      colors[["primary"]]
    plot$layers[[interval_layers[[1L]]]]$aes_params$linewidth <- 2.2
    plot$layers[[interval_layers[[1L]]]]$geom_params$lineend <- "round"
  }
  if (length(interval_layers) >= 2L) {
    plot$layers[[interval_layers[[2L]]]]$aes_params$colour <- colors[["gap"]]
    plot$layers[[interval_layers[[2L]]]]$aes_params$linewidth <- 3.8
    plot$layers[[interval_layers[[2L]]]]$aes_params$linetype <- "22"
    plot$layers[[interval_layers[[2L]]]]$geom_params$lineend <- "butt"
  }
  if (length(point_layers) >= 1L) {
    plot$layers[[point_layers[[1L]]]]$aes_params$colour <- colors[["start"]]
    plot$layers[[point_layers[[1L]]]]$aes_params$size <- 2.5
  }
  if (length(point_layers) >= 2L) {
    plot$layers[[point_layers[[2L]]]]$aes_params$colour <- colors[["end"]]
    plot$layers[[point_layers[[2L]]]]$aes_params$size <- 2.5
  }
  plot <- plot +
    ggplot2::labs(
      x = "Recorded time span",
      y = "Participant ID",
      caption = caption
    ) +
    lightlogweb_plot_theme(mode) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12),
      plot.caption = ggtext::element_markdown(size = 10.5, hjust = 0),
      plot.margin = ggplot2::margin(10, 14, 16, 10, "pt")
    )
  attr(plot, "llw_overview_source") <- "LightLogR::gg_overview"
  plot
}
