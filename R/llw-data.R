# Import, validation, preparation, grouping, and annotations ----------------

llw_file_provenance <- function(files) {
  if (is.null(files) || !length(files)) return(list())
  files <- normalizePath(files, mustWork = TRUE)
  info <- file.info(files)
  list(
    files = basename(files),
    paths = files,
    sizes = unname(info$size),
    modified = format(info$mtime, tz = "UTC", usetz = TRUE),
    md5 = unname(tools::md5sum(files))
  )
}

llw_read_normalized <- function(files, timezone = "UTC") {
  if (!length(files)) llw_abort("Provide at least one normalized data file.")
  pieces <- lapply(files, function(file) {
    ext <- tolower(tools::file_ext(file))
    data <- switch(
      ext,
      rds = readRDS(file),
      csv = utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE),
      tsv = utils::read.delim(file, stringsAsFactors = FALSE, check.names = FALSE),
      txt = utils::read.delim(file, stringsAsFactors = FALSE, check.names = FALSE),
      llw_abort(paste0("Unsupported normalized file extension `", ext, "`. Use CSV, TSV, TXT, or RDS."))
    )
    if (!is.data.frame(data)) llw_abort(paste0("Normalized file `", basename(file), "` did not contain a data frame."))
    if ("Datetime" %in% names(data) && !inherits(data$Datetime, "POSIXct")) {
      data$Datetime <- as.POSIXct(data$Datetime, tz = timezone)
    }
    if (!"Id" %in% names(data)) data$Id <- tools::file_path_sans_ext(basename(file))
    data
  })
  dplyr::bind_rows(pieces)
}

#' Preview participant identifiers derived from source filenames
#'
#' @param files Source file paths or names.
#' @param mode Automatic filename stems, regular-expression extraction, or one
#'   manual identifier.
#' @param pattern Regular expression used in extraction mode. The first capture
#'   group is preferred when present.
#' @param manual_id Identifier used in manual mode.
#'
#' @return A tibble with one source filename and proposed identifier per row.
#' @export
llw_preview_ids <- function(files,
                            mode = c("auto", "extract", "manual"),
                            pattern = "^([^_]+)",
                            manual_id = "Participant") {
  mode <- match.arg(mode)
  if (!length(files)) llw_abort("Provide one or more file names.")
  filenames <- basename(files)
  stems <- tools::file_path_sans_ext(filenames)
  ids <- switch(
    mode,
    auto = stems,
    manual = rep(manual_id, length(files)),
    extract = {
      matches <- regexec(pattern, stems, perl = TRUE)
      pieces <- regmatches(stems, matches)
      vapply(seq_along(pieces), function(i) {
        value <- pieces[[i]]
        if (!length(value)) NA_character_ else if (length(value) > 1L) value[[2]] else value[[1]]
      }, character(1))
    }
  )
  if (anyNA(ids) || any(!nzchar(ids))) llw_abort("The participant-ID expression did not match every filename.")
  tibble::tibble(file = filenames, Id = ids)
}

#' Import light logger or normalized data
#'
#' @param files Character vector of files.
#' @param device A LightLogR-supported device name, or `"normalized"` for an
#'   already tabular CSV/TSV/TXT/RDS file.
#' @param data An already loaded data frame. When supplied, `files` and `device`
#'   are not used.
#' @param variable Primary numeric measurement column.
#' @param metadata Additional metadata.
#' @param name Dataset name.
#' @param timezone IANA time zone for normalized input and the default passed to
#'   LightLogR imports.
#' @param ... Additional arguments passed to `LightLogR::import_Dataset()`.
#'
#' @return An `llw_dataset`.
#' @export
llw_import <- function(files = NULL,
                       device = NULL,
                       data = NULL,
                       variable = NULL,
                       metadata = list(),
                       name = NULL,
                       timezone = "UTC",
                       ...) {
  if (!is.null(data)) {
    imported <- data
    source_type <- "data.frame"
    file_provenance <- list()
  } else {
    if (is.null(files) || !length(files)) llw_abort("Supply `data` or one or more `files`.")
    if (is.null(device) || !nzchar(device)) llw_abort("Select a device or `normalized` input.")
    file_provenance <- llw_file_provenance(files)
    if (identical(device, "normalized")) {
      imported <- llw_read_normalized(files, timezone)
      source_type <- "normalized"
    } else {
      supported <- LightLogR::supported_devices()
      if (!device %in% supported) {
        llw_abort(
          paste0("Unsupported device `", device, "`. Supported devices: ", paste(supported, collapse = ", "), "."),
          "lightlogweb_unsupported_device"
        )
      }
      args <- list(device = device, filename = files, tz = timezone, ...)
      imported <- rlang::exec(LightLogR::import_Dataset, !!!args)
      source_type <- "LightLogR"
    }
  }

  if (is.null(variable)) {
    numeric_names <- names(imported)[vapply(imported, is.numeric, logical(1))]
    numeric_names <- setdiff(numeric_names, c("Id", "Datetime"))
    variable <- numeric_names[[1]] %||% NULL
  }
  metadata <- utils::modifyList(
    list(variable = variable, timezone = timezone, device = device %||% "data.frame"),
    metadata
  )
  llw_dataset(
    imported,
    metadata = metadata,
    name = name %||% if (length(files)) tools::file_path_sans_ext(basename(files[[1]])) else "dataset",
    provenance = list(
      source_type = source_type,
      imported_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      files = file_provenance,
      arguments = list(device = device, timezone = timezone)
    )
  )
}

llw_epoch_seconds <- function(datetime) {
  values <- sort(unique(as.numeric(datetime)))
  differences <- diff(values)
  differences <- differences[is.finite(differences) & differences > 0]
  if (!length(differences)) return(NA_real_)
  as.numeric(stats::median(differences))
}

llw_daily_coverage <- function(data, variable, timezone) {
  date <- as.Date(data$Datetime, tz = timezone)
  work <- tibble::tibble(
    Id = as.character(data$Id),
    Date = date,
    Datetime = data$Datetime,
    available = !is.na(data[[variable]])
  )
  groups <- split(work, interaction(work$Id, work$Date, drop = TRUE, lex.order = TRUE))
  rows <- lapply(groups, function(group) {
    epoch <- llw_epoch_seconds(group$Datetime)
    start <- as.POSIXct(as.character(group$Date[[1]]), tz = timezone)
    end <- as.POSIXct(as.character(group$Date[[1]] + 1), tz = timezone)
    day_seconds <- as.numeric(difftime(end, start, units = "secs"))
    expected <- if (is.finite(epoch) && epoch > 0) day_seconds / epoch else nrow(group)
    observed <- sum(group$available)
    tibble::tibble(
      Id = group$Id[[1]],
      Date = group$Date[[1]],
      epoch_seconds = epoch,
      expected = as.integer(round(expected)),
      observed = observed,
      coverage = min(1, observed / expected),
      missing = max(0, 1 - observed / expected)
    )
  })
  dplyr::bind_rows(rows)
}

llw_detect_dst <- function(data) {
  groups <- split(data$Datetime, as.character(data$Id))
  dplyr::bind_rows(lapply(names(groups), function(id) {
    times <- sort(unique(groups[[id]]))
    offsets <- format(times, "%z")
    changed <- c(FALSE, offsets[-1] != offsets[-length(offsets)])
    if (!any(changed)) return(tibble::tibble())
    tibble::tibble(Id = id, Datetime = times[changed], offset = offsets[changed])
  }))
}

llw_quiet_value <- function(expr) {
  value <- NULL
  invisible(utils::capture.output(value <- suppressMessages(suppressWarnings(force(expr)))))
  value
}

llw_quality_flag_by_id <- function(data, fn) {
  groups <- split(data, as.character(data$Id))
  flags <- vapply(groups, function(group) {
    tryCatch(isTRUE(llw_quiet_value(fn(group))), error = function(e) NA)
  }, logical(1))
  if (all(is.na(flags))) NA else any(flags, na.rm = TRUE)
}

#' Assess light-logger data quality
#'
#' @param x An `llw_dataset` or compatible data frame.
#' @param variable Primary numeric variable when `x` is a data frame.
#' @param stage Either prepared or raw data.
#'
#' @return A diagnostic `llw_result`. Its value contains `overview`, `by_day`,
#'   `epochs`, and `dst_changes` tables.
#' @export
llw_quality <- function(x, variable = NULL, stage = c("prepared", "raw")) {
  stage <- match.arg(stage)
  if (!inherits(x, "llw_dataset")) x <- llw_dataset(x, metadata = list(variable = variable))
  variable <- variable %||% llw_primary_variable(x)
  data <- if (stage == "prepared") x$prepared_data else x$raw_data
  llw_validate_data(data, variable)
  timezone <- x$metadata$timezone %||% lubridate::tz(data$Datetime)

  duplicate_rows <- duplicated(data[c("Id", "Datetime")]) | duplicated(data[c("Id", "Datetime")], fromLast = TRUE)
  has_gaps <- llw_quality_flag_by_id(data, LightLogR::has_gaps)
  has_irregulars <- llw_quality_flag_by_id(data, LightLogR::has_irregulars)
  epochs <- tryCatch(
    as.data.frame(LightLogR::dominant_epoch(data)),
    error = function(e) tibble::tibble(error = conditionMessage(e))
  )
  gap_summary <- tryCatch(
    tibble::as_tibble(llw_quiet_value(rlang::inject(LightLogR::gap_table(data, Variable.colname = !!rlang::sym(variable), get.df = TRUE)))),
    error = function(e) tibble::tibble(error = conditionMessage(e))
  )
  gaps <- tryCatch(
    tibble::as_tibble(llw_quiet_value(LightLogR::gap_finder(data, gap.data = TRUE, silent = TRUE))),
    error = function(e) tibble::tibble(error = conditionMessage(e))
  )
  by_day <- llw_daily_coverage(data, variable, timezone)
  by_day$valid <- by_day$missing <= (x$metadata$daily_missing_max %||% 0.2)
  invalidated <- if (".llw_invalid_reason" %in% names(data)) sum(!is.na(data$.llw_invalid_reason)) else 0L

  overview <- tibble::tibble(
    rows = nrow(data),
    ids = dplyr::n_distinct(data$Id),
    participant_days = nrow(by_day),
    start = min(data$Datetime, na.rm = TRUE),
    end = max(data$Datetime, na.rm = TRUE),
    explicit_missing = mean(is.na(data[[variable]])),
    median_daily_coverage = stats::median(by_day$coverage, na.rm = TRUE),
    duplicate_rows = sum(duplicate_rows),
    has_gaps = has_gaps,
    has_irregulars = has_irregulars,
    invalidated_rows = invalidated
  )
  value <- list(
    overview = overview,
    by_day = by_day,
    epochs = epochs,
    gap_summary = gap_summary,
    gaps = gaps,
    duplicates = data[duplicate_rows, c("Id", "Datetime"), drop = FALSE],
    dst_changes = llw_detect_dst(data)
  )
  llw_result(
    "diagnostic",
    value,
    label = paste("Quality assessment for", x$name),
    parameters = list(stage = stage, variable = variable),
    source = "llw_quality",
    data = data
  )
}

llw_numeric_handler <- function(name) {
  switch(
    name,
    mean = function(x) mean(x, na.rm = TRUE),
    median = function(x) stats::median(x, na.rm = TRUE),
    max = function(x) max(x, na.rm = TRUE),
    llw_abort("`aggregate_function` must be mean, median, or max.")
  )
}

#' Prepare a LightLogWeb dataset
#'
#' @param x An `llw_dataset` or compatible data frame.
#' @param timezone Optional analysis timezone.
#' @param timezone_mode Preserve instants with `"preserve_instant"`, or reinterpret
#'   clock readings with `"reinterpret"`.
#' @param date_range Optional two-element Date/POSIXct range.
#' @param value_range Optional plausible numeric range.
#' @param invalidate_outside Replace out-of-range values with `NA` and retain an
#'   invalidation reason.
#' @param gap_policy Convert implicit gaps to explicit `NA` observations or leave
#'   timestamps unchanged.
#' @param interval Optional regular aggregation interval such as `"5 min"`.
#' @param aggregate_function Numeric aggregation function name.
#' @param daily_missing_max Optional maximum missing proportion per participant-day.
#'
#' @return A new `llw_dataset`; the original input is unchanged.
#' @export
llw_prepare <- function(x,
                        timezone = NULL,
                        timezone_mode = c("preserve_instant", "reinterpret"),
                        date_range = NULL,
                        value_range = NULL,
                        invalidate_outside = TRUE,
                        gap_policy = c("explicit_na", "leave"),
                        interval = NULL,
                        aggregate_function = c("mean", "median", "max"),
                        daily_missing_max = NULL) {
  if (!inherits(x, "llw_dataset")) x <- llw_dataset(x)
  timezone_mode <- match.arg(timezone_mode)
  gap_policy <- match.arg(gap_policy)
  aggregate_function <- match.arg(aggregate_function)
  variable <- llw_primary_variable(x)
  data <- x$prepared_data

  if (!is.null(timezone)) {
    if (!timezone %in% OlsonNames()) llw_abort(paste0("Unknown timezone `", timezone, "`."))
    data$Datetime <- if (timezone_mode == "preserve_instant") {
      lubridate::with_tz(data$Datetime, timezone)
    } else {
      lubridate::force_tz(data$Datetime, timezone)
    }
    x$metadata$timezone <- timezone
  }

  if (!is.null(date_range)) {
    if (length(date_range) != 2L) llw_abort("`date_range` must contain a start and end.")
    tz <- x$metadata$timezone
    start <- as.POSIXct(as.character(date_range[[1]]), tz = tz)
    end <- as.POSIXct(as.character(as.Date(date_range[[2]]) + 1), tz = tz)
    data <- data[data$Datetime >= start & data$Datetime < end, , drop = FALSE]
  }

  if (!is.null(value_range)) {
    if (length(value_range) != 2L || any(!is.finite(value_range)) || value_range[[1]] >= value_range[[2]]) {
      llw_abort("`value_range` must contain finite increasing minimum and maximum values.")
    }
    outside <- !is.na(data[[variable]]) & (data[[variable]] < value_range[[1]] | data[[variable]] > value_range[[2]])
    if (!".llw_invalid_reason" %in% names(data)) data$.llw_invalid_reason <- NA_character_
    data$.llw_invalid_reason[outside] <- paste0("outside_", value_range[[1]], "_", value_range[[2]])
    if (isTRUE(invalidate_outside)) data[[variable]][outside] <- NA_real_
  }

  if (gap_policy == "explicit_na" && nrow(data) > 1L) {
    data <- llw_quiet_value(LightLogR::gap_handler(data))
  }
  if (!is.null(interval) && nzchar(interval)) {
    data <- LightLogR::aggregate_Datetime(
      data,
      unit = interval,
      type = "floor",
      numeric.handler = llw_numeric_handler(aggregate_function)
    )
  }

  if (!is.null(daily_missing_max)) {
    if (!is.numeric(daily_missing_max) || length(daily_missing_max) != 1L || daily_missing_max < 0 || daily_missing_max > 1) {
      llw_abort("`daily_missing_max` must be between zero and one.")
    }
    coverage <- llw_daily_coverage(data, variable, x$metadata$timezone)
    keep <- coverage[coverage$missing <= daily_missing_max, c("Id", "Date"), drop = FALSE]
    data$.llw_date_join <- as.Date(data$Datetime, tz = x$metadata$timezone)
    data$.llw_id_join <- as.character(data$Id)
    key <- paste(data$.llw_id_join, data$.llw_date_join)
    keep_key <- paste(keep$Id, keep$Date)
    data <- data[key %in% keep_key, , drop = FALSE]
    data$.llw_date_join <- NULL
    data$.llw_id_join <- NULL
    x$metadata$daily_missing_max <- daily_missing_max
  }

  x$prepared_data <- tibble::as_tibble(data)
  x$preparation <- c(
    x$preparation,
    list(list(
      timezone = timezone,
      timezone_mode = timezone_mode,
      date_range = date_range,
      value_range = value_range,
      invalidate_outside = invalidate_outside,
      gap_policy = gap_policy,
      interval = interval,
      aggregate_function = aggregate_function,
      daily_missing_max = daily_missing_max
    ))
  )
  x
}

#' Add interval annotations to a dataset
#'
#' @param x An `llw_dataset`.
#' @param intervals Data frame with participant ID, start, end, and state columns.
#' @param id_col,start_col,end_col,state_col Column names in `intervals`.
#' @param output_col Name of the new annotation column.
#' @param overwrite Whether an existing output column may be replaced.
#'
#' @return An updated `llw_dataset`.
#' @export
llw_annotate <- function(x,
                         intervals,
                         id_col = "Id",
                         start_col = "start",
                         end_col = "end",
                         state_col = "State",
                         output_col = state_col,
                         overwrite = FALSE) {
  if (!inherits(x, "llw_dataset")) x <- llw_dataset(x)
  if (!is.data.frame(intervals)) llw_abort("`intervals` must be a data frame.")
  required <- c(start_col, end_col, state_col)
  missing <- setdiff(required, names(intervals))
  if (length(missing)) llw_abort(paste0("Intervals are missing: ", paste(missing, collapse = ", "), "."))
  if (output_col %in% names(x$prepared_data) && !isTRUE(overwrite)) {
    llw_abort(paste0("Column `", output_col, "` already exists; set `overwrite = TRUE`."))
  }

  data <- x$prepared_data
  timezone <- x$metadata$timezone
  starts <- intervals[[start_col]]
  ends <- intervals[[end_col]]
  if (!inherits(starts, "POSIXct")) starts <- as.POSIXct(starts, tz = timezone)
  if (!inherits(ends, "POSIXct")) ends <- as.POSIXct(ends, tz = timezone)
  values <- rep(NA_character_, nrow(data))
  data_ids <- as.character(data$Id)
  interval_ids <- if (id_col %in% names(intervals)) as.character(intervals[[id_col]]) else rep(NA_character_, nrow(intervals))

  for (i in seq_len(nrow(intervals))) {
    id_match <- is.na(interval_ids[[i]]) | data_ids == interval_ids[[i]]
    time_match <- data$Datetime >= starts[[i]] & data$Datetime < ends[[i]]
    values[id_match & time_match] <- as.character(intervals[[state_col]][[i]])
  }
  data[[output_col]] <- values
  x$prepared_data <- tibble::as_tibble(data)
  x$annotations[[output_col]] <- tibble::as_tibble(intervals)
  x
}

#' Define analysis grouping dimensions
#'
#' @param x An `llw_dataset`.
#' @param dimensions Any of participant, date, day_type, clock_window,
#'   photoperiod, or annotation.
#' @param clock_window Optional numeric start/end hours; windows may cross midnight.
#' @param coordinates Latitude and longitude for photoperiod grouping.
#' @param solar_depression Solar depression angle.
#' @param annotation Character vector of annotation columns to group by.
#'
#' @return An updated `llw_dataset` with explicit grouping columns and names.
#' @export
llw_group <- function(x,
                      dimensions = c("participant", "date"),
                      clock_window = NULL,
                      coordinates = NULL,
                      solar_depression = 6,
                      annotation = NULL) {
  if (!inherits(x, "llw_dataset")) x <- llw_dataset(x)
  allowed <- c("participant", "date", "day_type", "clock_window", "photoperiod", "annotation")
  unknown <- setdiff(dimensions, allowed)
  if (length(unknown)) llw_abort(paste0("Unknown grouping dimensions: ", paste(unknown, collapse = ", "), "."))
  data <- x$prepared_data
  timezone <- x$metadata$timezone
  groups <- character()

  if ("participant" %in% dimensions) groups <- c(groups, "Id")
  if ("date" %in% dimensions) {
    data$.llw_date <- as.Date(data$Datetime, tz = timezone)
    groups <- c(groups, ".llw_date")
  }
  if ("day_type" %in% dimensions) {
    weekday <- as.POSIXlt(data$Datetime, tz = timezone)$wday
    data$.llw_day_type <- ifelse(weekday %in% c(0, 6), "weekend", "weekday")
    groups <- c(groups, ".llw_day_type")
  }
  if ("clock_window" %in% dimensions) {
    if (length(clock_window) != 2L || any(!is.finite(clock_window)) || any(clock_window < 0 | clock_window > 24)) {
      llw_abort("`clock_window` must contain start and end hours between 0 and 24.")
    }
    hour <- as.numeric(format(data$Datetime, "%H", tz = timezone)) + as.numeric(format(data$Datetime, "%M", tz = timezone)) / 60
    inside <- if (clock_window[[1]] <= clock_window[[2]]) {
      hour >= clock_window[[1]] & hour < clock_window[[2]]
    } else {
      hour >= clock_window[[1]] | hour < clock_window[[2]]
    }
    data$.llw_clock_window <- ifelse(inside, "inside", "outside")
    groups <- c(groups, ".llw_clock_window")
  }
  if ("photoperiod" %in% dimensions) {
    coordinates <- coordinates %||% x$metadata$coordinates
    if (length(coordinates) != 2L || any(!is.finite(coordinates))) {
      llw_abort("Photoperiod grouping requires finite latitude and longitude coordinates.")
    }
    data <- LightLogR::add_photoperiod(data, coordinates, solarDep = solar_depression, overwrite = TRUE)
    groups <- c(groups, "photoperiod.state")
  }
  if ("annotation" %in% dimensions) {
    if (is.null(annotation) || !length(annotation)) llw_abort("Annotation grouping requires one or more annotation columns.")
    missing <- setdiff(annotation, names(data))
    if (length(missing)) llw_abort(paste0("Missing annotation columns: ", paste(missing, collapse = ", "), "."))
    groups <- c(groups, annotation)
  }

  x$prepared_data <- tibble::as_tibble(data)
  x$groups <- unique(groups)
  x
}

#' Merge compatible LightLogWeb datasets
#'
#' @param x,y `llw_dataset` objects.
#' @param name Name of the merged dataset.
#'
#' @return A merged `llw_dataset`.
#' @export
llw_merge <- function(x, y, name = paste(x$name, y$name, sep = " + ")) {
  if (!inherits(x, "llw_dataset") || !inherits(y, "llw_dataset")) llw_abort("Both inputs must be `llw_dataset` objects.")
  if (!identical(llw_primary_variable(x), llw_primary_variable(y))) llw_abort("Datasets must use the same primary variable.")
  if (!identical(x$metadata$timezone, y$metadata$timezone)) llw_abort("Datasets must use the same analysis timezone before merging.")
  common <- intersect(names(x$raw_data), names(y$raw_data))
  if (!all(c("Id", "Datetime", llw_primary_variable(x)) %in% common)) llw_abort("Datasets do not share the required schema.")
  data <- dplyr::bind_rows(x$raw_data[, common, drop = FALSE], y$raw_data[, common, drop = FALSE]) |>
    dplyr::arrange(.data$Id, .data$Datetime)
  llw_dataset(
    data,
    metadata = x$metadata,
    name = name,
    provenance = list(operation = "merge", sources = list(x$provenance, y$provenance)),
    annotations = c(x$annotations, y$annotations)
  )
}
