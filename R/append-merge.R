append_provenance_columns <- function() {
  c(
    "llw_source_dataset_id",
    "llw_source_dataset_name",
    "llw_source_row",
    "llw_source_timezone",
    "llw_source_datetime_timezone",
    "llw_local_datetime",
    "llw_source_device",
    "llw_duplicate_key",
    "llw_overlap_key"
  )
}

append_user_column_names <- function(data) {
  if (!is.data.frame(data)) {
    abort_llw("`data` must be a data frame.", type = "validation")
  }
  setdiff(names(data), append_provenance_columns())
}

append_format_datetime_for_display <- function(datetime) {
  if (!inherits(datetime, "POSIXct")) {
    abort_llw("`datetime` must be POSIXct.", type = "validation")
  }
  timezone <- lubridate::tz(datetime)
  if (
    !is.character(timezone) ||
      length(timezone) != 1L ||
      is.na(timezone) ||
      !nzchar(timezone) ||
      !timezone %in% OlsonNames()
  ) {
    abort_llw(
      "Displayed datetimes require a valid recorded IANA time zone.",
      type = "validation"
    )
  }
  displayed <- format(
    datetime,
    tz = timezone,
    format = "%Y-%m-%dT%H:%M:%S%z"
  )
  available <- !is.na(datetime)
  displayed[available] <- paste0(
    displayed[available],
    " [",
    timezone,
    "]"
  )
  displayed[!available] <- NA_character_
  displayed
}

append_display_data <- function(data) {
  displayed <- data[append_user_column_names(data)]
  datetime_columns <- names(displayed)[vapply(
    displayed,
    inherits,
    logical(1),
    "POSIXct"
  )]
  for (column in datetime_columns) {
    displayed[[column]] <- append_format_datetime_for_display(
      displayed[[column]]
    )
  }
  displayed
}

append_error_step_from_message <- function(message, default = "mapping") {
  valid_steps <- c("sources", "mapping", "time", "review")
  if (!default %in% valid_steps) {
    abort_llw("Unknown append error step.", type = "validation")
  }
  message <- paste(message, collapse = " ")
  if (
    grepl(
      paste(
        c(
          "at least two source",
          "source dataset.*unavailable",
          "mapped ID",
          "ID mapping",
          "ID columns",
          "prefix",
          "overlap policy",
          "share ID/timestamp keys across sources"
        ),
        collapse = "|"
      ),
      message,
      ignore.case = TRUE
    )
  ) {
    return("sources")
  }
  if (
    grepl(
      paste(
        c(
          "output time zone",
          "output-time",
          "time alignment",
          "source time zone",
          "source timezone",
          "shared time zone",
          "daylight-saving",
          "absolute instants"
        ),
        collapse = "|"
      ),
      message,
      ignore.case = TRUE
    )
  ) {
    return("time")
  }
  if (
    grepl(
      "dataset name.*already|display name.*already",
      message,
      ignore.case = TRUE
    )
  ) {
    return("review")
  }
  default
}

append_preferred_error_step <- function(steps, errors = character()) {
  valid_steps <- c("sources", "mapping", "time", "review")
  steps <- steps[steps %in% valid_steps]
  if (length(steps) == 0L && length(errors) > 0L) {
    steps <- vapply(errors, append_error_step_from_message, character(1))
  }
  available <- valid_steps[valid_steps %in% steps]
  if (length(available) == 0L) NULL else available[[1L]]
}

dataset_record_device <- function(record) {
  record <- validate_dataset_record(record)
  candidates <- list(
    record$factual_metadata$device,
    record$source_manifest$details$device
  )
  for (candidate in candidates) {
    if (is.character(candidate) && length(candidate) > 0L) {
      candidate <- unique(candidate[!is.na(candidate) & nzchar(candidate)])
      if (length(candidate) > 0L) return(paste(candidate, collapse = ", "))
    }
  }
  NA_character_
}

dataset_record_variable_metadata <- function(record, variable) {
  record <- validate_dataset_record(record)
  assert_scalar_string(variable, "variable")
  metadata <- record$factual_metadata$variables[[variable]] %||% list()
  manifest_unit <- record$source_manifest$details$units[[variable]] %||%
    NA_character_
  label <- metadata$label %||% NA_character_
  unit <- metadata$unit %||% manifest_unit
  calibration <- metadata$calibration %||%
    record$source_manifest$details$calibration[[variable]] %||%
    NA_character_
  list(
    label = if (is.character(label) && length(label) == 1L) label else
      NA_character_,
    unit = if (is.character(unit) && length(unit) == 1L) unit else
      NA_character_,
    calibration = if (is.character(calibration) && length(calibration) == 1L)
      calibration else NA_character_
  )
}

dataset_record_warning_messages <- function(record) {
  record <- validate_dataset_record(record)
  quality <- record$provenance$raw_import_quality
  warnings <- if (inherits(quality, "llw_raw_import_quality")) {
    quality$warnings
  } else {
    character()
  }
  append_warnings <- record$provenance$append_merge$warnings %||% character()
  unique(c(warnings, append_warnings))
}

dominant_interval_seconds <- function(data, id_column, datetime_column) {
  if (
    !is.data.frame(data) ||
      !all(c(id_column, datetime_column) %in% names(data)) ||
      !inherits(data[[datetime_column]], "POSIXct")
  ) {
    return(NA_real_)
  }
  ids <- as.character(data[[id_column]])
  instants <- as.numeric(data[[datetime_column]])
  ordering <- order(ids, instants, na.last = TRUE)
  ids <- ids[ordering]
  instants <- instants[ordering]
  intervals <- diff(instants)
  same_id <- ids[-1L] == ids[-length(ids)]
  intervals <- intervals[same_id & is.finite(intervals) & intervals > 0]
  if (length(intervals) == 0L) return(NA_real_)
  rounded <- round(intervals, digits = 6L)
  counts <- table(rounded)
  as.numeric(names(counts)[which.max(counts)])
}

dataset_record_inventory <- function(record) {
  record <- validate_dataset_record(record)
  data <- dataset_raw_data(record)
  id_column <- if ("Id" %in% names(data)) "Id" else NA_character_
  datetime_column <- if ("Datetime" %in% names(data)) {
    "Datetime"
  } else {
    NA_character_
  }
  participants <- if (!is.na(id_column)) {
    ids <- as.character(data[[id_column]])
    length(unique(ids[!is.na(ids) & nzchar(ids)]))
  } else {
    NA_integer_
  }
  valid_datetimes <- if (
    !is.na(datetime_column) && inherits(data[[datetime_column]], "POSIXct")
  ) {
    data[[datetime_column]][!is.na(data[[datetime_column]])]
  } else {
    as.POSIXct(character(), tz = "UTC")
  }
  span_start <- if (length(valid_datetimes) > 0L) {
    as.POSIXct(
      min(as.numeric(valid_datetimes)),
      origin = "1970-01-01",
      tz = "UTC"
    )
  } else {
    as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
  }
  span_end <- if (length(valid_datetimes) > 0L) {
    as.POSIXct(
      max(as.numeric(valid_datetimes)),
      origin = "1970-01-01",
      tz = "UTC"
    )
  } else {
    as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
  }
  primary <- record$analysis_settings$primary_variable %||% NA_character_
  primary_metadata <- if (
    is.character(primary) && length(primary) == 1L && !is.na(primary)
  ) {
    dataset_record_variable_metadata(record, primary)
  } else {
    list(
      label = NA_character_,
      unit = NA_character_,
      calibration = NA_character_
    )
  }
  source_bytes <- record$source_manifest$details$size_bytes %||% NA_real_
  source_bytes <- suppressWarnings(as.numeric(source_bytes))
  source_bytes <- if (
    length(source_bytes) > 0L && any(is.finite(source_bytes))
  ) {
    sum(source_bytes[is.finite(source_bytes)])
  } else {
    NA_real_
  }
  warnings <- dataset_record_warning_messages(record)

  structure(
    list(
      id = record$id,
      display_name = record$display_name,
      source_type = record$source_manifest$source_type,
      source_files = record$source_manifest$original_filenames,
      source_size_bytes = source_bytes,
      canonical_size_bytes = length(record$raw_payload),
      rows = nrow(data),
      columns = ncol(data),
      participants = participants,
      span_start_utc = span_start,
      span_end_utc = span_end,
      source_timezone = record$source_manifest$source_timezone,
      datetime_timezone = if (!is.na(datetime_column)) {
        lubridate::tz(data[[datetime_column]])
      } else {
        NA_character_
      },
      sampling_seconds = if (!is.na(id_column) && !is.na(datetime_column)) {
        dominant_interval_seconds(data, id_column, datetime_column)
      } else {
        NA_real_
      },
      device = dataset_record_device(record),
      primary_variable = primary,
      primary_label = primary_metadata$label,
      primary_unit = primary_metadata$unit,
      calibration = primary_metadata$calibration,
      recipe_revision = record$revision,
      recipe_steps = length(record$recipe$steps),
      warning_count = length(warnings),
      warnings = warnings
    ),
    class = c("llw_dataset_inventory", "list")
  )
}

duplicate_dataset_record <- function(record, display_name) {
  record <- validate_dataset_record(record)
  display_name <- clean_dataset_display_name(display_name)
  source_reference <- list(
    dataset_id = record$id,
    display_name = record$display_name,
    revision = record$revision,
    raw_checksum = record$raw_checksum,
    duplicated_at = as.POSIXct(Sys.time(), tz = "UTC")
  )
  record$id <- new_stable_id("dataset")
  record$display_name <- display_name
  record$revision <- 0L
  record["draft"] <- list(NULL)
  record$history <- list()
  record$provenance$duplicated_from <- source_reference
  validate_dataset_record(record)
}

new_append_source_mapping <- function(
  dataset_id,
  participant_column,
  datetime_column,
  measurement_column,
  measurement_target,
  participant_prefix = NULL,
  optional_columns = character(),
  optional_targets = optional_columns,
  optional_coercions = rep("none", length(optional_columns))
) {
  arguments <- list(
    dataset_id = dataset_id,
    participant_column = participant_column,
    datetime_column = datetime_column,
    measurement_column = measurement_column,
    measurement_target = measurement_target
  )
  for (argument in names(arguments)) {
    assert_scalar_string(arguments[[argument]], argument)
  }
  if (!is.null(participant_prefix)) {
    assert_scalar_string(participant_prefix, "participant_prefix")
  }
  assert_character_vector(optional_columns, "optional_columns")
  assert_character_vector(optional_targets, "optional_targets")
  assert_character_vector(optional_coercions, "optional_coercions")
  if (
    length(optional_columns) != length(optional_targets) ||
      length(optional_columns) != length(optional_coercions)
  ) {
    abort_llw(
      "Optional columns, targets, and coercions must have equal lengths.",
      type = "validation"
    )
  }
  if (anyDuplicated(optional_columns) || anyDuplicated(optional_targets)) {
    abort_llw(
      "Optional source columns and output targets must be unique per source.",
      type = "validation"
    )
  }
  if (
    any(
      optional_columns %in%
        c(
          participant_column,
          datetime_column,
          measurement_column
        )
    )
  ) {
    abort_llw(
      "Optional columns cannot repeat an ID, datetime, or primary measurement mapping.",
      type = "validation"
    )
  }
  if (any(!optional_coercions %in% c("none", "character"))) {
    abort_llw(
      "Optional-column coercions must be `none` or `character`.",
      type = "validation"
    )
  }
  mapped_source_columns <- unique(c(
    participant_column,
    datetime_column,
    measurement_column,
    optional_columns
  ))
  mapped_internal_columns <- intersect(
    mapped_source_columns,
    append_provenance_columns()
  )
  if (length(mapped_internal_columns) > 0L) {
    abort_llw(
      paste0(
        "Internal append-provenance columns cannot be mapped: ",
        paste(mapped_internal_columns, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  structural <- c("Id", "Datetime", "Source", append_provenance_columns())
  if (measurement_target %in% structural) {
    abort_llw(
      "The measurement output name conflicts with a structural column.",
      type = "validation"
    )
  }
  if (any(optional_targets %in% c(structural, measurement_target))) {
    abort_llw(
      "An optional output name conflicts with a structural or measurement column.",
      type = "validation"
    )
  }
  mapping <- structure(
    list(
      dataset_id = dataset_id,
      participant_column = participant_column,
      datetime_column = datetime_column,
      measurement_column = measurement_column,
      measurement_target = measurement_target,
      participant_prefix = participant_prefix,
      optional_columns = optional_columns,
      optional_targets = optional_targets,
      optional_coercions = optional_coercions
    ),
    class = c("llw_append_source_mapping", "list")
  )
  assert_serializable_value(mapping, "append source mapping")
  mapping
}

validate_append_source_mapping <- function(mapping) {
  if (!inherits(mapping, "llw_append_source_mapping") || !is.list(mapping)) {
    abort_llw(
      "Append mappings must be created by `new_append_source_mapping()`.",
      type = "validation"
    )
  }
  do.call(new_append_source_mapping, unclass(mapping))
}

new_append_spec <- function(
  mappings,
  display_name,
  participant_policy,
  measurement_strategy,
  duplicate_policy = "keep_marked",
  overlap_policy = NULL,
  time_alignment = "preserve_clock",
  output_timezone = "UTC",
  output_measurement = NULL,
  output_unit = NA_character_,
  confirm_quantity = FALSE,
  acknowledge_unit_difference = FALSE,
  acknowledge_unknown_units = FALSE,
  acknowledge_device_difference = FALSE
) {
  if (!is.list(mappings) || length(mappings) < 2L) {
    abort_llw(
      "Choose at least two dataset mappings to append.",
      type = "validation"
    )
  }
  mappings <- lapply(mappings, validate_append_source_mapping)
  dataset_ids <- vapply(mappings, `[[`, character(1), "dataset_id")
  if (anyDuplicated(dataset_ids)) {
    abort_llw(
      "Each append source dataset must be selected once.",
      type = "validation"
    )
  }
  assert_scalar_string(display_name, "display_name")
  participant_policy <- match.arg(
    participant_policy,
    c("prefix_source", "preserve")
  )
  measurement_strategy <- match.arg(
    measurement_strategy,
    c("separate", "combine", "identical")
  )
  time_alignment <- match.arg(
    time_alignment,
    c("preserve_clock", "preserve_instant", "keep_source")
  )
  assert_scalar_string(output_timezone, "output_timezone")
  if (!output_timezone %in% OlsonNames()) {
    abort_llw(
      paste0("Unknown IANA output time zone `", output_timezone, "`."),
      type = "validation"
    )
  }
  duplicate_policy <- match.arg(
    duplicate_policy,
    "keep_marked"
  )
  if (identical(participant_policy, "prefix_source")) {
    overlap_policy <- "not_applicable"
  } else {
    overlap_policy <- overlap_policy %||% "error"
    overlap_policy <- match.arg(overlap_policy, c("error", "keep_marked"))
  }
  for (flag in c(
    "confirm_quantity",
    "acknowledge_unit_difference",
    "acknowledge_unknown_units",
    "acknowledge_device_difference"
  )) {
    assert_flag(get(flag), flag)
  }
  if (identical(participant_policy, "prefix_source")) {
    prefixes <- vapply(
      mappings,
      function(mapping) {
        mapping$participant_prefix %||% ""
      },
      character(1)
    )
    if (
      any(!nzchar(prefixes)) ||
        anyDuplicated(prefixes) ||
        any(!grepl("^[A-Za-z0-9-]+$", prefixes))
    ) {
      abort_llw(
        paste(
          "Source prefixes must be unique and contain only letters, numbers,",
          "or hyphens. The separating underscore is added automatically."
        ),
        type = "validation"
      )
    }
  }
  measurement_targets <- vapply(
    mappings,
    `[[`,
    character(1),
    "measurement_target"
  )
  optional_targets <- unlist(
    lapply(mappings, `[[`, "optional_targets"),
    use.names = FALSE
  )
  target_collisions <- intersect(measurement_targets, optional_targets)
  if (length(target_collisions) > 0L) {
    abort_llw(
      paste0(
        "Optional outputs cannot share primary measurement target(s): ",
        paste(target_collisions, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  if (measurement_strategy %in% c("combine", "identical")) {
    assert_scalar_string(output_measurement, "output_measurement")
    if (is.na(output_unit)) {
      abort_llw(
        paste(
          "A combined measurement needs an explicit output unit.",
          "Enter the reviewed unit or the literal label `Unknown`."
        ),
        type = "validation"
      )
    }
    if (!all(measurement_targets == output_measurement)) {
      abort_llw(
        "Every primary measurement must map to the confirmed combined output column.",
        type = "validation"
      )
    }
    if (identical(measurement_strategy, "identical")) {
      measurement_columns <- vapply(
        mappings,
        `[[`,
        character(1),
        "measurement_column"
      )
      if (!all(measurement_columns == output_measurement)) {
        abort_llw(
          paste(
            "An identical-column merge must retain one shared measurement",
            "column name without renaming it."
          ),
          type = "validation"
        )
      }
    }
  } else {
    if (anyDuplicated(measurement_targets)) {
      abort_llw(
        "Separate primary measurements must use distinct output column names.",
        type = "validation"
      )
    }
    output_measurement <- NULL
  }
  if (
    !is.character(output_unit) ||
      length(output_unit) != 1L ||
      (!is.na(output_unit) && !nzchar(trimws(output_unit)))
  ) {
    abort_llw(
      "`output_unit` must be one non-empty string or `NA`.",
      type = "validation"
    )
  }
  spec <- structure(
    list(
      mappings = mappings,
      display_name = display_name,
      participant_policy = participant_policy,
      measurement_strategy = measurement_strategy,
      duplicate_policy = duplicate_policy,
      overlap_policy = overlap_policy,
      time_alignment = time_alignment,
      output_timezone = output_timezone,
      output_measurement = output_measurement,
      output_unit = output_unit,
      confirm_quantity = confirm_quantity,
      acknowledge_unit_difference = acknowledge_unit_difference,
      acknowledge_unknown_units = acknowledge_unknown_units,
      acknowledge_device_difference = acknowledge_device_difference
    ),
    class = c("llw_append_spec", "list")
  )
  assert_serializable_value(spec, "append specification")
  spec
}

validate_append_spec <- function(spec) {
  if (!inherits(spec, "llw_append_spec") || !is.list(spec)) {
    abort_llw(
      "Append specifications must be created by `new_append_spec()`.",
      type = "validation"
    )
  }
  do.call(new_append_spec, unclass(spec))
}

append_mapping_record <- function(records, mapping) {
  if (!is.list(records) || is.null(names(records))) {
    abort_llw(
      "`records` must be a named list of dataset records.",
      type = "validation"
    )
  }
  record <- records[[mapping$dataset_id]]
  if (is.null(record)) {
    abort_llw(
      paste0(
        "Append source dataset `",
        mapping$dataset_id,
        "` is unavailable."
      ),
      type = "validation"
    )
  }
  validate_dataset_record(record)
}

append_column_family <- function(value) {
  if (inherits(value, "POSIXct")) return("datetime")
  if (inherits(value, "Date")) return("date")
  if (inherits(value, "difftime")) return("duration")
  if (is.factor(value)) return("factor")
  if (is.character(value)) return("character")
  if (is.integer(value)) return("integer")
  if (is.numeric(value)) return("double")
  if (is.logical(value)) return("logical")
  if (is.atomic(value) && is.null(dim(value))) return(class(value)[[1L]])
  "unsupported"
}

append_datetime_zone <- function(record, data, datetime_column) {
  manifest_zone <- record$source_manifest$source_timezone
  recorded_zone <- lubridate::tz(data[[datetime_column]])
  list(
    source = manifest_zone,
    recorded = recorded_zone,
    local = if (!is.na(manifest_zone) && nzchar(manifest_zone)) {
      manifest_zone
    } else {
      recorded_zone
    },
    source_known = !is.na(manifest_zone) && nzchar(manifest_zone)
  )
}

append_source_profile <- function(records, mapping) {
  mapping <- validate_append_source_mapping(mapping)
  record <- append_mapping_record(records, mapping)
  data <- dataset_raw_data(record)
  required <- c(
    mapping$participant_column,
    mapping$datetime_column,
    mapping$measurement_column,
    mapping$optional_columns
  )
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    abort_llw(
      paste0(
        "Dataset `",
        record$display_name,
        "` is missing mapped column(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  if (!inherits(data[[mapping$datetime_column]], "POSIXct")) {
    abort_llw(
      paste0(
        "Mapped datetime column `",
        mapping$datetime_column,
        "` must be POSIXct."
      ),
      type = "validation"
    )
  }
  if (anyNA(data[[mapping$datetime_column]])) {
    abort_llw(
      "Mapped datetime columns cannot contain missing instants.",
      type = "validation"
    )
  }
  participant <- data[[mapping$participant_column]]
  participant_text <- as.character(participant)
  if (
    !is.atomic(participant) ||
      !is.null(dim(participant)) ||
      anyNA(participant_text) ||
      any(!nzchar(participant_text))
  ) {
    abort_llw(
      "Mapped ID columns must contain non-missing scalar labels.",
      type = "validation"
    )
  }
  measurement <- data[[mapping$measurement_column]]
  if (!is.numeric(measurement) || !is.null(dim(measurement))) {
    abort_llw(
      paste0(
        "Mapped primary measurement `",
        mapping$measurement_column,
        "` must be a scalar numeric column."
      ),
      type = "validation"
    )
  }
  metadata <- dataset_record_variable_metadata(
    record,
    mapping$measurement_column
  )
  zones <- append_datetime_zone(record, data, mapping$datetime_column)
  list(
    dataset_id = record$id,
    display_name = record$display_name,
    revision = record$revision,
    raw_checksum = record$raw_checksum,
    rows = nrow(data),
    participants = length(unique(participant_text)),
    participant_column = mapping$participant_column,
    participant_type = append_column_family(participant),
    datetime_column = mapping$datetime_column,
    datetime_type = append_column_family(data[[mapping$datetime_column]]),
    source_timezone = zones$source,
    datetime_timezone = zones$recorded,
    local_timezone = zones$local,
    source_timezone_known = zones$source_known,
    sampling_seconds = dominant_interval_seconds(
      data,
      mapping$participant_column,
      mapping$datetime_column
    ),
    measurement_column = mapping$measurement_column,
    measurement_target = mapping$measurement_target,
    measurement_type = append_column_family(measurement),
    measurement_label = metadata$label,
    measurement_unit = metadata$unit,
    calibration = metadata$calibration,
    device = dataset_record_device(record),
    optional_columns = mapping$optional_columns,
    source_manifest = record$source_manifest
  )
}

normalize_append_unit <- function(unit) {
  if (!is.character(unit) || length(unit) != 1L || is.na(unit))
    return(NA_character_)
  normalized <- tolower(trimws(gsub("\\s+", " ", unit)))
  if (normalized %in% c("unknown", "n/a", "na")) return(NA_character_)
  aliases <- c(lx = "lux", lux = "lux")
  if (normalized %in% names(aliases)) aliases[[normalized]] else normalized
}

append_unit_label <- function(unit) {
  if (
    !is.character(unit) ||
      length(unit) != 1L ||
      is.na(unit) ||
      !nzchar(trimws(unit))
  ) {
    "Unknown"
  } else {
    trimws(unit)
  }
}

append_unit_list <- function(units) {
  labels <- unique(vapply(units, append_unit_label, character(1)))
  paste0("`", labels, "`", collapse = ", ")
}

append_literal_unit <- function(unit) {
  if (
    !is.character(unit) ||
      length(unit) != 1L ||
      is.na(unit) ||
      !nzchar(trimws(unit))
  ) {
    return(NA_character_)
  }
  literal <- tolower(trimws(gsub("\\s+", " ", unit)))
  if (literal %in% c("unknown", "n/a", "na")) NA_character_ else literal
}

append_adjust_datetime <- function(
  datetime,
  output_timezone,
  time_alignment
) {
  if (!inherits(datetime, "POSIXct")) {
    abort_llw(
      "Only POSIXct columns can be time-zone adjusted.",
      type = "validation"
    )
  }
  assert_scalar_string(output_timezone, "output_timezone")
  if (!output_timezone %in% OlsonNames()) {
    abort_llw(
      paste0("Unknown IANA output time zone `", output_timezone, "`."),
      type = "validation"
    )
  }
  time_alignment <- match.arg(
    time_alignment,
    c("preserve_clock", "preserve_instant", "keep_source")
  )
  adjusted <- switch(
    time_alignment,
    preserve_clock = lubridate::force_tz(
      datetime,
      tzone = output_timezone,
      roll_dst = c("NA", "NA")
    ),
    preserve_instant = lubridate::with_tz(datetime, tzone = output_timezone),
    keep_source = datetime
  )
  if (
    identical(time_alignment, "keep_source") &&
      !identical(lubridate::tz(adjusted), output_timezone)
  ) {
    abort_llw(
      "Keeping source time unchanged requires one shared source time zone.",
      type = "validation"
    )
  }
  newly_missing <- is.na(adjusted) & !is.na(datetime)
  if (any(newly_missing)) {
    abort_llw(
      paste(
        "A clock time is ambiguous or does not exist in the selected output",
        "time zone because of a daylight-saving transition. Choose to preserve",
        "absolute instants or review the affected source timestamps."
      ),
      type = "validation"
    )
  }
  adjusted
}

append_time_alignment_label <- function(time_alignment) {
  switch(
    time_alignment,
    preserve_clock = "Clock time preserved; instant reinterpreted",
    preserve_instant = "Absolute instant preserved; clock time converted",
    keep_source = "Shared source timezone retained unchanged",
    abort_llw("Unknown append time alignment.", type = "validation")
  )
}

append_time_preservation_label <- function(
  time_alignment,
  current_timezone,
  output_timezone
) {
  alignment_label <- append_time_alignment_label(time_alignment)
  same_timezone <-
    length(current_timezone) == 1L &&
    !is.na(current_timezone) &&
    nzchar(current_timezone) &&
    length(output_timezone) == 1L &&
    !is.na(output_timezone) &&
    nzchar(output_timezone) &&
    identical(current_timezone, output_timezone)
  if (same_timezone) {
    return("Clock time preserved; instant preserved")
  }
  alignment_label
}

append_time_plan <- function(profiles, spec) {
  current_timezones <- vapply(
    profiles,
    `[[`,
    character(1),
    "datetime_timezone"
  )
  output_timezones <- rep(spec$output_timezone, length(profiles))
  data.frame(
    dataset = vapply(profiles, `[[`, character(1), "display_name"),
    datetime_column = vapply(
      profiles,
      `[[`,
      character(1),
      "datetime_column"
    ),
    current_timezone = current_timezones,
    output_timezone = output_timezones,
    preservation = mapply(
      append_time_preservation_label,
      time_alignment = rep(spec$time_alignment, length(profiles)),
      current_timezone = current_timezones,
      output_timezone = output_timezones,
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

append_time_issues <- function(profiles, spec) {
  zones <- vapply(profiles, `[[`, character(1), "datetime_timezone")
  errors <- character()
  warnings <- character()
  invalid <- is.na(zones) | !nzchar(zones) | !zones %in% OlsonNames()
  if (any(invalid)) {
    errors <- c(
      errors,
      paste(
        "Every mapped POSIXct datetime needs a valid recorded IANA time zone",
        "before an output-time rule can be applied."
      )
    )
  }
  if (identical(spec$time_alignment, "keep_source")) {
    shared <- unique(zones[!invalid])
    if (
      any(invalid) ||
        length(shared) != 1L ||
        !identical(shared[[1L]], spec$output_timezone)
    ) {
      errors <- c(
        errors,
        paste(
          "Keeping source times unchanged is available only when every mapped",
          "datetime uses one shared time zone."
        )
      )
    }
  }
  if (
    identical(spec$time_alignment, "preserve_clock") &&
      any(zones != spec$output_timezone, na.rm = TRUE)
  ) {
    warnings <- c(
      warnings,
      paste0(
        "Clock times are preserved in `",
        spec$output_timezone,
        "`; their represented absolute instants can change. Original source ",
        "times and zones remain in append provenance."
      )
    )
  }
  list(errors = unique(errors), warnings = unique(warnings))
}

append_compatibility_issues <- function(profiles, spec) {
  errors <- character()
  warnings <- character()
  if (identical(spec$measurement_strategy, "separate")) {
    return(list(errors = errors, warnings = warnings))
  }
  units <- vapply(profiles, `[[`, character(1), "measurement_unit")
  if (identical(spec$measurement_strategy, "identical")) {
    columns <- vapply(profiles, `[[`, character(1), "measurement_column")
    literal_units <- vapply(units, append_literal_unit, character(1))
    output_literal <- append_literal_unit(spec$output_unit)
    identical_mapping <-
      length(unique(columns)) == 1L &&
      identical(columns[[1L]], spec$output_measurement) &&
      !anyNA(literal_units) &&
      length(unique(literal_units)) == 1L &&
      !is.na(output_literal) &&
      identical(literal_units[[1L]], output_literal)
    if (!identical_mapping) {
      errors <- c(
        errors,
        paste(
          "Automatic identical-column merging requires the same numeric column",
          "name and the same known recorded unit in every source."
        )
      )
    }
  } else {
    normalized_units <- vapply(units, normalize_append_unit, character(1))
    known_units <- unique(normalized_units[!is.na(normalized_units)])
    unknown_units <- any(is.na(normalized_units))
    output_unit <- normalize_append_unit(spec$output_unit)
    source_unit_conflict <- length(known_units) > 1L
    output_unit_conflict <- length(known_units) == 1L &&
      !is.na(output_unit) &&
      !identical(output_unit, known_units[[1L]])
    unit_conflict <- source_unit_conflict || output_unit_conflict
    unit_summary <- paste0(
      "Recorded source units: ",
      append_unit_list(units),
      ". Output unit: ",
      append_unit_list(spec$output_unit),
      "."
    )
    if (unit_conflict && !isTRUE(spec$acknowledge_unit_difference)) {
      errors <- c(
        errors,
        paste(
          unit_summary,
          "Confirm that these labels use the same numeric scale before combining; no automatic unit conversion is performed."
        )
      )
    }
    if (unit_conflict && isTRUE(spec$acknowledge_unit_difference)) {
      warnings <- c(
        warnings,
        paste(
          unit_summary,
          "Their same numeric scale was explicitly confirmed; values are combined without conversion."
        )
      )
    }
    source_literals <- unique(vapply(units, append_literal_unit, character(1)))
    source_literals <- source_literals[!is.na(source_literals)]
    output_literal <- append_literal_unit(spec$output_unit)
    alias_labels <- if (is.na(output_unit) || is.na(output_literal)) {
      character()
    } else {
      source_literals[
        vapply(source_literals, normalize_append_unit, character(1)) ==
          output_unit &
          source_literals != output_literal
      ]
    }
    if (!unit_conflict && length(alias_labels) > 0L) {
      warnings <- c(
        warnings,
        paste(
          paste0(
            "Recorded source unit ",
            if (length(alias_labels) == 1L) "label " else "labels ",
            append_unit_list(alias_labels),
            if (length(alias_labels) == 1L) " is" else " are",
            " treated as ",
            if (length(alias_labels) == 1L) "an alias" else "aliases",
            " of output unit ",
            append_unit_list(spec$output_unit),
            ";"
          ),
          "values are not converted."
        )
      )
    }
    if (unknown_units && !isTRUE(spec$acknowledge_unknown_units)) {
      errors <- c(
        errors,
        "At least one primary measurement has unknown units. Acknowledge that uncertainty before combining values."
      )
    }
    if (unknown_units && isTRUE(spec$acknowledge_unknown_units)) {
      warnings <- c(
        warnings,
        "Unknown source units were explicitly accepted; the append does not establish measurement equivalence."
      )
    }
  }
  if (!isTRUE(spec$confirm_quantity)) {
    errors <- c(
      errors,
      "Confirm that every mapped primary column represents the intended compatible quantity before combining it."
    )
  }
  devices <- vapply(profiles, `[[`, character(1), "device")
  known_devices <- unique(devices[!is.na(devices)])
  device_risk <- length(known_devices) > 1L || any(is.na(devices))
  if (device_risk && !isTRUE(spec$acknowledge_device_difference)) {
    errors <- c(
      errors,
      "Multiple or unknown devices feed the combined measurement. Acknowledge that equal labels or units do not establish device equivalence."
    )
  }
  if (device_risk && isTRUE(spec$acknowledge_device_difference)) {
    warnings <- c(
      warnings,
      "Multiple or unknown devices feed one measurement column; source device remains attached to every observation."
    )
  }
  list(errors = unique(errors), warnings = unique(warnings))
}

append_optional_comparison <- function(records, spec) {
  rows <- list()
  for (mapping in spec$mappings) {
    record <- append_mapping_record(records, mapping)
    data <- dataset_raw_data(record)
    if (length(mapping$optional_columns) == 0L) next
    for (index in seq_along(mapping$optional_columns)) {
      source <- mapping$optional_columns[[index]]
      target <- mapping$optional_targets[[index]]
      rows[[length(rows) + 1L]] <- data.frame(
        dataset_id = record$id,
        dataset = record$display_name,
        source_column = source,
        target_column = target,
        source_type = append_column_family(data[[source]]),
        coercion = mapping$optional_coercions[[index]],
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) {
    return(data.frame(
      dataset_id = character(),
      dataset = character(),
      source_column = character(),
      target_column = character(),
      source_type = character(),
      coercion = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

append_optional_issues <- function(comparison) {
  errors <- character()
  warnings <- character()
  if (nrow(comparison) == 0L) {
    return(list(errors = errors, warnings = warnings))
  }
  for (target in unique(comparison$target_column)) {
    rows <- comparison[comparison$target_column == target, , drop = FALSE]
    families <- unique(rows$source_type)
    compatible_numeric <- all(families %in% c("integer", "double"))
    if (length(families) <= 1L || compatible_numeric) next
    if (all(rows$coercion == "character")) {
      warnings <- c(
        warnings,
        paste0(
          "Optional column `",
          target,
          "` combines differing types as text: ",
          paste(families, collapse = ", "),
          "."
        )
      )
    } else {
      errors <- c(
        errors,
        paste0(
          "Optional column `",
          target,
          "` has incompatible types (",
          paste(families, collapse = ", "),
          "). Map to separate targets or explicitly convert every source to text."
        )
      )
    }
  }
  list(errors = unique(errors), warnings = unique(warnings))
}

format_append_local_datetime <- function(datetime, timezone) {
  if (!is.character(timezone) || length(timezone) != 1L || is.na(timezone)) {
    return(rep(NA_character_, length(datetime)))
  }
  format(datetime, tz = timezone, format = "%Y-%m-%dT%H:%M:%S%z")
}

append_prepare_source <- function(records, mapping, spec, profile, comparison) {
  record <- append_mapping_record(records, mapping)
  data <- dataset_raw_data(record)
  participant <- as.character(data[[mapping$participant_column]])
  if (identical(spec$participant_policy, "prefix_source")) {
    participant <- paste(mapping$participant_prefix, participant, sep = "_")
  }
  source_datetime <- data[[mapping$datetime_column]]
  output_datetime <- append_adjust_datetime(
    source_datetime,
    output_timezone = spec$output_timezone,
    time_alignment = spec$time_alignment
  )
  output <- data.frame(
    Id = participant,
    Datetime = output_datetime,
    llw_source_dataset_id = record$id,
    llw_source_dataset_name = record$display_name,
    llw_source_row = seq_len(nrow(data)),
    llw_source_timezone = if (profile$source_timezone_known) {
      profile$source_timezone
    } else {
      NA_character_
    },
    llw_source_datetime_timezone = profile$datetime_timezone,
    llw_local_datetime = format_append_local_datetime(
      source_datetime,
      profile$local_timezone
    ),
    llw_source_device = profile$device,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (identical(spec$participant_policy, "preserve")) {
    output$Source <- record$display_name
  }
  output[[mapping$measurement_target]] <- data[[mapping$measurement_column]]
  if (length(mapping$optional_columns) > 0L) {
    for (index in seq_along(mapping$optional_columns)) {
      source <- mapping$optional_columns[[index]]
      target <- mapping$optional_targets[[index]]
      value <- data[[source]]
      if (inherits(value, "POSIXct")) {
        value <- append_adjust_datetime(
          value,
          output_timezone = spec$output_timezone,
          time_alignment = spec$time_alignment
        )
      }
      target_rows <- comparison[
        comparison$target_column == target,
        ,
        drop = FALSE
      ]
      if (nrow(target_rows) > 0L && all(target_rows$coercion == "character")) {
        value <- as.character(value)
      }
      output[[target]] <- value
    }
  }
  key <- paste(
    output$Id,
    sprintf("%.9f", as.numeric(output$Datetime)),
    sep = "\r"
  )
  output$llw_duplicate_key <- duplicated(key) | duplicated(key, fromLast = TRUE)
  output
}

append_overlap_flags <- function(data) {
  key <- paste(data$Id, sprintf("%.9f", as.numeric(data$Datetime)), sep = "\r")
  source_count <- tapply(
    data$llw_source_dataset_id,
    key,
    function(value) length(unique(value))
  )
  overlap_keys <- names(source_count)[source_count > 1L]
  key %in% overlap_keys
}

append_source_fingerprint <- function(records, spec) {
  references <- lapply(spec$mappings, function(mapping) {
    record <- append_mapping_record(records, mapping)
    list(
      id = record$id,
      revision = record$revision,
      raw_checksum = record$raw_checksum
    )
  })
  paste0(
    "sha256:",
    digest::digest(
      list(references = references, spec = spec),
      algo = "sha256",
      serialize = TRUE
    )
  )
}

preview_append_merge <- function(records, spec, max_preview_rows = 100L) {
  spec <- validate_append_spec(spec)
  if (
    !is.numeric(max_preview_rows) ||
      length(max_preview_rows) != 1L ||
      is.na(max_preview_rows) ||
      max_preview_rows < 1 ||
      max_preview_rows != floor(max_preview_rows)
  ) {
    abort_llw(
      "`max_preview_rows` must be one positive whole number.",
      type = "validation"
    )
  }
  profiles <- lapply(
    spec$mappings,
    function(mapping) append_source_profile(records, mapping)
  )
  compatibility <- append_compatibility_issues(profiles, spec)
  time_issues <- append_time_issues(profiles, spec)
  time_plan <- append_time_plan(profiles, spec)
  optional_comparison <- append_optional_comparison(records, spec)
  optional_issues <- append_optional_issues(optional_comparison)
  existing_names <- vapply(records, `[[`, character(1), "display_name")
  name_conflict <- dataset_display_name_conflict(
    spec$display_name,
    existing_names
  )
  name_errors <- if (is.null(name_conflict)) {
    character()
  } else {
    dataset_name_conflict_message(spec$display_name, name_conflict)
  }
  errors <- unique(c(
    compatibility$errors,
    time_issues$errors,
    optional_issues$errors,
    name_errors
  ))
  error_steps <- c(
    rep("mapping", length(compatibility$errors)),
    rep("time", length(time_issues$errors)),
    rep("mapping", length(optional_issues$errors)),
    rep("review", length(name_errors))
  )
  warnings <- unique(c(
    compatibility$warnings,
    time_issues$warnings,
    optional_issues$warnings
  ))
  unknown_timezones <- vapply(
    profiles,
    function(profile) !isTRUE(profile$source_timezone_known),
    logical(1)
  )
  if (any(unknown_timezones)) {
    warnings <- c(
      warnings,
      paste(
        "At least one source timezone is unknown in provenance. The recorded",
        "POSIXct zone and original clock representation remain explicit."
      )
    )
  }
  payload <- NULL
  preview_data <- data.frame()
  prepared <- NULL
  diagnostics <- list(
    source_datasets = length(profiles),
    source_rows = sum(vapply(profiles, `[[`, numeric(1), "rows")),
    result_rows = NA_integer_,
    duplicate_rows = NA_integer_,
    overlap_rows = NA_integer_
  )
  if (
    length(optional_issues$errors) == 0L &&
      length(time_issues$errors) == 0L
  ) {
    prepared <- tryCatch(
      lapply(seq_along(spec$mappings), function(index) {
        append_prepare_source(
          records,
          spec$mappings[[index]],
          spec,
          profiles[[index]],
          optional_comparison
        )
      }),
      error = identity
    )
    if (inherits(prepared, "error")) {
      prepared_error <- llw_public_message(prepared)
      errors <- c(errors, prepared_error)
      error_steps <- c(
        error_steps,
        append_error_step_from_message(prepared_error)
      )
      prepared <- NULL
    }
  }
  if (!is.null(prepared)) {
    merged <- tryCatch(dplyr::bind_rows(prepared), error = identity)
    if (inherits(merged, "error")) {
      errors <- c(
        errors,
        paste0(
          "The mapped union could not be constructed: ",
          conditionMessage(merged)
        )
      )
      error_steps <- c(error_steps, "mapping")
    } else {
      merged$llw_overlap_key <- append_overlap_flags(merged)
      ordering <- order(
        merged$Id,
        as.numeric(merged$Datetime),
        merged$llw_source_dataset_id,
        merged$llw_source_row
      )
      merged <- merged[ordering, , drop = FALSE]
      rownames(merged) <- NULL
      duplicate_rows <- sum(merged$llw_duplicate_key)
      overlap_rows <- sum(merged$llw_overlap_key)
      if (duplicate_rows > 0L) {
        message <- paste(
          format(duplicate_rows, big.mark = ","),
          "row(s) already share ID/timestamp keys within a source."
        )
        warnings <- c(
          warnings,
          paste(message, "Append retains every row and marks it for review.")
        )
      }
      if (overlap_rows > 0L) {
        message <- paste(
          format(overlap_rows, big.mark = ","),
          "row(s) share ID/timestamp keys across sources."
        )
        if (identical(spec$overlap_policy, "error")) {
          errors <- c(
            errors,
            paste(
              message,
              "Change ID mapping or choose a reviewed keep policy."
            )
          )
          error_steps <- c(error_steps, "sources")
        } else {
          warnings <- c(
            warnings,
            paste(message, "All rows are retained and marked.")
          )
        }
      }
      payload <- serialize_dataset_data(merged, "append preview result")
      preview_indices <- raw_import_preview_indices(
        nrow(merged),
        max_rows = as.integer(max_preview_rows)
      )$indices
      preview_data <- merged[preview_indices, , drop = FALSE]
      diagnostics$result_rows <- nrow(merged)
      diagnostics$duplicate_rows <- duplicate_rows
      diagnostics$overlap_rows <- overlap_rows
    }
  }
  profile_table <- do.call(
    rbind,
    lapply(profiles, function(profile) {
      data.frame(
        dataset_id = profile$dataset_id,
        dataset = profile$display_name,
        rows = profile$rows,
        participants = profile$participants,
        participant = profile$participant_column,
        participant_type = profile$participant_type,
        datetime = profile$datetime_column,
        datetime_type = profile$datetime_type,
        source_timezone = profile$source_timezone,
        datetime_timezone = profile$datetime_timezone,
        output_timezone = spec$output_timezone,
        time_alignment = spec$time_alignment,
        sampling_seconds = profile$sampling_seconds,
        measurement = profile$measurement_column,
        measurement_target = profile$measurement_target,
        measurement_type = profile$measurement_type,
        unit = profile$measurement_unit,
        device = profile$device,
        calibration = profile$calibration,
        stringsAsFactors = FALSE
      )
    })
  )
  source_provenance <- lapply(seq_along(profiles), function(index) {
    profile <- profiles[[index]]
    list(
      dataset_id = profile$dataset_id,
      display_name = profile$display_name,
      revision = profile$revision,
      raw_checksum = profile$raw_checksum,
      source_manifest = profile$source_manifest,
      mapping = spec$mappings[[index]]
    )
  })
  preview <- structure(
    list(
      spec = spec,
      fingerprint = append_source_fingerprint(records, spec),
      can_apply = length(errors) == 0L && !is.null(payload),
      errors = unique(errors),
      error_steps = unique(error_steps),
      error_step = append_preferred_error_step(error_steps, errors),
      warnings = unique(warnings),
      profiles = profile_table,
      optional_comparison = optional_comparison,
      time_plan = time_plan,
      diagnostics = diagnostics,
      preview_data = preview_data,
      merged_payload = payload,
      source_provenance = source_provenance
    ),
    class = c("llw_append_preview", "list")
  )
  assert_serializable_value(preview, "append preview")
  preview
}

append_manifest_file_pairs <- function(source_provenance) {
  filenames <- character()
  hashes <- character()
  complete_hashes <- TRUE
  for (source in source_provenance) {
    manifest <- source$source_manifest
    source_names <- unname(manifest$original_filenames)
    source_hashes <- unname(manifest$hashes)
    filenames <- c(filenames, source_names)
    if (length(source_names) > 0L) {
      if (length(source_hashes) == length(source_names)) {
        hashes <- c(hashes, source_hashes)
      } else {
        complete_hashes <- FALSE
      }
    }
  }
  if (!complete_hashes) hashes <- character()
  list(filenames = filenames, hashes = hashes)
}

mark_append_provenance_ineligible <- function(quality) {
  if (!inherits(quality, "llw_raw_import_quality")) {
    abort_llw("Append quality provenance is invalid.", type = "validation")
  }
  index <- quality$eligibility$variable %in% append_provenance_columns()
  quality$eligibility$eligible[index] <- FALSE
  quality$eligibility$reason[index] <- paste(
    "Structural append-provenance column, not a selectable analysis variable."
  )
  quality
}

new_appended_dataset_record <- function(preview) {
  if (!inherits(preview, "llw_append_preview") || !is.list(preview)) {
    abort_llw(
      "`preview` must be created by `preview_append_merge()`.",
      type = "validation"
    )
  }
  if (!isTRUE(preview$can_apply) || is.null(preview$merged_payload)) {
    abort_llw(
      "The append preview contains unresolved safety checks and cannot be applied.",
      type = "validation",
      public_message = "Resolve every append preview error before creating a merged dataset."
    )
  }
  merged <- tryCatch(unserialize(preview$merged_payload), error = identity)
  if (inherits(merged, "error") || !is.data.frame(merged)) {
    abort_llw("The append preview payload is corrupt.", type = "validation")
  }
  spec <- preview$spec
  if (
    !inherits(merged$Datetime, "POSIXct") ||
      !identical(lubridate::tz(merged$Datetime), spec$output_timezone)
  ) {
    abort_llw(
      paste0(
        "Merged datetimes must use the selected output time zone `",
        spec$output_timezone,
        "`."
      ),
      type = "validation"
    )
  }
  file_pairs <- append_manifest_file_pairs(preview$source_provenance)
  manifest <- new_source_manifest(
    source_type = "append_merge",
    original_filenames = file_pairs$filenames,
    hashes = file_pairs$hashes,
    import_arguments = list(
      operation = "row_wise_append",
      mappings = spec$mappings,
      participant_policy = spec$participant_policy,
      measurement_strategy = spec$measurement_strategy,
      duplicate_policy = spec$duplicate_policy,
      overlap_policy = spec$overlap_policy,
      time_alignment = spec$time_alignment,
      output_timezone = spec$output_timezone,
      confirmations = list(
        compatible_quantity = spec$confirm_quantity,
        differing_units_same_scale = spec$acknowledge_unit_difference,
        unknown_units = spec$acknowledge_unknown_units,
        device_difference = spec$acknowledge_device_difference
      )
    ),
    source_timezone = spec$output_timezone,
    details = list(
      canonical_datetime_timezone = spec$output_timezone,
      time_alignment = spec$time_alignment,
      local_time_column = "llw_local_datetime",
      source_timezone_column = "llw_source_timezone",
      source_datetime_timezone_column = "llw_source_datetime_timezone",
      source_records = preview$source_provenance,
      source_group_column = if (
        identical(
          spec$participant_policy,
          "preserve"
        )
      )
        "Source" else NULL,
      output_measurement = spec$output_measurement,
      output_unit = spec$output_unit
    )
  )
  devices <- unique(preview$profiles$device[!is.na(preview$profiles$device)])
  variable_metadata <- list()
  if (spec$measurement_strategy %in% c("combine", "identical")) {
    variable_metadata[[spec$output_measurement]] <- list(
      label = spec$output_measurement,
      unit = spec$output_unit,
      calibration = "Source-specific; review append provenance"
    )
  } else {
    for (index in seq_len(nrow(preview$profiles))) {
      target <- preview$profiles$measurement_target[[index]]
      variable_metadata[[target]] <- list(
        label = preview$profiles$measurement[[index]],
        unit = preview$profiles$unit[[index]],
        calibration = preview$profiles$calibration[[index]],
        source_dataset_id = preview$profiles$dataset_id[[index]]
      )
    }
  }
  quality <- summarize_raw_import_quality(merged, spec$output_timezone)
  quality <- mark_append_provenance_ineligible(quality)
  new_dataset_record(
    raw_data = merged,
    display_name = spec$display_name,
    source_manifest = manifest,
    factual_metadata = list(
      device = if (length(devices) > 0L) devices else NA_character_,
      variables = variable_metadata
    ),
    analysis_settings = list(
      primary_variable = spec$output_measurement,
      analysis_timezone = spec$output_timezone,
      source_local_time_column = "llw_local_datetime",
      source_timezone_column = "llw_source_timezone"
    ),
    provenance = list(
      LightLogR_version = installed_package_version("LightLogR"),
      raw_import_quality = quality,
      primary_variable_eligibility = quality$eligibility,
      append_merge = list(
        fingerprint = preview$fingerprint,
        source_records = preview$source_provenance,
        profiles = preview$profiles,
        optional_comparison = preview$optional_comparison,
        diagnostics = preview$diagnostics,
        warnings = preview$warnings,
        spec = spec
      )
    )
  )
}
