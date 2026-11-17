# Each mapping is a short, explicit set of public R calls. This is
# data for future script export, not executable text or an expression language.
preparation_timezones <- local({
  zones <- NULL
  function() {
    if (is.null(zones)) zones <<- OlsonNames()
    zones
  }
})

preparation_valid_timezone <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value) &&
    value %in% preparation_timezones()
}

preparation_call_map <- function() {
  base <- data.frame(
    type = c(
      "participants",
      "dates",
      "datetimes",
      "clock",
      "duration",
      "variable",
      "photoperiod",
      "coverage",
      "invalidate",
      "calibrate",
      "transform",
      "variability"
    ),
    label = c(
      "Participants and sources",
      "Calendar dates",
      "Date and time",
      "Filter by clock time",
      "Length from start or end",
      "Variable condition",
      "Day / night",
      "Coverage",
      "Mask measurement by condition",
      "Calibrate measurement",
      "Transform measurement",
      "Variability"
    ),
    version = c(1L, 2L, 2L, 2L, 1L, 1L, 1L, 3L, 3L, 2L, 3L, 3L),
    public_call = c(
      "dplyr::semi_join",
      "LightLogR::filter_Date",
      "LightLogR::filter_Datetime",
      "dplyr::filter; lubridate::with_tz/hour/minute/second",
      "dplyr::group_by; LightLogR::filter_Datetime; lubridate::days/dseconds",
      "base::[; base::is.na; typed comparisons",
      "LightLogR::add_photoperiod; base::[",
      "LightLogR::remove_partial_data",
      "base::[<-; base::is.na; base::%in%; inclusive numeric comparisons",
      "base::`*`; base::`+`; base::[[<-",
      "base::log; base::`^`; base::sign; base::abs; LightLogR::log_zero_inflated/exp_zero_inflated; base::[[<-",
      "LightLogR::dominant_epoch; rolling sample SD; base::[[<-"
    ),
    boundary = c(
      "Exact retained identities",
      "Both dates included",
      "Start included; end excluded",
      "Start included; end excluded; start-date anchor",
      "From first: upper boundary excluded; from last: lower boundary included",
      "Inclusive numeric interval or exact categories; explicit NA policy",
      "Existing state or explicit coordinates and solar depression; explicit NA policy",
      "Preserve current groups; optionally split by local calendar day; explicit gap handling",
      "Independent numeric interval or categorical source condition; explicit missing-source policy; rows and existing target missingness retained",
      "new = old * multiplier + offset; missing preserved; nonfinite results rejected; units unchanged",
      "Typed log/root/power/exponential/absolute value; explicit invalid-result policy; source rows preserved",
      "Elapsed-time SD or CV within current groups and participant/file identity; minimum observed-time coverage; explicit unevaluable-window policy"
    ),
    stringsAsFactors = FALSE
  )
  rbind(
    base,
    data.frame(
      type = c("timezone", "dst", "gaps", "aggregate"),
      label = c(
        "Change time zone",
        "Correct device DST",
        "Expand missing timestamps",
        "Aggregate observations"
      ),
      version = c(1L, 1L, 1L, 1L),
      public_call = c(
        "lubridate::with_tz / force_tz",
        "UTC-offset difference from recording start",
        "LightLogR::gap_handler(full_sequence); full-day base::seq/dplyr::full_join adapter",
        "LightLogR::aggregate_Datetime(type = floor); typed summary handlers, numeric missingness policy and observed-duration coverage"
      ),
      boundary = c(
        "Keep instant or explicitly reinterpret clock; skipped times rejected",
        "Fixed device offset from each recording start; explicit correction only",
        "Current groups and participant/file identity; added rows marked is.implicit; no imputation",
        "Calendar-aligned bins labelled at start; preserve current groups and participant/file identity; explicit NA policy; no empty-bin insertion"
      ),
      stringsAsFactors = FALSE
    )
  )
}

preparation_date <- function(x) {
  assert_scalar_string(x, "date")
  value <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  if (is.na(value) || !identical(format(value, "%Y-%m-%d"), x))
    abort_llw("Enter a valid date as YYYY-MM-DD.", "preparation")
  value
}

preparation_instant <- function(x) {
  assert_scalar_string(x, "datetime")
  if (
    !grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2} [+-][0-9]{4}$",
      x
    )
  )
    abort_llw(
      "Enter date and time with an explicit offset, for example 2026-03-28 22:00:00 +0100.",
      "preparation"
    )
  preparation_date(substr(x, 1L, 10L))
  preparation_clock_seconds(substr(x, 12L, 19L))
  hours <- as.integer(substr(x, 22L, 23L))
  minutes <- as.integer(substr(x, 24L, 25L))
  if (hours > 14L || minutes > 59L || (hours == 14L && minutes != 0L))
    abort_llw("The UTC offset must be between -1400 and +1400.", "preparation")
  value <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S %z", tz = "UTC")
  if (is.na(value)) abort_llw("The date and time are invalid.", "preparation")
  value
}

preparation_clock_seconds <- function(x) {
  assert_scalar_string(x, "clock time")
  if (!grepl("^([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$", x))
    abort_llw(
      "Enter clock times as HH:MM:SS, from 00:00:00 to 23:59:59.",
      "preparation"
    )
  parts <- as.numeric(strsplit(x, ":", fixed = TRUE)[[1L]])
  parts[[1L]] * 3600 + parts[[2L]] * 60 + parts[[3L]]
}

# Parse endpoint controls explicitly. Never guess which occurrence of a repeated
# clock time was intended, or roll a nonexistent local time across a DST gap.
preparation_local_instant <- function(date, time, timezone) {
  preparation_date(date)
  if (!preparation_valid_timezone(timezone))
    abort_llw(
      "Choose a time zone from the available Olson names.",
      "preparation"
    )
  assert_scalar_string(time, "time")
  time <- trimws(time)
  if (grepl("^[0-9]{2}:[0-9]{2}$", time)) time <- paste0(time, ":00")
  if (grepl("^[0-9]{2}:[0-9]{2}:[0-9]{2} [+-][0-9]{4}$", time)) {
    value <- preparation_instant(paste(date, time))
    if (
      !identical(
        format(value, "%Y-%m-%d %H:%M:%S %z", tz = timezone),
        paste(date, time)
      )
    )
      abort_llw(
        "That UTC offset does not match the selected date and time zone.",
        "preparation"
      )
  } else {
    preparation_clock_seconds(time)
    clock <- as.POSIXct(paste(date, time), tz = "UTC")
    value <- lubridate::force_tz(clock, tzone = timezone, roll_dst = "NA")
    if (is.na(value)) {
      before <- lubridate::force_tz(clock, tzone = timezone, roll_dst = "pre")
      if (
        identical(
          format(before, "%Y-%m-%d %H:%M:%S", tz = timezone),
          paste(date, time)
        )
      )
        abort_llw(
          "This local time occurs twice. Add its UTC offset to the time, for example 02:30:00 +0100.",
          "preparation"
        )
      abort_llw(
        "This local time does not exist because the clock moves forward. Choose a valid time.",
        "preparation"
      )
    }
  }
  format(value, "%Y-%m-%d %H:%M:%S %z", tz = timezone)
}

preparation_arguments <- function(type, args) {
  if (type %in% preparation_time_types())
    return(preparation_time_arguments(type, args))
  if (type %in% c("invalidate", "calibrate", "transform", "variability"))
    return(preparation_value_arguments(type, args))
  if (type %in% c("variable", "photoperiod", "coverage"))
    return(preparation_filter_arguments(type, args))
  expected <- switch(
    type,
    participants = c("identities", "identity_columns"),
    dates = c("start", "end", "timezone"),
    datetimes = if (any(c("start_timezone", "end_timezone") %in% names(args)))
      c("start", "end", "start_timezone", "end_timezone") else
      c("start", "end"),
    clock = c("start", "end", "timezone", "anchor"),
    duration = c("length", "unit", "from", "scope"),
    abort_llw(
      "This recipe step is not supported by preparation version 1.",
      "preparation"
    )
  )
  if (
    !is.list(args) ||
      !setequal(names(args), expected) ||
      anyDuplicated(names(args))
  )
    abort_llw(
      "The step arguments are incomplete or unsupported.",
      "preparation"
    )
  args <- args[expected]
  assert_serializable_value(args, "preparation arguments")
  if (type == "datetimes" && "start_timezone" %in% names(args)) {
    for (name in c("start_timezone", "end_timezone"))
      if (
        !identical(args[[name]], "native") &&
          !preparation_valid_timezone(args[[name]])
      )
        abort_llw(
          "Use the dataset native time zone or choose an Olson time zone.",
          "preparation"
        )
  }
  if (type == "participants") {
    assert_character_vector(
      args$identity_columns,
      "identity columns",
      allow_empty = FALSE
    )
    if (
      !is.data.frame(args$identities) ||
        !identical(names(args$identities), args$identity_columns) ||
        !"Id" %in% args$identity_columns ||
        !all(vapply(args$identities, is.character, logical(1))) ||
        anyDuplicated(args$identities)
    )
      abort_llw(
        "Choose exact retained participant/source identities.",
        "preparation"
      )
    if (nrow(args$identities) == 0L)
      abort_llw(
        "Choose at least one participant/source identity, or remove this step to keep all.",
        "preparation"
      )
  } else if (type == "duration") {
    for (name in c("unit", "from", "scope"))
      assert_scalar_string(args[[name]], name)
    if (
      !is.numeric(args$length) ||
        length(args$length) != 1L ||
        !is.finite(args$length) ||
        args$length <= 0
    )
      abort_llw("Enter a positive, finite length.", "preparation")
    if (
      !args$unit %in%
        c("calendar_days", "elapsed_days", "hours", "minutes", "seconds") ||
        !args$from %in% c("start", "end") ||
        !args$scope %in%
          c("overall", "source", "participant", "identity", "group")
    )
      abort_llw(
        "Choose a supported length unit, anchor and scope.",
        "preparation"
      )
    if (args$unit == "calendar_days" && args$length != floor(args$length))
      abort_llw(
        "Use a whole number of calendar days, or choose elapsed hours.",
        "preparation"
      )
  } else {
    if (
      "timezone" %in%
        names(args) &&
        !(type %in%
          c("dates", "clock") &&
          identical(args$timezone, "native")) &&
        !preparation_valid_timezone(args$timezone)
    )
      abort_llw(
        "Choose a valid IANA time zone, such as Europe/Berlin or UTC.",
        "preparation"
      )
    parser <- switch(
      type,
      dates = preparation_date,
      datetimes = preparation_instant,
      clock = preparation_clock_seconds
    )
    start <- parser(args$start)
    end <- parser(args$end)
    if (
      (type == "dates" && start > end) || (type == "datetimes" && start >= end)
    )
      abort_llw(
        "The start must precede the end. Calendar dates may be the same day.",
        "preparation"
      )
    if (type == "clock" && start == end)
      abort_llw(
        "Equal clock bounds are ambiguous. Use different times, or disable the step to keep the full day.",
        "preparation"
      )
    if (type == "clock" && !identical(args$anchor, "start_date"))
      abort_llw(
        "Version 1 clock windows are anchored to the date they start.",
        "preparation"
      )
  }
  args
}

new_preparation_step <- function(
  type,
  args,
  context,
  enabled = TRUE,
  id = new_stable_id("step")
) {
  args <- preparation_arguments(type, args)
  new_recipe_step(
    type,
    list(
      mapping_version = if (
        (type == "coverage" && "mode" %in% names(args)) ||
          (type == "variability" && "unevaluable" %in% names(args)) ||
          (type == "invalidate" && "source" %in% names(args)) ||
          (type == "transform" && "offset" %in% names(args))
      )
        3L else if (
        type == "coverage" ||
          (type %in%
            c("dates", "clock") &&
            identical(args$timezone, "native")) ||
          (type %in%
            c("invalidate", "calibrate", "transform", "variability") &&
            "output" %in% names(args)) ||
          (type == "datetimes" && "start_timezone" %in% names(args))
      )
        2L else 1L,
      dataset_id = context$dataset_id,
      raw_checksum = context$raw_checksum,
      arguments = args
    ),
    enabled = enabled,
    id = id
  )
}

validate_preparation_recipe <- function(recipe, context) {
  recipe <- validate_recipe(recipe)
  if (!identical(recipe$version, 1L) || length(recipe$steps) > 30L)
    abort_llw(
      "Preparation recipes use version 1 and at most 30 steps.",
      "preparation"
    )
  outputs <- preparation_output_names(recipe)
  if (
    anyDuplicated(outputs) ||
      any(
        outputs %in%
          (context$columns %||% names(context$variables))
      )
  )
    abort_llw(
      "That variable name already exists in the source or recipe. Choose a new name.",
      "preparation"
    )
  available <- context
  for (step in recipe$steps) {
    p <- step$parameters
    if (
      !identical(
        names(p),
        c("mapping_version", "dataset_id", "raw_checksum", "arguments")
      ) ||
        !(identical(p$mapping_version, 1L) ||
          (step$type %in%
            c(
              "dates",
              "datetimes",
              "clock",
              "coverage",
              "invalidate",
              "calibrate",
              "transform",
              "variability"
            ) &&
            identical(p$mapping_version, 2L)) ||
          (step$type %in%
            c("coverage", "variability", "invalidate", "transform") &&
            identical(p$mapping_version, 3L))) ||
        !identical(p$dataset_id, context$dataset_id) ||
        !identical(p$raw_checksum, context$raw_checksum)
    )
      abort_llw(
        "This step belongs to another dataset or an unsupported recipe version.",
        "preparation"
      )
    args <- preparation_arguments(step$type, p$arguments)
    if (
      step$type == "transform" &&
        (identical(p$mapping_version, 3L) != ("offset" %in% names(args)))
    )
      abort_llw(
        "Zero-inflated transformations require mapping version 3.",
        "preparation"
      )
    if (
      step$type %in%
        c("dates", "clock") &&
        !identical(
          identical(p$mapping_version, 2L),
          identical(args$timezone, "native")
        )
    )
      abort_llw(
        "The native-time selection does not match its mapping version.",
        "preparation"
      )
    if (
      step$enabled &&
        step$type == "variability" &&
        !args$source %in% available$measurements
    )
      abort_llw(
        paste0(
          "Variability source '",
          args$source,
          "' is unavailable at this step. Choose a numeric measurement available here."
        ),
        "preparation"
      )
    if (
      step$type %in%
        c("invalidate", "calibrate", "transform", "variability") &&
        !identical(p$mapping_version, 3L) &&
        (identical(p$mapping_version, 2L) != ("output" %in% names(args)))
    )
      abort_llw(
        "A new variable requires measurement mapping version 2.",
        "preparation"
      )
    if (
      step$type == "variability" &&
        (identical(p$mapping_version, 3L) != ("unevaluable" %in% names(args)))
    )
      abort_llw(
        "An explicit unevaluable-window policy requires variability version 3.",
        "preparation"
      )
    if (
      step$type == "invalidate" &&
        (identical(p$mapping_version, 3L) != ("source" %in% names(args)))
    )
      abort_llw(
        "An independent condition source requires measurement version 3.",
        "preparation"
      )
    if (step$enabled && step$type == "invalidate" && !is.null(args$source)) {
      source <- available$variables[[args$source]]
      if (is.null(source))
        abort_llw(
          paste0(
            "Condition source '",
            args$source,
            "' is unavailable at this step."
          ),
          "preparation"
        )
      if (args$mode == "range" && !source$numeric)
        abort_llw(
          "Numeric intervals require a numeric source variable.",
          "preparation"
        )
    }
    if (
      step$type == "coverage" &&
        (identical(p$mapping_version, 3L) != ("mode" %in% names(args)))
    )
      abort_llw(
        "Duration coverage requires coverage version 3; percentage coverage uses version 1 or 2.",
        "preparation"
      )
    if (
      step$type %in%
        c("invalidate", "calibrate", "transform", "variability") &&
        step$enabled &&
        !args$variable %in% available$measurements
    )
      abort_llw(
        paste0(
          "Measurement '",
          args$variable,
          "' is unavailable at this step. Choose a numeric measurement, not an identifier, provenance or grouping column. Keep its creating step enabled and earlier in the recipe."
        ),
        "preparation"
      )
    if (
      step$type == "coverage" &&
        identical(p$mapping_version, 1L) &&
        !args$basis %in% c("full_day", "observed_span")
    )
      abort_llw(
        "Whole-group coverage requires coverage version 2.",
        "preparation"
      )
    if (step$enabled && step$type %in% c("variable", "coverage")) {
      variable <- available$variables[[args$variable]]
      if (is.null(variable))
        abort_llw(
          paste0(
            "Variable '",
            args$variable,
            "' is unavailable at this step. Keep its creating step enabled and earlier in the recipe."
          ),
          "preparation"
        )
      if (step$type == "variable" && args$mode == "range" && !variable$numeric)
        abort_llw(
          "Numeric intervals require a numeric variable.",
          "preparation"
        )
      # Saved categorical conditions remain literal after reordering or
      # retargeting. A now-absent category matches no rows; it is not an error.
    }
    if (
      step$type == "photoperiod" &&
        args$method == "existing" &&
        !context$has_photoperiod
    )
      abort_llw(
        "No photoperiod.state column is available. Supply coordinates to derive day/night.",
        "preparation"
      )
    if (
      step$type == "datetimes" &&
        !identical(
          identical(p$mapping_version, 2L),
          "start_timezone" %in% names(args)
        )
    )
      abort_llw(
        "The datetime argument shape does not match its mapping version.",
        "preparation"
      )
    if (
      step$type == "duration" &&
        args$scope == "source" &&
        !isTRUE(context$source_identity_complete)
    )
      abort_llw(
        "This record has no separate source dataset identifiers. Choose overall or a participant scope.",
        "preparation"
      )
    if (step$type == "datetimes" && identical(p$mapping_version, 2L)) {
      for (endpoint in c("start", "end")) {
        zone <- args[[paste0(endpoint, "_timezone")]]
        if (is.null(zone))
          abort_llw("The endpoint time zone is missing.", "preparation")
        # Native is an editor preference. The saved offset-bearing endpoint
        # is an absolute instant and survives later zone changes/reordering.
        # User-entered native clock times are checked when the form is saved.
        if (zone == "native") next
        preparation_local_instant(
          substr(args[[endpoint]], 1L, 10L),
          substr(args[[endpoint]], 12L, 25L),
          zone
        )
      }
    }
    if (step$type == "participants") {
      if (
        !identical(args$identity_columns, context$identity_columns) ||
          nrow(dplyr::anti_join(
            args$identities,
            context$identities,
            by = args$identity_columns,
            na_matches = "na"
          )) >
            0L
      )
        abort_llw(
          "A participant/source identity is not present in this source dataset.",
          "preparation"
        )
    }
    available <- preparation_recipe_context(available, new_recipe(list(step)))
  }
  recipe
}

# Scientific selection stays here. The caller supplies immutable canonical raw
# data. No source data are decoded, cached, mutated or evaluated as R text here.
evaluate_preparation <- function(
  raw_data,
  recipe,
  context,
  details = FALSE,
  budget = 1024^3,
  variability_reference = NULL
) {
  recipe <- validate_preparation_recipe(recipe, context)
  data <- raw_data
  marker <- utils::tail(
    make.unique(c(names(data), ".llw_prepare_source_row")),
    1L
  )
  if (details) data[[marker]] <- seq_len(nrow(data))
  # Compact many-to-one lineage: one token per source row, never per-bin lists.
  source_tokens <- if (details) seq_len(nrow(data)) else integer()
  next_token <- nrow(data)
  aggregated <- FALSE
  removed_at <- integer(nrow(data))
  step_counts <- list()
  variables_after <- list()
  time_after <- list()
  time_notes <- list()
  working_bytes <- 0
  if (
    is.null(variability_reference) &&
      any(vapply(
        recipe$steps,
        function(s) s$enabled && s$type %in% c("variability", "aggregate"),
        logical(1)
      ))
  )
    variability_reference <- preparation_variability_reference(raw_data)
  for (i in seq_along(recipe$steps)) {
    step <- recipe$steps[[i]]
    before <- nrow(data)
    before_index <- if (details) data[[marker]] else integer()
    args <- step$parameters$arguments
    before_times <- data$Datetime
    context$timezone <- lubridate::tz(data$Datetime)
    if (
      step$type %in% c("dates", "clock") && identical(args$timezone, "native")
    )
      args$timezone <- context$timezone
    time_note <- NULL
    variability_diagnostics <- NULL
    changes_values <- step$enabled &&
      step$type %in% c("invalidate", "calibrate", "transform", "variability")
    before_values <- if (details && changes_values) data[[args$variable]] else
      NULL
    if (
      step$enabled &&
        (nrow(data) > 0L ||
          changes_values ||
          step$type %in% preparation_time_types())
    )
      data <- switch(
        step$type,
        timezone = preparation_change_timezone(data, args),
        dst = preparation_correct_dst(data),
        gaps = {
          expanded <- preparation_expand_gaps(data, args, budget)
          time_note <- expanded$plan
          working_bytes <- max(working_bytes, time_note$working_bytes)
          expanded$data
        },
        aggregate = {
          input <- if (details) data[setdiff(names(data), marker)] else data
          focus <- preparation_focus_context(
            context,
            new_recipe(recipe$steps[seq_len(i)])
          )$variable
          result <- preparation_aggregate(
            input,
            args,
            variability_reference,
            focus,
            budget
          )
          time_note <- result$note
          time_note$step <- i
          time_note$previous_aggregation <- aggregated
          working_bytes <- max(working_bytes, time_note$working_bytes)
          if (details) {
            tokens <- next_token + seq_len(nrow(result$data))
            source_tokens <- tokens[result$membership[match(
              source_tokens,
              before_index
            )]]
            result$data[[marker]] <- tokens
            next_token <- next_token + nrow(result$data)
          }
          aggregated <- TRUE
          variability_reference <- preparation_variability_reference(
            result$data
          )
          variability_reference$keys$.llw_variability_epoch <- rep(
            args$length *
              switch(
                args$unit,
                seconds = 1,
                minutes = 60,
                hours = 3600,
                days = 86400
              ),
            nrow(variability_reference$keys)
          )
          variability_reference$windows <- result$windows
          result$data
        },
        invalidate = preparation_change_values(data, step$type, args),
        calibrate = preparation_change_values(data, step$type, args),
        transform = preparation_change_values(data, step$type, args),
        variability = {
          result <- preparation_mask_variability(
            data,
            args,
            variability_reference
          )
          variability_diagnostics <- result$diagnostics
          result$data
        },
        variable = preparation_filter_variable(data, args),
        photoperiod = preparation_filter_photoperiod(data, args),
        coverage = preparation_filter_coverage(
          data,
          args,
          context,
          step$parameters$mapping_version
        ),
        participants = {
          # Join only on explicit stored identities, never labels or row positions.
          keys <- data[args$identity_columns]
          keys[] <- lapply(keys, as.character)
          keep <- dplyr::semi_join(
            cbind(keys, .row = seq_len(nrow(keys))),
            args$identities,
            by = args$identity_columns,
            na_matches = "na"
          )$.row
          data[keep, , drop = FALSE]
        },
        dates = LightLogR::filter_Date(
          data,
          start = args$start,
          end = args$end,
          tz = args$timezone,
          Datetime.colname = Datetime,
          length = NULL,
          length_from_start = TRUE,
          full.day = FALSE,
          only_Id = NULL,
          filter.expr = NULL
        ),
        datetimes = LightLogR::filter_Datetime(
          data,
          start = preparation_instant(args$start),
          end = preparation_instant(args$end),
          tz = "UTC",
          Datetime.colname = Datetime,
          length = NULL,
          length_from_start = TRUE,
          full.day = FALSE,
          only_Id = NULL,
          filter.expr = NULL
        ),
        duration = preparation_filter_length(data, args, context),
        clock = {
          time <- lubridate::with_tz(data$Datetime, tzone = args$timezone)
          seconds <- lubridate::hour(time) *
            3600 +
            lubridate::minute(time) * 60 +
            lubridate::second(time)
          start <- preparation_clock_seconds(args$start)
          end <- preparation_clock_seconds(args$end)
          keep <- if (start < end) seconds >= start & seconds < end else
            seconds >= start | seconds < end
          .env <- NULL
          # Imported data may retain grouping. Match the full-row mask to the
          # current group without changing those groups or the source row order.
          dplyr::filter(data, .env$keep[dplyr::cur_group_rows()])
        }
      )
    if (
      step$enabled &&
        step$type %in% c("timezone", "dst") &&
        !is.null(variability_reference$windows) &&
        !identical(as.numeric(before_times), as.numeric(data$Datetime))
    )
      variability_reference$unknown_support <- TRUE
    if (details) {
      time_after[[i]] <- list(
        timezone = lubridate::tz(data$Datetime),
        start = if (nrow(data)) min(data$Datetime) else as.POSIXct(NA),
        end = if (nrow(data)) max(data$Datetime) else as.POSIXct(NA)
      )
      time_notes[[i]] <- time_note %||% list()
      variables_after[[i]] <- preparation_variable_context(
        data[setdiff(names(data), marker)]
      )
      lost <- if (step$enabled && step$type == "aggregate") integer() else
        setdiff(before_index, data[[marker]])
      lost_sources <- !is.na(source_tokens) & source_tokens %in% lost
      removed_at[lost_sources] <- i
      source_tokens[lost_sources] <- NA_integer_
      removed <- if (step$type %in% c("gaps", "aggregate")) 0L else
        max(0L, before - nrow(data))
      step_counts[[i]] <- data.frame(
        id = step$id,
        before = before,
        after = nrow(data),
        removed = removed,
        added = max(0L, nrow(data) - before),
        combined = if (step$enabled && step$type == "aggregate")
          before - nrow(data) else 0L,
        retimed = if (step$enabled && step$type %in% c("timezone", "dst"))
          sum(as.numeric(before_times) != as.numeric(data$Datetime)) else 0L,
        evaluated_windows = if (is.null(variability_diagnostics)) 0L else
          sum(variability_diagnostics$evaluable),
        unevaluable_windows = if (is.null(variability_diagnostics)) 0L else
          sum(!variability_diagnostics$evaluable),
        variability_notes = if (is.null(variability_diagnostics)) "" else {
          reasons <- table(variability_diagnostics$reason[
            !variability_diagnostics$evaluable
          ])
          paste(
            paste(names(reasons), as.integer(reasons), sep = ": "),
            collapse = "; "
          )
        },
        changed = if (changes_values)
          sum(preparation_values_differ(
            before_values,
            data[[args$output %||% args$variable]]
          )) else 0L,
        created = if (changes_values && !is.null(args$output)) nrow(data) else
          0L,
        invalidated = if (changes_values)
          sum(
            !is.na(before_values) &
              is.na(data[[args$output %||% args$variable]])
          ) else 0L,
        previous_percent = if (before) 100 * removed / before else 0,
        source_percent = if (nrow(raw_data))
          100 * sum(lost_sources) / nrow(raw_data) else 0,
        cumulative_percent = if (nrow(raw_data))
          100 * sum(removed_at != 0L) / nrow(raw_data) else 0
      )
    }
  }
  if (details) {
    source_rows <- match(data[[marker]], source_tokens)
    # NA tokens on newly expanded rows never match an absent source token.
    source_rows[is.na(data[[marker]])] <- NA_integer_
    source_membership <- match(source_tokens, data[[marker]])
    source_membership[is.na(source_tokens)] <- NA_integer_
    data[[marker]] <- NULL
    if (
      !any(removed_at) &&
        !any(vapply(
          recipe$steps,
          function(x)
            x$enabled &&
              x$type %in%
                c(
                  "invalidate",
                  "calibrate",
                  "transform",
                  "variability",
                  "timezone",
                  "dst",
                  "gaps",
                  "aggregate"
                ),
          logical(1)
        ))
    )
      data <- raw_data
    return(list(
      data = data,
      removed_at = removed_at,
      source_rows = source_rows,
      source_membership = source_membership,
      aggregated = aggregated,
      time_after = time_after,
      time_notes = time_notes,
      working_bytes = working_bytes,
      steps = dplyr::bind_rows(step_counts),
      variables_after = variables_after,
      variability_reference = variability_reference
    ))
  }
  data
}

preparation_filter_length <- function(data, args, context) {
  columns <- switch(
    args$scope,
    overall = character(),
    source = "llw_source_dataset_id",
    participant = "Id",
    identity = context$identity_columns,
    group = dplyr::group_vars(data)
  )
  # Filter an indexed view, then subset the untouched input. This preserves
  # source columns, row order, row names and data-frame classes after grouping.
  marker <- utils::tail(make.unique(c(names(data), ".llw_duration_row")), 1L)
  indexed <- data.frame(Datetime = data$Datetime)
  for (column in columns) indexed[[column]] <- data[[column]]
  indexed[[marker]] <- seq_len(nrow(data))
  grouped <- if (length(columns))
    dplyr::group_by(indexed, dplyr::across(dplyr::all_of(columns))) else indexed
  span <- if (args$unit == "calendar_days") lubridate::days(args$length) else {
    seconds <- args$length *
      switch(
        args$unit,
        elapsed_days = 86400,
        hours = 3600,
        minutes = 60,
        seconds = 1
      )
    if (!is.finite(seconds))
      abort_llw("The requested duration is too large.", "preparation")
    lubridate::dseconds(seconds)
  }
  if (args$unit == "calendar_days") {
    groups <- if (length(columns)) dplyr::group_rows(grouped) else
      list(seq_len(nrow(data)))
    anchors <- vapply(
      groups,
      function(rows) {
        times <- as.numeric(data$Datetime[rows])
        if (args$from == "start") min(times) else max(times)
      },
      numeric(1)
    )
    anchors <- as.POSIXct(anchors, origin = "1970-01-01", tz = context$timezone)
    wall <- lubridate::force_tz(anchors, "UTC")
    target <- if (args$from == "start") wall + span else wall - span
    boundary <- lubridate::force_tz(target, context$timezone, roll_dst = "NA")
    if (anyNA(boundary))
      abort_llw(
        "A calendar-day boundary is skipped or repeated by a clock change. Choose elapsed time or an explicit date-and-time range.",
        "preparation"
      )
  }
  selected <- LightLogR::filter_Datetime(
    grouped,
    length = span,
    length_from_start = args$from == "start",
    start = NULL,
    end = NULL,
    full.day = FALSE,
    only_Id = NULL,
    filter.expr = NULL,
    Datetime.colname = Datetime,
    tz = context$timezone
  )
  data[sort(selected[[marker]]), , drop = FALSE]
}

edit_preparation_recipe <- function(
  recipe,
  action,
  step = NULL,
  id = NULL,
  ordered_ids = NULL
) {
  recipe <- validate_recipe(recipe)
  ids <- vapply(recipe$steps, `[[`, character(1), "id")
  position <- match(id, ids)
  if (action == "reorder") {
    if (
      !is.character(ordered_ids) ||
        anyNA(ordered_ids) ||
        anyDuplicated(ordered_ids) ||
        length(ordered_ids) != length(ids) ||
        !setequal(ordered_ids, ids)
    )
      abort_llw(
        "The recipe changed. Reorder the current steps again.",
        "preparation"
      )
    recipe$steps <- recipe$steps[match(ordered_ids, ids)]
  } else if (action == "save") {
    step <- validate_recipe_step(step)
    position <- match(step$id, ids)
    if (is.na(position)) recipe$steps <- c(recipe$steps, list(step)) else
      recipe$steps[[position]] <- step
  } else {
    if (length(position) != 1L || is.na(position))
      abort_llw("Select a recipe step first.", "preparation")
    if (action == "remove") recipe$steps <- recipe$steps[-position] else if (
      action == "toggle"
    )
      recipe$steps[[position]]$enabled <- !recipe$steps[[
        position
      ]]$enabled else if (action %in% c("up", "down")) {
      other <- position + if (action == "up") -1L else 1L
      if (other >= 1L && other <= length(ids))
        recipe$steps[c(position, other)] <- recipe$steps[c(other, position)]
    } else abort_llw("Unknown recipe edit.", "preparation")
  }
  if (length(recipe$steps) > 30L)
    abort_llw("A recipe can contain at most 30 steps.", "preparation")
  new_recipe(recipe$steps, recipe$version)
}

preparation_binding <- function(record, recipe = record$recipe) {
  list(
    dataset_id = record$id,
    revision = record$revision,
    raw_checksum = record$raw_checksum,
    recipe = recipe_fingerprint(recipe)
  )
}

preparation_preview_matches <- function(preview, record, recipe) {
  !is.null(preview) &&
    !is.null(record) &&
    identical(preview$binding, preparation_binding(record, recipe))
}

# The worker owns costly validation, decoding, summaries and snapshot creation.
# A preview contains the entire validated candidate, not its displayed sample.
preparation_task_response <- function(payload, spec = NULL) {
  # Return known public errors as data so the background transport preserves
  # their safe message and class. Unexpected errors retain normal sanitization.
  tryCatch(
    preparation_task_worker(payload, spec),
    llw_error = function(cnd) list(error = cnd)
  )
}

preparation_task_worker <- function(payload, spec = NULL) {
  action <- match.arg(
    payload$action,
    c("inspect", "preview", "view", "reset", "undo")
  )
  record <- payload$record
  budget <- payload$budget
  preparation_check_budget(record, budget)
  record <- validate_dataset_record(record)
  raw <- unserialize(record$raw_payload)
  preparation_check_budget(record, budget, raw)
  context <- preparation_context(record, raw)
  context <- preparation_focus_request(context, record, payload$focus_variable)
  if (action == "view")
    return(list(
      action = action,
      view = preparation_recipe_view(
        raw,
        payload$recipe,
        context,
        payload$view,
        budget = budget
      )
    ))
  if (action == "inspect") {
    context$prepared_rows <- nrow(unserialize(record$prepared_payload))
    inspected_recipe <- payload$recipe %||% preparation_saved_recipe(record)
    catalogs <- if (length(inspected_recipe$steps)) {
      evaluated <- evaluate_preparation(
        raw,
        inspected_recipe,
        context,
        details = TRUE,
        budget = budget
      )
      preparation_catalog_snapshot(
        context,
        inspected_recipe,
        evaluated$variables_after,
        evaluated$time_after
      )
    } else NULL
    return(list(action = action, context = context, catalogs = catalogs))
  }
  started <- proc.time()[["elapsed"]]
  if (action == "preview") {
    recipe <- validate_preparation_recipe(payload$recipe, context)
    draft <- stage_dataset_draft(record, list(recipe = recipe))
    evaluated <- NULL
    draft <- preview_dataset_draft(draft, function(raw_data, recipe, ...) {
      evaluated <<- evaluate_preparation(
        raw_data,
        recipe,
        context,
        details = TRUE,
        budget = budget
      )
      evaluated$data
    })
    after <- unserialize(draft$draft$preview$prepared_payload)
    candidate <- apply_dataset_draft(draft)
    comparison_context <- preparation_focus_context(context, recipe)
    comparison <- preparation_comparison(raw, after, comparison_context, recipe)
    comparison$value_impact <- preparation_value_impact(
      raw,
      after,
      evaluated$source_rows,
      if (isTRUE(evaluated$aggregated)) new_recipe() else recipe
    )
    comparison$aggregated <- evaluated$aggregated
    comparison$represented_rows <- sum(!is.na(evaluated$source_membership))
    comparison$invalidated_values <- sum(comparison$value_impact$invalidated)
    comparison$changed_values <- sum(comparison$value_impact$changed)
    comparison$created_values <- sum(comparison$value_impact$created)
    comparison$variables_after <- evaluated$variables_after
    comparison$steps <- evaluated$steps
    comparison$removed_at <- evaluated$removed_at
    comparison$removed_rows <- sum(evaluated$removed_at != 0L)
    comparison$removed_fraction <- if (nrow(raw))
      comparison$removed_rows / nrow(raw) else 0
    comparison$added_rows <- if (evaluated$aggregated) 0L else
      sum(is.na(evaluated$source_rows))
    original <- !is.na(evaluated$source_rows)
    comparison$retimed_rows <- if (evaluated$aggregated) 0L else
      sum(
        as.numeric(after$Datetime[original]) !=
          as.numeric(raw$Datetime[evaluated$source_rows[original]])
      )
    comparison$time_after <- evaluated$time_after
    comparison$time_notes <- evaluated$time_notes
    aggregation <- Filter(
      function(x) identical(x$type, "aggregate"),
      evaluated$time_notes
    )
    comparison$aggregations <- aggregation
    comparison$aggregation <- if (length(aggregation))
      utils::tail(aggregation, 1L)[[1L]] else NULL
    if (evaluated$aggregated)
      comparison$table$Preview[
        comparison$table$Measure == "Intended-window coverage"
      ] <-
        "See aggregation input coverage below"
    comparison$time_diagnostics <- preparation_time_diagnostics(after)
    comparison$table <- rbind(
      comparison$table,
      data.frame(
        Measure = c(
          "Timestamp time zone",
          "Explicit gap rows added",
          "Source timestamps shifted"
        ),
        Source = c(lubridate::tz(raw$Datetime), "0", "0"),
        Preview = c(
          lubridate::tz(after$Datetime),
          if (evaluated$aggregated) "See individual recipe steps" else
            as.character(comparison$added_rows),
          if (evaluated$aggregated) "Not compared after aggregation" else
            as.character(comparison$retimed_rows)
        )
      )
    )
    comparison$view <- preparation_recipe_view(
      raw,
      recipe,
      context,
      payload$view %||% list(),
      evaluated
    )
    preparation_check_budget(candidate, budget)
    return(list(
      action = action,
      context = context,
      binding = preparation_binding(record, recipe),
      candidate = candidate,
      comparison = comparison,
      elapsed_seconds = proc.time()[["elapsed"]] - started,
      estimated_working_bytes = max(
        preparation_check_budget(record, budget, raw),
        evaluated$working_bytes
      )
    ))
  }
  candidate <- if (action == "reset") reset_dataset_record(record) else
    undo_dataset_record(record)
  preparation_check_budget(candidate, budget)
  list(
    action = action,
    binding = preparation_binding(record),
    candidate = candidate
  )
}
