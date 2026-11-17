preparation_identity_columns <- function(data) {
  intersect(
    c("Id", "llw_source_dataset_id", "file_group_id", "file.name"),
    names(data)
  )
}

preparation_has_file_identity <- function(data) {
  columns <- intersect(c("file_group_id", "file.name"), names(data))
  if (!length(columns)) return(FALSE)
  known <- lapply(data[columns], function(value) {
    value <- as.character(value)
    !is.na(value) & nzchar(trimws(value))
  })
  all(Reduce(`|`, known))
}

preparation_memory_estimate <- function(raw_bytes) 64 * 1024^2 + 16 * raw_bytes

preparation_check_budget <- function(record, budget, raw = NULL) {
  if (
    !is.numeric(budget) ||
      length(budget) != 1L ||
      !is.finite(budget) ||
      budget <= 0
  )
    abort_llw("The preparation resource budget is invalid.", "resource")
  estimated <- preparation_memory_estimate(length(record$raw_payload))
  retained <- as.numeric(utils::object.size(record))
  if (!is.null(raw))
    estimated <- max(
      estimated,
      64 * 1024^2 + retained + 8 * as.numeric(utils::object.size(raw))
    )
  if (estimated > budget || retained > budget)
    abort_llw(
      "This preparation exceeds the session's per-record memory budget. Use fewer input rows or an explicitly resourced local session.",
      "resource"
    )
  invisible(estimated)
}

preparation_context <- function(record, raw) {
  dashboard_assert_canonical_data(raw, "canonical raw data")
  columns <- preparation_identity_columns(raw)
  identities <- unique(as.data.frame(
    lapply(raw[columns], as.character),
    stringsAsFactors = FALSE
  ))
  rownames(identities) <- NULL
  labels <- apply(identities, 1L, function(row) {
    paste(ifelse(is.na(row), "(unspecified)", row), collapse = " | ")
  })
  keys <- vapply(
    seq_len(nrow(identities)),
    function(i) {
      digest::digest(as.list(identities[i, , drop = FALSE]), algo = "sha256")
    },
    character(1)
  )
  timezone <- lubridate::tz(raw$Datetime)
  if (!preparation_valid_timezone(timezone))
    abort_llw(
      "The dataset timestamps need a valid native time zone.",
      "preparation"
    )
  numeric <- names(raw)[vapply(
    raw,
    function(x) is.numeric(x) && !inherits(x, c("POSIXt", "Date")),
    logical(1)
  )]
  numeric <- setdiff(numeric, append_provenance_columns())
  variable <- record$analysis_settings$primary_variable
  if (is.null(variable) || !variable %in% numeric)
    variable <- if (length(numeric)) numeric[[1L]] else NULL
  list(
    dataset_id = record$id,
    time_diagnostics = preparation_time_diagnostics(raw),
    raw_checksum = record$raw_checksum,
    identity_columns = columns,
    group_columns = dplyr::group_vars(raw),
    file_identity_complete = preparation_has_file_identity(raw),
    source_identity_complete = "llw_source_dataset_id" %in%
      names(identities) &&
      all(
        !is.na(identities$llw_source_dataset_id) &
          nzchar(trimws(identities$llw_source_dataset_id))
      ),
    identities = identities,
    choices = stats::setNames(keys, labels),
    timezone = timezone,
    start = if (nrow(raw)) min(raw$Datetime) else as.POSIXct(NA, tz = timezone),
    end = if (nrow(raw)) max(raw$Datetime) else as.POSIXct(NA, tz = timezone),
    rows = nrow(raw),
    columns = names(raw),
    variable = variable,
    variable_label = if (is.null(variable)) "No numeric measurement" else
      preparation_variable_label(record, variable),
    variable_labels = stats::setNames(
      lapply(
        names(raw),
        function(name) preparation_variable_label(record, name)
      ),
      names(raw)
    ),
    variables = preparation_variable_context(raw),
    measurements = preparation_measurement_columns(raw),
    measurement_labels = stats::setNames(
      lapply(preparation_measurement_columns(raw), function(name) {
        preparation_variable_label(record, name)
      }),
      preparation_measurement_columns(raw)
    ),
    has_photoperiod = "photoperiod.state" %in% names(raw),
    raw_bytes = length(record$raw_payload)
  )
}

preparation_summary <- function(data, context) {
  context$timezone <- lubridate::tz(data$Datetime)
  if (!nrow(data))
    return(list(
      rows = 0L,
      participants = 0L,
      identities = 0L,
      file_identity_complete = preparation_has_file_identity(data),
      start = NA_character_,
      end = NA_character_,
      epochs = "Not estimable",
      missing = 0L,
      values = 0L,
      gaps = NA_real_,
      duplicates = 0L
    ))
  keys <- data[context$identity_columns]
  keys[] <- lapply(keys, as.character)
  # Selection uses only retained identities. Without a file discriminator,
  # participant/source rows may combine recordings, so temporal diagnostics
  # cannot be attributed to individual files.
  groups <- dplyr::group_by(keys, dplyr::across(dplyr::everything()))
  rows <- dplyr::group_rows(groups)
  identified <- preparation_has_file_identity(data)
  diagnostics <- if (identified)
    lapply(rows, function(index) {
      part <- data[index, c("Id", "Datetime"), drop = FALSE]
      if (!is.null(context$variable))
        part[[context$variable]] <- data[[context$variable]][index]
      participant_import_quality_details(
        part,
        as.character(part$Id[[1L]]),
        context$timezone
      )$summary
    }) else list()
  quality <- dplyr::bind_rows(diagnostics)
  epochs <- if (identified) sort(unique(quality$dominant_epoch_seconds)) else
    numeric()
  values <- if (is.null(context$variable)) NULL else data[[context$variable]]
  list(
    rows = nrow(data),
    participants = length(unique(as.character(data$Id))),
    identities = length(rows),
    file_identity_complete = identified,
    start = format(min(data$Datetime), tz = context$timezone, usetz = TRUE),
    end = format(max(data$Datetime), tz = context$timezone, usetz = TRUE),
    epochs = if (!identified)
      "Unavailable: file identity not retained" else if (
      !length(epochs) || all(is.na(epochs))
    )
      "Not estimable" else
      paste(paste(epochs[!is.na(epochs)], "s"), collapse = ", "),
    missing = sum(is.na(values)),
    values = length(values),
    gaps = if (!identified || anyNA(quality$implicit_gap_epochs)) NA_real_ else
      sum(quality$implicit_gap_epochs),
    duplicates = if (!identified) NA_integer_ else
      sum(quality$duplicate_timestamp_rows)
  )
}

preparation_sample <- function(data, context, n = 100L) {
  fields <- unique(c(
    "Id",
    "Datetime",
    "is.implicit",
    context$variable,
    context$identity_columns,
    names(data)
  ))
  fields <- utils::head(intersect(fields, names(data)), 8L)
  shown <- utils::head(data[fields], n)
  for (name in names(shown)) {
    if (inherits(shown[[name]], "POSIXct"))
      shown[[name]] <-
        format(
          shown[[name]],
          "%Y-%m-%d %H:%M:%S %z",
          tz = lubridate::tz(data[[name]])
        )
  }
  shown
}

preparation_plot_sample <- function(data, context, stage) {
  if (is.null(context$variable)) return(data.frame())
  data <- utils::head(data, 1000L)
  data.frame(
    Datetime = data$Datetime,
    Value = data[[context$variable]],
    Stage = factor(rep(stage, nrow(data)), levels = c("Source", "Preview")),
    stringsAsFactors = FALSE
  )
}

preparation_comparison <- function(
  raw,
  prepared,
  context,
  recipe = new_recipe()
) {
  source_context <- context
  source_context$variable <- context$source_variable %||% context$variable
  before <- preparation_summary(raw, source_context)
  after <- preparation_summary(prepared, context)
  value <- function(x)
    c(
      x$rows,
      x$participants,
      x$identities,
      x$start,
      x$end,
      x$epochs,
      paste(x$missing, "of", x$values),
      if (!x$file_identity_complete && x$rows > 0L)
        "Unavailable: file identity not retained" else if (is.na(x$gaps))
        "Not estimable" else x$gaps,
      if (is.na(x$duplicates)) "Unavailable: file identity not retained" else
        x$duplicates,
      "Not assessed: no intended window"
    )
  after_sample <- preparation_sample(prepared, context)
  clocks <- which(vapply(
    recipe$steps,
    function(step) step$enabled && step$type == "clock",
    logical(1)
  ))
  clock_at <- if (length(clocks)) utils::tail(clocks, 1L) else 0L
  later <- recipe$steps[seq_along(recipe$steps) > clock_at]
  shifted_later <- any(vapply(
    later,
    function(step)
      step$enabled &&
        (step$type == "dst" ||
          (step$type == "timezone" &&
            step$parameters$arguments$method == "force")),
    logical(1)
  ))
  # The selection anchor can only be reconstructed from unchanged instants.
  if (length(clocks) && nrow(after_sample) && !shifted_later) {
    args <- recipe$steps[[clock_at]]$parameters$arguments
    if (identical(args$timezone, "native"))
      args$timezone <- preparation_recipe_context(
        context,
        new_recipe(recipe$steps[seq_len(clock_at - 1L)])
      )$timezone
    time <- lubridate::with_tz(
      utils::head(prepared$Datetime, 100L),
      args$timezone
    )
    seconds <- lubridate::hour(time) *
      3600 +
      lubridate::minute(time) * 60 +
      lubridate::second(time)
    anchor <- as.Date(time, tz = args$timezone)
    if (
      preparation_clock_seconds(args$start) >
        preparation_clock_seconds(args$end)
    )
      anchor <- anchor -
        as.integer(seconds < preparation_clock_seconds(args$end))
    after_sample <- after_sample[utils::head(names(after_sample), 7L)]
    after_sample$Window_start_date <- format(anchor, "%Y-%m-%d")
  }
  list(
    before = before,
    after = after,
    removed_rows = nrow(raw) - nrow(prepared),
    removed_fraction = if (nrow(raw)) 1 - nrow(prepared) / nrow(raw) else 0,
    invalidated_values = 0L,
    table = data.frame(
      Measure = c(
        "Rows",
        "Participant IDs",
        "Selectable identities",
        "First timestamp",
        "Last timestamp",
        "Dominant sampling interval within identified files",
        paste("Missing values:", context$variable_label),
        "Implicit gaps within identified file spans",
        "Rows at duplicate timestamps within identified files",
        "Intended-window coverage"
      ),
      Source = value(before),
      Preview = value(after),
      check.names = FALSE
    ),
    before_sample = preparation_sample(raw, source_context),
    after_sample = after_sample,
    plot = dplyr::bind_rows(
      preparation_plot_sample(raw, source_context, "Source"),
      preparation_plot_sample(prepared, context, "Preview")
    )
  )
}

preparation_step_label <- function(step, index) {
  map <- preparation_call_map()
  args <- step$parameters$arguments
  detail <- if (step$type == "aggregate")
    paste(
      names(preparation_aggregation_intervals())[
        match(
          preparation_aggregation_unit(args),
          preparation_aggregation_intervals()
        )
      ],
      "\u00b7",
      args$numeric,
      "\u00b7",
      switch(
        args$missing,
        available = "use available readings",
        threshold = paste0("NA above ", args$max_missing, "% missing readings"),
        "any NA \u2192 NA"
      ),
      if (!is.null(args$categorical) && args$categorical != "mode")
        paste0("\u00b7 categories: ", args$categorical) else "",
      if (!is.null(args$logical) && args$logical != "majority")
        paste0("\u00b7 logical: ", args$logical) else ""
    ) else if (step$type == "timezone")
    paste(
      args$timezone,
      if (args$method == "with") "(keep instant)" else
        paste0("(keep clock; repeated: ", args$ambiguous, ")")
    ) else if (step$type == "dst")
    "Device kept the UTC offset from each recording start" else if (
    step$type == "gaps"
  )
    paste(
      if (args$epoch == "dominant") "Dominant interval" else
        paste(args$seconds, "seconds"),
      if (args$bounds == "days") "\u00b7 complete local days" else
        "\u00b7 within recording",
      "\u00b7 added rows marked"
    ) else if (step$type == "variability")
    paste0(
      args$variable,
      " \u2192 NA when ",
      toupper(args$statistic),
      " of ",
      args$source,
      " is ",
      args$direction,
      " ",
      args$threshold,
      if (args$statistic == "cv") "%" else "",
      " \u00b7 ",
      args$length,
      " ",
      args$unit,
      ", ",
      args$alignment,
      " \u00b7 \u2265 ",
      args$coverage,
      "% window coverage \u00b7 unevaluable: ",
      if (identical(args$unevaluable, "missing")) "NA" else "keep"
    ) else if (step$type == "invalidate" && !is.null(args$source))
    paste0(
      args$variable,
      " \u2192 NA when ",
      args$source,
      if (args$outside) " does not match " else " matches ",
      if (args$mode == "range") paste(args$lower, "to", args$upper) else
        paste(args$values, collapse = ", "),
      " (source NA: ",
      if (args$missing == "missing") "mask target" else "keep target",
      ")"
    ) else if (step$type == "invalidate")
    paste(
      args$variable,
      "outside",
      args$lower,
      "to",
      args$upper,
      "\u2192 NA"
    ) else if (step$type == "calibrate")
    paste0(
      args$variable,
      " \u00d7 ",
      args$multiplier,
      " + (",
      args$offset,
      ")",
      if (nzchar(args$reference)) paste0(" \u00b7 ", args$reference) else ""
    ) else if (step$type == "transform")
    paste0(
      preparation_transform_formula(
        args$variable,
        args$operation,
        args$parameter,
        args$offset
      ),
      " (invalid: ",
      if (args$invalid == "missing") "NA" else "stop",
      ")",
      if (nzchar(args$reference)) paste0(" \u00b7 ", args$reference) else ""
    ) else if (step$type == "duration")
    paste(
      if (args$from == "start") "First" else "Last",
      args$length,
      switch(
        args$unit,
        calendar_days = "calendar days",
        elapsed_days = "24-hour days",
        hours = "hours",
        minutes = "minutes",
        seconds = "seconds"
      ),
      switch(
        args$scope,
        overall = "overall",
        source = "per source dataset",
        participant = "per participant",
        identity = "per participant and file",
        group = "per current group"
      )
    ) else if (step$type == "participants")
    paste(nrow(args$identities), "selected identities") else if (
    step$type == "variable"
  )
    paste(
      args$variable,
      if (args$outside) "outside" else "matching",
      if (args$mode == "range") paste(args$lower, "to", args$upper) else
        paste(args$values, collapse = ", "),
      paste0("(NA: ", args$missing, ")")
    ) else if (step$type == "photoperiod")
    paste(
      args$state,
      if (args$method == "existing") "from existing classification" else
        paste(
          "at",
          args$latitude,
          args$longitude,
          paste0("\u00b7 ", args$solar_depression, "\u00b0")
        ),
      paste0("(unknown: ", args$missing, ")")
    ) else if (step$type == "coverage")
    paste0(
      args$variable,
      if (is.null(args$mode))
        paste0(" \u2265 ", args$minimum, "% available") else
        paste0(
          if (args$mode == "available_duration") " \u2265 " else " \u2264 ",
          args$threshold,
          " ",
          if (args$unit == "days") "24-hour days" else args$unit,
          if (args$mode == "available_duration") " available" else " missing"
        ),
      if (args$basis %in% c("full_day", "observed_span"))
        " per local day (" else " per whole group (",
      if (args$basis %in% c("full_day", "full_group")) "including gaps" else
        "observed data",
      ")"
    ) else if (step$type %in% c("dates", "clock"))
    paste(
      args$start,
      "to",
      args$end,
      "\u00b7",
      if (identical(args$timezone, "native"))
        "local time entering this step" else args$timezone
    ) else
    paste(args$start, "to", args$end, args$timezone %||% "explicit UTC offsets")
  paste0(
    index,
    ". ",
    if (!step$enabled) "[disabled] " else "",
    map$label[match(step$type, map$type)],
    ": ",
    detail,
    if (!is.null(args$output))
      paste0(" \u2192 ", args$output, " (new variable)") else ""
  )
}
