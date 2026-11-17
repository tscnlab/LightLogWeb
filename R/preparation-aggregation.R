# Aggregation uses public LightLogR binning and handlers. Provenance and
# observed-duration diagnostics are separate from the values being aggregated.
preparation_aggregation_intervals <- function()
  c(
    "1 second" = "1 seconds",
    "5 seconds" = "5 seconds",
    "10 seconds" = "10 seconds",
    "30 seconds" = "30 seconds",
    "1 minute" = "1 minutes",
    "5 minutes" = "5 minutes",
    "10 minutes" = "10 minutes",
    "15 minutes" = "15 minutes",
    "30 minutes" = "30 minutes",
    "1 hour" = "1 hours",
    "2 hours" = "2 hours",
    "3 hours" = "3 hours",
    "6 hours" = "6 hours",
    "12 hours" = "12 hours",
    "1 local day" = "1 days"
  )

preparation_aggregation_arguments <- function(args) {
  required <- c("length", "unit", "numeric", "missing")
  defaults <- list(categorical = "mode", logical = "majority", max_missing = 20)
  if (
    !is.list(args) ||
      anyDuplicated(names(args)) ||
      !all(required %in% names(args)) ||
      any(!names(args) %in% c(required, names(defaults)))
  )
    abort_llw(
      "The aggregation arguments are incomplete or unsupported.",
      "preparation"
    )
  for (field in setdiff(names(defaults), names(args)))
    args[[field]] <- defaults[[field]]
  args <- args[c(required, names(defaults))]
  for (field in c("unit", "numeric", "missing", "categorical", "logical"))
    assert_scalar_string(args[[field]], field)
  if (
    !args$unit %in% c("seconds", "minutes", "hours", "days") ||
      !args$numeric %in% c("mean", "median", "min", "max", "sum") ||
      !args$missing %in% c("propagate", "available", "threshold") ||
      !args$categorical %in% c("mode", "first", "last") ||
      !args$logical %in% c("majority", "any", "all")
  )
    abort_llw("Choose supported aggregation settings.", "preparation")
  # A hidden, inactive limit must not prevent selecting another NA policy.
  # Canonical recipes retain no unused threshold state.
  if (args$missing != "threshold") args$max_missing <- defaults$max_missing
  if (
    !is.numeric(args$max_missing) ||
      length(args$max_missing) != 1L ||
      !is.finite(args$max_missing) ||
      args$max_missing < 0 ||
      args$max_missing > 100
  )
    abort_llw(
      "Enter a maximum missing percentage from 0 to 100.",
      "preparation"
    )
  if (
    !is.numeric(args$length) ||
      length(args$length) != 1L ||
      !is.finite(args$length) ||
      !preparation_aggregation_unit(args) %in%
        unname(preparation_aggregation_intervals())
  )
    abort_llw(
      "Choose an interval from the supported aggregation intervals.",
      "preparation"
    )
  args
}

preparation_aggregation_unit <- function(args) paste(args$length, args$unit)

# Find the next boundary of the same lubridate floor bins used by LightLogR.
# ceiling_date skips the first repeated hour in autumn. Only offset-changing
# bins need a short integer-second search for their first different floor label.
preparation_bin_end <- function(start, args) {
  unit <- preparation_aggregation_unit(args)
  unique_start <- unique(start)
  end <- lubridate::ceiling_date(unique_start + 0.001, unit)
  width <- args$length *
    switch(args$unit, seconds = 1, minutes = 60, hours = 3600, days = 86400)
  unusual <- which(as.numeric(end - unique_start, units = "secs") != width)
  for (i in unusual) {
    lower <- as.numeric(unique_start[[i]])
    upper <- as.numeric(end[[i]])
    while (upper - lower > 1) {
      middle <- floor((lower + upper) / 2)
      time <- as.POSIXct(
        middle,
        origin = "1970-01-01",
        tz = lubridate::tz(start)
      )
      if (
        as.numeric(lubridate::floor_date(time, unit)) ==
          as.numeric(unique_start[[i]])
      )
        lower <- middle else upper <- middle
    }
    end[[i]] <- as.POSIXct(
      upper,
      origin = "1970-01-01",
      tz = lubridate::tz(start)
    )
  }
  end[match(start, unique_start)]
}

# Union of non-overlapping forward epochs, capped by the next timestamp.
# Duplicates share one epoch and gaps never inherit the preceding value.
preparation_bin_coverage <- function(
  time,
  value,
  epoch,
  start,
  end,
  support_end = NULL
) {
  if (
    !length(time) || (is.null(support_end) && (!is.finite(epoch) || epoch <= 0))
  )
    return(rep(NA_real_, length(start)))
  order <- order(time)
  time <- as.numeric(time[order])
  finite <- is.finite(value[order])
  support_end <- if (is.null(support_end)) time + epoch else
    as.numeric(support_end[order])
  if (any(finite & (!is.finite(support_end) | support_end <= time)))
    return(rep(NA_real_, length(start)))
  unique_time <- unique(time)
  # Bounds belong to timestamps, including after earlier aggregation. Missing
  # readings contribute no duration even if their support is unknown.
  support_end <- support_end[match(unique_time, time)]
  finite <- tabulate(
    match(time[finite], unique_time),
    nbins = length(unique_time)
  ) >
    0L
  right <- pmin(support_end, c(unique_time[-1L], Inf))
  left <- unique_time[finite]
  right <- right[finite]
  if (!length(left)) return(rep(0, length(start)))
  cumulative <- c(0, cumsum(right - left))
  area <- function(bound) {
    index <- findInterval(as.numeric(bound), left)
    cumulative[index + 1L] -
      ifelse(index > 0L, pmax(0, right[pmax(1L, index)] - as.numeric(bound)), 0)
  }
  pmax(
    0,
    pmin(
      100,
      100 * (area(end) - area(start)) / as.numeric(end - start, units = "secs")
    )
  )
}

preparation_aggregate <- function(
  data,
  args,
  reference,
  variable = NULL,
  budget = 1024^3
) {
  args <- preparation_aggregation_arguments(args)
  preparation_assert_times(data)
  original_groups <- dplyr::group_vars(data)
  grouped <- preparation_time_groups(data)
  groups <- dplyr::group_vars(grouped)
  reserved <- intersect(c("Datetime.rounded", "group.indices2"), names(data))
  if (length(reserved))
    abort_llw(
      paste(
        "Aggregation reserves these LightLogR column names:",
        paste(reserved, collapse = ", ")
      ),
      "preparation"
    )
  supported <- vapply(
    data,
    function(x)
      is.numeric(x) ||
        is.character(x) ||
        is.logical(x) ||
        is.factor(x) ||
        inherits(x, c("POSIXct", "Duration", "hms")),
    logical(1)
  )
  unsupported <- setdiff(names(data)[!supported], groups)
  if (length(unsupported))
    abort_llw(
      paste(
        "Aggregation cannot preserve these column types:",
        paste(unsupported, collapse = ", "),
        ". Remove or convert them before aggregating."
      ),
      "preparation"
    )
  memory <- 64 * 1024^2 + 12 * as.numeric(utils::object.size(data))
  if (memory > budget)
    abort_llw(
      "Aggregation exceeds the session memory budget. Reduce the input rows first.",
      "resource"
    )
  empty <- list(
    data = data,
    membership = integer(),
    note = list(
      type = "aggregate",
      before = 0L,
      after = 0L,
      variable = variable,
      coverage = data.frame(),
      intended_seconds = 0,
      observed_seconds = 0,
      coverage_percent = NA_real_,
      working_bytes = memory
    )
  )
  if (!nrow(data)) return(empty)
  group_keys <- dplyr::group_keys(grouped)
  epochs <- dplyr::left_join(
    group_keys,
    reference$keys,
    by = reference$columns
  )$.llw_variability_epoch
  support_end <- NULL
  if (isTRUE(reference$unknown_support)) {
    support_end <- rep(NA_real_, nrow(data))
  } else if (!is.null(reference$windows)) {
    bounds <- reference$windows$keys
    field <- utils::tail(make.unique(c(names(bounds), ".llw_bin_end")), 1L)
    bounds[[field]] <- reference$windows$end
    support_end <- dplyr::left_join(
      dplyr::ungroup(data)[c(reference$columns, "Datetime")],
      bounds,
      by = c(reference$columns, "Datetime"),
      relationship = "many-to-one"
    )[[field]]
  }
  width <- args$length *
    switch(args$unit, seconds = 1, minutes = 60, hours = 3600, days = 86400)
  if (any(is.finite(epochs) & epochs > width + 1e-8))
    abort_llw(
      "Choose an aggregation interval at least as long as every recording's sampling interval entering this step. Aggregation does not upsample readings.",
      "preparation"
    )
  numeric_handler <- function(x) {
    # Duration is numeric to LightLogR. Summarise its seconds, then restore its
    # class consistently; median otherwise returns different classes for odd
    # and even counts, and a missing bin can fail to bind to a Duration bin.
    duration <- inherits(x, "Duration")
    if (duration) x <- as.numeric(x)
    exceeds_limit <- args$missing == "threshold" &&
      100 * sum(is.na(x)) > args$max_missing * length(x)
    if (args$missing %in% c("available", "threshold")) x <- x[!is.na(x)]
    value <- if (exceeds_limit || !length(x) || anyNA(x)) NA_real_ else
      switch(
        args$numeric,
        mean = mean(x),
        median = stats::median(x),
        min = min(x),
        max = max(x),
        sum = sum(x)
      )
    if (duration) lubridate::dseconds(value) else value
  }
  categorical_handler <- function(x) {
    if (args$categorical == "first") return(x[1L])
    if (args$categorical == "last") return(x[length(x)])
    # Retain LightLogR's mode, including its NA and tie handling.
    value <- names(which.max(table(x, useNA = "ifany")))
    if (is.factor(x)) factor(value) else value
  }
  logical_handler <- function(x) {
    if (!length(x) || anyNA(x)) return(NA)
    switch(args$logical, majority = mean(x) >= 0.5, any = any(x), all = all(x))
  }
  # First/last means chronological order, with original row order for ties.
  # Coverage and provenance below still use the original input row indices.
  summary_input <- if (args$categorical == "mode") grouped else
    grouped[order(grouped$Datetime, method = "radix"), , drop = FALSE]
  result <- LightLogR::aggregate_Datetime(
    summary_input,
    unit = preparation_aggregation_unit(args),
    type = "floor",
    numeric.handler = numeric_handler,
    character.handler = categorical_handler,
    factor.handler = categorical_handler,
    logical.handler = logical_handler
  )
  # Gap provenance means every contributing row was synthetic. Majority vote
  # would incorrectly relabel mixed bins as wholly synthetic observations.
  binned <- LightLogR::cut_Datetime(
    grouped,
    unit = preparation_aggregation_unit(args),
    type = "floor",
    group_by = TRUE
  )
  rows <- dplyr::group_rows(binned)
  keys <- dplyr::group_keys(binned)
  stopifnot(identical(
    as.numeric(result$Datetime),
    as.numeric(keys$Datetime.rounded)
  ))
  membership <- integer(nrow(data))
  for (i in seq_along(rows)) membership[rows[[i]]] <- i
  if ("is.implicit" %in% names(data))
    result$is.implicit <- vapply(
      rows,
      function(i) all(data$is.implicit[i] %in% TRUE),
      logical(1)
    )
  end <- preparation_bin_end(result$Datetime, args)
  numeric_columns <- setdiff(preparation_measurement_columns(data), groups)
  measured <- intersect(variable, numeric_columns)
  coverage <- matrix(
    NA_real_,
    nrow(result),
    length(measured),
    dimnames = list(NULL, measured)
  )
  group_rows <- dplyr::group_rows(grouped)
  intended_seconds <- 0
  for (g in seq_along(group_rows)) {
    input <- group_rows[[g]]
    output <- unique(membership[input])
    input_end <- if (!is.null(support_end))
      as.numeric(support_end[input]) else if (is.finite(epochs[[g]]))
      as.numeric(data$Datetime[input]) + epochs[[g]] else NULL
    if (!is.null(input_end))
      input_end <- pmin(input_end, as.numeric(end[membership[input]]))
    intended_seconds <- intended_seconds +
      as.numeric(
        max(end[output]) - min(result$Datetime[output]),
        units = "secs"
      )
    for (name in measured)
      coverage[output, name] <- preparation_bin_coverage(
        data$Datetime[input],
        data[[name]][input],
        epochs[[g]],
        result$Datetime[output],
        end[output],
        input_end
      )
  }
  diagnostics <- as.data.frame(result[groups])
  diagnostics$Start <- result$Datetime
  diagnostics$End <- end
  diagnostics$Input_rows <- lengths(rows)
  diagnostics$Missing_readings_percent <- if (length(measured) == 1L)
    vapply(
      rows,
      function(i) 100 * mean(is.na(data[[measured]][i])),
      numeric(1)
    ) else NA_real_
  if (args$missing == "threshold")
    diagnostics$Missingness_limit_exceeded <- diagnostics$Missing_readings_percent >
      args$max_missing
  diagnostics$Window_seconds <- as.numeric(
    end - result$Datetime,
    units = "secs"
  )
  diagnostics$Coverage_percent <- if (
    length(variable) == 1L && variable %in% measured
  )
    coverage[, variable] else NA_real_
  diagnostics$Observed_seconds <- diagnostics$Window_seconds *
    diagnostics$Coverage_percent /
    100
  observed_seconds <- sum(diagnostics$Observed_seconds)
  result <- dplyr::group_by(
    result,
    dplyr::across(dplyr::all_of(original_groups))
  )
  result <- result[, names(data), drop = FALSE]
  list(
    data = result,
    membership = membership,
    windows = list(
      keys = as.data.frame(result[c(groups, "Datetime")]),
      end = end
    ),
    note = list(
      type = "aggregate",
      before = nrow(data),
      after = nrow(result),
      variable = variable,
      coverage = utils::head(diagnostics, 100L),
      intended_seconds = intended_seconds,
      observed_seconds = observed_seconds,
      coverage_percent = if (intended_seconds > 0)
        100 * observed_seconds / intended_seconds else NA_real_,
      retimed_support = isTRUE(reference$unknown_support),
      working_bytes = memory,
      args = args
    )
  )
}
