# Typed selection arguments only. No user-supplied expressions are evaluated.
preparation_filter_arguments <- function(type, args) {
  expected <- switch(
    type,
    variable = c(
      "variable",
      "mode",
      "values",
      "lower",
      "upper",
      "outside",
      "missing"
    ),
    photoperiod = c(
      "method",
      "state",
      "latitude",
      "longitude",
      "solar_depression",
      "missing"
    ),
    coverage = if (is.list(args) && "mode" %in% names(args))
      c("variable", "threshold", "basis", "mode", "unit") else
      c("variable", "minimum", "basis")
  )
  if (
    !is.list(args) ||
      !setequal(names(args), expected) ||
      anyDuplicated(names(args))
  )
    abort_llw(
      "The filter arguments are incomplete or unsupported.",
      "preparation"
    )
  args <- args[expected]
  assert_serializable_value(args, "filter arguments")
  for (field in intersect(
    c("mode", "method", "state", "basis", "missing", "unit"),
    names(args)
  ))
    assert_scalar_string(args[[field]], field)
  if (
    type %in%
      c("variable", "photoperiod") &&
      !identical(length(args$missing), 1L)
  )
    abort_llw("Choose how to handle missing values.", "preparation")
  if (
    type %in%
      c("variable", "photoperiod") &&
      !args$missing %in% c("keep", "drop", "only")
  )
    abort_llw("Choose how to handle missing values.", "preparation")
  finite_scalar <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  if (type == "variable") {
    assert_scalar_string(args$variable, "variable")
    if (!args$mode %in% c("categories", "range"))
      abort_llw("Choose categories or a numeric interval.", "preparation")
    if (
      !is.logical(args$outside) ||
        length(args$outside) != 1L ||
        is.na(args$outside)
    )
      abort_llw(
        "Choose whether to keep matching or other values.",
        "preparation"
      )
    if (!is.character(args$values))
      abort_llw("Choose categorical values from the source.", "preparation")
    if (
      args$missing != "only" &&
        args$mode == "categories" &&
        !length(args$values)
    )
      abort_llw("Choose at least one category.", "preparation")
    if (
      args$missing != "only" &&
        args$mode == "range" &&
        (!finite_scalar(args$lower) ||
          !finite_scalar(args$upper) ||
          args$lower > args$upper)
    )
      abort_llw(
        "Enter a finite lower and upper bound, with lower no greater than upper.",
        "preparation"
      )
  } else if (type == "photoperiod") {
    if (
      !args$method %in% c("existing", "coordinates") ||
        !args$state %in% c("day", "night")
    )
      abort_llw(
        "Choose a day/night source and the period to keep.",
        "preparation"
      )
    if (
      args$method == "coordinates" &&
        (!finite_scalar(args$latitude) ||
          abs(args$latitude) > 90 ||
          !finite_scalar(args$longitude) ||
          abs(args$longitude) > 180 ||
          !finite_scalar(args$solar_depression) ||
          !args$solar_depression %in% c(0, 6, 12, 18))
    )
      abort_llw(
        "Enter valid latitude and longitude and choose a solar-depression definition.",
        "preparation"
      )
  } else {
    assert_scalar_string(args$variable, "coverage variable")
    if (
      !args$basis %in%
        c("full_day", "observed_span", "full_group", "observed_group")
    )
      abort_llw("Choose a supported coverage basis.", "preparation")
    if ("mode" %in% names(args)) {
      if (
        !args$mode %in% c("available_duration", "missing_duration") ||
          !args$unit %in% c("seconds", "minutes", "hours", "days")
      )
        abort_llw(
          "Choose an available or missing duration and a supported unit.",
          "preparation"
        )
      if (
        !finite_scalar(args$threshold) ||
          args$threshold < 0 ||
          (args$mode == "available_duration" && args$threshold == 0) ||
          !is.finite(args$threshold * preparation_coverage_seconds(args$unit))
      )
        abort_llw(
          "Enter a finite duration. Minimum available duration must be greater than zero; maximum missing duration may be zero.",
          "preparation"
        )
    } else if (
      !finite_scalar(args$minimum) || args$minimum < 0 || args$minimum > 100
    )
      abort_llw(
        "Choose a minimum from 0 to 100%.",
        "preparation"
      )
  }
  args
}

preparation_coverage_seconds <- function(unit)
  switch(unit, seconds = 1, minutes = 60, hours = 3600, days = 86400)

preparation_coverage_threshold <- function(args) {
  if (is.null(args$mode)) return(1 - args$minimum / 100)
  duration <- lubridate::dseconds(
    args$threshold * preparation_coverage_seconds(args$unit)
  )
  if (args$mode == "available_duration") -duration else duration
}

preparation_keep_missing <- function(keep, missing, policy) {
  if (policy == "only") return(missing)
  keep[missing] <- identical(policy, "keep")
  keep[is.na(keep)] <- FALSE
  keep
}

preparation_filter_variable <- function(data, args) {
  value <- data[[args$variable]]
  keep <- if (args$mode == "range")
    value >= args$lower & value <= args$upper else
    as.character(value) %in% args$values
  if (args$outside) keep <- !keep
  data[
    preparation_keep_missing(keep, is.na(value), args$missing),
    ,
    drop = FALSE
  ]
}

preparation_filter_photoperiod <- function(data, args) {
  state <- if (args$method == "existing")
    as.character(data$photoperiod.state) else {
    periods <- LightLogR::add_photoperiod(
      data.frame(Datetime = data$Datetime),
      coordinates = c(args$latitude, args$longitude),
      Datetime.colname = Datetime,
      solarDep = args$solar_depression
    )
    as.character(periods$photoperiod.state)
  }
  state[!state %in% c("day", "night")] <- NA_character_
  data[
    preparation_keep_missing(state == args$state, is.na(state), args$missing),
    ,
    drop = FALSE
  ]
}

preparation_filter_coverage <- function(data, args, context, version = 2L) {
  # Use the public LightLogR definition on a temporary analysis view. Return
  # only exact original rows, never the gap-expanded working table.
  keys <- if (identical(version, 1L)) context$identity_columns else
    dplyr::group_vars(data)
  by_date <- args$basis %in% c("full_day", "observed_span")
  handle_gaps <- args$basis %in% c("full_day", "full_group")
  work <- as.data.frame(data[unique(c(keys, "Datetime", args$variable))])
  work$.llw_coverage_row <- seq_len(nrow(data))
  if (length(keys))
    work <- dplyr::group_by(work, dplyr::across(dplyr::all_of(keys)))
  # Conservative expansion bound uses the shortest positive source interval.
  # This is a memory preflight, not an alternate coverage calculation.
  if (handle_gaps) {
    projected <- sum(vapply(
      dplyr::group_rows(work),
      function(index) {
        times <- sort(unique(as.numeric(work$Datetime[index])))
        gaps <- diff(times)
        if (!length(gaps)) return(1)
        (diff(range(times)) + 2 * 90000) / min(gaps[gaps > 0]) + 1
      },
      numeric(1)
    ))
    if (!is.finite(projected) || projected > max(100000, 4 * context$rows))
      abort_llw(
        "Coverage including gaps would create too many temporary rows. Use observed coverage or a shorter input recording.",
        "resource"
      )
  }
  variable <- rlang::sym(args$variable)
  kept <- LightLogR::remove_partial_data(
    work,
    Variable.colname = !!variable,
    threshold.missing = preparation_coverage_threshold(args),
    by.date = by_date,
    Datetime.colname = Datetime,
    show.result = FALSE,
    handle.gaps = handle_gaps
  )
  data[sort(unique(kept$.llw_coverage_row)), , drop = FALSE]
}

preparation_variable_context <- function(raw) {
  eligible <- vapply(
    raw,
    function(x) is.atomic(x) && !inherits(x, c("POSIXt", "Date")),
    logical(1)
  )
  lapply(raw[eligible], function(x) {
    is_numeric <- is.numeric(x) && !is.factor(x)
    values <- unique(x[!is.na(x)])
    levels <- sort(as.character(utils::head(values, 500L)))
    finite <- if (is_numeric) x[is.finite(x)] else numeric()
    list(
      numeric = is_numeric,
      categories = utils::head(levels, 500L),
      categories_complete = length(values) <= 500L,
      lower = if (length(finite)) min(finite) else 0,
      upper = if (length(finite)) max(finite) else 1,
      missing = sum(is.na(x))
    )
  })
}
