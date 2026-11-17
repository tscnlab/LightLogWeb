# Measurement changes are explicit arithmetic and assignment, never R text.
preparation_measurement_columns <- function(data) {
  numeric <- names(data)[vapply(
    data,
    function(x) is.numeric(x) && !is.object(x),
    logical(1)
  )]
  setdiff(
    numeric,
    c(
      "Id",
      "Datetime",
      preparation_identity_columns(data),
      append_provenance_columns(),
      dplyr::group_vars(data)
    )
  )
}

preparation_value_arguments <- function(type, args) {
  expected <- switch(
    type,
    invalidate = if (is.list(args) && "source" %in% names(args))
      c(
        "variable",
        "source",
        "mode",
        "values",
        "lower",
        "upper",
        "outside",
        "missing"
      ) else c("variable", "lower", "upper"),
    calibrate = c("variable", "multiplier", "offset", "reference"),
    transform = c("variable", "operation", "parameter", "invalid", "reference"),
    variability = c(
      "variable",
      "source",
      "length",
      "unit",
      "alignment",
      "statistic",
      "threshold",
      "direction",
      "coverage"
    )
  )
  if (type == "variability" && is.list(args) && "unevaluable" %in% names(args))
    expected <- c(expected, "unevaluable")
  if (
    type == "transform" &&
      is.list(args) &&
      isTRUE(args$operation %in% c("log_zero_inflated", "exp_zero_inflated"))
  )
    expected <- c(expected, "offset")
  if (is.list(args) && "output" %in% names(args))
    expected <- c(expected, "output")
  if (
    !is.list(args) ||
      !setequal(names(args), expected) ||
      anyDuplicated(names(args))
  )
    abort_llw(
      "The measurement step arguments are incomplete or unsupported.",
      "preparation"
    )
  args <- args[expected]
  assert_scalar_string(args$variable, "measurement variable")
  if ("output" %in% names(args))
    args$output <- preparation_output_name(args$output)
  if (type == "variability") return(preparation_variability_arguments(args))
  if (type == "invalidate" && "source" %in% names(args)) {
    assert_scalar_string(args$source, "condition source variable")
    if (
      !is.character(args$missing) ||
        length(args$missing) != 1L ||
        is.na(args$missing) ||
        !args$missing %in% c("keep", "missing")
    )
      abort_llw("Choose how to handle missing source values.", "preparation")
    # Reuse the typed condition contract; its row-selection policy is not used.
    preparation_filter_arguments(
      "variable",
      list(
        variable = args$source,
        mode = args$mode,
        values = args$values,
        lower = args$lower,
        upper = args$upper,
        outside = args$outside,
        missing = "drop"
      )
    )
    return(args)
  }
  if (type == "transform") {
    if (
      !is.character(args$operation) ||
        length(args$operation) != 1L ||
        is.na(args$operation) ||
        !args$operation %in%
          c(
            "log",
            "root",
            "power",
            "exp",
            "abs",
            "log_zero_inflated",
            "exp_zero_inflated"
          )
    )
      abort_llw("Choose a supported transformation.", "preparation")
    if (
      !is.character(args$invalid) ||
        length(args$invalid) != 1L ||
        is.na(args$invalid) ||
        !args$invalid %in% c("error", "missing")
    )
      abort_llw(
        "Choose whether invalid results stop the preview or become missing.",
        "preparation"
      )
    if (args$operation == "abs" && !is.null(args$parameter))
      abort_llw("Absolute value does not take a parameter.", "preparation")
  }
  numbers <- switch(
    type,
    invalidate = c("lower", "upper"),
    calibrate = c("multiplier", "offset"),
    transform = if (args$operation == "abs") character() else "parameter"
  )
  for (name in numbers) {
    value <- args[[name]]
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value))
      abort_llw(
        paste("Enter a finite number for", name, "in the measurement step."),
        "preparation"
      )
  }
  if (
    type == "transform" &&
      args$operation %in%
        c("log_zero_inflated", "exp_zero_inflated")
  ) {
    if (
      !is.numeric(args$offset) ||
        length(args$offset) != 1L ||
        !is.finite(args$offset) ||
        args$offset < 0
    )
      abort_llw("Use a finite, nonnegative offset.", "preparation")
    if (args$parameter <= 0 || args$parameter == 1)
      abort_llw("Use a positive base other than 1.", "preparation")
  }
  if (type == "invalidate" && args$lower > args$upper)
    abort_llw("The lower bound must not exceed the upper bound.", "preparation")
  if (
    type == "transform" &&
      args$operation %in% c("log", "exp") &&
      (args$parameter <= 0 || (args$operation == "log" && args$parameter == 1))
  )
    abort_llw(
      "Use a positive base; a logarithm base must also differ from 1.",
      "preparation"
    )
  if (
    type == "transform" &&
      args$operation == "root" &&
      (args$parameter < 1 ||
        args$parameter > .Machine$integer.max ||
        args$parameter != floor(args$parameter))
  )
    abort_llw("Root degree must be a positive whole number.", "preparation")
  if (type %in% c("calibrate", "transform")) {
    if (
      !is.character(args$reference) ||
        length(args$reference) != 1L ||
        is.na(args$reference) ||
        nchar(args$reference) > 1000L
    )
      abort_llw(
        "Use at most 1,000 characters for the transformation reference.",
        "preparation"
      )
    args$reference <- trimws(args$reference)
  }
  args
}

preparation_transform_values <- function(value, args) {
  p <- args$parameter
  transformed <- suppressWarnings(switch(
    args$operation,
    log = log(value, base = p),
    root = if (p %% 2 == 1) sign(value) * abs(value)^(1 / p) else value^(1 / p),
    power = value^p,
    exp = p^value,
    abs = abs(value),
    log_zero_inflated = LightLogR::log_zero_inflated(
      value,
      offset = args$offset,
      base = p
    ),
    exp_zero_inflated = LightLogR::exp_zero_inflated(
      value,
      offset = args$offset,
      base = p
    )
  ))
  invalid <- !is.na(value) & (!is.finite(value) | !is.finite(transformed))
  if (any(invalid) && args$invalid == "error") {
    guidance <- switch(
      args$operation,
      log = "Logarithms require positive finite readings.",
      root = "Even roots require nonnegative finite readings; odd roots allow negatives.",
      power = "This power is undefined or nonfinite for some readings.",
      exp = "This exponential overflows or has nonfinite input.",
      abs = "Absolute value requires finite readings.",
      log_zero_inflated = "Readings plus the offset must be positive and finite.",
      exp_zero_inflated = "The inverse transformation overflows or has nonfinite input."
    )
    abort_llw(
      paste(
        format(sum(invalid), big.mark = ","),
        "reading(s) cannot be transformed.",
        guidance,
        "Adjust earlier steps or parameters, or explicitly choose to mark invalid results missing."
      ),
      "preparation",
      subclass = "llw_preparation_value_error"
    )
  }
  transformed[invalid | is.na(value)] <- NA_real_
  transformed
}

preparation_transform_formula <- function(
  variable,
  operation,
  parameter,
  offset = NULL
) {
  p <- if (length(parameter) != 1L || is.na(parameter)) "?" else if (
    identical(parameter, exp(1))
  )
    "e" else as.character(parameter)
  switch(
    operation,
    log = paste0("log base ", p, " (", variable, ")"),
    root = paste0("root degree ", p, " (", variable, ")"),
    power = paste0(variable, " ^ ", p),
    exp = paste0(p, " ^ ", variable),
    abs = paste0("abs(", variable, ")"),
    log_zero_inflated = paste0(
      "log base ",
      p,
      " (",
      variable,
      " + ",
      offset,
      ")"
    ),
    exp_zero_inflated = paste0(p, " ^ ", variable, " - ", offset)
  )
}

preparation_change_values <- function(data, type, args) {
  if (!args$variable %in% preparation_measurement_columns(data))
    abort_llw(
      "Choose a numeric measurement, not an identifier, provenance or grouping column.",
      "preparation"
    )
  if (!is.null(args$output) && args$output %in% names(data))
    abort_llw(
      paste0(
        "The variable '",
        args$output,
        "' already exists. Choose a new name."
      ),
      "preparation"
    )
  value <- data[[args$variable]]
  if (type == "invalidate") {
    if (is.null(args$source)) {
      hit <- !is.na(value) & (value < args$lower | value > args$upper)
    } else {
      source <- data[[args$source]]
      if (
        is.null(source) ||
          !is.atomic(source) ||
          inherits(source, c("POSIXt", "Date"))
      )
        abort_llw(
          "Choose an available numeric or categorical condition source.",
          "preparation"
        )
      if (args$mode == "range" && (!is.numeric(source) || is.factor(source)))
        abort_llw(
          "Numeric intervals require a numeric source variable.",
          "preparation"
        )
      hit <- if (args$mode == "range")
        source >= args$lower & source <= args$upper else
        as.character(source) %in% args$values
      if (args$outside) hit <- !hit
      hit[is.na(source)] <- identical(args$missing, "missing")
      hit[is.na(hit)] <- FALSE
    }
    value[hit] <- NA
  } else if (type == "transform") {
    value <- preparation_transform_values(value, args)
  } else {
    if (any(!is.na(value) & !is.finite(value)))
      abort_llw(
        "Calibration needs finite readings. Mark infinite values as missing with a range step first.",
        "preparation",
        subclass = "llw_preparation_value_error"
      )
    calibrated <- value * args$multiplier + args$offset
    if (any(!is.na(value) & !is.finite(calibrated)))
      abort_llw(
        "Calibration would create infinite values. Use a smaller multiplier or offset.",
        "preparation",
        subclass = "llw_preparation_value_error"
      )
    value <- calibrated
  }
  data[[args$output %||% args$variable]] <- value
  data
}

preparation_values_differ <- function(before, after) {
  xor(is.na(before), is.na(after)) |
    (!is.na(before) & !is.na(after) & before != after)
}

# Count final cell differences only among rows that remain in the preview.
# A value changed twice is counted once; a later reversal is not a net change.
preparation_value_impact <- function(raw, prepared, source_rows, recipe) {
  steps <- Filter(
    function(step)
      step$enabled &&
        step$type %in% c("invalidate", "calibrate", "transform", "variability"),
    recipe$steps
  )
  columns <- unique(vapply(
    steps,
    function(step)
      step$parameters$arguments$output %||%
        step$parameters$arguments$variable,
    character(1)
  ))
  if (!length(columns))
    return(data.frame(
      variable = character(),
      changed = integer(),
      invalidated = integer(),
      created = integer()
    ))
  lineage <- preparation_variable_lineage(recipe)
  dplyr::bind_rows(lapply(columns, function(variable) {
    before <- raw[[lineage[[variable]]$source %||% variable]][source_rows]
    after <- prepared[[variable]]
    data.frame(
      variable = variable,
      changed = sum(preparation_values_differ(before, after)),
      invalidated = sum(!is.na(before) & is.na(after)),
      created = if (variable %in% names(raw)) 0L else length(after)
    )
  }))
}

# A step comparison shows its recipe prefix. The last measurement step in
# that prefix determines the plotted variable; filtering keeps the usual focus.
preparation_focus_context <- function(context, recipe) {
  # In the full app the dashboard owns the focus. A prefix before a derived
  # variable's creator shows its available source, with an explicit caption.
  if (!is.null(context$requested_variable)) {
    available <- preparation_recipe_context(context, recipe)$measurements
    variable <- if (context$requested_variable %in% available)
      preparation_follow_focus(context$requested_variable, recipe) else
      context$requested_source
    lineage <- preparation_variable_lineage(recipe)[[variable]]
    context$variable <- variable
    context$source_variable <- lineage$source %||% context$requested_source
    context$variable_label <- context$measurement_labels[[
      context$source_variable
    ]]
    if (!identical(variable, context$source_variable))
      context$variable_label <- paste(
        context$variable_label,
        "\u2192",
        variable
      )
    context$transformation_note <- paste(
      "Dashboard focus:",
      context$requested_variable,
      if (!context$requested_variable %in% available)
        paste("\u00b7 Not created at this point; showing", variable) else if (
        !identical(variable, context$requested_variable)
      )
        paste("\u00b7 Showing", variable, "at this point in the recipe") else ""
    )
    if (
      any(vapply(
        lineage$history,
        function(x) identical(x$type, "transform"),
        logical(1)
      ))
    ) {
      context$variable_label <- paste(variable, "(source / transformed values)")
      context$transformation_note <- paste(
        context$transformation_note,
        "\u00b7 Transformed values do not retain the source measurement unit."
      )
    }
    return(context)
  }
  values <- Filter(
    function(step)
      step$enabled &&
        step$type %in% c("invalidate", "calibrate", "transform", "variability"),
    recipe$steps
  )
  if (length(values)) {
    args <- utils::tail(values, 1L)[[1L]]$parameters$arguments
    context$variable <- args$output %||% args$variable
    lineage <- preparation_variable_lineage(recipe)[[context$variable]]
    context$source_variable <- lineage$source
    context$variable_label <- context$measurement_labels[[lineage$source]]
    transformed <- any(vapply(
      lineage$history,
      function(step) identical(step$type, "transform"),
      logical(1)
    ))
    if (!identical(context$variable, context$source_variable)) {
      context$transformation_note <- paste(
        "Grey:",
        context$source_variable,
        "\u00b7 Teal:",
        context$variable,
        "\u00b7 Metadata inherited from",
        context$source_variable,
        "with transformation history saved in the recipe."
      )
      context$variable_label <- paste(
        context$source_variable,
        "/",
        context$variable
      )
    }
    if (transformed) {
      context$transformation_note <- paste(
        context$transformation_note %||% "",
        "Source:",
        context$measurement_labels[[lineage$source]],
        "Preview: transformed values; do not interpret them in the original measurement unit."
      )
      context$variable_label <- paste(
        context$variable,
        "(source / transformed values)"
      )
    }
  }
  context
}

preparation_recipe_view <- function(
  raw,
  recipe,
  context,
  selection = list(),
  evaluated = NULL,
  budget = 1024^3
) {
  through <- as.character(selection$step %||% "all")
  only <- startsWith(through, "only:")
  comparison_label <- NULL
  if (!identical(through, "all")) {
    index <- suppressWarnings(as.integer(sub("^only:", "", through)))
    if (
      length(index) != 1L ||
        is.na(index) ||
        index < 1L ||
        index > length(recipe$steps)
    )
      abort_llw("Choose a step in the current recipe.", "preparation")
    recipe <- new_recipe(recipe$steps[seq_len(index)], recipe$version)
    evaluated <- NULL
  }
  plot_context <- preparation_focus_context(context, recipe)
  if (only) {
    preceding <- new_recipe(recipe$steps[seq_len(index - 1L)], recipe$version)
    step <- recipe$steps[[index]]
    reference <- if (step$type %in% c("variability", "aggregate"))
      preparation_variability_reference(raw) else NULL
    before <- evaluate_preparation(
      raw,
      preceding,
      context,
      details = TRUE,
      budget = budget,
      variability_reference = reference
    )
    raw <- before$data
    context <- preparation_recipe_context(context, preceding)
    context$columns <- names(raw)
    context$variables <- preparation_variable_context(raw)
    context$measurements <- preparation_measurement_columns(raw)
    context$timezone <- lubridate::tz(raw$Datetime)
    if (nrow(raw)) {
      context$start <- min(raw$Datetime)
      context$end <- max(raw$Datetime)
    }
    recipe <- new_recipe(list(step), recipe$version)
    evaluated <- evaluate_preparation(
      raw,
      recipe,
      context,
      details = TRUE,
      budget = budget,
      variability_reference = before$variability_reference
    )
    args <- step$parameters$arguments
    plot_context$source_variable <- if (plot_context$variable %in% names(raw))
      plot_context$variable else args$variable %||% plot_context$source_variable
    plot_context$timezone <- context$timezone
    comparison_label <- paste("Only step", index)
    plot_context$transformation_note <- paste(
      comparison_label,
      "\u00b7 Grey: before this step. Teal: after this step.",
      "Earlier steps still supply the data and variables. Recipe totals describe the whole recipe."
    )
    selection$step <- "1"
  }
  if (is.null(evaluated))
    evaluated <- evaluate_preparation(
      raw,
      recipe,
      context,
      details = TRUE,
      budget = budget
    )
  view <- if (isTRUE(evaluated$aggregated))
    preparation_time_day_view(
      raw,
      evaluated$data,
      evaluated$source_rows,
      evaluated$removed_at,
      plot_context,
      selection,
      source_membership = evaluated$source_membership,
      aggregated = TRUE
    ) else
    preparation_day_view(
      raw,
      evaluated$removed_at,
      plot_context,
      selection,
      prepared = evaluated$data,
      source_rows = evaluated$source_rows
    )
  view$selection_step <- through
  view$comparison_label <- comparison_label
  view$before_label <- if (only) paste("Before step", index) else "Source"
  view$after_label <- if (only) paste("After step", index) else "Preview"
  source_values <- raw[[
    plot_context$source_variable %||% plot_context$variable
  ]]
  preview_values <- evaluated$data[[plot_context$variable]]
  view$domain <- preparation_plot_domain(source_values, preview_values)
  view$positive_domain <- preparation_plot_domain(
    source_values,
    preview_values,
    positive = TRUE
  )
  view
}

preparation_view_step <- function(view) {
  view$selection_step %||%
    if (is.infinite(view$step)) "all" else as.character(view$step)
}

preparation_comparison_choices <- function(recipe) {
  indices <- seq_along(recipe$steps)
  if (!length(indices)) return(c("Whole recipe" = "all"))
  c(
    "Whole recipe" = "all",
    stats::setNames(as.character(indices), paste("Through step", indices)),
    stats::setNames(paste0("only:", indices), paste("Only step", indices))
  )
}
