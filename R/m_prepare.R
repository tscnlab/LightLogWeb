preparation_date_ui <- function(id, label, value, bounds) {
  empty <- length(value) != 1L || is.na(value) || !nzchar(value)
  input <- shiny::dateInput(
    id,
    label,
    value = if (empty) NULL else value,
    min = bounds[[1L]],
    max = bounds[[2L]],
    format = "yyyy-mm-dd",
    weekstart = 1,
    width = "100%"
  )
  # The bundled calendar otherwise falls back to today's month after a clear.
  # This native option changes the open month, never the selected date.
  calendar <- htmltools::tagQuery(input)$find("input")$addAttrs(
    `data-date-default-view-date` = as.character(bounds[[1L]])
  )
  if (empty) calendar$addAttrs(`data-initial-date` = "")
  calendar$allTags()
}

preparation_endpoint_ui <- function(ns, endpoint, label) {
  prefix <- paste0("instant_", endpoint)
  shiny::tags$fieldset(
    class = "llw-prepare-endpoint",
    shiny::tags$legend(class = "h6", label),
    shiny::tags$div(
      class = "row g-2",
      shiny::tags$div(
        class = "col-12 col-sm-6",
        shiny::uiOutput(ns(paste0(prefix, "_date_control")))
      ),
      shiny::tags$div(
        class = "col-12 col-sm-6",
        shiny::textInput(
          ns(paste0(prefix, "_time")),
          "Time",
          "00:00:00",
          placeholder = "HH:MM or HH:MM:SS",
          updateOn = "blur",
          width = "100%"
        )
      )
    ),
    shiny::selectInput(
      ns(paste0(prefix, "_zone")),
      "Interpret time in",
      choices = c(
        "Dataset native time zone" = "native",
        "Choose a time zone" = "custom"
      ),
      width = "100%"
    ),
    shiny::conditionalPanel(
      paste0("input.", prefix, "_zone === 'custom'"),
      ns = ns,
      shiny::selectizeInput(
        ns(paste0(prefix, "_timezone")),
        "Time zone",
        choices = stats::setNames(
          preparation_timezones(),
          preparation_timezones()
        ),
        selected = "UTC",
        width = "100%"
      )
    )
  )
}

preparation_step_header <- function(number, title, subtitle) {
  shiny::tags$div(
    class = "llw-prepare-step-heading",
    shiny::tags$span(class = "llw-prepare-step-number", number),
    shiny::tags$div(
      shiny::tags$h3(title),
      if (!is.null(subtitle)) shiny::tags$p(subtitle)
    )
  )
}

preparation_help <- function(title, ...) {
  shiny::tags$details(
    class = "llw-prepare-help",
    shiny::tags$summary(title),
    shiny::tags$div(...)
  )
}

preparation_form_defaults <- function(context) {
  start <- context$start
  end <- context$end
  measurement <- if (length(context$measurements)) {
    if (isTRUE(context$variable %in% context$measurements))
      context$variable else context$measurements[[1L]]
  } else ""
  if (is.na(start)) start <- as.POSIXct("2026-01-01", tz = "UTC")
  if (is.na(end)) end <- start
  # Keep the date picker inside the available local dates, even at midnight.
  next_end <- end + 1
  if (
    as.Date(next_end, tz = context$timezone) ==
      as.Date(end, tz = context$timezone)
  )
    end <- next_end
  list(
    step_type = "participants",
    time_timezone = context$timezone,
    time_method = "with",
    time_ambiguous = "error",
    gap_epoch = "dominant",
    gap_seconds = 60,
    gap_bounds = "recording",
    aggregate_interval = "15 minutes",
    aggregate_numeric = "mean",
    aggregate_missing = "propagate",
    aggregate_max_missing = 20,
    aggregate_categorical = "mode",
    aggregate_logical = "majority",
    participants = character(),
    date_start = format(start, "%Y-%m-%d", tz = context$timezone),
    date_end = format(end, "%Y-%m-%d", tz = context$timezone),
    instant_start_date = format(start, "%Y-%m-%d", tz = context$timezone),
    instant_start_time = preparation_time_input(start, context$timezone),
    instant_start_zone = "native",
    instant_start_timezone = context$timezone,
    instant_end_date = format(end, "%Y-%m-%d", tz = context$timezone),
    instant_end_time = preparation_time_input(end, context$timezone),
    instant_end_zone = "native",
    instant_end_timezone = context$timezone,
    clock_start = "22:00:00",
    clock_end = "06:00:00",
    duration_length = 1,
    duration_unit = "elapsed_days",
    duration_from = "start",
    duration_scope = "overall",
    variable_name = context$variable %||% names(context$variables)[[1L]],
    variable_mode = if (
      isTRUE(
        context$variables[[
          context$variable %||% names(context$variables)[[1L]]
        ]]$numeric
      )
    )
      "range" else "categories",
    variable_values = character(),
    variable_lower = context$variables[[
      context$variable %||% names(context$variables)[[1L]]
    ]]$lower,
    variable_upper = context$variables[[
      context$variable %||% names(context$variables)[[1L]]
    ]]$upper,
    variable_condition = "inside",
    variable_missing = "keep",
    photo_method = if (isTRUE(context$has_photoperiod)) "existing" else
      "coordinates",
    photo_state = "day",
    photo_latitude = NA_real_,
    photo_longitude = NA_real_,
    photo_solar = 6,
    photo_missing = "keep",
    coverage_variable = context$variable %||% names(context$variables)[[1L]],
    coverage_minimum = 80,
    coverage_basis = "full_day",
    coverage_mode = "percentage",
    coverage_duration = 16,
    coverage_unit = "hours",
    measurement_variable = measurement,
    measurement_output = measurement,
    invalidation_source = measurement,
    invalidation_mode = "range",
    invalidation_values = character(),
    invalidation_condition = "outside",
    invalidation_missing = "keep",
    variability_source = measurement,
    variability_length = 10,
    variability_unit = "minutes",
    variability_alignment = "centered",
    variability_statistic = "sd",
    variability_threshold = 1,
    variability_direction = "below",
    variability_coverage = 80,
    variability_unevaluable = "keep",
    measurement_lower = 0,
    measurement_upper = 100000,
    calibration_multiplier = 1,
    calibration_offset = 0,
    calibration_reference = "",
    measurement_operation = "calibrate",
    zero_operation = "log_zero_inflated",
    zero_base = 10,
    zero_offset = 0.1,
    transform_base = "e",
    transform_base_custom = 10,
    transform_degree = 2,
    transform_power = 2,
    transform_invalid = "error"
  )
}

# Numeric controls round-trip through JSON. Ignore only machine-scale changes
# in that transport; strings, choices, dates and missingness still match exactly.
preparation_same_form <- function(left, right) {
  if (identical(left, right)) return(TRUE)
  identical(names(left), names(right)) &&
    all(vapply(
      names(left),
      function(name) {
        a <- left[[name]]
        b <- right[[name]]
        if (identical(a, b)) TRUE else if (is.numeric(a) && is.numeric(b))
          isTRUE(all.equal(a, b, tolerance = 10 * .Machine$double.eps)) else
          identical(a, b)
      },
      logical(1)
    ))
}

# Browsing an operation does not edit the saved recipe. Changing a setting
# still marks the form dirty, including a setting on a different operation.
# Keep the full form comparison for server/browser acknowledgement transactions.
preparation_form_dirty <- function(current, baseline) {
  fields <- setdiff(names(current), "step_type")
  !is.null(baseline) &&
    !preparation_same_form(current[fields], baseline[fields])
}

preparation_merge_form_update <- function(actual, expected, origins) {
  if (preparation_same_form(actual, expected))
    return(list(form = expected, origins = list()))
  for (name in names(expected)) {
    acknowledged <- preparation_same_form(actual[name], expected[name])
    if (!acknowledged) {
      acknowledged_old_value <- any(vapply(
        origins,
        function(origin) preparation_same_form(actual[name], origin[name]),
        logical(1)
      ))
      if (acknowledged_old_value) next
      expected[name] <- actual[name]
    }
    # A value other than either side of an issued update is a user edit. Keep
    # it in the intended form, including if a dataset switch caches this draft.
    # Once this field has acknowledged (or been edited), retire its older
    # values. Returning to one of them is now a new edit, not a stale update.
    for (i in seq_along(origins))
      if (!identical(origins[[i]][[name]], expected[[name]]))
        origins[[i]][name] <- expected[name]
  }
  list(form = expected, origins = origins)
}

preparation_unused_variable_defaults <- function(values, context) {
  defaults <- preparation_form_defaults(context)
  if (
    !values$step_type %in%
      c("invalidate", "calibrate", "transform", "variability") &&
      !identical(values$measurement_variable, defaults$measurement_variable)
  ) {
    fields <- c("measurement_variable", "measurement_output")
    values[fields] <- defaults[fields]
  }
  if (
    values$step_type != "variable" &&
      !identical(values$variable_name, defaults$variable_name)
  ) {
    fields <- c(
      "variable_name",
      "variable_mode",
      "variable_values",
      "variable_lower",
      "variable_upper"
    )
    values[fields] <- defaults[fields]
  }
  if (values$step_type != "coverage")
    values$coverage_variable <- defaults$coverage_variable
  if (values$step_type != "variability")
    values$variability_source <- defaults$variability_source
  if (
    values$step_type != "invalidate" &&
      !identical(values$invalidation_source, defaults$invalidation_source)
  ) {
    fields <- c(
      "invalidation_source",
      "invalidation_mode",
      "invalidation_values",
      "invalidation_condition",
      "invalidation_missing",
      "measurement_lower",
      "measurement_upper"
    )
    values[fields] <- defaults[fields]
  }
  values
}

# Refresh a saved editor when its input recipe changes. This also updates
# inactive date controls before their new bounds can clear an old value.
preparation_editor_needs_reload <- function(before, after, id) {
  if (is.null(id)) return(FALSE)
  old <- match(id, vapply(before$steps, `[[`, character(1), "id"))
  new <- match(id, vapply(after$steps, `[[`, character(1), "id"))
  if (is.na(old) || is.na(new)) return(FALSE)
  old_step <- before$steps[[old]]
  new_step <- after$steps[[new]]
  # Enabling this step changes its output, not its settings or entering data.
  # Earlier steps still matter because they can change its variables or dates.
  old_step$enabled <- new_step$enabled <- NULL
  if (!identical(old_step, new_step)) return(TRUE)
  !identical(
    before$steps[seq_len(old - 1L)],
    after$steps[seq_len(new - 1L)]
  )
}

# A worker may refine the available span after DST correction, gap expansion
# or filtering. Refresh inactive date controls before their bounds redraw.
preparation_unused_time_defaults <- function(values, context) {
  defaults <- preparation_form_defaults(context)
  if (values$step_type != "dates") {
    fields <- c("date_start", "date_end")
    values[fields] <- defaults[fields]
  }
  if (values$step_type != "datetimes") {
    fields <- grep("^instant_", names(defaults), value = TRUE)
    values[fields] <- defaults[fields]
  }
  if (values$step_type != "timezone")
    values$time_timezone <- defaults$time_timezone
  values
}

# A newly created column is available to the next step, but never to its own
# creator. Finish that edit before using the new column as an editor default.
preparation_created_variable_form <- function(values, context) {
  defaults <- preparation_form_defaults(context)
  fields <- c(
    "measurement_variable",
    "measurement_output",
    "variability_source",
    "invalidation_source",
    "invalidation_mode",
    "invalidation_values",
    "invalidation_condition",
    "invalidation_missing",
    "measurement_lower",
    "measurement_upper",
    "variable_name",
    "variable_mode",
    "variable_values",
    "variable_lower",
    "variable_upper",
    "coverage_variable"
  )
  values[fields] <- defaults[fields]
  values
}

preparation_defer_preview <- function(callback)
  later::later(callback, delay = 0.15)

preparation_time_input <- function(instant, timezone) {
  local <- format(instant, "%Y-%m-%d %H:%M:%S", tz = timezone)
  repeated <- is.na(lubridate::force_tz(
    as.POSIXct(local, tz = "UTC"),
    tzone = timezone,
    roll_dst = "NA"
  ))
  format(instant, if (repeated) "%H:%M:%S %z" else "%H:%M:%S", tz = timezone)
}

preparation_date_limits <- function(context, timezone = context$timezone) {
  # These endpoints are already timestamps. Concatenating them first dispatches
  # through timestamp coercion and repeatedly validates their timezones.
  c(
    as.Date(context$start, tz = timezone),
    as.Date(context$end, tz = timezone)
  )
}

preparation_bounded_date <- function(
  value,
  context,
  timezone = context$timezone,
  label = "a date"
) {
  bounds <- preparation_date_limits(context, timezone)
  if (anyNA(bounds))
    abort_llw("Choose a dataset containing observations.", "preparation")
  guidance <- paste(
    "Choose",
    label,
    "within the available span:",
    bounds[[1L]],
    "to",
    bounds[[2L]],
    paste0("(", timezone, ").")
  )
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value))
  )
    abort_llw(
      paste("The date is missing or was cleared.", guidance),
      "preparation"
    )
  date <- preparation_date(value)
  if (date < bounds[[1L]] || date > bounds[[2L]])
    abort_llw(
      paste("The date is outside the available range.", guidance),
      "preparation"
    )
  value
}

preparation_transform_parameter <- function(form) {
  if (form$measurement_operation == "zero_inflated") return(form$zero_base)
  if (form$measurement_operation %in% c("log", "exp")) {
    if (!form$transform_base %in% c("e", "2", "10", "custom"))
      abort_llw(
        "Choose a base from the options or select Custom base.",
        "preparation"
      )
    return(switch(
      form$transform_base,
      e = exp(1),
      `2` = 2,
      `10` = 10,
      custom = form$transform_base_custom
    ))
  }
  switch(
    form$measurement_operation,
    root = form$transform_degree,
    power = form$transform_power,
    abs = NULL
  )
}

preparation_form_arguments <- function(form, context) {
  args <- switch(
    form$step_type,
    timezone = list(
      timezone = form$time_timezone,
      method = form$time_method,
      ambiguous = form$time_ambiguous
    ),
    dst = list(method = "fixed_start_offset"),
    gaps = list(
      epoch = form$gap_epoch,
      seconds = form$gap_seconds,
      bounds = form$gap_bounds
    ),
    aggregate = list(
      length = as.numeric(strsplit(form$aggregate_interval, " ", fixed = TRUE)[[
        1L
      ]][1L]),
      unit = strsplit(form$aggregate_interval, " ", fixed = TRUE)[[1L]][2L],
      numeric = form$aggregate_numeric,
      missing = form$aggregate_missing,
      max_missing = form$aggregate_max_missing,
      categorical = form$aggregate_categorical,
      logical = form$aggregate_logical
    ),
    variability = list(
      variable = form$measurement_variable,
      source = form$variability_source,
      length = form$variability_length,
      unit = form$variability_unit,
      alignment = form$variability_alignment,
      statistic = form$variability_statistic,
      threshold = form$variability_threshold,
      direction = form$variability_direction,
      coverage = form$variability_coverage,
      unevaluable = form$variability_unevaluable
    ),
    invalidate = list(
      variable = form$measurement_variable,
      source = form$invalidation_source,
      mode = form$invalidation_mode,
      values = as.character(form$invalidation_values %||% character()),
      lower = form$measurement_lower,
      upper = form$measurement_upper,
      outside = identical(form$invalidation_condition, "outside"),
      missing = form$invalidation_missing
    ),
    calibrate = list(
      variable = form$measurement_variable,
      multiplier = form$calibration_multiplier,
      offset = form$calibration_offset,
      reference = form$calibration_reference
    ),
    transform = list(
      variable = form$measurement_variable,
      operation = if (form$measurement_operation == "zero_inflated")
        form$zero_operation else form$measurement_operation,
      parameter = preparation_transform_parameter(form),
      invalid = form$transform_invalid,
      reference = form$calibration_reference
    ),
    variable = list(
      variable = form$variable_name,
      mode = form$variable_mode,
      values = form$variable_values,
      lower = form$variable_lower,
      upper = form$variable_upper,
      outside = form$variable_condition == "outside",
      missing = form$variable_missing
    ),
    photoperiod = list(
      method = form$photo_method,
      state = form$photo_state,
      latitude = form$photo_latitude,
      longitude = form$photo_longitude,
      solar_depression = form$photo_solar,
      missing = form$photo_missing
    ),
    coverage = if (form$coverage_mode != "percentage")
      list(
        variable = form$coverage_variable,
        threshold = form$coverage_duration,
        basis = form$coverage_basis,
        mode = form$coverage_mode,
        unit = form$coverage_unit
      ) else
      list(
        variable = form$coverage_variable,
        minimum = form$coverage_minimum,
        basis = form$coverage_basis
      ),
    participants = {
      positions <- match(form$participants, unname(context$choices))
      if (anyNA(positions))
        abort_llw(
          "Choose participants from the current dataset.",
          "preparation"
        )
      list(
        identities = context$identities[positions, , drop = FALSE],
        identity_columns = context$identity_columns
      )
    },
    dates = list(
      start = preparation_bounded_date(
        form$date_start,
        context,
        label = "the first date"
      ),
      end = preparation_bounded_date(
        form$date_end,
        context,
        label = "the last date"
      ),
      timezone = "native"
    ),
    datetimes = {
      args <- list()
      for (endpoint in c("start", "end")) {
        prefix <- paste0("instant_", endpoint)
        mode <- form[[paste0(prefix, "_zone")]]
        if (!mode %in% c("native", "custom"))
          abort_llw("Choose native time or a custom time zone.", "preparation")
        zone <- if (mode == "native") context$timezone else
          form[[paste0(prefix, "_timezone")]]
        if (!preparation_valid_timezone(zone))
          abort_llw(
            "Choose a time zone from the available Olson names.",
            "preparation"
          )
        date <- preparation_bounded_date(
          form[[paste0(prefix, "_date")]],
          context,
          zone,
          paste("the", endpoint, "date")
        )
        args[[endpoint]] <- preparation_local_instant(
          date,
          form[[paste0(prefix, "_time")]],
          zone
        )
        args[[paste0(endpoint, "_timezone")]] <- if (mode == "native")
          "native" else zone
      }
      args[c("start", "end", "start_timezone", "end_timezone")]
    },
    clock = list(
      start = form$clock_start,
      end = form$clock_end,
      timezone = "native",
      anchor = "start_date"
    ),
    duration = list(
      length = form$duration_length,
      unit = form$duration_unit,
      from = form$duration_from,
      scope = form$duration_scope
    ),
    abort_llw("Choose a supported operation.", "preparation")
  )
  if (
    form$step_type %in%
      c("invalidate", "calibrate", "transform", "variability")
  ) {
    if (
      form$step_type == "transform" &&
        form$measurement_operation == "zero_inflated"
    )
      args$offset <- form$zero_offset
    target <- preparation_output_name(form$measurement_output)
    if (!identical(target, args$variable)) args$output <- target
  }
  args
}

# dataset is an explicit reactive record. on_commit(candidate, expected) is the
# parent's atomic publication seam: it must reject a different ID/revision and
# return the committed record. The module never reaches into another module.
prepare_server <- function(
  id,
  dataset,
  runtime,
  on_commit,
  worker = preparation_task_worker,
  plot_settings = NULL,
  focus_variable = NULL,
  active = NULL,
  on_draft = NULL,
  color_mode = NULL
) {
  if (!shiny::is.reactive(dataset) || !is.function(on_commit))
    abort_llw(
      "Prepare requires a dataset reactive and a commit callback.",
      "validation"
    )
  if (!is.null(plot_settings) && !shiny::is.reactive(plot_settings))
    abort_llw(
      "Plot settings must be a dashboard settings reactive.",
      "validation"
    )
  for (value in list(focus_variable, active, color_mode))
    if (!is.null(value) && !shiny::is.reactive(value))
      abort_llw("Prepare display inputs must be reactive.", "validation")
  if (!is.null(on_draft) && !is.function(on_draft))
    abort_llw("Prepare draft callback must be a function.", "validation")
  shiny::moduleServer(id, function(input, output, session) {
    record_input <- dataset
    dataset <- shiny::reactiveVal(NULL)
    shiny::observe(
      {
        record <- record_input()
        # Saving recipe settings changes no applied measurements or metadata.
        # Keep their presentation stable while the draft observer persists them.
        if (identical(record$draft$kind, "preparation"))
          record["draft"] <- list(NULL)
        dataset(record)
      },
      priority = 100
    )
    context <- shiny::reactiveVal(NULL)
    recipe <- shiny::reactiveVal(new_recipe())
    preview <- shiny::reactiveVal(NULL)
    editing_id <- shiny::reactiveVal(NULL)
    form_base <- shiny::reactiveVal(NULL)
    loading_form <- shiny::reactiveVal(NULL)
    loading_origins <- list()
    notice_value <- shiny::reactiveVal("Select a dataset to begin.")
    notice_level <- shiny::reactiveVal("info")
    notice_title <- shiny::reactiveVal(NULL)
    notice <- function(text, level = "info", title = NULL) {
      if (missing(text)) return(notice_value())
      notice_level(level)
      notice_title(title)
      notice_value(text)
    }
    editor_error <- shiny::reactiveVal(NULL)
    generation <- shiny::reactiveVal(0L)
    pending <- shiny::reactiveVal(NULL)
    record_key <- NULL
    editor_cache <- list()
    restore_editor <- NULL
    requested_focus <- shiny::reactive(
      if (is.null(focus_variable)) NULL else focus_variable()
    )
    is_active <- shiny::reactive(
      if (is.null(active)) TRUE else isTRUE(active())
    )
    current_key <- function(record)
      if (is.null(record)) NULL else
        list(record$id, record$revision, record$raw_checksum, requested_focus())
    running_request <- NULL
    selection_after_edit <- NULL
    variable_form_name <- NULL
    invalidation_form_name <- NULL
    invalidation_control_name <- NULL
    recipe_history <- shiny::reactiveVal(list())
    recipe_render <- shiny::reactiveVal(0L)
    auto_preview <- shiny::reactiveVal(FALSE)
    view_loading <- shiny::reactiveVal(NULL)
    automatic_day <- shiny::reactiveVal(TRUE)
    last_view <- shiny::reactiveVal(NULL)
    variable_catalog <- shiny::reactiveVal(NULL)
    chain_changes <- shiny::reactiveVal(character())
    variable_control_name <- NULL
    control_choices <- list()
    update_choices <- function(
      name,
      choices,
      selected,
      fun = shiny::updateSelectizeInput
    ) {
      # Reuse the browser's catalogue when only the selection changes.
      # Numeric variables can have hundreds of category options.
      changed <- !identical(control_choices[[name]], choices)
      if (!changed && identical(input[[name]] %||% character(), selected))
        return(invisible(NULL))
      control_choices[[name]] <<- choices
      fun(
        session,
        name,
        choices = if (changed) choices else NULL,
        selected = selected
      )
    }
    axis_settings <- shiny::reactive(preparation_axis_settings(
      dataset(),
      if (is.null(plot_settings)) NULL else plot_settings()
    ))
    output$plot_axis_label <- shiny::renderText(paste(
      "Grey source \u00b7 Teal preview \u00b7",
      preparation_axis_label(axis_settings())
    ))

    form_type <- shiny::reactive(
      if (identical(input$preparation_category, "values")) {
        if (
          identical(input$value_type, "calibrate") &&
            !identical(
              input$measurement_operation %||% "calibrate",
              "calibrate"
            )
        )
          "transform" else input$value_type %||% "invalidate"
      } else if (identical(input$preparation_category, "sampling"))
        input$sampling_type %||% "timezone" else if (
        identical(input$step_type, "time")
      )
        input$time_mode %||% "dates" else input$step_type %||% "participants"
    )
    form <- shiny::reactive(list(
      step_type = form_type(),
      time_timezone = input$time_timezone %||% "UTC",
      time_method = input$time_method %||% "with",
      time_ambiguous = input$time_ambiguous %||% "error",
      gap_epoch = input$gap_epoch %||% "dominant",
      gap_seconds = as.numeric(input$gap_seconds %||% 60),
      gap_bounds = input$gap_bounds %||% "recording",
      aggregate_interval = input$aggregate_interval %||% "15 minutes",
      aggregate_numeric = input$aggregate_numeric %||% "mean",
      aggregate_missing = input$aggregate_missing %||% "propagate",
      aggregate_max_missing = as.numeric(
        input$aggregate_max_missing %||% NA_real_
      ),
      aggregate_categorical = input$aggregate_categorical %||% "mode",
      aggregate_logical = input$aggregate_logical %||% "majority",
      participants = input$participants %||% character(),
      date_start = as.character(input$date_start %||% ""),
      date_end = as.character(input$date_end %||% ""),
      instant_start_date = as.character(input$instant_start_date %||% ""),
      instant_start_time = input$instant_start_time %||% "",
      instant_start_zone = input$instant_start_zone %||% "native",
      instant_start_timezone = input$instant_start_timezone %||% "UTC",
      instant_end_date = as.character(input$instant_end_date %||% ""),
      instant_end_time = input$instant_end_time %||% "",
      instant_end_zone = input$instant_end_zone %||% "native",
      instant_end_timezone = input$instant_end_timezone %||% "UTC",
      clock_start = input$clock_start %||% "",
      clock_end = input$clock_end %||% "",
      duration_length = as.numeric(input$duration_length %||% NA_real_),
      duration_unit = input$duration_unit %||% "elapsed_days",
      duration_from = input$duration_from %||% "start",
      duration_scope = input$duration_scope %||% "overall",
      variable_name = input$variable_name %||% "",
      variable_mode = input$variable_mode %||% "range",
      variable_values = input$variable_values %||% character(),
      variable_lower = as.numeric(input$variable_lower %||% NA_real_),
      variable_upper = as.numeric(input$variable_upper %||% NA_real_),
      variable_condition = input$variable_condition %||% "inside",
      variable_missing = input$variable_missing %||% "keep",
      photo_method = input$photo_method %||% "coordinates",
      photo_state = input$photo_state %||% "day",
      photo_latitude = as.numeric(input$photo_latitude %||% NA_real_),
      photo_longitude = as.numeric(input$photo_longitude %||% NA_real_),
      photo_solar = as.numeric(input$photo_solar %||% 6),
      photo_missing = input$photo_missing %||% "keep",
      coverage_variable = input$coverage_variable %||% "",
      coverage_minimum = as.numeric(input$coverage_minimum %||% 80),
      coverage_basis = input$coverage_basis %||% "full_day",
      coverage_mode = input$coverage_mode %||% "percentage",
      coverage_duration = as.numeric(input$coverage_duration %||% 16),
      coverage_unit = input$coverage_unit %||% "hours",
      measurement_variable = input$measurement_variable %||% "",
      measurement_output = input$measurement_output %||% "",
      invalidation_source = input$invalidation_source %||% "",
      invalidation_mode = input$invalidation_mode %||% "range",
      invalidation_values = as.character(
        input$invalidation_values %||% character()
      ),
      invalidation_condition = input$invalidation_condition %||% "outside",
      invalidation_missing = input$invalidation_missing %||% "keep",
      variability_source = input$variability_source %||% "",
      variability_length = as.numeric(input$variability_length %||% 10),
      variability_unit = input$variability_unit %||% "minutes",
      variability_alignment = input$variability_alignment %||% "centered",
      variability_statistic = input$variability_statistic %||% "sd",
      variability_threshold = as.numeric(input$variability_threshold %||% 1),
      variability_direction = input$variability_direction %||% "below",
      variability_coverage = as.numeric(input$variability_coverage %||% 80),
      variability_unevaluable = input$variability_unevaluable %||% "keep",
      measurement_lower = as.numeric(input$measurement_lower %||% 0),
      measurement_upper = as.numeric(input$measurement_upper %||% 100000),
      calibration_multiplier = as.numeric(input$calibration_multiplier %||% 1),
      calibration_offset = as.numeric(input$calibration_offset %||% 0),
      calibration_reference = input$calibration_reference %||% "",
      measurement_operation = input$measurement_operation %||% "calibrate",
      zero_operation = input$zero_operation %||% "log_zero_inflated",
      zero_base = as.numeric(input$zero_base %||% 10),
      zero_offset = as.numeric(input$zero_offset %||% 0.1),
      transform_base = input$transform_base %||% "e",
      transform_base_custom = as.numeric(input$transform_base_custom %||% 10),
      transform_degree = as.numeric(input$transform_degree %||% 2),
      transform_power = as.numeric(input$transform_power %||% 2),
      transform_invalid = input$transform_invalid %||% "error"
    ))
    dirty <- shiny::reactiveVal(FALSE)
    shiny::observe(
      {
        # Server-issued control updates are one form transaction. Compare the
        # intended form while its browser acknowledgements are outstanding, then
        # the actual inputs. Unchanged acknowledgements must not invalidate every
        # output that depends on whether there are unsaved edits.
        actual <- form()
        expected <- loading_form()
        update <- if (is.null(expected))
          list(form = actual, origins = list()) else
          preparation_merge_form_update(actual, expected, loading_origins)
        current <- update$form
        loading_origins <<- update$origins
        if (!is.null(expected)) {
          if (preparation_same_form(actual, current)) {
            loading_form(NULL)
            loading_origins <<- list()
          } else if (!preparation_same_form(current, expected)) {
            loading_form(current)
          }
        }
        dirty(preparation_form_dirty(current, form_base()))
      },
      priority = 90
    )
    editor_recipe <- shiny::reactive({
      steps <- recipe()$steps
      position <- match(editing_id(), vapply(steps, `[[`, character(1), "id"))
      if (length(position) == 1L && !is.na(position))
        steps <- steps[seq_len(position - 1L)]
      new_recipe(steps)
    })
    editor_context <- shiny::reactive({
      shiny::req(context())
      result <- preparation_recipe_context(context(), editor_recipe())
      catalog <- preparation_cached_variables(
        variable_catalog(),
        context(),
        editor_recipe()
      )
      if (!is.null(catalog)) result$variables <- catalog
      snapshot <- variable_catalog()
      n <- length(editor_recipe()$steps)
      if (!is.null(catalog) && length(snapshot$time_after) >= n)
        result <- preparation_merge_time_context(
          result,
          snapshot$time_after[[n]]
        )
      result
    })
    editor_display_context <- shiny::reactiveVal(NULL)
    date_context <- shiny::reactiveVal(NULL)
    shiny::observe(
      {
        current <- editor_context()
        editor_display_context(current)
        # Date controls depend only on these bounds, not every recipe edit.
        date_context(current[c("start", "end", "timezone")])
      },
      priority = 10
    )
    new_form_defaults <- function() {
      preparation_form_defaults(preparation_new_step_context(
        context(),
        recipe(),
        variable_catalog()
      ))
    }
    output$chain_changes <- shiny::renderUI({
      changes <- chain_changes()
      if (!length(changes)) return(NULL)
      shiny::tags$div(
        class = "llw-prepare-notice",
        role = "status",
        shiny::tags$strong("Variable chain updated"),
        shiny::tags$ul(lapply(changes, shiny::tags$li))
      )
    })
    shiny::observe({
      current <- editor_display_context()
      shiny::req(current)
      expected <- loading_form() %||% shiny::isolate(form())
      for (name in c(
        "measurement_variable",
        "variability_source",
        "invalidation_source",
        "variable_name",
        "coverage_variable"
      )) {
        choices <- if (
          name %in% c("measurement_variable", "variability_source")
        )
          current$measurements else names(current$variables)
        selected <- expected[[name]]
        choices <- preparation_variable_choices(
          choices,
          selected,
          current$variable_labels
        )
        update_choices(
          name,
          choices = choices,
          selected = selected,
          fun = shiny::updateSelectInput
        )
      }
    })
    reserved_names <- shiny::reactive({
      steps <- Filter(
        function(s) !identical(s$id, editing_id()),
        recipe()$steps
      )
      union(context()$columns, preparation_output_names(new_recipe(steps)))
    })
    output_source <- NULL
    shiny::observeEvent(
      input$measurement_variable,
      {
        if (is.null(context()) || !is.null(loading_form())) return()
        if (!identical(output_source, input$measurement_variable)) {
          output_source <<- input$measurement_variable
          shiny::updateTextInput(
            session,
            "measurement_output",
            value = input$measurement_variable
          )
        }
      },
      ignoreInit = TRUE
    )
    output$measurement_output_help <- shiny::renderUI({
      name <- tryCatch(
        preparation_output_name(input$measurement_output),
        error = function(cnd) NULL
      )
      message <- if (is.null(name)) "Enter the variable to write to." else if (
        identical(name, input$measurement_variable)
      )
        paste(
          "Updates",
          name,
          "and records the operation in its history."
        ) else if (name %in% reserved_names())
        "That name already exists. Only the selected target can be overwritten; choose it on the left or enter a new name." else
        "The new variable inherits source metadata and records its transformation history."
      shiny::tags$p(class = "llw-prepare-hint", role = "status", message)
    })

    update_control <- function(fun, name, ...) {
      args <- list(...)
      desired <- args$selected %||% args$value
      if (
        is.null(args$choices) &&
          identical(as.character(input[[name]]), as.character(desired))
      )
        return(invisible(NULL))
      do.call(fun, c(list(session = session, inputId = name), args))
    }

    set_form <- function(values, id = NULL) {
      editor_error(NULL)
      output_source <<- values$measurement_variable
      variable_form_name <<- values$variable_name
      invalidation_form_name <<- values$invalidation_source
      values$duration_length <- as.numeric(values$duration_length)
      for (name in c(
        "gap_seconds",
        "variable_lower",
        "variable_upper",
        "photo_latitude",
        "photo_longitude",
        "photo_solar",
        "coverage_minimum",
        "coverage_duration",
        "measurement_lower",
        "measurement_upper",
        "variability_length",
        "variability_threshold",
        "variability_coverage",
        "calibration_multiplier",
        "calibration_offset",
        "transform_base_custom",
        "transform_degree",
        "transform_power",
        "zero_base",
        "zero_offset"
      ))
        values[[name]] <- as.numeric(values[[name]])
      if (context()$rows == 0L) {
        loading_form(NULL)
        form_base(NULL)
        editing_id(NULL)
        return(invisible(NULL))
      }
      if (
        identical(id, editing_id()) &&
          is.null(loading_form()) &&
          preparation_same_form(values, form_base()) &&
          preparation_same_form(values, form())
      )
        return(invisible(NULL))
      loading_origins <<- if (is.null(loading_form())) list(form()) else
        c(loading_origins, list(loading_form(), form()))
      loading_form(values)
      editing_id(id)
      form_base(values)
      measurement <- values$step_type %in%
        c("invalidate", "calibrate", "transform", "variability")
      category <- if (measurement) "values" else if (
        values$step_type %in% preparation_time_types()
      )
        "sampling" else "filter"
      if (!identical(input$preparation_category, category))
        bslib::nav_select(
          "preparation_category",
          selected = category,
          session = session
        )
      update_control(
        shiny::updateRadioButtons,
        "sampling_type",
        selected = if (values$step_type %in% preparation_time_types())
          values$step_type else "timezone"
      )
      update_control(
        shiny::updateRadioButtons,
        "value_type",
        selected = if (values$step_type == "transform") "calibrate" else if (
          measurement
        )
          values$step_type else "invalidate"
      )
      update_control(
        shiny::updateRadioButtons,
        "step_type",
        selected = if (values$step_type %in% c("dates", "datetimes", "clock"))
          "time" else if (
          measurement || values$step_type %in% preparation_time_types()
        )
          "participants" else values$step_type
      )
      update_control(
        shiny::updateSelectInput,
        "time_mode",
        selected = if (values$step_type %in% c("dates", "datetimes", "clock"))
          values$step_type else "dates"
      )
      extra_selects <- c(
        "time_timezone",
        "time_method",
        "time_ambiguous",
        "gap_epoch",
        "gap_bounds",
        "aggregate_interval",
        "aggregate_numeric",
        "aggregate_missing",
        "aggregate_categorical",
        "aggregate_logical",
        "variable_name",
        "variable_mode",
        "variable_condition",
        "variable_missing",
        "photo_method",
        "photo_state",
        "photo_solar",
        "photo_missing",
        "coverage_variable",
        "coverage_basis",
        "coverage_mode",
        "coverage_unit",
        "measurement_variable",
        "measurement_operation",
        "zero_operation",
        "variability_source",
        "invalidation_source",
        "invalidation_mode",
        "invalidation_condition",
        "invalidation_missing",
        "variability_unit",
        "variability_alignment",
        "variability_statistic",
        "variability_direction",
        "variability_unevaluable",
        "transform_base",
        "transform_invalid"
      )
      extra_numbers <- c(
        "gap_seconds",
        "aggregate_max_missing",
        "variable_lower",
        "variable_upper",
        "photo_latitude",
        "photo_longitude",
        "coverage_minimum",
        "coverage_duration",
        "measurement_lower",
        "measurement_upper",
        "variability_length",
        "variability_threshold",
        "variability_coverage",
        "calibration_multiplier",
        "calibration_offset",
        "transform_base_custom",
        "transform_degree",
        "transform_power",
        "zero_base",
        "zero_offset"
      )
      for (name in extra_selects)
        update_control(
          shiny::updateSelectInput,
          name,
          selected = as.character(values[[name]])
        )
      for (name in extra_numbers)
        update_control(shiny::updateNumericInput, name, value = values[[name]])
      update_choices(
        "invalidation_values",
        choices = union(
          editor_context()$variables[[values$invalidation_source]]$categories,
          values$invalidation_values
        ),
        selected = values$invalidation_values
      )
      update_choices(
        "variable_values",
        choices = union(
          editor_context()$variables[[values$variable_name]]$categories,
          values$variable_values
        ),
        selected = values$variable_values
      )

      update_control(
        shiny::updateSelectizeInput,
        "participants",
        selected = values$participants
      )
      zones <- c("instant_start_zone", "instant_end_zone")
      timezones <- c("instant_start_timezone", "instant_end_timezone")
      update_control(
        shiny::updateNumericInput,
        "duration_length",
        value = values$duration_length
      )
      update_control(
        shiny::updateSelectInput,
        "duration_unit",
        selected = values$duration_unit
      )
      update_control(
        shiny::updateRadioButtons,
        "duration_from",
        selected = values$duration_from
      )
      scopes <- c(
        "Overall" = "overall",
        "Per participant (Id)" = "participant",
        "Per participant and file" = "identity",
        "Per current group" = "group"
      )
      if (isTRUE(context()$source_identity_complete))
        scopes <- c(scopes, "Per source dataset" = "source")
      update_choices(
        "duration_scope",
        choices = scopes,
        selected = values$duration_scope,
        fun = shiny::updateSelectInput
      )
      for (name in zones)
        update_control(
          shiny::updateSelectInput,
          name,
          selected = values[[name]]
        )
      for (name in timezones)
        update_control(
          shiny::updateSelectizeInput,
          name,
          selected = values[[name]]
        )
      for (name in setdiff(
        names(values),
        c(
          "step_type",
          "participants",
          "date_start",
          "date_end",
          "instant_start_date",
          "instant_end_date",
          zones,
          timezones,
          "duration_length",
          "duration_unit",
          "duration_from",
          "duration_scope",
          extra_selects,
          extra_numbers,
          "variable_values",
          "invalidation_values"
        )
      ))
        update_control(shiny::updateTextInput, name, value = values[[name]])
    }
    date_names <- c(
      "date_start",
      "date_end",
      "instant_start_date",
      "instant_end_date"
    )
    date_views <- shiny::reactiveValues()
    shiny::observe(
      {
        shiny::req(context(), context()$rows > 0L)
        expected <- loading_form()
        values <- expected %||% shiny::isolate(form())
        span <- date_context()
        bounds_by_zone <- list()
        for (name in date_names) {
          zone <- span$timezone
          if (grepl("^instant_", name)) {
            prefix <- sub("_date$", "", name)
            mode <- if (is.null(expected)) input[[paste0(prefix, "_zone")]] else
              expected[[paste0(prefix, "_zone")]]
            if (identical(mode, "custom"))
              zone <- if (is.null(expected))
                input[[paste0(prefix, "_timezone")]] else
                expected[[paste0(prefix, "_timezone")]]
          }
          if (!preparation_valid_timezone(zone)) next
          if (is.null(bounds_by_zone[[zone]]))
            bounds_by_zone[[zone]] <- preparation_date_limits(span, zone)
          bounds <- bounds_by_zone[[zone]]
          value <- values[[name]]
          date <- suppressWarnings(as.Date(value, format = "%Y-%m-%d"))
          if (
            length(date) != 1L ||
              is.na(date) ||
              date < bounds[[1L]] ||
              date > bounds[[2L]]
          )
            value <- NA_character_
          label <- switch(
            name,
            date_start = "First date (included)",
            date_end = "Last date (included)",
            "Date"
          )
          # Loading acknowledgements and time-only edits do not replace a date
          # widget whose displayed value and calendar bounds are unchanged.
          date_views[[name]] <- list(
            label = label,
            value = value,
            bounds = bounds
          )
        }
      },
      priority = 5
    )
    for (date_name in date_names)
      local({
        name <- date_name
        output_name <- paste0(name, "_control")
        output[[output_name]] <- shiny::renderUI({
          view <- date_views[[name]]
          shiny::req(view)
          preparation_date_ui(
            session$ns(name),
            view$label,
            view$value,
            view$bounds
          )
        })
        shiny::outputOptions(output, output_name, suspendWhenHidden = FALSE)
      })
    output$native_timezone <- shiny::renderText({
      shiny::req(context())
      paste("Time zone entering this step:", editor_display_context()$timezone)
    })
    output$time_entering_timezone <- shiny::renderText({
      shiny::req(context())
      editor_display_context()$timezone
    })
    output$operation_docs <- shiny::renderUI({
      preparation_operation_documentation(list(
        step_type = form_type(),
        photo_method = input$photo_method,
        measurement_operation = input$measurement_operation,
        zero_operation = input$zero_operation,
        time_method = input$time_method,
        gap_bounds = input$gap_bounds
      ))
    })
    output$time_diagnostics <- shiny::renderUI({
      shiny::req(context())
      diag <- context()$time_diagnostics
      shiny::tags$p(
        class = "llw-prepare-hint",
        paste(
          "Source diagnostics:",
          sum(diag$Offset_changes > 0),
          "recordings span an offset change;",
          sum(diag$Duplicate_instants),
          "duplicate instants;",
          sum(diag$Repeated_clocks),
          "repeated local clocks.",
          "Diagnosis alone does not establish a device error."
        )
      )
    })
    output$duration_scope_help <- shiny::renderText({
      shiny::req(context())
      switch(
        input$duration_scope %||% "overall",
        overall = "Use one start or end for all observations reaching this step.",
        source = "Count from each source dataset's own start or end, including all its participants and files.",
        participant = "Count from each participant's own start or end, combining their observations across source datasets and files.",
        identity = if (context()$file_identity_complete)
          "Count from each participant's own start or end within each source file. Files from different source datasets stay separate." else
          "Count separately using the participant, source and file information available here. Missing file information can combine several recordings into one.",
        group = if (length(context()$group_columns))
          paste0(
            "Current groups: ",
            paste(context()$group_columns, collapse = " + "),
            ". Count from each group's own first or last observation reaching this step."
          ) else
          "No grouping is set; all rows form one group, so this is the same as Overall."
      )
    })
    output$coverage_basis_help <- shiny::renderText({
      shiny::req(context())
      groups <- context()$group_columns
      paste(
        if (length(groups))
          paste0("Current groups: ", paste(groups, collapse = " + "), ".") else
          "No grouping is set; all rows form one group.",
        if (
          (input$coverage_basis %||% "full_day") %in%
            c("full_day", "observed_span")
        )
          "Each group's entire local day is removed if it fails the threshold." else
          "The entire current group is removed if it fails the threshold."
      )
    })
    invalidate_preview <- function(cancel_pending = TRUE) {
      generation(generation() + 1L)
      preview(NULL)
      # A deferred request is a snapshot of the old form/recipe. Do not try to
      # launch it after a later edit has invalidated its generation.
      if (
        cancel_pending && !is.null(pending()) && pending()$action != "inspect"
      )
        pending(NULL)
    }
    shiny::observeEvent(
      form(),
      {
        if (is.null(loading_form()) && dirty()) {
          editor_error(NULL)
          invalidate_preview()
          notice(
            "Form edits are not saved. Saving the step updates the preview automatically."
          )
        }
      },
      ignoreInit = TRUE
    )
    safely <- function(code, editor = FALSE) {
      if (editor) editor_error(NULL)
      tryCatch(force(code), error = function(cnd) {
        notice(llw_public_message(cnd), "error", "Check the settings")
        if (editor) editor_error(llw_public_message(cnd))
        invisible(FALSE)
      })
    }
    output$editor_error <- shiny::renderText(editor_error())

    publish_view <- function(view) {
      expected <- list(
        id = as.character(view$id %||% ""),
        day = as.character(view$day %||% ""),
        step = preparation_view_step(view)
      )
      last_view(expected)
      selected <- list(
        id = input$plot_id %||% "",
        day = input$plot_day %||% "",
        step = input$plot_step %||% "all"
      )
      # Unchanged browser values do not send another input event.
      view_loading(if (identical(selected, expected)) NULL else expected)
      steps <- recipe()$steps
      choices <- preparation_comparison_choices(recipe())
      shiny::updateSelectInput(
        session,
        "plot_step",
        choices = choices,
        selected = expected$step
      )
      shiny::updateSelectizeInput(
        session,
        "plot_id",
        choices = unique(view$days$Id),
        selected = expected$id
      )
      shiny::updateSelectizeInput(
        session,
        "plot_day",
        choices = view$days$Day[view$days$Id == expected$id],
        selected = expected$day
      )
    }
    handle_result <- function(value, spec) {
      if (inherits(value$error, "llw_error")) stop(value$error)
      if (value$action == "inspect") {
        context(value$context)
        variable_catalog(value$catalogs)
        saved <- restore_editor
        restore_editor <<- NULL
        restoring <- !is.null(saved) &&
          (isTRUE(saved$dirty) || !is.null(saved$editing_id))
        values <- if (restoring) saved$form else new_form_defaults()
        # Catalogue refreshes must use the intended editor selection. Sending
        # source defaults first can overwrite an already-correct saved input
        # whose subsequent set_form() update is deliberately skipped.
        shiny::updateSelectizeInput(
          session,
          "participants",
          choices = value$context$choices,
          selected = values$participants,
          server = TRUE
        )
        # The editor catalogue observer updates all variable choices using the
        # same loading_form transaction, including derived measurements.
        photo_choices <- c("Derive from coordinates" = "coordinates")
        if (value$context$has_photoperiod)
          photo_choices <- c(
            "Use existing photoperiod.state" = "existing",
            photo_choices
          )
        shiny::updateSelectInput(
          session,
          "photo_method",
          choices = photo_choices,
          selected = values$photo_method
        )
        chain_changes(character())
        if (restoring) {
          set_form(values, saved$editing_id)
          form_base(saved$base)
        } else set_form(values)
        if (!is.null(saved)) {
          recipe_history(saved$history)
          automatic_day(saved$automatic_day)
          last_view(saved$view)
          shiny::updateCheckboxInput(
            session,
            "automatic_day",
            value = saved$automatic_day
          )
        }
        if (!is.null(active)) auto_preview(TRUE)
        notice(
          if (value$context$rows == 0L)
            "This source has no observations. Choose a dataset containing observations before preparing data." else
            "Ready. Save a step to preview the complete recipe automatically."
        )
      } else if (value$action == "preview") {
        current_form <- shiny::isolate(form())
        variable_catalog(preparation_catalog_snapshot(
          value$context,
          value$candidate$recipe,
          value$comparison$variables_after,
          value$comparison$time_after
        ))
        updated_form <- preparation_unused_time_defaults(
          current_form,
          editor_context()
        )
        if (!preparation_same_form(updated_form, current_form)) {
          saved_type <- form_base()$step_type
          set_form(updated_form, editing_id())
          baseline <- form_base()
          baseline$step_type <- saved_type
          form_base(baseline)
        }
        preview(value)
        publish_view(value$comparison$view)
        notice(
          if (value$comparison$before$rows == 0L)
            "This source has no observations. Choose a dataset containing observations before preparing data." else if (
            value$comparison$after$rows == 0L
          )
            "Preview retains no rows. Edit, disable or remove a step before applying." else
            "Preview is ready. Review the comparison before Apply."
        )
      } else if (value$action == "view") {
        current <- preview()
        if (is.null(current)) return(invisible(NULL))
        current$comparison$view <- value$view
        preview(current)
        publish_view(value$view)
        notice(
          "Day view updated. Recipe counts still describe all observations."
        )
      } else {
        on_commit(value$candidate, value$binding)
        notice(
          if (value$action == "reset")
            "Reset restored the exact source data." else
            "Undo restored the previous applied snapshot."
        )
      }
      if (value$action %in% c("inspect", "preview")) {
        # A full refresh replaces data/plot snapshots and leaves temporary
        # objects behind. Reclaim them after rendering, at this heavy-work
        # boundary, instead of letting a major collection stall the next edit.
        session$onFlushed(
          function() invisible(gc(verbose = FALSE)),
          once = TRUE
        )
      }
    }
    lookup <- function(dataset_id) {
      record <- shiny::isolate(dataset())
      if (is.null(record) || !identical(record$id, dataset_id)) return(NULL)
      if (
        !is.null(running_request) &&
          running_request$action != "inspect" &&
          !identical(running_request$generation, shiny::isolate(generation()))
      )
        return(NULL)
      record$revision
    }
    task <- if (identical(worker, preparation_task_worker)) {
      new_long_task(
        preparation_task_response,
        "preparation",
        runtime,
        revision_lookup = lookup,
        on_result = handle_result
      )
    } else
      new_long_task(
        worker,
        "preparation",
        runtime,
        revision_lookup = lookup,
        on_result = handle_result
      )
    busy <- shiny::reactive(
      task$state() %in%
        c("queued", "running", "finalizing") ||
        identical(task$extended_task$status(), "running")
    )

    enqueue <- function(action, replace = FALSE, view = NULL, defer = FALSE) {
      record <- dataset()
      if (is.null(record)) return(invisible(FALSE))
      if (
        (busy() || !is.null(pending())) &&
          !replace &&
          task$state() != "cancelled"
      ) {
        notice(
          "Preparation is already running. Cancel it before submitting another action."
        )
        return(invisible(FALSE))
      }
      if (replace && busy()) task$cancel()
      preparation_check_budget(record, runtime$profile$cache_max_bytes)
      if (action == "preview") {
        if (is.null(context()) || !is.null(loading_form()))
          abort_llw(
            "Wait for the dataset controls to finish loading.",
            "preparation"
          )
        if (dirty())
          abort_llw(
            "Save the step or discard form edits before Preview.",
            "preparation"
          )
        if (
          preparation_memory_estimate(length(record$raw_payload)) >
            runtime$profile$cache_max_bytes / 2 &&
            !isTRUE(input$ack_cost)
        )
          abort_llw(
            "This preview is estimated to use more than half the preparation budget. Review the memory estimate and acknowledge it before Preview.",
            "resource"
          )
        validate_preparation_recipe(recipe(), context())
        if (is.null(view) && !automatic_day()) {
          view <- last_view()
          if (
            !is.null(view) &&
              view$step != "all" &&
              (!view$step %in% unname(preparation_comparison_choices(recipe())))
          )
            view$step <- "all"
        }
        invalidate_preview()
      }
      if (action %in% c("reset", "undo")) invalidate_preview()
      pending(list(
        action = action,
        record = record,
        recipe = recipe(),
        budget = runtime$profile$cache_max_bytes,
        generation = generation(),
        view = view,
        defer = defer,
        focus_variable = requested_focus()
      ))
      notice(
        if (task$state() == "cancelled" && busy())
          "Retry queued. Waiting for the cancelled worker to release." else
          paste("Queued:", action, "from immutable source data.")
      )
      invisible(TRUE)
    }
    # Starting after the UI flush makes queued feedback truthful even in the
    # explicitly labelled synchronous fallback. A cancelled worker must drain.
    shiny::observe(
      {
        request <- pending()
        if (is.null(request) || busy()) return()
        session$onFlushed(
          function() {
            run <- function() {
              if (session$isClosed()) return()
              shiny::withReactiveDomain(
                session,
                shiny::isolate({
                  if (!identical(request, pending()) || busy()) return()
                  if (
                    request$action != "inspect" &&
                      !identical(request$generation, generation())
                  ) {
                    pending(NULL)
                    return()
                  }
                  running_request <<- request
                  accepted <- task$invoke(
                    request,
                    dataset_id = request$record$id
                  )
                  # A rejected invocation is terminal, too. Leaving it pending
                  # retries the same invalid snapshot on every reactive flush.
                  if (!identical(accepted, FALSE) || task$state() == "error")
                    pending(NULL)
                })
              )
            }
            # Coalesce quick recipe edits before starting their worker. Pending
            # feedback is immediate; replacement, Cancel and navigation invalidate
            # the captured request. An explicit Preview starts without this delay.
            if (isTRUE(request$defer)) preparation_defer_preview(run) else run()
          },
          once = TRUE
        )
      },
      priority = -10
    )

    shiny::observeEvent(
      list(record_input(), is_active(), requested_focus()),
      {
        record <- record_input()
        key <- current_key(record)
        if (identical(key, record_key)) {
          if (
            is_active() &&
              !is.null(record) &&
              is.null(context()) &&
              !busy() &&
              is.null(pending())
          )
            safely(enqueue("inspect", replace = TRUE))
          return()
        }
        if (!is.null(record_key) && !is.null(context())) {
          editor_cache[[record_key[[1L]]]] <<- list(
            key = record_key,
            recipe = recipe(),
            form = loading_form() %||% form(),
            base = form_base(),
            editing_id = editing_id(),
            dirty = dirty(),
            history = recipe_history(),
            automatic_day = automatic_day(),
            view = last_view()
          )
          # This cache has no datasets or preview results. Saved recipes also live
          # in records, so evicting an old form never loses its saved steps.
          editor_cache <<- utils::tail(editor_cache, 20L)
        }
        saved <- if (is.null(record)) NULL else editor_cache[[record$id]]
        keep_saved <- !is.null(saved) &&
          identical(saved$key[[3L]], record$raw_checksum) &&
          (identical(saved$key[[2L]], record$revision) ||
            identical(record$draft$kind, "preparation"))
        restore_editor <<- if (keep_saved) saved else NULL
        record_key <<- key
        invalidate_preview()
        context(NULL)
        variable_catalog(NULL)
        variable_control_name <<- NULL
        form_base(NULL)
        loading_form(NULL)
        editing_id(NULL)
        editor_error(NULL)
        selection_after_edit <<- NULL
        shiny::updateCheckboxInput(session, "ack_cost", value = FALSE)
        recipe_history(list())
        auto_preview(FALSE)
        view_loading(NULL)
        last_view(NULL)
        automatic_day(TRUE)
        shiny::updateCheckboxInput(session, "automatic_day", value = TRUE)
        recipe(
          if (keep_saved) saved$recipe else preparation_saved_recipe(record)
        )
        pending(NULL)
        if (busy()) task$cancel()
        if (is.null(record)) notice("Select a dataset to begin.") else if (
          is_active()
        )
          safely(enqueue("inspect", replace = TRUE))
      },
      ignoreNULL = FALSE
    )
    shiny::observe({
      if (is.null(on_draft) || is.null(context())) return()
      record <- record_input()
      if (!identical(context()$dataset_id, record$id)) return()
      # A reset or external revision must be loaded before this editor can
      # persist again. Otherwise the old form can recreate a just-cleared draft.
      if (!identical(current_key(record), record_key)) return()
      saved <- recipe()
      if (identical(saved, preparation_saved_recipe(record))) return()
      safely(on_draft(saved, preparation_binding(record)))
    })

    output$recipe_choices <- shiny::renderUI({
      recipe_render()
      steps <- recipe()$steps
      ids <- vapply(steps, `[[`, character(1), "id")
      value <- preview()
      applied <- dataset()$recipe %||% new_recipe()
      counts <- if (preparation_preview_matches(value, dataset(), recipe()))
        value$comparison$steps else NULL
      labels <- lapply(seq_along(steps), function(i) {
        count <- if (!is.null(counts))
          counts[counts$id == ids[[i]], , drop = FALSE] else NULL
        state <- preparation_recipe_status(recipe(), applied, i)
        shiny::tags$span(
          class = "llw-prepare-recipe-label",
          shiny::tags$span(
            class = "llw-prepare-recipe-title",
            preparation_step_label(steps[[i]], i)
          ),
          shiny::tags$span(
            class = paste0("llw-prepare-step-state is-", tolower(state)),
            state
          ),
          shiny::tags$small(
            if (
              !is.null(count) &&
                nrow(count) &&
                steps[[i]]$type %in%
                  c("invalidate", "calibrate", "transform", "variability")
            ) {
              if (count$created > 0L)
                sprintf(
                  "%s values created \u00b7 %s differ from input \u00b7 %s newly missing \u00b7 0 rows removed",
                  format(count$created, big.mark = ","),
                  format(count$changed, big.mark = ","),
                  format(count$invalidated, big.mark = ",")
                ) else
                sprintf(
                  "%s values changed \u00b7 %s newly missing \u00b7 %.1f%% of entering rows \u00b7 %.1f%% of source rows \u00b7 0 rows removed",
                  format(count$changed, big.mark = ","),
                  format(count$invalidated, big.mark = ","),
                  if (count$before) 100 * count$changed / count$before else 0,
                  if (context()$rows) 100 * count$changed / context()$rows else
                    0
                )
            } else if (
              !is.null(count) && nrow(count) && steps[[i]]$type == "aggregate"
            )
              paste(
                count$before,
                "input rows \u2192",
                count$after,
                "bins \u00b7",
                count$combined,
                "rows combined \u00b7 0 rows filtered out"
              ) else if (
              !is.null(count) &&
                nrow(count) &&
                steps[[i]]$type %in% preparation_time_types()
            )
              paste(
                count$added,
                "gap rows added \u00b7",
                count$retimed,
                "timestamps shifted \u00b7",
                count$removed,
                "rows removed"
              ) else if (!is.null(count) && nrow(count))
              sprintf(
                "%s removed \u00b7 %.1f%% of previous step \u00b7 %.1f%% of source \u00b7 %.1f%% removed in total",
                format(count$removed, big.mark = ","),
                count$previous_percent,
                count$source_percent,
                count$cumulative_percent
              ) else "Save and preview to calculate step impact.",
            if (
              !is.null(count) && nrow(count) && steps[[i]]$type == "variability"
            )
              paste0(
                " \u00b7 ",
                format(count$unevaluable_windows, big.mark = ","),
                " unevaluable windows (",
                if (
                  identical(
                    steps[[i]]$parameters$arguments$unevaluable,
                    "missing"
                  )
                )
                  "target set to NA" else "target unchanged",
                ")"
              )
          )
        )
      })
      selected <- selection_after_edit %||% shiny::isolate(input$selected_step)
      selection_after_edit <<- NULL
      if (!length(selected) || !selected %in% ids)
        selected <- if (length(ids)) ids[[1L]] else character()
      preparation_recipe_choices_ui(
        session$ns,
        labels,
        ids,
        selected,
        disabled = dirty()
      )
    })

    shiny::observeEvent(
      list(input$variable_name, editor_display_context()),
      {
        if (identical(input$variable_name, variable_form_name)) {
          variable_form_name <<- NULL
          variable_control_name <<- input$variable_name
          return()
        }
        if (is.null(context()) || !is.null(loading_form())) return()
        variable_form_name <<- NULL
        variable <- editor_display_context()$variables[[input$variable_name]]
        if (is.null(variable)) return()
        same_variable <- identical(input$variable_name, variable_control_name)
        variable_control_name <<- input$variable_name
        selected <- if (same_variable)
          input$variable_values %||% character() else character()
        update_choices(
          "variable_values",
          choices = union(variable$categories, selected),
          selected = selected
        )
        if (same_variable) return()
        shiny::updateSelectInput(
          session,
          "variable_mode",
          selected = if (variable$numeric) "range" else "categories"
        )
        shiny::updateNumericInput(
          session,
          "variable_lower",
          value = variable$lower
        )
        shiny::updateNumericInput(
          session,
          "variable_upper",
          value = variable$upper
        )
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      list(input$invalidation_source, editor_display_context()),
      {
        if (identical(input$invalidation_source, invalidation_form_name)) {
          invalidation_form_name <<- NULL
          invalidation_control_name <<- input$invalidation_source
          return()
        }
        if (is.null(context()) || !is.null(loading_form())) return()
        invalidation_form_name <<- NULL
        variable <- editor_display_context()$variables[[
          input$invalidation_source
        ]]
        if (is.null(variable)) return()
        same <- identical(input$invalidation_source, invalidation_control_name)
        invalidation_control_name <<- input$invalidation_source
        selected <- if (same) input$invalidation_values %||% character() else
          character()
        update_choices(
          "invalidation_values",
          choices = union(variable$categories, selected),
          selected = selected
        )
        if (same) return()
        shiny::updateSelectInput(
          session,
          "invalidation_mode",
          selected = if (variable$numeric) "range" else "categories"
        )
        shiny::updateSelectInput(
          session,
          "invalidation_condition",
          selected = if (variable$numeric) "outside" else "inside"
        )
        shiny::updateNumericInput(
          session,
          "measurement_lower",
          value = variable$lower
        )
        shiny::updateNumericInput(
          session,
          "measurement_upper",
          value = variable$upper
        )
      },
      ignoreInit = TRUE
    )
    output$invalidation_help <- shiny::renderText({
      variable <- editor_display_context()$variables[[
        input$invalidation_source %||% ""
      ]]
      if (is.null(variable)) return("Choose a source available at this step.")
      paste0(
        "Source: ",
        input$invalidation_source,
        " \u00b7 ",
        variable$missing,
        " missing value(s) entering this step.",
        if (
          !isTRUE(variable$categories_complete) &&
            identical(input$invalidation_mode, "categories")
        )
          " The first 500 distinct categories are offered; saved selections are retained." else
          ""
      )
    })
    output$measurement_help <- shiny::renderText({
      shiny::req(context())
      if (!length(context()$measurements))
        return("No editable numeric measurement is available in this dataset.")
      variable <- editor_display_context()$variables[[
        input$measurement_variable %||% ""
      ]]
      if (is.null(variable))
        return(
          "This input variable is unavailable here. Choose another variable or restore its earlier creating step."
        )
      paste(
        "Target:",
        input$measurement_variable,
        "\u00b7",
        if (is.na(variable$missing)) "Save to calculate missing values." else
          paste(variable$missing, "missing value(s) entering this step.")
      )
    })
    output$calibration_formula <- shiny::renderText({
      if (
        !identical(input$measurement_operation %||% "calibrate", "calibrate")
      ) {
        parameter <- tryCatch(
          preparation_transform_parameter(form()),
          error = function(cnd) NULL
        )
        return(paste(
          "Preview value =",
          preparation_transform_formula(
            input$measurement_variable %||% "measurement",
            if (input$measurement_operation == "zero_inflated")
              input$zero_operation else input$measurement_operation,
            parameter,
            input$zero_offset
          )
        ))
      }
      paste(
        "Preview value =",
        input$measurement_variable %||% "measurement",
        "\u00d7",
        input$calibration_multiplier %||% "?",
        "+ (",
        input$calibration_offset %||% "?",
        ")"
      )
    })
    output$transformation_help <- shiny::renderText({
      switch(
        input$measurement_operation %||% "calibrate",
        calibrate = "Use coefficients from your calibration procedure. Units are unchanged; coefficients are not estimated here.",
        log = "Requires positive values and a positive base other than 1. For log(x + 1), add an offset of 1 in an earlier step. The original measurement unit does not describe logarithmic values.",
        root = "Use a positive whole-number degree. Odd roots retain the sign of negative values; even roots require nonnegative values. These are transformed values, not the original measurement units.",
        power = "Raises each value to the exponent. Fractional powers of negative values and negative powers of zero are invalid. An exponent of zero returns 1, including for zero input.",
        exp = "Raises a positive base to each value. Very large results may overflow. These are transformed values, not the original measurement units.",
        abs = "Returns the magnitude of each reading, without its sign. Infinite inputs are invalid.",
        zero_inflated = "Log adds the offset before taking the logarithm. Inverse raises the base to the reading, then subtracts the offset. Use the same base and offset to reverse a previous log transformation."
      )
    })
    output$variable_help <- shiny::renderText({
      variable <- editor_display_context()$variables[[
        input$variable_name %||% ""
      ]]
      if (is.null(variable)) return("")
      if (is.na(variable$missing))
        return(
          "Variable details will be available after the preceding steps have finished previewing."
        )
      paste(
        variable$missing,
        "missing value(s) entering this step.",
        if (!variable$categories_complete)
          "Category choices show the first 500 distinct values entering this step. Use a numeric interval for other numeric values." else
          ""
      )
    })
    shiny::observeEvent(
      list(input$plot_step, input$plot_id, input$plot_day),
      {
        selected <- list(
          id = input$plot_id %||% "",
          day = input$plot_day %||% "",
          step = input$plot_step %||% "all"
        )
        expected <- view_loading()
        if (!is.null(expected)) {
          if (identical(selected, expected)) view_loading(NULL)
          return()
        }
        if (
          !preparation_preview_matches(preview(), dataset(), recipe()) ||
            dirty() ||
            busy()
        )
          return()
        previous <- preview()$comparison$view
        previous_step <- preparation_view_step(previous)
        if (
          identical(
            selected,
            list(
              id = as.character(previous$id),
              day = as.character(previous$day),
              step = previous_step
            )
          )
        )
          return()
        if (
          !identical(selected$id, as.character(previous$id)) ||
            !identical(selected$day, as.character(previous$day))
        ) {
          automatic_day(FALSE)
          shiny::updateCheckboxInput(session, "automatic_day", value = FALSE)
        }
        selection <- if (selected$step != previous_step && automatic_day())
          list(step = selected$step) else selected
        safely(enqueue("view", replace = TRUE, view = selection))
      },
      ignoreInit = TRUE
    )
    choose_day <- function() {
      if (
        !preparation_preview_matches(preview(), dataset(), recipe()) || dirty()
      )
        return()
      safely(enqueue(
        "view",
        replace = TRUE,
        view = list(step = input$plot_step %||% "all")
      ))
    }
    shiny::observeEvent(input$auto_day, choose_day())
    shiny::observeEvent(
      input$automatic_day,
      {
        enabled <- isTRUE(input$automatic_day)
        if (identical(enabled, automatic_day())) return()
        automatic_day(enabled)
        if (enabled) choose_day()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$new_step,
      safely({
        shiny::req(context())
        set_form(new_form_defaults())
        notice("New step. Save it to add it to the recipe.")
      })
    )
    shiny::observeEvent(input$all_streams, {
      shiny::req(context())
      shiny::updateSelectizeInput(
        session,
        "participants",
        selected = unname(context()$choices)
      )
    })
    edit_step <- function(id) {
      shiny::req(context())
      ids <- vapply(recipe()$steps, `[[`, character(1), "id")
      position <- match(id, ids)
      if (length(position) != 1L || is.na(position))
        abort_llw("Select a recipe step first.", "preparation")
      step <- recipe()$steps[[position]]
      args <- step$parameters$arguments
      # Other tools should start from the data reaching this step. The saved
      # operation below still restores its own explicit variable and settings.
      prefix <- new_recipe(recipe()$steps[seq_len(position - 1L)])
      entering <- preparation_new_step_context(
        context(),
        prefix,
        variable_catalog()
      )
      values <- preparation_form_defaults(entering)
      values$step_type <- step$type
      if (step$type == "timezone") {
        values$time_timezone <- args$timezone
        values$time_method <- args$method
        values$time_ambiguous <- args$ambiguous
      } else if (step$type == "gaps") {
        values$gap_epoch <- args$epoch
        values$gap_seconds <- args$seconds
        values$gap_bounds <- args$bounds
      } else if (step$type == "aggregate") {
        args <- preparation_aggregation_arguments(args)
        values$aggregate_interval <- preparation_aggregation_unit(args)
        values$aggregate_numeric <- args$numeric
        values$aggregate_missing <- args$missing
        values$aggregate_max_missing <- args$max_missing
        values$aggregate_categorical <- args$categorical
        values$aggregate_logical <- args$logical
      } else if (step$type == "dst") {
        values$step_type <- "dst"
      } else if (step$type == "participants") {
        indexed <- cbind(
          context()$identities,
          .position = seq_len(nrow(context()$identities))
        )
        wanted <- dplyr::semi_join(
          indexed,
          args$identities,
          by = args$identity_columns,
          na_matches = "na"
        )$.position
        values$participants <- unname(context()$choices[wanted])
      } else if (
        step$type %in% c("invalidate", "calibrate", "transform", "variability")
      ) {
        values$measurement_variable <- args$variable
        values$measurement_output <- args$output %||% args$variable
        if (step$type == "variability") {
          for (name in c(
            "source",
            "length",
            "unit",
            "alignment",
            "statistic",
            "threshold",
            "direction",
            "coverage"
          ))
            values[[paste0("variability_", name)]] <- args[[name]]
          values$variability_unevaluable <- args$unevaluable %||% "keep"
        } else if (step$type == "invalidate") {
          values$measurement_lower <- args$lower
          values$measurement_upper <- args$upper
          values$invalidation_source <- args$source %||% args$variable
          values$invalidation_mode <- args$mode %||% "range"
          values$invalidation_values <- args$values %||% character()
          values$invalidation_condition <- if (isFALSE(args$outside))
            "inside" else "outside"
          values$invalidation_missing <- args$missing %||% "keep"
        } else if (step$type == "calibrate") {
          values$calibration_multiplier <- args$multiplier
          values$calibration_offset <- args$offset
          values$calibration_reference <- args$reference
        } else {
          values$measurement_operation <- if (
            args$operation %in%
              c("log_zero_inflated", "exp_zero_inflated")
          )
            "zero_inflated" else args$operation
          values$transform_invalid <- args$invalid
          values$calibration_reference <- args$reference
          if (values$measurement_operation == "zero_inflated") {
            values$zero_operation <- args$operation
            values$zero_base <- args$parameter
            values$zero_offset <- args$offset
          } else if (args$operation %in% c("log", "exp")) {
            values$transform_base <- if (identical(args$parameter, exp(1)))
              "e" else if (args$parameter %in% c(2, 10))
              as.character(args$parameter) else "custom"
            if (values$transform_base == "custom")
              values$transform_base_custom <- args$parameter
          } else if (args$operation == "root")
            values$transform_degree <- args$parameter else if (
            args$operation == "power"
          )
            values$transform_power <- args$parameter
        }
      } else if (step$type == "variable") {
        values$variable_name <- args$variable
        values$variable_mode <- args$mode
        values$variable_values <- args$values
        values$variable_lower <- args$lower
        values$variable_upper <- args$upper
        values$variable_condition <- if (args$outside) "outside" else "inside"
        values$variable_missing <- args$missing
      } else if (step$type == "photoperiod") {
        values$photo_method <- args$method
        values$photo_state <- args$state
        values$photo_latitude <- args$latitude
        values$photo_longitude <- args$longitude
        values$photo_solar <- args$solar_depression
        values$photo_missing <- args$missing
      } else if (step$type == "coverage") {
        values$coverage_variable <- args$variable
        values$coverage_basis <- args$basis
        if (is.null(args$mode)) values$coverage_minimum <- args$minimum else {
          values$coverage_mode <- args$mode
          values$coverage_duration <- args$threshold
          values$coverage_unit <- args$unit
        }
      } else if (step$type == "duration") {
        values$duration_length <- args$length
        values$duration_unit <- args$unit
        values$duration_from <- args$from
        values$duration_scope <- args$scope
      } else if (step$type == "datetimes") {
        for (endpoint in c("start", "end")) {
          prefix <- paste0("instant_", endpoint)
          zone <- args[[paste0(endpoint, "_timezone")]] %||% "native"
          values[[paste0(prefix, "_zone")]] <- if (zone == "native")
            "native" else "custom"
          if (zone == "native") zone <- entering$timezone
          instant <- preparation_instant(args[[endpoint]])
          values[[paste0(prefix, "_timezone")]] <- zone
          values[[paste0(prefix, "_date")]] <- format(
            instant,
            "%Y-%m-%d",
            tz = zone
          )
          values[[paste0(prefix, "_time")]] <- preparation_time_input(
            instant,
            zone
          )
        }
      } else {
        prefix <- switch(
          step$type,
          dates = "date",
          datetimes = "instant",
          clock = "clock"
        )
        values[[paste0(prefix, "_start")]] <- args$start
        values[[paste0(prefix, "_end")]] <- args$end
      }
      set_form(values, step$id)
    }
    shiny::observeEvent(
      input$edit,
      safely({
        edit_step(input$selected_step)
        notice("Editing the selected step. Save it to update the recipe.")
      })
    )
    save_filter <- function(as_new = FALSE)
      safely(
        {
          shiny::req(context())
          if (!is.null(loading_form()))
            abort_llw("Wait for the step controls to load.", "preparation")
          id <- if (as_new) new_stable_id("step") else
            editing_id() %||% new_stable_id("step")
          previous <- Filter(function(s) identical(s$id, id), recipe()$steps)
          enabled <- if (length(previous)) previous[[1L]]$enabled else TRUE
          step <- new_preparation_step(
            form()$step_type,
            preparation_form_arguments(form(), editor_context()),
            context(),
            enabled,
            id
          )
          next_recipe <- edit_preparation_recipe(recipe(), "save", step)
          next_recipe <- preparation_retarget_following(
            recipe(),
            next_recipe,
            id
          )
          next_recipe <- preparation_reconnect_sources(
            recipe(),
            next_recipe,
            context()
          )
          validate_preparation_recipe(next_recipe, context())
          # Keep queued feedback while this saved edit's controls acknowledge.
          # The replacement below supersedes the request; the generation guard
          # prevents the old request from running in the meantime.
          invalidate_preview(cancel_pending = FALSE)
          chain_changes(preparation_chain_changes(recipe(), next_recipe))
          selection_after_edit <<- id
          recipe_history(utils::tail(c(recipe_history(), list(recipe())), 30L))
          recipe(next_recipe)
          editing_id(id)
          position <- match(
            id,
            vapply(next_recipe$steps, `[[`, character(1), "id")
          )
          defaults <- preparation_new_step_context(
            context(),
            new_recipe(next_recipe$steps[seq_len(position - 1L)]),
            variable_catalog()
          )
          saved_form <- preparation_unused_variable_defaults(form(), defaults)
          # Saving as a later step changes its entering span immediately. Move
          # inactive date controls into that span in the same form transaction,
          # before their calendars redraw and send empty out-of-range values.
          saved_form <- preparation_unused_time_defaults(saved_form, defaults)
          if (step$type == "variability")
            saved_form$variability_source <- next_recipe$steps[[
              position
            ]]$parameters$arguments$source
          if (step$type == "invalidate")
            saved_form$invalidation_source <- next_recipe$steps[[
              position
            ]]$parameters$arguments$source %||%
              next_recipe$steps[[position]]$parameters$arguments$variable
          created <- as_new && !is.null(step$parameters$arguments$output)
          if (created) {
            next_context <- preparation_new_step_context(
              context(),
              next_recipe,
              variable_catalog()
            )
            set_form(preparation_created_variable_form(form(), next_context))
          } else if (preparation_same_form(saved_form, form())) {
            form_base(form())
          } else {
            set_form(saved_form, id)
          }
          shiny::updateRadioButtons(session, "selected_step", selected = id)
          notice(
            if (created)
              paste(
                "Step saved.",
                step$parameters$arguments$output,
                "is selected for the next step. Updating the preview."
              ) else
              "Step saved. Updating the preview from the original source."
          )
          auto_preview(TRUE)
        },
        editor = TRUE
      )
    shiny::observeEvent(input$save_step, save_filter())
    shiny::observeEvent(input$save_new, save_filter(as_new = TRUE))
    shiny::observeEvent(
      input$step_order,
      {
        current_ids <- vapply(recipe()$steps, `[[`, character(1), "id")
        if (identical(input$step_order, current_ids)) return()
        # Re-render the canonical order even when a stale or invalid drag is rejected.
        recipe_render(recipe_render() + 1L)
        safely({
          # Reordering operates on saved steps, so it can replace a pending
          # server-issued editor update. Saving inputs and Apply still wait for
          # the browser to acknowledge that update.
          if (dirty())
            abort_llw(
              "Save the form or discard its edits before reordering the recipe.",
              "preparation"
            )
          next_recipe <- edit_preparation_recipe(
            recipe(),
            "reorder",
            ordered_ids = input$step_order
          )
          if (identical(next_recipe, recipe())) return()
          next_recipe <- preparation_reconnect_order(
            recipe(),
            next_recipe,
            context()
          )
          validate_preparation_recipe(next_recipe, context())
          invalidate_preview(cancel_pending = FALSE)
          chain_changes(preparation_chain_changes(recipe(), next_recipe))
          recipe_history(utils::tail(c(recipe_history(), list(recipe())), 30L))
          reload_editor <- preparation_editor_needs_reload(
            recipe(),
            next_recipe,
            editing_id()
          )
          recipe(next_recipe)
          if (is.null(editing_id())) set_form(new_form_defaults()) else if (
            reload_editor
          )
            edit_step(editing_id())
          notice(
            "Recipe reordered. Updating the preview from the original source."
          )
          auto_preview(TRUE)
        })
      },
      ignoreInit = TRUE
    )
    for (action in c("up", "down", "toggle", "remove"))
      local({
        operation <- action
        shiny::observeEvent(
          input[[operation]],
          safely({
            if (dirty())
              abort_llw(
                "Save the form or discard its edits before changing the ordered recipe.",
                "preparation"
              )
            next_recipe <- edit_preparation_recipe(
              recipe(),
              operation,
              id = input$selected_step
            )
            if (operation %in% c("up", "down"))
              next_recipe <- preparation_reconnect_order(
                recipe(),
                next_recipe,
                context()
              )
            next_recipe <- preparation_reconnect_sources(
              recipe(),
              next_recipe,
              context()
            )
            validate_preparation_recipe(next_recipe, context())
            invalidate_preview(cancel_pending = FALSE)
            chain_changes(preparation_chain_changes(recipe(), next_recipe))
            recipe_history(utils::tail(
              c(recipe_history(), list(recipe())),
              30L
            ))
            reload_editor <- preparation_editor_needs_reload(
              recipe(),
              next_recipe,
              editing_id()
            )
            recipe(next_recipe)
            if (
              is.null(editing_id()) ||
                (operation == "remove" &&
                  identical(editing_id(), input$selected_step))
            )
              set_form(new_form_defaults()) else if (reload_editor)
              edit_step(editing_id())
            notice(
              "Recipe updated. Updating the preview from the original source."
            )
            auto_preview(TRUE)
          })
        )
      })
    shiny::observeEvent(input$preview, {
      if (dirty()) save_filter(as_new = is.null(editing_id())) else
        safely(enqueue("preview"))
    })
    shiny::observeEvent(input$reset, safely(enqueue("reset")))
    shiny::observeEvent(
      input$undo,
      safely({
        if (!length(dataset()$history))
          abort_llw("There is no applied change to undo.", "preparation")
        enqueue("undo")
      })
    )
    shiny::observeEvent(input$cancel, {
      auto_preview(FALSE)
      if (!busy() && is.null(pending())) {
        notice("No preparation is running. Applied data are unchanged.")
        return()
      }
      pending(NULL)
      task$cancel()
      # Busy feedback takes precedence until the existing controller releases
      # its worker. The terminal message becomes visible only after that.
      notice(
        if (is.null(context()))
          "Preparation cancelled. Applied data are unchanged. Select another dataset to load its controls." else
          "Preparation cancelled. Applied data are unchanged. Preview is available again."
      )
    })
    shiny::observeEvent(
      input$apply,
      safely({
        value <- preview()
        if (
          dirty() ||
            !is.null(loading_form()) ||
            busy() ||
            !is.null(pending()) ||
            !preparation_preview_matches(value, dataset(), recipe())
        )
          abort_llw(
            "Preview the current saved recipe before Apply.",
            "preparation"
          )
        if (
          value$comparison$before$rows > 0L &&
            identical(recipe(), dataset()$recipe)
        )
          abort_llw(
            "This recipe is already applied. Edit or add a step first.",
            "preparation"
          )
        if (value$comparison$after$rows == 0L)
          abort_llw(
            if (value$comparison$before$rows == 0L)
              "This source has no observations. Choose a dataset containing observations before preparing data." else
              "No rows remain. Edit, disable or remove a step before Apply.",
            "preparation"
          )
        committed <- on_commit(value$candidate, value$binding)
        record_key <<- current_key(committed)
        updated_context <- context()
        updated_context$prepared_rows <- value$comparison$after$rows
        context(updated_context)
        value$binding <- preparation_binding(committed, recipe())
        preview(value)
        notice(
          "Recipe applied from the original source. You can edit this same recipe and Apply again."
        )
      })
    )

    shiny::observeEvent(task$status(), {
      if (task$state() == "error")
        notice(task$status()$message, "error", "Preview failed") else if (
        task$state() == "stale"
      )
        notice(task$status()$message, "warning", "Preview needs updating")
    })
    output$heading <- shiny::renderUI({
      record <- dataset()
      if (is.null(record)) return(shiny::tags$p("No dataset selected."))
      shiny::tags$div(
        class = "llw-prepare-dataset",
        shiny::tags$div(
          shiny::tags$span(
            class = "llw-prepare-dataset-label",
            "Working dataset"
          ),
          shiny::tags$strong(record$display_name)
        ),
        if (!is.null(context()))
          shiny::tags$div(
            class = "llw-prepare-dataset-counts",
            shiny::tags$span(
              format(context()$rows, big.mark = ",", scientific = FALSE),
              "source rows"
            ),
            shiny::tags$span(
              format(
                context()$prepared_rows,
                big.mark = ",",
                scientific = FALSE
              ),
              "currently prepared"
            )
          )
      )
    })
    # Publish small presentation values once per flush. A changed recipe or
    # queued request need not re-render outputs whose visible state is identical.
    ui_state <- shiny::reactiveValues()
    button_states <- list()
    update_button <- function(name, ...) {
      values <- list(...)
      if (identical(button_states[[name]], values)) return(invisible(NULL))
      button_states[[name]] <<- values
      do.call(shiny::updateActionButton, c(list(session, name), values))
    }
    shiny::observe(
      {
        waiting <- !is.null(pending())
        running <- busy()
        active <- running || waiting
        loading <- !is.null(loading_form())
        value <- preview()
        saved <- recipe()
        record <- dataset()
        has_steps <- length(saved$steps) > 0L
        current <- !active &&
          !loading &&
          !dirty() &&
          preparation_preview_matches(value, record, saved)
        ui_state$pending <- waiting
        ui_state$active <- active
        ui_state$editor_title <- if (is.null(editing_id())) {
          if (has_steps) "Define next step" else "Define a step"
        } else {
          ids <- vapply(saved$steps, `[[`, character(1), "id")
          paste("Edit step", match(editing_id(), ids))
        }
        ui_state$form_loading <- if (loading || (is.null(context()) && active))
          "yes" else "no"
        edited_step <- match(
          editing_id(),
          vapply(saved$steps, `[[`, character(1), "id")
        )
        saved_type <- if (length(edited_step) && !is.na(edited_step))
          saved$steps[[edited_step]]$type else form_base()$step_type
        ui_state$form_status <- if (dirty())
          "Unsaved changes. Save the step to update the preview." else if (
          is.null(editing_id())
        )
          "Ready to add a new step." else if (
          !identical(form_type(), saved_type)
        )
          "Browsing another operation. Adjust current step replaces the selected step; Save as new step adds it." else
          "Saved in your recipe."
        ui_state$has_steps <- if (has_steps) "yes" else "no"
        ui_state$work_active <- if (active) "yes" else "no"
        ui_state$preview_ready <- if (current) "yes" else "no"
        update_button(
          "save_step",
          label = "Adjust current step",
          disabled = is.null(editing_id()) || loading
        )
        update_button(
          "preview",
          disabled = active || loading || is.null(context()),
          label = if (active) "Updating preview\u2026" else if (
            is.null(context())
          )
            "Preview unavailable" else if (dirty())
            if (is.null(editing_id())) "Add step and preview" else
              "Save step and preview" else if (
            preparation_memory_estimate(context()$raw_bytes) >
              runtime$profile$cache_max_bytes / 2 &&
              !isTRUE(input$ack_cost)
          )
            "Acknowledge before Preview" else "Preview recipe"
        )
        update_button(
          "apply",
          disabled = !current ||
            value$comparison$after$rows == 0L ||
            identical(saved, record$recipe),
          label = if (has_steps && identical(saved, record$recipe))
            "Applied" else if (active) "Updating preview\u2026" else "Apply"
        )
        update_button("undo_edit", disabled = !length(recipe_history()))
        update_button(
          "reset",
          disabled = is.null(record) ||
            running ||
            (!has_steps && !length(record$recipe$steps))
        )
      },
      priority = -2
    )
    for (output_name in c(
      "editor_title",
      "form_loading",
      "form_status",
      "has_steps",
      "work_active",
      "preview_ready"
    ))
      local({
        name <- output_name
        output[[name]] <- shiny::renderText(ui_state[[name]])
      })
    for (name in c("has_steps", "work_active", "preview_ready"))
      shiny::outputOptions(output, name, suspendWhenHidden = FALSE)
    output$resource_notice <- shiny::renderUI({
      shiny::req(dataset())
      shiny::tags$p(paste(
        "Estimated working memory:",
        round(
          preparation_memory_estimate(length(dataset()$raw_payload)) / 1024^2,
          1
        ),
        "MiB. Session budget:",
        round(runtime$profile$cache_max_bytes / 1024^2),
        "MiB."
      ))
    })
    output$large_work <- shiny::renderText({
      record <- dataset()
      if (
        !is.null(record) &&
          preparation_memory_estimate(length(record$raw_payload)) >
            runtime$profile$cache_max_bytes / 2
      )
        "yes" else "no"
    })
    shiny::outputOptions(output, "large_work", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "form_loading", suspendWhenHidden = FALSE)
    status_message <- shiny::reactive({
      status <- task$status()
      text <- if (isTRUE(ui_state$pending)) notice() else if (busy()) {
        if (status$state == "cancelled")
          "Cancelled worker is draining. You may queue one retry." else
          paste(
            "Preparation",
            status$state,
            "- you may cancel or switch datasets."
          )
      } else notice()
      active <- isTRUE(ui_state$active)
      severity <- if (active) "info" else notice_level()
      recovery <- if (
        severity == "error" && status$state == "error" && !dirty()
      ) {
        if (inherits(task$error(), "llw_preparation_value_error"))
          "Applied data are unchanged. Edit the measurement step, then save to update the preview." else
          "Applied data are unchanged. Edit and save the step, or choose Preview recipe to retry."
      } else NULL
      shiny::tags$div(
        class = paste(
          "llw-prepare-status",
          paste0("is-", severity),
          if (active) "is-busy"
        ),
        role = if (severity == "info") "status" else "alert",
        `aria-live` = if (severity == "info") "polite" else "assertive",
        `aria-atomic` = "true",
        `aria-busy` = if (active) "true" else "false",
        if (active)
          shiny::tags$strong(
            class = "llw-prepare-alert-title",
            shiny::icon("hourglass-half"),
            "Updating preview\u2026"
          ),
        if (severity != "info")
          shiny::tags$strong(
            class = "llw-prepare-alert-title",
            shiny::icon("triangle-exclamation"),
            notice_title()
          ),
        shiny::tags$p(text),
        if (!is.null(recovery))
          shiny::tags$p(class = "llw-prepare-recovery", recovery),
        if (runtime$profile$synchronous)
          shiny::tags$p(
            "Synchronous fallback: computation can temporarily block this session."
          )
      )
    })
    output$status_text <- shiny::renderUI(status_message())
    output$action_status <- shiny::renderUI(status_message())
    output$selection_scope <- shiny::renderUI({
      value <- context()
      if (is.null(value)) return(NULL)
      shiny::tags$p(
        class = "small",
        if (value$file_identity_complete)
          "Each choice matches the shown participant and retained source/file identifiers. Separate file identities remain separately selectable. Empty selection is not allowed." else
          "File identity is missing for some or all observations. Choices use only the shown participant and retained source identifiers; each may include several recordings. Files without retained identities cannot be selected separately, and their sampling intervals, gaps and duplicate counts are unavailable. Empty selection is not allowed."
      )
    })
    shiny::observeEvent(
      input$undo_edit,
      safely({
        history <- recipe_history()
        if (!length(history)) return()
        restored <- history[[length(history)]]
        recipe_history(utils::head(history, -1L))
        invalidate_preview(cancel_pending = FALSE)
        chain_changes(character())
        recipe(restored)
        set_form(new_form_defaults())
        auto_preview(TRUE)
        notice("Recipe edit undone. Previewing again from the original source.")
      })
    )
    shiny::observe({
      if (
        !auto_preview() ||
          is.null(context()) ||
          !is.null(loading_form()) ||
          dirty()
      )
        return()
      auto_preview(FALSE)
      safely(enqueue("preview", replace = TRUE, defer = TRUE))
    })
    shiny::observeEvent(
      input$ack_cost,
      {
        if (isTRUE(input$ack_cost) && !dirty() && !is.null(context()))
          auto_preview(TRUE)
      },
      ignoreInit = TRUE
    )
    output$recipe_list <- shiny::renderUI({
      steps <- recipe()$steps
      removed <- preparation_pending_removals(
        recipe(),
        dataset()$recipe %||% new_recipe()
      )
      if (!length(steps) && !length(removed))
        return(shiny::tags$div(
          class = "llw-prepare-empty-recipe",
          shiny::icon("list-check"),
          shiny::tags$div(
            shiny::tags$strong("Your recipe starts here"),
            shiny::tags$p(
              "Save a step from the editor. With no steps, the recipe keeps all source rows and values."
            )
          )
        ))
      shiny::tags$p(
        class = "llw-prepare-recipe-summary",
        paste(
          length(steps),
          "saved step(s) \u00b7",
          sum(vapply(steps, `[[`, logical(1), "enabled")),
          "enabled",
          if (length(removed))
            paste0(" \u00b7 ", length(removed), " removal(s) pending")
        )
      )
    })
    output$pending_removals <- shiny::renderUI({
      applied <- dataset()$recipe %||% new_recipe()
      removed <- preparation_pending_removals(recipe(), applied)
      if (!length(removed)) return(NULL)
      shiny::tags$div(
        class = "llw-prepare-pending-removals",
        `aria-live` = "polite",
        shiny::tags$p(
          class = "llw-prepare-hint",
          "These steps are excluded from the preview but still affect the prepared data until you Apply."
        ),
        lapply(
          removed,
          function(i)
            shiny::tags$div(
              class = "llw-prepare-removed-step llw-prepare-recipe-label",
              shiny::tags$span(
                class = "llw-prepare-recipe-title",
                paste(
                  "Applied step",
                  preparation_step_label(applied$steps[[i]], i)
                )
              ),
              shiny::tags$span(
                class = "llw-prepare-step-state is-removal-pending",
                "Removal pending"
              )
            )
        )
      )
    })
    output$preview_notice <- shiny::renderUI({
      value <- preview()
      if (is.null(value)) {
        if (busy() || !is.null(pending()) || is.null(context())) return(NULL)
        message <-
          if (context()$rows == 0L)
            "This source has no observations. Choose a dataset containing observations." else if (
            dirty()
          )
            "Use the button below to save this step and update the preview. Apply is unavailable until the preview is ready." else if (
            preparation_memory_estimate(context()$raw_bytes) >
              runtime$profile$cache_max_bytes / 2 &&
              !isTRUE(input$ack_cost)
          )
            "Review the memory estimate and select the acknowledgement above before Preview. Apply is unavailable." else
            NULL
        return(
          if (!is.null(message))
            shiny::tags$p(class = "llw-prepare-hint", message)
        )
      }
      comparison <- value$comparison
      if (comparison$after$rows == 0L)
        shiny::tags$p(
          class = "llw-prepare-hint",
          "No rows remain. Edit, disable or remove a recipe step before Apply."
        ) else NULL
    })
    output$impact_summary <- shiny::renderUI({
      shiny::req(preview())
      comparison <- preview()$comparison
      number <- function(value)
        format(value, big.mark = ",", scientific = FALSE)
      metric <- function(label, value, detail)
        shiny::tags$div(
          class = "llw-prepare-impact-metric",
          shiny::tags$span(label),
          shiny::tags$strong(number(value)),
          shiny::tags$small(detail)
        )
      shiny::tags$div(
        class = "llw-prepare-impact-metrics",
        metric(
          if (isTRUE(comparison$aggregated)) "Rows after aggregation" else if (
            (comparison$added_rows %||% 0L) > 0L
          )
            "Rows in preview" else "Rows retained",
          comparison$after$rows,
          paste("from", number(comparison$before$rows), "source rows")
        ),
        metric(
          if (isTRUE(comparison$aggregated)) "Source rows removed" else
            "Rows removed",
          comparison$removed_rows,
          paste0(
            round(100 * comparison$removed_fraction, 1),
            "% of source rows"
          )
        ),
        if (isTRUE(comparison$aggregated))
          metric(
            "Source rows represented",
            comparison$represented_rows,
            "Combined into bins; filtering counted separately"
          ) else if ((comparison$added_rows %||% 0L) > 0L)
          metric(
            "Gap rows added",
            comparison$added_rows,
            "Missing measurements \u00b7 is.implicit = TRUE"
          ) else if ((comparison$retimed_rows %||% 0L) > 0L)
          metric(
            "Timestamps shifted",
            comparison$retimed_rows,
            "Measurements unchanged by time correction"
          ) else if (nrow(comparison$value_impact))
          metric(
            if (comparison$created_values > 0L) "Values created" else
              "Values changed",
            if (comparison$created_values > 0L) comparison$created_values else
              comparison$changed_values,
            paste(
              number(comparison$invalidated_values),
              "newly missing in retained rows"
            )
          ) else
          metric(
            "Participants",
            comparison$after$participants,
            paste("of", comparison$before$participants, "in source")
          )
      )
    })
    output$preview_details <- shiny::renderUI({
      shiny::req(preview())
      value <- preview()
      shiny::tagList(
        if (isTRUE(value$comparison$aggregated))
          shiny::tags$p(
            "Aggregation combines observations. Per-step counts describe value changes before or after aggregation; a one-to-one final value comparison is not available. Coverage in Measures describes the data entering aggregation, before any later recipe steps."
          ) else
          shiny::tags$p(paste(
            value$comparison$changed_values,
            "measurement values differ from their source in retained rows;",
            value$comparison$created_values,
            "values are in new variables;",
            value$comparison$invalidated_values,
            "are newly missing. Preview time:",
            round(value$elapsed_seconds, 2),
            "s. Estimated working memory:",
            round(value$estimated_working_bytes / 1024^2, 1),
            "MiB."
          )),
        lapply(seq_along(value$comparison$time_notes), function(i) {
          note <- value$comparison$time_notes[[i]]
          if (is.null(note$total)) return(NULL)
          shiny::tags$p(paste(
            "Gap step",
            i,
            ": estimated",
            format(note$total, big.mark = ","),
            "rows after expansion,",
            round(note$working_bytes / 1024^2, 1),
            "MiB working memory;",
            note$singletons,
            "single-observation recordings kept without expansion."
          ))
        })
      )
    })
    output$variable_history <- shiny::renderUI({
      shiny::req(preview())
      candidate <- preview()$candidate
      lineage <- preparation_variable_lineage(candidate$recipe)
      if (!length(lineage))
        return(shiny::tags$p("No measurement transformations in this recipe."))
      htmltools::tagList(lapply(names(lineage), function(name) {
        info <- lineage[[name]]
        metadata <- metadata_resolve_variable(candidate, name)
        inherited <- metadata$inherited_metadata %||% metadata
        shiny::tags$section(
          shiny::tags$strong(name),
          shiny::tags$p(
            paste(
              "Source:",
              info$source,
              "\u00b7",
              inherited$display_label,
              "\u00b7 Unit:",
              inherited$unit,
              "\u00b7 Calibration:",
              inherited$calibration
            )
          ),
          if (
            !is.null(metadata$source_unit) &&
              !identical(metadata$unit, metadata$source_unit)
          )
            shiny::tags$p(
              "The source unit is preserved as metadata; it does not describe the transformed values."
            ),
          shiny::tags$ul(lapply(seq_along(info$history), function(i) {
            event <- info$history[[i]]
            shiny::tags$li(preparation_step_label(
              list(
                type = event$type,
                enabled = TRUE,
                parameters = list(arguments = event$arguments)
              ),
              i
            ))
          }))
        )
      }))
    })
    output$comparison <- shiny::renderTable(
      {
        shiny::req(preview())
        comparison <- preview()$comparison
        values <- comparison$value_impact
        dplyr::bind_rows(
          comparison$table,
          lapply(comparison$aggregations, function(aggregation) {
            display <- function(x, suffix = "")
              if (length(x) != 1L || !is.finite(x)) "Not estimable" else
                paste0(format(round(x, 2), big.mark = ","), suffix)
            data.frame(
              Measure = paste0(
                "Step ",
                aggregation$step,
                " \u00b7 ",
                c(
                  "Aggregation input rows",
                  "Aggregation output bins",
                  paste(
                    "Observed duration entering aggregation:",
                    aggregation$variable,
                    "(hours)"
                  ),
                  "Full bin window duration (hours; includes empty intervals)",
                  paste("Aggregation input coverage:", aggregation$variable)
                )
              ),
              Source = "Not applicable",
              Preview = c(
                aggregation$before,
                aggregation$after,
                display(aggregation$observed_seconds / 3600),
                display(aggregation$intended_seconds / 3600),
                display(aggregation$coverage_percent, "%")
              )
            )
          }),
          if (nrow(values))
            data.frame(
              Measure = c(
                paste("Changed values in retained rows:", values$variable),
                paste(
                  "Newly missing values in retained rows:",
                  values$variable
                ),
                paste("Values in new variables:", values$variable)
              ),
              Source = "0",
              Preview = as.character(c(
                values$changed,
                values$invalidated,
                values$created
              ))
            ),
          if (
            nrow(comparison$steps) &&
              any(comparison$steps$unevaluable_windows > 0)
          ) {
            counts <- comparison$steps[
              comparison$steps$unevaluable_windows > 0,
              ,
              drop = FALSE
            ]
            data.frame(
              Measure = paste0(
                "Unevaluable windows, step ",
                match(counts$id, comparison$steps$id),
                ": ",
                counts$variability_notes
              ),
              Source = "",
              Preview = as.character(counts$unevaluable_windows)
            )
          }
        )
      },
      striped = TRUE
    )
    aggregation_note <- shiny::reactive({
      shiny::req(preview())
      notes <- preview()$comparison$aggregations
      shiny::req(length(notes))
      steps <- vapply(notes, function(note) as.character(note$step), "")
      index <- match(input$aggregation_step %||% utils::tail(steps, 1L), steps)
      if (is.na(index)) index <- length(notes)
      notes[[index]]
    })
    output$aggregation_coverage <- shiny::renderUI({
      shiny::req(preview())
      notes <- preview()$comparison$aggregations
      if (!length(notes)) return(NULL)
      steps <- vapply(notes, function(note) as.character(note$step), "")
      shiny::tagList(
        shiny::tags$h4("Coverage entering aggregation"),
        shiny::selectInput(
          session$ns("aggregation_step"),
          "Aggregation step",
          stats::setNames(steps, paste("Step", steps)),
          selected = utils::tail(steps, 1L),
          width = "100%"
        ),
        shiny::tags$p(
          class = "llw-prepare-hint",
          "First 100 occupied bins for the focus variable. Missing readings (%) counts NA among input rows; absent timestamps do not count. The optional missingness limit applies separately to each numeric variable. Time coverage uses full bins, including empty intervals; unknown cadence is reported as NA. Low coverage does not change the aggregate. Later steps do not change this report."
        ),
        shiny::uiOutput(session$ns("aggregation_note")),
        shiny::tags$div(
          class = "table-responsive",
          shiny::tableOutput(session$ns("bin_coverage"))
        )
      )
    })
    output$aggregation_note <- shiny::renderUI({
      note <- aggregation_note()
      shiny::tags$p(paste(
        "Coverage variable:",
        note$variable,
        if (isTRUE(note$previous_aggregation))
          "\u00b7 Coverage of input bins, not original readings within those bins." else
          "",
        if (isTRUE(note$retimed_support))
          "Coverage is unavailable because an earlier time correction shifted the input bins." else
          ""
      ))
    })
    output$bin_coverage <- shiny::renderTable(
      {
        data <- aggregation_note()$coverage
        if (!nrow(data)) return(NULL)
        for (name in c("Start", "End"))
          data[[name]] <- format(data[[name]], "%Y-%m-%d %H:%M:%S %z")
        identity <- intersect(
          c("Id", "file_group_id", "file.name"),
          names(data)
        )
        data[c(
          utils::head(identity, 2L),
          "Start",
          "End",
          "Input_rows",
          "Window_seconds",
          "Observed_seconds",
          "Coverage_percent"
        )]
      },
      striped = TRUE,
      digits = 2
    )
    output$source_sample <- shiny::renderTable(
      {
        shiny::req(preview())
        preview()$comparison$before_sample
      },
      striped = TRUE
    )
    output$preview_sample <- shiny::renderTable(
      {
        shiny::req(preview())
        preview()$comparison$after_sample
      },
      striped = TRUE
    )
    output$plot_caption <- shiny::renderText({
      shiny::req(preview())
      preview()$comparison$view$caption
    })
    output$transformation_note <- shiny::renderUI({
      shiny::req(preview())
      note <- preview()$comparison$view$transformation_note
      if (!is.null(note)) shiny::tags$p(class = "llw-prepare-hint", note)
    })
    output$participant_impact <- shiny::renderUI({
      shiny::req(preview())
      participants <- preview()$comparison$view$participants
      if (is.null(participants) || !nrow(participants)) return(NULL)
      removed <- participants$Removed == participants$Rows
      partial <- participants$Removed > 0 & !removed
      shiny::tags$p(
        class = "llw-prepare-hint",
        sprintf(
          "Across participants: %s fully removed, %s partly retained, %s with changed values, %s unchanged. Choose a participant above to inspect their day.",
          sum(removed),
          sum(partial),
          sum(participants$Changed > 0),
          sum(participants$Removed == 0 & participants$Changed == 0)
        )
      )
    })
    output$plot <- shiny::renderPlot(
      {
        shiny::req(preview())
        view <- preview()$comparison$view
        data <- view$plot
        shiny::validate(shiny::need(
          nrow(data) && any(is.finite(data$Value)),
          "No finite measurements on this day. Choose another participant or date."
        ))
        preparation_preview_plot(view, context(), axis_settings()) +
          preparation_plot_theme(
            if (is.null(color_mode)) "light" else color_mode()
          )
      },
      alt = shiny::reactive({
        paste(
          "Source in grey and preview values in teal on one participant-day.",
          preparation_axis_label(axis_settings()),
          preview()$comparison$view$caption
        )
      })
    )
    for (stage in c("source", "preview"))
      local({
        which_stage <- stage
        output[[paste0("plot_", stage)]] <- shiny::renderPlot(
          {
            shiny::req(preview())
            view <- preparation_split_view(
              preview()$comparison$view,
              which_stage
            )
            preparation_preview_plot(view, context(), axis_settings()) +
              preparation_plot_theme(
                if (is.null(color_mode)) "light" else color_mode()
              ) +
              ggplot2::labs(
                title = paste(
                  view$panel_title,
                  view$timezone %||% context()$timezone,
                  sep = "\n"
                ),
                subtitle = if (!is.null(view$empty_note))
                  preparation_plot_label(view$empty_note, 32L)
              )
          },
          alt = shiny::reactive(paste(
            which_stage,
            "values on the selected date in this stage's own time zone."
          ))
        )
      })
    list(
      recipe = shiny::reactive(recipe()),
      preview = shiny::reactive(preview()),
      context = shiny::reactive(context()),
      dirty = dirty,
      active = busy,
      pending = shiny::reactive(!is.null(pending())),
      status = task$status,
      notice = shiny::reactive(notice()),
      automatic_day = shiny::reactive(automatic_day())
    )
  })
}

prepare_app <- function(
  records = list(m1_showcase_record()),
  profile = "local",
  workers = 1L,
  worker = preparation_task_worker,
  plot_settings = NULL
) {
  stopifnot(length(records) > 0L)
  names(records) <- vapply(records, `[[`, character(1), "id")
  ui <- lightlogweb_page(bslib::page_fluid(
    title = "Prepare: Slice 4 showcase",
    theme = lightlogweb_theme(),
    lightlogweb_head(),
    lightlogweb_skip_link(),
    shiny::tags$div(
      class = "llw-prepare-showcase",
      shiny::tags$header(
        class = "llw-prepare-toolbar",
        shiny::tags$div(
          class = "llw-prepare-brand",
          lightlogweb_wordmark(),
          shiny::tags$span("Preprocessing showcase")
        ),
        shiny::selectInput(
          "dataset",
          "Example dataset",
          stats::setNames(
            names(records),
            vapply(records, `[[`, character(1), "display_name")
          ),
          width = "100%"
        )
      ),
      shiny::tags$main(
        id = "llw-main-content",
        class = "llw-prepare-main",
        shiny::uiOutput("example_note"),
        prepare_ui("prepare"),
        shiny::tags$details(
          class = "llw-prepare-development",
          shiny::tags$summary("Showcase details"),
          shiny::tags$p(
            "Slice 4: aggregation with observed-duration and bin-coverage reports, alongside the accepted time, measurement and filtering tools. Changing examples discards unapplied edits. Full app integration follows in Slice 5."
          ),
          shiny::verbatimTextOutput("development")
        )
      )
    )
  ))
  server <- function(input, output, session) {
    records_value <- shiny::reactiveVal(records)
    selected <- shiny::reactive(records_value()[[
      input$dataset %||% names(records)[[1L]]
    ]])
    output$example_note <- shiny::renderUI({
      note <- selected()$source_manifest$details$showcase_note
      if (is.null(note)) return(NULL)
      shiny::tags$p(class = "llw-prepare-example-note llw-prepare-hint", note)
    })
    runtime <- new_session_runtime(
      resolve_runtime_profile(profile, workers = workers),
      session
    )
    commit <- function(candidate, expected) {
      current <- selected()
      if (
        !identical(current$id, expected$dataset_id) ||
          !identical(current$revision, expected$revision) ||
          !identical(current$raw_checksum, expected$raw_checksum) ||
          !identical(current$raw_payload, candidate$raw_payload)
      )
        abort_llw(
          "The dataset changed. Preview again before applying.",
          "preparation"
        )
      values <- records_value()
      values[[candidate$id]] <- candidate
      records_value(values)
      candidate
    }
    prepared <- prepare_server(
      "prepare",
      selected,
      runtime,
      commit,
      worker,
      plot_settings
    )
    output$development <- shiny::renderPrint({
      record <- selected()
      list(
        dataset_id = record$id,
        revision = record$revision,
        raw_checksum = record$raw_checksum,
        applied_steps = length(record$recipe$steps),
        draft_steps = length(prepared$recipe()$steps),
        task = prepared$status()$state,
        profile = runtime$profile$name,
        workers = runtime$profile$workers,
        estimated_working_MiB = round(
          preparation_memory_estimate(length(record$raw_payload)) / 1024^2,
          1
        ),
        preparation_budget_MiB = round(
          runtime$profile$cache_max_bytes / 1024^2
        ),
        history = length(record$history)
      )
    })
  }
  shiny::shinyApp(ui, server)
}
