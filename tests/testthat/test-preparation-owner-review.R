test_that("automatic shared axes retain transformed values across days and split panels", {
  raw <- unserialize(m8_record()$raw_payload)
  raw$MEDI[seq.int(1L, nrow(raw), by = 12L)] <- 0
  record <- new_dataset_record(
    raw,
    "Synthetic zero readings",
    new_source_manifest("synthetic")
  )
  context <- m8_context(record)
  recipe <- new_recipe(list(new_preparation_step(
    "transform",
    list(
      variable = "MEDI",
      output = "MEDI_processed",
      operation = "log_zero_inflated",
      parameter = 10,
      offset = 0.1,
      invalid = "error",
      reference = ""
    ),
    context
  )))
  days <- unique(format(raw$Datetime, "%Y-%m-%d", tz = context$timezone))
  first <- preparation_recipe_view(
    raw,
    recipe,
    context,
    list(id = "P01", day = days[[1L]])
  )
  last <- preparation_recipe_view(
    raw,
    recipe,
    context,
    list(id = "P01", day = utils::tail(days, 1L))
  )
  expect_equal(first$domain, c(-1, max(raw$MEDI)))
  expect_identical(first$domain, last$domain)
  expect_true(first$positive_domain[[1L]] > 0)
  for (scale in c("linear", "symlog", "log")) {
    settings <- preparation_axis_settings(
      settings = list(
        scale = scale,
        domain = c(0, 10),
        positive_domain = c(1, 10)
      )
    )
    plots <- lapply(
      list(
        first,
        last,
        preparation_split_view(first, "source"),
        preparation_split_view(first, "preview")
      ),
      function(view) preparation_preview_plot(view, context, settings)
    )
    limits <- lapply(plots, function(plot) plot$scales$get_scales("y")$limits)
    expect_true(all(vapply(limits[-1L], identical, logical(1), limits[[1L]])))
    drawn <- ggplot2::ggplot_build(plots[[1L]])$data[[4L]]
    expected <- first$plot$Preview_value[first$plot$Kept]
    expect_equal(
      sum(is.finite(drawn$y)),
      sum(is.finite(expected) & (scale != "log" | expected > 0))
    )
    manual <- settings
    manual$y_limits <- if (scale == "log") c(0.1, 10) else c(0, 10)
    expect_equal(
      preparation_preview_plot(first, context, manual)$coordinates$limits$y,
      manual$y_limits
    )
  }
})

test_that("focus refresh restores clean and edited forms without sending source defaults", {
  original <- m8_record()
  source_context <- m8_context(original)
  saved_recipe <- new_recipe(list(
    new_preparation_step(
      "calibrate",
      list(
        variable = "MEDI",
        multiplier = 2,
        offset = 0,
        reference = "",
        output = "MEDI_processed"
      ),
      source_context
    ),
    new_preparation_step(
      "timezone",
      list(timezone = "UTC", method = "force", ambiguous = "error"),
      source_context
    )
  ))
  record <- shiny::reactiveVal(m8_preview(original, saved_recipe)$candidate)
  test_focus <- shiny::reactiveVal("MEDI")
  test_active <- shiny::reactiveVal(TRUE)
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0L),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  updates <- new.env(parent = emptyenv())
  updates$messages <- list()
  test_session <- shiny::MockShinySession$new()
  test_session$sendInputMessage <- function(inputId, message) {
    if (!is.null(message$value)) updates$messages[[inputId]] <- message$value
  }
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record),
      focus_variable = shiny::reactive(test_focus()),
      active = shiny::reactive(test_active())
    ),
    session = test_session,
    {
      acknowledge <- function() {
        for (i in seq_len(12L)) {
          if (!length(updates$messages)) return(invisible(NULL))
          batch <- updates$messages
          updates$messages <- list()
          batch <- batch[
            !vapply(
              names(batch),
              function(name) identical(input[[name]], batch[[name]]),
              logical(1)
            )
          ]
          if (length(batch)) do.call(session$setInputs, batch)
        }
        stop("Input acknowledgements did not settle")
      }
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      values <- loading_form()
      do.call(
        m8_set_form,
        c(
          list(
            session = session,
            context = result$context(),
            type = values$step_type
          ),
          values[setdiff(names(values), "step_type")]
        )
      )
      updates$messages <- list()
      session$setInputs(
        selected_step = saved_recipe$steps[[2L]]$id,
        edit = 1L,
        preparation_category = "sampling",
        sampling_type = "timezone"
      )
      acknowledge()
      expect_false(result$dirty())
      expect_null(loading_form())
      clean <- form()
      test_active(FALSE)
      test_focus("MEDI_processed")
      session$flushReact()
      test_active(TRUE)
      session$flushReact()
      m8_wait(
        session,
        function()
          identical(result$context()$requested_variable, "MEDI_processed")
      )
      acknowledge()
      expect_false(result$dirty())
      expect_null(loading_form())
      expect_identical(form(), clean)
      m8_wait(session, function() !is.null(result$preview()))
      acknowledge()
      expect_false(result$dirty())
      # A real edit, including a selected participant, remains protected on the
      # same focus-refresh path. Refreshing choices must not clear it.
      selected <- unname(result$context()$choices[[1L]])
      session$setInputs(calibration_offset = 27, participants = selected)
      expect_true(result$dirty())
      edited <- form()
      test_focus("MEDI")
      session$flushReact()
      m8_wait(
        session,
        function() identical(result$context()$requested_variable, "MEDI")
      )
      acknowledge()
      expect_true(result$dirty())
      expect_null(loading_form())
      expect_identical(form(), edited)
      expect_identical(input$participants, selected)
      expect_null(result$preview())
    }
  )
})

test_that("saving after a shortened span refreshes inactive dates before browser redraw", {
  record <- shiny::reactiveVal(m8_record(days = 7L))
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0L),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  messages <- list()
  mock <- shiny::MockShinySession$new()
  mock$sendInputMessage <- function(inputId, message) {
    if (!is.null(message$value)) messages[[inputId]] <<- message$value
  }
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record),
      active = shiny::reactive(TRUE)
    ),
    session = mock,
    {
      acknowledge <- function() {
        for (i in seq_len(12L)) {
          batch <- messages
          messages <<- list()
          # Dynamic date inputs are recreated by renderUI, not sendInputMessage.
          # Model the empty value sent by the browser for an out-of-bounds date.
          for (name in date_names) {
            value <- date_views[[name]]$value
            if (length(value)) batch[[name]] <- if (is.na(value)) "" else value
          }
          batch <- batch[
            !vapply(
              names(batch),
              function(name) identical(input[[name]], batch[[name]]),
              logical(1)
            )
          ]
          if (!length(batch)) return(invisible(NULL))
          do.call(session$setInputs, batch)
        }
        stop("Input acknowledgements did not settle")
      }
      ready <- function() {
        m8_wait(session, function() {
          acknowledge()
          !is.null(preview()) && !dirty() && is.null(loading_form())
        })
      }
      m8_wait(session, function() !is.null(context()))
      m8_set_form(session, context())
      messages <<- list()
      ready()
      session$setInputs(
        step_type = "duration",
        duration_length = 4,
        duration_scope = "participant"
      )
      session$setInputs(save_new = 1L)
      ready()
      session$setInputs(
        preparation_category = "values",
        value_type = "invalidate",
        measurement_upper = 5000
      )
      session$setInputs(save_new = 2L)
      ready()
      expect_length(recipe()$steps, 2L)
      expect_equal(recipe()$steps[[2L]]$parameters$arguments$upper, 5000)
      expect_false(dirty())
      expect_false(any(vapply(
        form()[c("date_end", "instant_end_date")],
        identical,
        logical(1),
        ""
      )))
      session$setInputs(
        value_type = "variability",
        variability_statistic = "cv",
        variability_threshold = 5
      )
      session$setInputs(save_new = 3L)
      ready()
      expect_length(recipe()$steps, 3L)
      expect_equal(recipe()$steps[[3L]]$parameters$arguments$threshold, 5)
      expect_identical(recipe(), preview()$candidate$recipe)
      expect_false(dirty())
      session$setInputs(variability_threshold = 6)
      expect_true(dirty())
      expect_null(preview())
    }
  )
})

test_that("operation browsing preserves the saved preview; preview can save an actual edit", {
  record <- shiny::reactiveVal(m8_record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0L),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      session$setInputs(preview = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      saved <- result$preview()
      session$setInputs(step_type = "time", time_mode = "dates")
      expect_false(result$dirty())
      expect_identical(result$preview(), saved)
      session$setInputs(
        preparation_category = "values",
        value_type = "variability"
      )
      expect_false(result$dirty())
      expect_identical(result$preview(), saved)
      expect_length(result$recipe()$steps, 0L)
      session$setInputs(variability_threshold = 2)
      expect_true(result$dirty())
      expect_null(result$preview())
      session$setInputs(preview = 2L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_false(result$dirty())
      expect_length(result$recipe()$steps, 1L)
      expect_equal(result$recipe()$steps[[1]]$parameters$arguments$threshold, 2)
      saved <- result$preview()
      session$setInputs(value_type = "invalidate")
      expect_false(result$dirty())
      expect_identical(result$preview(), saved)
      expect_identical(result$recipe()$steps[[1]]$type, "variability")
    }
  )
})

test_that("queued duration and mask previews survive navigation and obsolete work settles", {
  scheduled <- list()
  local_mocked_bindings(preparation_defer_preview = function(callback) {
    scheduled[[length(scheduled) + 1L]] <<- callback
  })
  record <- shiny::reactiveVal(m8_record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0L),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(
        session,
        result$context(),
        "duration",
        duration_length = 1,
        duration_unit = "elapsed_days",
        duration_scope = "participant"
      )
      session$setInputs(save_new = 1L)
      scheduled[[length(scheduled)]]()
      m8_wait(session, function() !is.null(result$preview()))
      session$setInputs(
        preparation_category = "values",
        value_type = "invalidate",
        measurement_upper = 10000,
        save_new = 2L
      )
      # Saving now refreshes the inactive calendars before their bounds redraw.
      # Acknowledge those issued controls before navigating to another tool.
      values <- loading_form()
      if (!is.null(values))
        do.call(
          m8_set_form,
          c(
            list(
              session = session,
              context = editor_context(),
              type = values$step_type
            ),
            values[setdiff(names(values), "step_type")]
          )
        )
      session$setInputs(value_type = "variability")
      expect_false(result$dirty())
      scheduled[[length(scheduled)]]()
      m8_wait(session, function() !is.null(result$preview()))
      expect_false(result$pending())
      expect_false(result$active())
      expect_length(result$recipe()$steps, 2L)
      expect_match(output$form_status, "Browsing another operation")
      # The narrower prefix refreshes hidden date bounds. Acknowledge those
      # server-issued values before exercising the next independent user edit.
      values <- loading_form()
      if (!is.null(values))
        do.call(
          m8_set_form,
          c(
            list(
              session = session,
              context = editor_context(),
              type = values$step_type
            ),
            values[setdiff(names(values), "step_type")]
          )
        )
      session$setInputs(
        value_type = "invalidate",
        measurement_upper = 1000,
        save_step = 1L
      )
      expect_true(result$pending())
      obsolete <- scheduled[[length(scheduled)]]
      session$setInputs(measurement_upper = 100)
      obsolete()
      session$flushReact()
      expect_false(result$pending())
      expect_false(result$active())
      expect_true(result$dirty())
      expect_false(result$status()$state == "error")
      session$setInputs(save_step = 2L)
      expect_true(result$pending())
      session$setInputs(selected_step = result$recipe()$steps[[2L]]$id, up = 1L)
      expect_true(result$pending())
      expect_true(ui_state$active)
      session$setInputs(cancel = 1L)
      expect_false(auto_preview())
      values <- loading_form()
      if (!is.null(values))
        do.call(
          m8_set_form,
          c(
            list(
              session = session,
              context = editor_context(),
              type = values$step_type
            ),
            values[setdiff(names(values), "step_type")]
          )
        )
      for (callback in scheduled) callback()
      session$flushReact()
      expect_false(result$pending())
      expect_false(result$active())
      expect_null(result$preview())
    }
  )
})

test_that("a task startup rejection releases the Prepare pending state", {
  record <- shiny::reactiveVal(m8_record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0L),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      with_mocked_bindings(
        {
          session$setInputs(preview = 1L)
          session$flushReact()
          expect_false(result$pending())
          expect_false(result$active())
          expect_identical(result$status()$state, "error")
          expect_match(output$action_status$html, "Synthetic startup rejection")
        },
        new_task_spec = function(...)
          abort_llw("Synthetic startup rejection", "validation")
      )
    }
  )
})

test_that("zero inflated transformations call LightLogR and retain recorded parameters", {
  raw <- c(0, 0.01, 1, 10, 100, NA_real_)
  context <- m8_context()
  for (base in c(2, 10, exp(1)))
    for (offset in c(0.1, 1)) {
      args <- list(
        variable = "MEDI",
        operation = "log_zero_inflated",
        parameter = base,
        invalid = "error",
        reference = "fixture",
        offset = offset
      )
      step <- new_preparation_step("transform", args, context)
      expect_identical(step$parameters$mapping_version, 3L)
      expect_identical(
        validate_preparation_recipe(new_recipe(list(step)), context)$steps[[1]],
        step
      )
      logged <- preparation_transform_values(raw, args)
      expect_equal(logged, LightLogR::log_zero_inflated(raw, offset, base))
      args$operation <- "exp_zero_inflated"
      expect_equal(preparation_transform_values(logged, args), raw)
      expect_match(
        preparation_step_label(step, 1L),
        as.character(offset),
        fixed = TRUE
      )
    }
  args$operation <- "log_zero_inflated"
  expect_error(
    preparation_transform_values(c(-1, 0), args),
    "positive and finite"
  )
  args$invalid <- "missing"
  expect_true(is.na(preparation_transform_values(-1, args)))
  args$parameter <- 1
  expect_error(preparation_value_arguments("transform", args), "base")
  args$parameter <- 10
  args$offset <- -1
  expect_error(preparation_value_arguments("transform", args), "offset")
})

test_that("single-step comparisons keep derived inputs and exclude earlier value changes", {
  record <- m8_record()
  context <- m8_context(record)
  raw <- unserialize(record$raw_payload)
  first <- new_preparation_step(
    "calibrate",
    list(
      variable = "MEDI",
      multiplier = 2,
      offset = 0,
      reference = "",
      output = "MEDI_processed"
    ),
    context
  )
  second <- new_preparation_step(
    "calibrate",
    list(
      variable = "MEDI_processed",
      multiplier = 1,
      offset = 10,
      reference = ""
    ),
    context
  )
  recipe <- new_recipe(list(first, second))
  view <- preparation_recipe_view(raw, recipe, context, list(step = "only:2"))
  expect_identical(preparation_view_step(view), "only:2")
  expect_equal(
    view$plot$Preview_value - view$plot$Value,
    rep(10, nrow(view$plot))
  )
  expect_identical(view$before_label, "Before step 2")
  expect_match(view$transformation_note, "Earlier steps still supply")
  disabled <- recipe
  disabled$steps[[2]]$enabled <- FALSE
  view <- preparation_recipe_view(raw, disabled, context, list(step = "only:2"))
  expect_equal(view$plot$Preview_value, view$plot$Value)
  expect_error(
    preparation_recipe_view(raw, recipe, context, list(step = "only:3")),
    "Choose a step"
  )
})

test_that("single-step variability retains the sampling reference across filtering", {
  record <- m8_record(interval = 60)
  context <- m8_context(record)
  raw <- unserialize(record$raw_payload)
  mask <- new_preparation_step(
    "variable",
    list(
      variable = "MEDI",
      mode = "categories",
      values = as.character(seq(1, 4300, by = 3)),
      lower = 0,
      upper = 1,
      outside = FALSE,
      missing = "keep"
    ),
    context
  )
  variability <- new_preparation_step(
    "variability",
    list(
      variable = "MEDI",
      source = "MEDI",
      length = 10,
      unit = "minutes",
      alignment = "centered",
      statistic = "sd",
      threshold = 100,
      direction = "below",
      coverage = 80,
      unevaluable = "missing"
    ),
    context
  )
  recipe <- new_recipe(list(mask, variability))
  whole <- m8_preview(record, recipe)$comparison$view
  only <- preparation_recipe_view(
    raw,
    recipe,
    context,
    list(step = "only:2", id = whole$id, day = whole$day)
  )
  expect_true(all(is.na(only$plot$Preview_value)))
  expect_equal(
    only$gaps$Datetime[only$gaps$Kind == "explicit"],
    whole$gaps$Datetime[whole$gaps$Kind == "explicit"]
  )
})

test_that("time zone transformations and split plots keep their distinct clock semantics", {
  record <- m8_record()
  raw <- unserialize(record$raw_payload)
  context <- m8_context(record)
  for (method in c("force", "with")) {
    step <- new_preparation_step(
      "timezone",
      list(timezone = "UTC", method = method, ambiguous = "earlier"),
      context
    )
    result <- m8_preview(record, new_recipe(list(step)))
    after <- unserialize(result$candidate$prepared_payload)
    if (method == "force")
      expect_identical(
        format(raw$Datetime, tz = "Europe/Berlin"),
        format(after$Datetime, tz = "UTC")
      ) else
      expect_identical(as.numeric(raw$Datetime), as.numeric(after$Datetime))
    expect_identical(
      dashboard_dataset_snapshot(result$candidate)$display_timezone,
      "UTC"
    )
    expect_identical(result$candidate$raw_payload, record$raw_payload)
    view <- result$comparison$view
    before_panel <- preparation_split_view(view, "source")
    after_panel <- preparation_split_view(view, "preview")
    expect_identical(before_panel$timezone, "Europe/Berlin")
    expect_identical(after_panel$timezone, "UTC")
    expect_true(all(
      format(before_panel$plot$Datetime, "%Y-%m-%d", tz = "Europe/Berlin") ==
        view$day
    ))
    expect_true(all(is.na(after_panel$plot$Value)))
    expect_false(any(before_panel$plot$Kept))
    expect_identical(
      preparation_preview_plot(view, context)$scales$get_scales("x")$timezone,
      "UTC"
    )
  }
})

test_that("the dashboard follows only the applied focus chain and reset restores its source", {
  context <- m8_context()
  first <- new_preparation_step(
    "calibrate",
    list(
      variable = "MEDI",
      multiplier = 2,
      offset = 0,
      reference = "",
      output = "MEDI_processed"
    ),
    context
  )
  recipe <- new_recipe(list(first))
  expect_identical(preparation_follow_focus("MEDI", recipe), "MEDI_processed")
  expect_identical(preparation_follow_focus("Other", recipe), "Other")
  expect_identical(
    preparation_follow_focus("MEDI_processed", new_recipe(), recipe),
    "MEDI"
  )
  applied <- m8_preview(m8_record(), recipe)$candidate
  expect_identical(
    dashboard_dataset_snapshot(reset_dataset_record(applied))$primary_variable,
    "MEDI"
  )
})

test_that("selected derived focus stays on its branch in Dashboard and Prepare", {
  record <- m8_record()
  context <- m8_context(record)
  branch <- function(output)
    new_preparation_step(
      "calibrate",
      list(
        variable = "MEDI",
        multiplier = 2,
        offset = 0,
        reference = "",
        output = output
      ),
      context
    )
  first <- branch("first_branch")
  second <- branch("chosen_branch")
  previous <- new_recipe(list(first, second))
  recipe <- new_recipe(list(first, second, branch("other_branch")))
  expect_identical(
    preparation_follow_focus("chosen_branch", recipe, previous),
    "chosen_branch"
  )
  applied <- m8_preview(record, previous)$candidate
  requested <- preparation_focus_request(
    m8_context(applied),
    applied,
    "chosen_branch"
  )
  expect_identical(
    preparation_focus_context(requested, recipe)$variable,
    "chosen_branch"
  )
  before_creator <- preparation_focus_context(
    requested,
    new_recipe(list(first))
  )
  expect_identical(before_creator$variable, "MEDI")
  expect_match(before_creator$transformation_note, "Not created at this point")
  expect_identical(
    preparation_follow_focus("chosen_branch", new_recipe(), previous),
    "MEDI"
  )
  expect_identical(
    preparation_follow_focus(
      "chosen_branch",
      new_recipe(list(first)),
      previous
    ),
    "MEDI"
  )
  descendant <- new_preparation_step(
    "calibrate",
    list(
      variable = "chosen_branch",
      multiplier = 2,
      offset = 0,
      reference = "",
      output = "chosen_twice"
    ),
    context
  )
  expect_identical(
    preparation_follow_focus(
      "chosen_twice",
      previous,
      new_recipe(c(previous$steps, list(descendant)))
    ),
    "chosen_branch"
  )
  sibling <- descendant
  sibling$id <- new_stable_id("step")
  sibling$parameters$arguments$output <- "sibling_child"
  expect_identical(
    preparation_follow_focus(
      "chosen_twice",
      new_recipe(c(previous$steps, list(sibling))),
      new_recipe(c(previous$steps, list(descendant, sibling)))
    ),
    "chosen_branch"
  )
})

test_that("Dashboard acknowledges applied focus and time zone changes, including Reset", {
  record <- m8_record()
  context <- m8_context(record)
  recipe <- new_recipe(list(
    new_preparation_step(
      "calibrate",
      list(
        variable = "MEDI",
        multiplier = 2,
        offset = 0,
        reference = "",
        output = "MEDI_processed"
      ),
      context
    ),
    new_preparation_step(
      "timezone",
      list(timezone = "UTC", method = "force", ambiguous = "earlier"),
      context
    )
  ))
  applied <- m8_preview(record, recipe)$candidate
  dataset <- shiny::reactiveVal(record)
  messages <- list()
  test_session <- shiny::MockShinySession$new()
  test_session$sendInputMessage <- function(inputId, message) {
    if (identical(inputId, "focus_variable"))
      messages[[length(messages) + 1L]] <<- message
  }
  shiny::testServer(
    datasetDashboardServer,
    args = list(
      dataset = dataset,
      active_panel = shiny::reactive("dashboard"),
      color_mode = shiny::reactive("light")
    ),
    session = test_session,
    {
      session$setInputs(focus_variable = "MEDI", data_stage = "preprocessed")
      session$flushReact()
      dataset(applied)
      m8_wait(
        session,
        function() identical(snapshot()$record$revision, applied$revision)
      )
      expect_identical(tail(messages, 1L)[[1L]]$value, "MEDI_processed")
      expect_identical(snapshot()$display_timezone, "UTC")
      session$setInputs(focus_variable = "MEDI_processed")
      expect_identical(session$getReturned()$focus_variable(), "MEDI_processed")
      reset <- reset_dataset_record(applied)
      dataset(reset)
      m8_wait(
        session,
        function() identical(snapshot()$record$revision, reset$revision)
      )
      expect_identical(tail(messages, 1L)[[1L]]$value, "MEDI")
      expect_identical(snapshot()$display_timezone, "Europe/Berlin")
      expect_identical(snapshot()$record$raw_payload, record$raw_payload)
    }
  )
})

test_that("Only-step comparisons retain bin support after earlier aggregation", {
  record <- m8_record(interval = 60)
  raw <- unserialize(record$raw_payload)
  context <- m8_context(record)
  aggregate <- new_preparation_step(
    "aggregate",
    list(
      length = 10,
      unit = "minutes",
      numeric = "mean",
      missing = "available"
    ),
    context
  )
  variability <- new_preparation_step(
    "variability",
    list(
      variable = "MEDI",
      source = "MEDI",
      length = 30,
      unit = "minutes",
      alignment = "centered",
      statistic = "sd",
      threshold = 100,
      direction = "below",
      coverage = 80,
      unevaluable = "missing"
    ),
    context
  )
  recipe <- new_recipe(list(aggregate, variability))
  whole <- m8_preview(record, recipe)$comparison$view
  only <- preparation_recipe_view(
    raw,
    recipe,
    context,
    list(step = "only:2", id = whole$id, day = whole$day)
  )
  after <- function(view)
    view$plot[view$plot$Kept, c("Datetime", "Preview_value")]
  expect_equal(after(only), after(whole), ignore_attr = TRUE)
})

test_that("Split view can select dates observed only in the original time zone", {
  raw <- data.frame(
    Id = factor(rep("P01", 3L)),
    Datetime = as.POSIXct("2026-01-02 00:00:00", tz = "UTC") + c(0, 1800, 3600),
    MEDI = c(1, 2, 3),
    file_group_id = "one"
  )
  record <- new_dataset_record(
    raw,
    "Midnight recording",
    new_source_manifest("synthetic", source_timezone = "UTC"),
    analysis_settings = list(primary_variable = "MEDI")
  )
  context <- m8_context(record)
  recipe <- new_recipe(list(new_preparation_step(
    "timezone",
    list(timezone = "Pacific/Honolulu", method = "with", ambiguous = "earlier"),
    context
  )))
  whole <- m8_preview(record, recipe)$comparison$view
  expect_setequal(whole$days$Day, c("2026-01-01", "2026-01-02"))
  view <- preparation_recipe_view(
    raw,
    recipe,
    context,
    list(id = "P01", day = "2026-01-02")
  )
  expect_identical(view$day, "2026-01-02")
  source <- preparation_split_view(view, "source")
  expect_equal(source$plot$Value, raw$MEDI)
  expect_identical(source$timezone, "UTC")
  preview <- preparation_split_view(view, "preview")
  expect_equal(nrow(preview$plot), 0L)
  expect_match(
    preview$empty_note,
    "No observations.*2026-01-02.*Pacific/Honolulu"
  )
  expect_no_error(ggplot2::ggplot_build(preparation_preview_plot(
    preview,
    context
  )))
})
