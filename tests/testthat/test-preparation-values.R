m8_value_record <- function(values = c(-2, 0, 1, 2, 10, NA_real_, Inf, -Inf)) {
  data <- dplyr::group_by(
    data.frame(
      Id = factor(rep(c("A", "B"), each = length(values))),
      Datetime = rep(
        as.POSIXct("2026-01-01", tz = "UTC") + seq_along(values) * 60,
        2
      ),
      MEDI = rep(values, 2),
      Other = seq_len(2 * length(values)),
      file.name = rep(c("a.csv", "b.csv"), each = length(values))
    ),
    Id
  )
  new_dataset_record(
    data,
    "Synthetic measurement edges",
    new_source_manifest("synthetic", source_timezone = "UTC"),
    analysis_settings = list(primary_variable = "MEDI")
  )
}

m8_condition_record <- function() {
  data <- data.frame(
    Id = rep(c("A", "B"), each = 8),
    Datetime = rep(as.POSIXct("2026-01-01", tz = "UTC") + (0:7) * 60, 2),
    MEDI = rep(c(10, NA, 30, 40, 50, 60, 70, 80), 2),
    Sensor = rep(c(-1, 0, 1, 2, 3, NA, Inf, NaN), 2),
    State = factor(rep(c("a", "b", "c", "a", NA, "b", "c", "z"), 2)),
    file.name = rep(c("a.csv", "b.csv"), each = 8)
  )
  new_dataset_record(
    dplyr::group_by(data, Id),
    "Condition fixture",
    new_source_manifest("synthetic", source_timezone = "UTC"),
    analysis_settings = list(primary_variable = "MEDI"),
    factual_metadata = list(
      variables = list(MEDI = list(unit = "lx", label = "Melanopic EDI"))
    )
  )
}

m8_condition_step <- function(context, ...) {
  args <- utils::modifyList(
    list(
      variable = "MEDI",
      source = "Sensor",
      mode = "range",
      values = character(),
      lower = 0,
      upper = 2,
      outside = TRUE,
      missing = "keep"
    ),
    list(...)
  )
  new_preparation_step("invalidate", args, context)
}

test_that("independent numeric and factor conditions mask only the target", {
  record <- m8_condition_record()
  context <- m8_context(record)
  raw <- dataset_raw_data(record)
  for (mode in c("range", "categories"))
    for (outside in c(FALSE, TRUE)) {
      for (missing in c("keep", "missing"))
        for (output in c("MEDI", "MEDI_processed")) {
          args <- list(
            source = if (mode == "range") "Sensor" else "State",
            mode = mode,
            values = if (mode == "range") character() else c("a", "b"),
            outside = outside,
            missing = missing
          )
          if (output != "MEDI") args$output <- output
          step <- do.call(m8_condition_step, c(list(context), args))
          result <- m8_preview(record, new_recipe(list(step)))
          expected <- raw$MEDI
          hit <- if (mode == "range") {
            if (outside) c(1, 5, 7) else c(2, 3, 4)
          } else if (outside) c(3, 7, 8) else c(1, 2, 4, 6)
          if (missing == "missing")
            hit <- union(hit, if (mode == "range") c(6, 8) else 5)
          expected[c(hit, hit + 8)] <- NA
          actual <- dataset_prepared_data(result$candidate)
          expect_identical(actual[[output]], expected)
          expect_identical(
            actual[setdiff(names(raw), output)],
            raw[setdiff(names(raw), output)]
          )
          expect_identical(result$candidate$raw_payload, record$raw_payload)
          expect_identical(dplyr::group_vars(actual), "Id")
          expect_equal(result$comparison$removed_rows, 0)
          expect_equal(
            result$comparison$invalidated_values,
            sum(is.na(expected) & !is.na(raw$MEDI))
          )
          expect_identical(step$parameters$mapping_version, 3L)
          if (output != "MEDI") {
            meta <- metadata_resolve_variable(result$candidate, output)
            expect_identical(meta$unit, "lx")
            expect_identical(meta$source_variable, "MEDI")
            expect_identical(
              meta$transformation_history[[1]]$arguments$source,
              args$source
            )
          }
        }
    }
})

test_that("condition sources stay pinned and recover through their own lineage", {
  record <- m8_condition_record()
  context <- m8_context(record)
  creator <- new_preparation_step(
    "calibrate",
    list(
      variable = "Sensor",
      multiplier = 1,
      offset = 1,
      reference = "",
      output = "Sensor_processed"
    ),
    context
  )
  mask <- m8_condition_step(context, source = "Sensor_processed")
  original <- new_recipe(list(creator, mask))
  moved <- preparation_reconnect_order(
    original,
    new_recipe(list(mask, creator)),
    context
  )
  expect_identical(moved$steps[[1]]$parameters$arguments$source, "Sensor")
  expect_identical(moved$steps[[1]]$parameters$mapping_version, 3L)
  expect_silent(validate_preparation_recipe(moved, context))
  removed <- preparation_reconnect_order(
    original,
    new_recipe(list(mask)),
    context
  )
  expect_identical(removed$steps[[1]]$parameters$arguments$source, "Sensor")
  target_creator <- new_preparation_step(
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
  categorical <- m8_condition_step(
    context,
    variable = "MEDI_processed",
    source = "State",
    mode = "categories",
    values = c("a", "b"),
    outside = FALSE
  )
  original <- new_recipe(list(target_creator, categorical))
  moved <- preparation_reconnect_order(
    original,
    new_recipe(list(categorical, target_creator)),
    context
  )
  expect_identical(moved$steps[[1]]$parameters$arguments$source, "State")
  expect_identical(moved$steps[[1]]$parameters$arguments$variable, "MEDI")
  expect_identical(
    moved$steps[[1]]$parameters$arguments$output,
    "MEDI_processed"
  )
  expect_silent(validate_preparation_recipe(moved, context))
  renamed <- target_creator
  renamed$parameters$arguments$output <- "MEDI_clean"
  changed <- preparation_retarget_following(
    original,
    new_recipe(list(renamed, categorical)),
    target_creator$id
  )
  expect_identical(
    changed$steps[[2]]$parameters$arguments$variable,
    "MEDI_clean"
  )
  expect_identical(changed$steps[[2]]$parameters$arguments$source, "State")
})

test_that("condition arguments reject incompatible sources and preserve literal categories", {
  context <- m8_context(m8_condition_record())
  for (patch in list(
    list(mode = "wrong"),
    list(values = 1),
    list(outside = NA),
    list(missing = "wrong"),
    list(mode = "categories", values = character())
  ))
    expect_error(
      do.call(m8_condition_step, c(list(context), patch)),
      class = "llw_error"
    )
  for (source in c("State", "Datetime", "absent"))
    expect_error(
      validate_preparation_recipe(
        new_recipe(list(m8_condition_step(context, source = source))),
        context
      ),
      class = "llw_error"
    )
  literal <- m8_condition_step(
    context,
    source = "State",
    mode = "categories",
    values = "absent"
  )
  expect_silent(validate_preparation_recipe(new_recipe(list(literal)), context))
  invalid <- literal
  invalid$parameters$mapping_version <- 1L
  expect_error(
    validate_preparation_recipe(new_recipe(list(invalid)), context),
    "version 3"
  )
})

test_that("categorical masking round-trips through the worker editor and Apply", {
  original <- m8_condition_record()
  record <- shiny::reactiveVal(original)
  runtime <- new_session_runtime(resolve_runtime_profile(
    "hosted",
    workers = 1L
  ))
  on.exit(runtime$cleanup(), add = TRUE)
  if (inherits(runtime$startup_error, "llw_error"))
    skip("mirai worker sockets are unavailable")
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
      m8_set_form(
        session,
        result$context(),
        "invalidate",
        invalidation_source = "State",
        invalidation_mode = "categories",
        invalidation_values = c("a", "b"),
        invalidation_condition = "inside",
        invalidation_missing = "missing"
      )
      session$setInputs(save_new = 1L)
      expect_null(editor_error())
      m8_wait(session, function() !is.null(result$preview()))
      step <- result$recipe()$steps[[1]]
      expect_identical(step$parameters$arguments$source, "State")
      expect_identical(step$parameters$arguments$values, c("a", "b"))
      session$setInputs(apply = 1L)
      expect_identical(record()$raw_payload, original$raw_payload)
      expect_equal(sum(is.na(dataset_prepared_data(record())$MEDI)), 10)
      session$setInputs(selected_step = step$id, edit = 1L)
      expect_identical(form_base()$invalidation_source, "State")
      expect_identical(form_base()$invalidation_values, c("a", "b"))
      expect_identical(form_base()$invalidation_missing, "missing")
      m8_set_form(
        session,
        result$context(),
        "invalidate",
        invalidation_source = "State",
        invalidation_mode = "categories",
        invalidation_values = c("a", "b"),
        invalidation_condition = "inside",
        invalidation_missing = "missing"
      )
      expect_null(loading_form())
      expect_false(result$dirty())
      session$setInputs(measurement_lower = -1)
      expect_identical(input$invalidation_values, c("a", "b"))
    }
  )
})

test_that("range invalidation preserves rows and grouping and has inclusive boundaries", {
  record <- m8_value_record()
  raw <- unserialize(record$raw_payload)
  context <- m8_context(record)
  step <- new_preparation_step(
    "invalidate",
    list(variable = "MEDI", lower = 0, upper = 2),
    context
  )
  recipe <- new_recipe(list(step))
  expected <- dplyr::mutate(
    raw,
    MEDI = dplyr::if_else(
      MEDI >= 0 & MEDI <= 2,
      MEDI,
      NA_real_,
      missing = NA_real_
    )
  )
  result <- m8_preview(record, recipe)
  expect_identical(unserialize(result$candidate$prepared_payload), expected)
  expect_identical(evaluate_preparation(raw, recipe, context), expected)
  expect_identical(result$candidate$raw_payload, record$raw_payload)
  expect_identical(result$candidate$source_manifest, record$source_manifest)
  expect_identical(result$candidate$metadata_bundle, record$metadata_bundle)
  expect_equal(result$comparison$removed_rows, 0)
  expect_equal(result$comparison$invalidated_values, 8)
  expect_equal(result$comparison$changed_values, 8)
  expect_equal(result$comparison$steps$invalidated, 8)
  expect_equal(dplyr::group_vars(expected), "Id")
  expect_identical(expected$Datetime, raw$Datetime)
})

test_that("calibration and invalidation compose in the explicit recipe order", {
  record <- m8_value_record(c(-2, 0, 1, 2, 10, NA_real_))
  raw <- unserialize(record$raw_payload)
  context <- m8_context(record)
  calibration <- new_preparation_step(
    "calibrate",
    list(
      variable = "MEDI",
      multiplier = 2,
      offset = 1,
      reference = "Reference A"
    ),
    context
  )
  invalidation <- new_preparation_step(
    "invalidate",
    list(variable = "MEDI", lower = 0, upper = 3),
    context
  )
  forward <- new_recipe(list(calibration, invalidation))
  reverse <- new_recipe(list(invalidation, calibration))
  expected_forward <- raw |>
    dplyr::mutate(MEDI = MEDI * 2 + 1) |>
    dplyr::mutate(MEDI = dplyr::if_else(MEDI >= 0 & MEDI <= 3, MEDI, NA_real_))
  expected_reverse <- raw |>
    dplyr::mutate(
      MEDI = dplyr::if_else(MEDI >= 0 & MEDI <= 3, MEDI, NA_real_)
    ) |>
    dplyr::mutate(MEDI = MEDI * 2 + 1)
  a <- m8_preview(record, forward)
  b <- m8_preview(record, reverse)
  expect_identical(unserialize(a$candidate$prepared_payload), expected_forward)
  expect_identical(unserialize(b$candidate$prepared_payload), expected_reverse)
  expect_false(identical(expected_forward, expected_reverse))
  expect_equal(a$comparison$steps$changed, c(10, 6))
  expect_equal(a$comparison$changed_values, 10)
  expect_equal(a$comparison$invalidated_values, 6)
  expect_equal(b$comparison$invalidated_values, 4)
  expect_identical(a$candidate$recipe, forward)
  # Apply a revised recipe to the original source, never to already calibrated values.
  revised <- m8_preview(a$candidate, new_recipe(list(calibration)))
  expect_identical(
    unserialize(revised$candidate$prepared_payload),
    dplyr::mutate(raw, MEDI = MEDI * 2 + 1)
  )
  expect_identical(revised$candidate$raw_payload, record$raw_payload)
})

test_that("validation rejects unsafe targets coefficients and nonfinite arithmetic", {
  record <- m8_value_record()
  context <- m8_context(record)
  for (name in c("Id", "Datetime", "file.name", "unknown")) {
    step <- new_preparation_step(
      "invalidate",
      list(variable = name, lower = 0, upper = 1),
      context
    )
    expect_error(
      m8_preview(record, new_recipe(list(step))),
      "numeric measurement"
    )
  }
  for (value in list(NA_real_, Inf, numeric(), c(1, 2), "1"))
    expect_error(
      new_preparation_step(
        "calibrate",
        list(variable = "MEDI", multiplier = value, offset = 0, reference = ""),
        context
      ),
      "finite number"
    )
  expect_error(
    new_preparation_step(
      "invalidate",
      list(variable = "MEDI", lower = 3, upper = 1),
      context
    ),
    "lower bound"
  )
  step <- new_preparation_step(
    "calibrate",
    list(variable = "MEDI", multiplier = 2, offset = 0, reference = ""),
    context
  )
  expect_error(m8_preview(record, new_recipe(list(step))), "finite readings")
  large <- m8_value_record(.Machine$double.xmax)
  step <- new_preparation_step(
    "calibrate",
    list(variable = "MEDI", multiplier = 2, offset = 0, reference = ""),
    m8_context(large)
  )
  expect_error(m8_preview(large, new_recipe(list(step))), "infinite values")
  grouped <- unserialize(record$raw_payload) |> dplyr::group_by(Other)
  grouped_record <- new_dataset_record(
    grouped,
    "Grouped numeric key",
    record$source_manifest
  )
  grouped_context <- m8_context(grouped_record)
  expect_false("Other" %in% grouped_context$measurements)
  step <- new_preparation_step(
    "calibrate",
    list(variable = "Other", multiplier = 2, offset = 0, reference = ""),
    grouped_context
  )
  expect_error(
    m8_preview(grouped_record, new_recipe(list(step))),
    "grouping column"
  )
})

test_that("step and final cell impacts distinguish repeated edits reversals and row removal", {
  record <- m8_value_record(c(1, 2, 3, NA_real_))
  context <- m8_context(record)
  first <- new_preparation_step(
    "calibrate",
    list(variable = "MEDI", multiplier = 2, offset = 0, reference = ""),
    context
  )
  second <- new_preparation_step(
    "calibrate",
    list(variable = "MEDI", multiplier = .5, offset = 0, reference = ""),
    context
  )
  result <- m8_preview(record, new_recipe(list(first, second)))
  expect_equal(result$comparison$changed_values, 0)
  expect_equal(result$comparison$steps$changed, c(6, 6))
  expect_identical(
    unserialize(result$candidate$prepared_payload),
    unserialize(record$raw_payload)
  )
  remove <- new_preparation_step(
    "variable",
    list(
      variable = "MEDI",
      mode = "range",
      values = character(),
      lower = 2,
      upper = 4,
      outside = FALSE,
      missing = "drop"
    ),
    context
  )
  result <- m8_preview(record, new_recipe(list(first, remove)))
  expect_equal(result$comparison$changed_values, 4)
  expect_equal(result$comparison$removed_rows, 4)
  expect_equal(result$comparison$steps$changed, c(6, 0))
  first$enabled <- FALSE
  result <- m8_preview(record, new_recipe(list(first)))
  expect_equal(result$comparison$changed_values, 0)
  expect_identical(result$candidate$prepared_payload, record$raw_payload)
})

test_that("measurement views show actual values for each prefix and preserve row identity", {
  record <- m8_value_record(c(1, 2, 3, 4, NA_real_))
  raw <- unserialize(record$raw_payload)
  raw$Datetime[2] <- raw$Datetime[1]
  record <- new_dataset_record(
    raw,
    "Duplicate time",
    record$source_manifest,
    analysis_settings = record$analysis_settings
  )
  context <- m8_context(record)
  first <- new_preparation_step(
    "calibrate",
    list(variable = "MEDI", multiplier = 2, offset = 1, reference = ""),
    context
  )
  second <- new_preparation_step(
    "invalidate",
    list(variable = "MEDI", lower = 0, upper = 5),
    context
  )
  third <- new_preparation_step(
    "calibrate",
    list(variable = "Other", multiplier = -1, offset = 0, reference = ""),
    context
  )
  recipe <- new_recipe(list(first, second, third))
  view <- preparation_task_worker(list(
    action = "view",
    record = record,
    recipe = recipe,
    budget = 1024^3,
    view = list(id = "A", day = "2026-01-01", step = "2")
  ))$view
  expect_equal(view$variable, "MEDI")
  expect_equal(view$plot$Value, c(1, 2, 3, 4, NA))
  expect_equal(view$plot$Preview_value, c(3, 5, NA, NA, NA))
  expect_true(all(view$plot$Kept))
  expect_gt(view$plot$Preview_segment[3], view$plot$Preview_segment[2])
  final <- m8_preview(record, recipe)
  expect_equal(final$comparison$view$variable, "Other")
  expect_true(all(
    final$comparison$view$plot$Preview_value ==
      -final$comparison$view$plot$Value
  ))
  expect_equal(final$comparison$changed_values, 18)
  expect_equal(final$comparison$invalidated_values, 4)
  expect_s3_class(
    preparation_preview_plot(final$comparison$view, context),
    "ggplot"
  )
})

test_that("measurement forms auto-preview apply and recover through pending removals", {
  record <- shiny::reactiveVal(m8_value_record(c(1, 2, 3, 4, NA_real_)))
  original <- shiny::isolate(record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0),
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
      session$setInputs(
        preparation_category = "values",
        value_type = "invalidate",
        measurement_lower = 0,
        measurement_upper = 2
      )
      session$setInputs(save_new = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(result$preview()$comparison$invalidated_values, 4)
      expect_equal(result$recipe()$steps[[1]]$type, "invalidate")
      session$setInputs(apply = 1L)
      expect_equal(sum(is.na(unserialize(record()$prepared_payload)$MEDI)), 6)
      session$setInputs(
        value_type = "calibrate",
        calibration_multiplier = 3,
        calibration_offset = 2,
        calibration_reference = "test reference"
      )
      session$setInputs(save_new = 2L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(result$preview()$comparison$changed_values, 8)
      expect_match(output$calibration_formula, "3")
      expect_match(output$impact_summary$html, "Values changed")
      session$setInputs(apply = 2L)
      calibration_id <- result$recipe()$steps[[2]]$id
      session$setInputs(selected_step = calibration_id, remove = 1L)
      # testServer does not execute the browser's form-reset messages.
      m8_set_form(
        session,
        preparation_new_step_context(
          result$context(),
          result$recipe(),
          variable_catalog()
        )
      )
      m8_wait(session, function() !is.null(result$preview()))
      expect_match(output$pending_removals$html, "Removal pending")
      expect_equal(unserialize(record()$prepared_payload)$MEDI[1], 5)
      session$setInputs(apply = 3L)
      expect_equal(unserialize(record()$prepared_payload)$MEDI[1], 1)
      expect_identical(record()$raw_payload, original$raw_payload)
      session$setInputs(reset = 1L)
      m8_wait(session, function() length(record()$recipe$steps) == 0L)
      expect_identical(record()$prepared_payload, original$raw_payload)
    }
  )
})

test_that("real workers preserve actionable measurement errors and recover without applying", {
  record <- shiny::reactiveVal(m8_value_record(c(1, 2, 3)))
  runtime <- new_session_runtime(resolve_runtime_profile(
    "hosted",
    workers = 1L
  ))
  on.exit(runtime$cleanup(), add = TRUE)
  if (inherits(runtime$startup_error, "llw_error"))
    skip("mirai worker sockets are unavailable")
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
      m8_set_form(
        session,
        result$context(),
        "calibrate",
        calibration_multiplier = 2
      )
      session$setInputs(save_new = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      session$setInputs(apply = 1L)
      applied <- record()

      session$setInputs(calibration_multiplier = 1e308, save_step = 1L)
      m8_wait(session, function() result$status()$state == "error")
      expect_match(
        output$action_status$html,
        "Use a smaller multiplier or offset"
      )
      expect_match(output$action_status$html, "Edit the measurement step")
      expect_false(grepl("retry", output$action_status$html, fixed = TRUE))
      expect_null(output$preview_notice)
      expect_identical(output$preview_ready, "no")
      expect_null(result$preview())
      expect_identical(record(), applied)
      expect_equal(runtime$active_count(), 0L)
      session$setInputs(apply = 2L)
      expect_identical(record(), applied)

      session$setInputs(calibration_multiplier = 3, save_step = 2L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(
        unserialize(result$preview()$candidate$prepared_payload)$MEDI,
        rep(c(3, 6, 9), 2)
      )
      expect_identical(record(), applied)
      session$setInputs(apply = 3L)
      expect_equal(
        unserialize(record()$prepared_payload)$MEDI,
        rep(c(3, 6, 9), 2)
      )
      expect_identical(record()$raw_payload, applied$raw_payload)

      infinite <- m8_value_record(c(1, Inf, NA_real_))
      record(infinite)
      m8_wait(
        session,
        function() identical(result$context()$dataset_id, infinite$id)
      )
      m8_set_form(session, result$context())
      m8_set_form(session, result$context(), "calibrate")
      session$setInputs(save_new = 2L)
      m8_wait(session, function() result$status()$state == "error")
      expect_match(
        output$action_status$html,
        "Mark infinite values as missing with a range step first"
      )
      expect_match(output$action_status$html, "Edit the measurement step")
      expect_null(result$preview())
      expect_identical(record(), infinite)

      session$setInputs(value_type = "invalidate", save_step = 3L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(
        unserialize(result$preview()$candidate$prepared_payload)$MEDI,
        rep(c(1, NA_real_, NA_real_), 2)
      )
      expect_identical(record(), infinite)
    }
  )
})

m8_transform <- function(
  context,
  operation,
  parameter = NULL,
  invalid = "error",
  variable = "MEDI"
)
  new_preparation_step(
    "transform",
    list(
      variable = variable,
      operation = operation,
      parameter = parameter,
      invalid = invalid,
      reference = "Synthetic reference"
    ),
    context
  )

test_that("named transformations preserve rows and produce independently expected values", {
  cases <- list(
    list("log", 10, c(0, -1, 1, 10, 100, NA, Inf), c(NA, NA, 0, 1, 2, NA, NA)),
    list("root", 2, c(-4, 0, 4, 9, NA, Inf), c(NA, 0, 2, 3, NA, NA)),
    list("root", 3, c(-8, -1, 0, 8, NA, -Inf), c(-2, -1, 0, 2, NA, NA)),
    list("power", -1, c(0, 1, 2, -2, NA, Inf), c(NA, 1, .5, -.5, NA, NA)),
    list("power", 0, c(0, 1, -2, NA, Inf), c(1, 1, 1, NA, NA)),
    list("exp", 2, c(-1, 0, 1, 2, 1024, NA), c(.5, 1, 2, 4, NA, NA)),
    list("abs", NULL, c(-2, 0, 2, NA, Inf, -Inf), c(2, 0, 2, NA, NA, NA))
  )
  for (case in cases) {
    record <- m8_value_record(case[[3]])
    raw <- unserialize(record$raw_payload)
    recipe <- new_recipe(list(m8_transform(
      m8_context(record),
      case[[1]],
      case[[2]],
      "missing"
    )))
    result <- m8_preview(record, recipe)
    expected <- raw
    expected$MEDI <- rep(as.numeric(case[[4]]), 2)
    expect_equal(unserialize(result$candidate$prepared_payload), expected)
    expect_identical(result$candidate$raw_payload, record$raw_payload)
    expect_identical(result$candidate$source_manifest, record$source_manifest)
    expect_equal(result$comparison$removed_rows, 0)
    expect_match(
      result$comparison$view$variable_label,
      "transformed values",
      fixed = TRUE
    )
    expect_match(
      result$comparison$view$transformation_note,
      "original measurement unit",
      fixed = TRUE
    )
  }
})

test_that("transformation validation and default domain failures are actionable", {
  record <- m8_value_record(c(-8, 0, 1, Inf))
  context <- m8_context(record)
  for (case in list(
    list("log", 10),
    list("root", 2),
    list("power", .5),
    list("exp", 2),
    list("abs", NULL)
  )) {
    recipe <- new_recipe(list(m8_transform(context, case[[1]], case[[2]])))
    expect_error(
      m8_preview(record, recipe),
      "reading\\(s\\) cannot be transformed",
      class = "llw_preparation_value_error"
    )
  }
  for (base in c(-1, 0, 1))
    expect_error(m8_transform(context, "log", base), "positive base")
  for (degree in c(0, -1, 1.5))
    expect_error(m8_transform(context, "root", degree), "whole number")
  for (parameter in list(NA_real_, Inf, numeric(), c(1, 2), "log(x)"))
    expect_error(m8_transform(context, "power", parameter), "finite number")
  expect_error(m8_transform(context, "eval", 2), "supported transformation")
  expect_error(m8_transform(context, "abs", 2), "does not take a parameter")
  expect_error(m8_transform(context, "log", 2, "discard"), "invalid results")
  expect_error(
    m8_preview(
      record,
      new_recipe(list(m8_transform(context, "abs", variable = "Id")))
    ),
    "numeric measurement"
  )
})

test_that("nonlinear recipes compose from source and expose exact prefix values", {
  record <- m8_value_record(c(0, 1, 3, 7, NA))
  raw <- unserialize(record$raw_payload)
  context <- m8_context(record)
  shift <- new_preparation_step(
    "calibrate",
    list(variable = "MEDI", multiplier = 1, offset = 1, reference = ""),
    context
  )
  log_step <- m8_transform(context, "log", 2)
  square <- m8_transform(context, "power", 2)
  recipe <- new_recipe(list(shift, log_step, square))
  result <- m8_preview(record, recipe)
  expected <- dplyr::mutate(raw, MEDI = log2(MEDI + 1)^2)
  expect_equal(unserialize(result$candidate$prepared_payload), expected)
  prefix <- preparation_recipe_view(
    raw,
    recipe,
    context,
    list(step = "2")
  )
  expect_setequal(
    prefix$plot$Preview_value,
    c(0, 1, 2, 3, NA)
  )
  # Updating an applied recipe restores any previously changed source values.
  replacement <- m8_preview(result$candidate, new_recipe(list(square)))
  expect_equal(
    unserialize(replacement$candidate$prepared_payload),
    dplyr::mutate(raw, MEDI = MEDI^2)
  )
  square$enabled <- FALSE
  expect_identical(
    m8_preview(record, new_recipe(list(square)))$candidate$prepared_payload,
    record$raw_payload
  )
  inverse <- new_recipe(list(m8_transform(context, "exp", 2), log_step))
  expect_equal(
    unserialize(m8_preview(record, inverse)$candidate$prepared_payload),
    raw
  )
})

test_that("transform and duration coverage forms save edit and apply through real workers", {
  original <- m8_value_record(c(-1, 0, 1, 8, NA))
  record <- shiny::reactiveVal(original)
  runtime <- new_session_runtime(resolve_runtime_profile(
    "hosted",
    workers = 1L
  ))
  on.exit(runtime$cleanup(), add = TRUE)
  if (inherits(runtime$startup_error, "llw_error"))
    skip("mirai worker sockets are unavailable")
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
      m8_set_form(
        session,
        result$context(),
        "transform",
        measurement_operation = "log",
        transform_base = "custom",
        transform_base_custom = 8
      )
      session$setInputs(save_new = 1L)
      m8_wait(session, function() result$status()$state == "error")
      expect_match(output$action_status$html, "positive finite readings")
      expect_identical(record(), original)
      session$setInputs(transform_invalid = "missing", save_step = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(
        unserialize(result$preview()$candidate$prepared_payload)$MEDI,
        rep(c(NA, NA, 0, 1, NA), 2)
      )
      session$setInputs(apply = 1L)
      applied <- record()
      id <- result$recipe()$steps[[1]]$id
      session$setInputs(selected_step = id, edit = 1L)
      expected_form <- form_base()
      expect_equal(expected_form$measurement_operation, "log")
      expect_equal(expected_form$transform_base_custom, 8)
      expect_equal(expected_form$transform_invalid, "missing")
      m8_set_form(
        session,
        result$context(),
        "transform",
        measurement_operation = "log",
        transform_base = "custom",
        transform_base_custom = 8,
        transform_invalid = "missing"
      )
      session$setInputs(
        measurement_operation = "root",
        transform_degree = 3,
        transform_invalid = "error",
        save_step = 2L
      )
      m8_wait(session, function() !is.null(result$preview()))
      expect_identical(record(), applied)
      expect_equal(
        unserialize(result$preview()$candidate$prepared_payload)$MEDI,
        rep(c(-1, 0, 1, 2, NA), 2)
      )
      session$setInputs(apply = 2L)
      expect_identical(record()$raw_payload, original$raw_payload)
      session$setInputs(
        preparation_category = "filter",
        step_type = "coverage",
        coverage_mode = "available_duration",
        coverage_duration = 3,
        coverage_unit = "minutes",
        coverage_basis = "observed_group",
        save_new = 2L
      )
      m8_wait(session, function() !is.null(result$preview()))
      coverage <- result$recipe()$steps[[2]]
      expect_equal(coverage$parameters$mapping_version, 3L)
      expect_match(
        output$coverage_basis_help,
        "entire current group is removed"
      )
      session$setInputs(selected_step = coverage$id, edit = 2L)
      expect_equal(form_base()$coverage_mode, "available_duration")
      expect_equal(form_base()$coverage_duration, 3)
      expect_equal(form_base()$coverage_unit, "minutes")
    }
  )
})
