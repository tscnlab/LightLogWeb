derived_fixture <- function() {
  data <- data.frame(
    Id = rep(c("A", "B"), each = 4),
    Datetime = rep(as.POSIXct("2026-01-01", tz = "UTC") + (0:3) * 60, 2),
    MEDI = rep(c(1, 2, 4, NA_real_), 2),
    file.name = rep(c("a", "b"), each = 4)
  )
  new_dataset_record(
    dplyr::group_by(data, Id),
    "Derived-variable fixture",
    new_source_manifest("synthetic", source_timezone = "UTC"),
    factual_metadata = list(
      variables = list(
        MEDI = list(
          label = "Melanopic EDI",
          unit = "lx",
          calibration = "Reference A",
          custom_note = "Retain this metadata"
        )
      )
    ),
    analysis_settings = list(primary_variable = "MEDI")
  )
}

derived_step <- function(
  context,
  output = "MEDI_processed",
  variable = "MEDI",
  multiplier = 2,
  offset = 1
) {
  args <- list(
    variable = variable,
    multiplier = multiplier,
    offset = offset,
    reference = "Traceable coefficient"
  )
  if (!is.null(output)) args$output <- output
  new_preparation_step("calibrate", args, context)
}

test_that("linear children inherit a transformed parent's declared unit", {
  source <- derived_fixture()
  context <- m8_context(source)
  parent <- new_preparation_step(
    "transform",
    list(
      variable = "MEDI",
      operation = "log",
      parameter = 2,
      invalid = "missing",
      reference = "Log parent",
      output = "MEDI_processed"
    ),
    context
  )
  record <- m8_preview(source, new_recipe(list(parent)))$candidate
  overlay_context <- metadata_overlay_context(record)
  record <- apply_metadata_overlay(
    record,
    overlay_context,
    metadata_overlay_focus_preview(
      record,
      overlay_context,
      "MEDI_processed",
      "Log MEDI parent",
      "log2(lx)"
    )
  )
  child <- derived_step(context, "MEDI_child", "MEDI_processed", 1, 0)
  masked <- new_preparation_step(
    "invalidate",
    list(
      variable = "MEDI_processed",
      lower = 0,
      upper = 1,
      output = "MEDI_masked"
    ),
    context
  )
  transformed <- new_preparation_step(
    "transform",
    list(
      variable = "MEDI_processed",
      operation = "log",
      parameter = 2,
      invalid = "missing",
      reference = "Additional transformation",
      output = "MEDI_log_again"
    ),
    context
  )
  applied <- m8_preview(
    record,
    new_recipe(list(parent, child, masked, transformed))
  )$candidate
  for (variable in c("MEDI_child", "MEDI_masked")) {
    metadata <- metadata_resolve_variable(applied, variable)
    expect_identical(metadata$label, "Log MEDI parent")
    expect_identical(metadata$unit, "log2(lx)")
    expect_identical(metadata$source_unit, "lx")
    expect_identical(metadata$transformation_history[[1L]]$step_id, parent$id)
  }
  expect_identical(
    metadata_resolve_variable(applied, "MEDI_log_again")$unit,
    "transformed values; source: log2(lx)"
  )
  expect_identical(
    metadata_resolve_variable(applied, "MEDI_processed")$unit,
    "log2(lx)"
  )
  expect_identical(metadata_resolve_variable(applied, "MEDI")$unit, "lx")
  expect_identical(applied$raw_payload, source$raw_payload)
})

test_that("new variables preserve source data and inherit metadata with history", {
  record <- derived_fixture()
  context <- m8_context(record)
  raw <- dataset_raw_data(record)
  step <- derived_step(context)
  result <- m8_preview(record, new_recipe(list(step)))
  expected <- raw
  expected$MEDI_processed <- raw$MEDI * 2 + 1
  expect_identical(dataset_prepared_data(result$candidate), expected)
  expect_identical(result$candidate$raw_payload, record$raw_payload)
  expect_identical(result$candidate$factual_metadata, record$factual_metadata)
  expect_identical(result$candidate$metadata_bundle, record$metadata_bundle)
  expect_identical(step$parameters$mapping_version, 2L)
  metadata <- metadata_resolve_variable(result$candidate, "MEDI_processed")
  expect_identical(metadata$label, "Melanopic EDI")
  expect_identical(metadata$unit, "lx")
  expect_identical(metadata$calibration, "Reference A")
  expect_identical(
    metadata$inherited_factual_metadata,
    record$factual_metadata$variables$MEDI
  )
  expect_identical(
    metadata$transformation_history[[1]]$arguments,
    step$parameters$arguments
  )
  expect_equal(result$comparison$created_values, nrow(raw))
  expect_equal(result$comparison$removed_rows, 0)
  expect_equal(result$comparison$view$plot$Value, raw$MEDI[1:4])
  expect_equal(
    result$comparison$view$plot$Preview_value,
    expected$MEDI_processed[1:4]
  )
  restored <- reset_dataset_record(result$candidate)
  expect_identical(dataset_prepared_data(restored), raw)
  expect_null(
    metadata_resolve_variable(restored, "MEDI_processed")$transformation_history
  )
  undone <- undo_dataset_record(restored)
  expect_identical(
    metadata_resolve_variable(undone, "MEDI_processed"),
    metadata
  )
})

test_that("prepared-variable metadata edits have their own identity and preserve provenance", {
  source <- derived_fixture()
  creator <- derived_step(m8_context(source))
  record <- m8_preview(source, new_recipe(list(creator)))$candidate
  context <- metadata_overlay_context(record)
  inherited <- metadata_resolve_variable(record, "MEDI_processed")
  state <- metadata_overlay_focus_state(record, context, "MEDI_processed")
  expect_equal(nrow(state$targets), 2L)
  expect_identical(state$fields$label$value, "Melanopic EDI")
  expect_identical(state$fields$unit$value, "lx")
  patch <- metadata_overlay_focus_preview(
    record,
    context,
    "MEDI_processed",
    "Cleaned melanopic EDI",
    "klx"
  )
  edited <- apply_metadata_overlay(record, context, patch)
  metadata <- metadata_resolve_variable(edited, "MEDI_processed")
  expect_identical(metadata$label, "Cleaned melanopic EDI")
  expect_identical(metadata$unit, "klx")
  expect_identical(
    metadata$transformation_history,
    inherited$transformation_history
  )
  expect_identical(metadata$inherited_metadata, inherited$inherited_metadata)
  expect_identical(
    metadata_resolve_variable(edited, "MEDI")$label,
    "Melanopic EDI"
  )
  expect_identical(edited$raw_payload, record$raw_payload)
  expect_identical(edited$prepared_payload, record$prepared_payload)
  expect_identical(
    edited$metadata_bundle$sources,
    record$metadata_bundle$sources
  )
  expect_identical(
    edited$metadata_bundle$source_fingerprint,
    record$metadata_bundle$source_fingerprint
  )
  expect_identical(edited$recipe, record$recipe)
  expect_identical(
    metadata_resolve_variable(
      undo_metadata_overlay(edited, context),
      "MEDI_processed"
    ),
    inherited
  )
  reapplied <- m8_preview(edited, edited$recipe)$candidate
  expect_identical(
    metadata_resolve_variable(reapplied, "MEDI_processed")$label,
    metadata$label
  )
  child <- derived_step(
    m8_context(source),
    variable = "MEDI_processed",
    output = "MEDI_child"
  )
  child_record <- m8_preview(edited, new_recipe(list(creator, child)))$candidate
  expect_identical(
    metadata_resolve_variable(child_record, "MEDI_child")$label,
    "Cleaned melanopic EDI"
  )
  expect_identical(
    metadata_resolve_variable(child_record, "MEDI_child")$unit,
    "klx"
  )
  expect_length(
    metadata_resolve_variable(
      child_record,
      "MEDI_child"
    )$transformation_history,
    2L
  )
  expect_error(
    metadata_overlay_focus_state(
      record,
      metadata_overlay_context(source),
      "MEDI_processed"
    ),
    "changed"
  )
  reset <- reset_dataset_record(edited)
  expect_error(
    metadata_overlay_focus_state(
      reset,
      metadata_overlay_context(reset),
      "MEDI_processed"
    ),
    "exact editable"
  )
  restored <- undo_dataset_record(reset)
  expect_identical(
    metadata_resolve_variable(restored, "MEDI_processed")$label,
    metadata$label
  )
  replacement <- derived_step(m8_context(source), multiplier = 3)
  replaced <- m8_preview(edited, new_recipe(list(replacement)))$candidate
  expect_identical(
    metadata_resolve_variable(replaced, "MEDI_processed")$label,
    inherited$label
  )
  replacement_context <- metadata_overlay_context(replaced)
  replaced <- apply_metadata_overlay(
    replaced,
    replacement_context,
    metadata_overlay_focus_preview(
      replaced,
      replacement_context,
      "MEDI_processed",
      "Replacement label",
      "lx"
    )
  )
  expect_identical(
    metadata_resolve_variable(replaced, "MEDI_processed")$label,
    "Replacement label"
  )
  expect_identical(
    metadata_resolve_focus_request(
      replaced$metadata_bundle,
      metadata_resolve_variable(replaced, "MEDI_processed")$target
    )$state,
    "resolved"
  )
  # Dormant metadata must not prevent editing an original variable after Reset.
  reset_context <- metadata_overlay_context(reset)
  reset_edited <- apply_metadata_overlay(
    reset,
    reset_context,
    metadata_overlay_focus_preview(
      reset,
      reset_context,
      "MEDI",
      "Original only",
      "lx"
    )
  )
  expect_identical(
    metadata_resolve_variable(reset_edited, "MEDI")$label,
    "Original only"
  )
  cleared <- apply_metadata_overlay(
    edited,
    context,
    metadata_overlay_focus_preview(edited, context, "MEDI_processed", "", "")
  )
  expect_identical(
    metadata_resolve_variable(cleared, "MEDI_processed")$label,
    inherited$label
  )
  expect_identical(
    metadata_resolve_variable(cleared, "MEDI_processed")$unit,
    inherited$unit
  )
})

test_that("derived metadata keeps appended source mappings isolated", {
  record <- metadata_explorer_fixture_records(m7_fixture_directory())[[
    "dataset_m7appendedreview"
  ]]
  before <- metadata_resolve_variable(record, "iztech_MEDI")
  creator <- derived_step(m8_context(record), variable = "iztech_MEDI")
  applied <- m8_preview(record, new_recipe(list(creator)))$candidate
  context <- metadata_overlay_context(applied)
  patch <- metadata_overlay_focus_preview(
    applied,
    context,
    "MEDI_processed",
    "Prepared appended signal",
    "lx"
  )
  result <- apply_metadata_overlay(applied, context, patch)
  expect_identical(
    metadata_resolve_variable(result, "MEDI_processed")$label,
    "Prepared appended signal"
  )
  expect_identical(metadata_resolve_variable(result, "iztech_MEDI"), before)
  expect_identical(
    result$metadata_bundle$sources,
    record$metadata_bundle$sources
  )
})

test_that("lineage branches at creation and supports subsequent derived operations", {
  record <- derived_fixture()
  context <- m8_context(record)
  raw <- dataset_raw_data(record)
  original <- derived_step(context, output = NULL, multiplier = 3, offset = 0)
  branch <- derived_step(context, offset = 2)
  later_parent <- derived_step(
    context,
    output = NULL,
    multiplier = 1,
    offset = 10
  )
  log_step <- new_preparation_step(
    "transform",
    list(
      variable = "MEDI_processed",
      operation = "log",
      parameter = 2,
      invalid = "error",
      reference = "Base two",
      output = "MEDI_log"
    ),
    context
  )
  recipe <- new_recipe(list(original, branch, later_parent, log_step))
  result <- m8_preview(record, recipe)
  data <- dataset_prepared_data(result$candidate)
  expect_equal(data$MEDI, raw$MEDI * 3 + 10)
  expect_equal(data$MEDI_processed, raw$MEDI * 6 + 2)
  expect_equal(data$MEDI_log, log2(raw$MEDI * 6 + 2))
  history <- metadata_resolve_variable(result$candidate, "MEDI_log")
  expect_identical(
    vapply(history$transformation_history, `[[`, "", "step_id"),
    c(original$id, branch$id, log_step$id)
  )
  expect_identical(history$source_unit, "lx")
  expect_match(history$unit, "transformed")
  expect_identical(history$inherited_metadata$unit, "lx")
  prefix <- preparation_recipe_view(raw, recipe, context, list(step = "2"))
  expect_equal(prefix$plot$Preview_value, data$MEDI_processed[1:4])
  range <- new_preparation_step(
    "invalidate",
    list(variable = "MEDI_log", lower = 3, upper = 4, output = "MEDI_valid"),
    context
  )
  full <- m8_preview(record, new_recipe(c(recipe$steps, list(range))))
  expect_equal(
    dataset_prepared_data(full$candidate)$MEDI_valid,
    ifelse(data$MEDI_log >= 3 & data$MEDI_log <= 4, data$MEDI_log, NA_real_)
  )
})

test_that("names cannot overwrite source columns or other recipe outputs", {
  record <- derived_fixture()
  context <- m8_context(record)
  expect_identical(
    preparation_suggest_name("MEDI", context$columns),
    "MEDI_processed"
  )
  expect_identical(
    preparation_suggest_name(
      "MEDI",
      c(context$columns, "MEDI_processed", "MEDI_processed_2")
    ),
    "MEDI_processed_3"
  )
  for (name in c("MEDI", "Id", "Datetime", "file.name")) {
    step <- derived_step(context, name)
    expect_error(m8_preview(record, new_recipe(list(step))), "already exists")
    expect_error(
      preparation_change_values(
        dataset_raw_data(record),
        step$type,
        step$parameters$arguments
      ),
      "already exists"
    )
  }
  for (name in c("", "  ", ".llw_internal", "bad\nname"))
    expect_error(derived_step(context, name), class = "llw_error")
  first <- derived_step(context)
  second <- derived_step(context)
  second$enabled <- FALSE
  expect_error(
    m8_preview(record, new_recipe(list(first, second))),
    "already exists"
  )
  expect_identical(
    derived_step(context, "  My processed MEDI  ")$parameters$arguments$output,
    "My processed MEDI"
  )
  corrupt <- first
  corrupt$parameters$mapping_version <- 1L
  expect_error(m8_preview(record, new_recipe(list(corrupt))), "version 2")
})

test_that("step catalogs expose data columns without the temporary row marker", {
  record <- derived_fixture()
  context <- m8_context(record)
  recipe <- new_recipe(list(derived_step(context)))
  raw <- dataset_raw_data(record)
  for (existing_marker in c(FALSE, TRUE)) {
    if (existing_marker) raw$.llw_prepare_source_row <- seq_len(nrow(raw))
    result <- evaluate_preparation(raw, recipe, context, details = TRUE)
    expect_identical(
      names(result$variables_after[[1]]),
      c(setdiff(names(raw), "Datetime"), "MEDI_processed")
    )
    expect_identical(
      names(result$data),
      c(names(raw), "MEDI_processed")
    )
  }
})

test_that("dependent steps require their producing column in recipe order", {
  record <- derived_fixture()
  context <- m8_context(record)
  creator <- derived_step(context)
  consumer <- derived_step(context, "twice", "MEDI_processed")
  expect_error(
    m8_preview(record, new_recipe(list(consumer, creator))),
    "unavailable"
  )
  creator$enabled <- FALSE
  expect_error(
    m8_preview(record, new_recipe(list(creator, consumer))),
    "unavailable"
  )
  creator$enabled <- TRUE
  filter <- new_preparation_step(
    "variable",
    list(
      variable = "MEDI_processed",
      mode = "range",
      values = character(),
      lower = 3,
      upper = 5,
      outside = FALSE,
      missing = "drop"
    ),
    context
  )
  result <- m8_preview(record, new_recipe(list(creator, filter)))
  expect_equal(
    dataset_prepared_data(result$candidate)$MEDI_processed,
    c(3, 5, 3, 5)
  )
  filter$parameters$arguments$variable <- "MEDI"
  filter$parameters$arguments$lower <- 100
  filter$parameters$arguments$upper <- 200
  empty <- m8_preview(record, new_recipe(list(filter, creator)))
  expect_equal(nrow(dataset_prepared_data(empty$candidate)), 0)
  expect_true(
    "MEDI_processed" %in% names(dataset_prepared_data(empty$candidate))
  )
})

test_that("disabled orphan inputs remain editable after their producer changes", {
  record <- derived_fixture()
  context <- m8_context(record)
  creator <- derived_step(context, "renamed")
  disabled <- derived_step(context, "child", "old_name")
  disabled$enabled <- FALSE
  recipe <- new_recipe(list(creator, disabled))
  expect_no_error(validate_preparation_recipe(recipe, context))
  options <- preparation_variable_choices(
    preparation_recipe_context(context, new_recipe(list(creator)))$measurements,
    "old_name"
  )
  expect_true("old_name" %in% unname(options))
  expect_identical(
    names(options)[options == "old_name"],
    "old_name (unavailable at this step)"
  )
  expect_true("renamed" %in% unname(options))
  disabled$parameters$arguments$variable <- "renamed"
  disabled$enabled <- TRUE
  result <- m8_preview(record, new_recipe(list(creator, disabled)))
  expect_equal(
    dataset_prepared_data(result$candidate)$child,
    (dataset_raw_data(record)$MEDI * 2 + 1) * 2 + 1
  )
})

test_that("derived catalogs are bound to the source and unchanged recipe prefix", {
  record <- derived_fixture()
  context <- m8_context(record)
  step <- derived_step(context)
  recipe <- new_recipe(list(step))
  inspected <- preparation_task_worker(list(
    action = "inspect",
    record = m8_preview(record, recipe)$candidate,
    budget = 1024^3
  ))
  catalog <- preparation_cached_variables(inspected$catalogs, context, recipe)
  expect_identical(catalog$MEDI_processed$categories, c("3", "5", "9"))
  expect_equal(catalog$MEDI_processed$missing, 2)
  altered <- recipe
  altered$steps[[1]]$parameters$arguments$offset <- 10
  expect_null(preparation_cached_variables(
    inspected$catalogs,
    context,
    altered
  ))
  other_context <- context
  other_context$dataset_id <- "dataset_other"
  expect_null(preparation_cached_variables(
    inspected$catalogs,
    other_context,
    recipe
  ))
  other_context <- context
  other_context$raw_checksum <- "different"
  expect_null(preparation_cached_variables(
    inspected$catalogs,
    other_context,
    recipe
  ))
})

test_that("derived exact categories remain available while a new filter is edited", {
  original <- derived_fixture()
  context <- m8_context(original)
  applied <- m8_preview(
    original,
    new_recipe(list(derived_step(context)))
  )$candidate
  record <- shiny::reactiveVal(applied)
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
      next_context <- preparation_new_step_context(
        result$context(),
        result$recipe(),
        variable_catalog()
      )
      m8_set_form(session, next_context)
      m8_set_form(
        session,
        next_context,
        "variable",
        variable_name = "MEDI_processed",
        variable_mode = "categories",
        variable_values = c("3", "9"),
        variable_missing = "drop"
      )
      expect_true(dirty())
      expect_null(result$preview())
      expect_identical(
        editor_context()$variables$MEDI_processed$categories,
        c("3", "5", "9")
      )
      expect_match(output$variable_help, "2 missing value")
      session$setInputs(save_new = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(
        dataset_prepared_data(result$preview()$candidate)$MEDI_processed,
        c(3, 9, 3, 9)
      )
      expect_identical(record(), applied)
      session$setInputs(apply = 1L)
      expect_equal(dataset_prepared_data(record())$MEDI, c(1, 4, 1, 4))
    }
  )
})

test_that("new-variable forms round-trip and block collisions without changing applied data", {
  original <- derived_fixture()
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
        "calibrate",
        measurement_output = "file.name",
        calibration_multiplier = 2
      )
      session$setInputs(save_new = 1L)
      expect_length(result$recipe()$steps, 0L)
      expect_match(editor_error(), "already exists")
      expect_identical(record(), original)
      session$setInputs(measurement_output = "My processed MEDI", save_new = 2L)
      m8_set_form(
        session,
        preparation_new_step_context(
          result$context(),
          result$recipe(),
          variable_catalog()
        ),
        "calibrate",
        calibration_multiplier = 2
      )
      m8_wait(session, function() !is.null(result$preview()))
      expect_identical(
        result$recipe()$steps[[1]]$parameters$arguments$output,
        "My processed MEDI"
      )
      expect_match(output$variable_history$html, "My processed MEDI")
      expect_match(output$variable_history$html, "Melanopic EDI")
      session$setInputs(apply = 1L)
      applied <- record()
      expect_true(
        "My processed MEDI" %in% names(dataset_prepared_data(applied))
      )
      id <- result$recipe()$steps[[1]]$id
      session$setInputs(selected_step = id, edit = 1L)
      expect_identical(form_base()$measurement_output, "My processed MEDI")
      m8_set_form(
        session,
        result$context(),
        "calibrate",
        measurement_output = "My processed MEDI",
        calibration_multiplier = 2
      )
      session$setInputs(calibration_multiplier = 3, save_step = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_length(result$recipe()$steps, 1L)
      expect_identical(record(), applied)
      expect_equal(
        dataset_prepared_data(result$preview()$candidate)[[
          "My processed MEDI"
        ]],
        dataset_raw_data(original)$MEDI * 3
      )
      session$setInputs(save_new = 3L)
      expect_length(result$recipe()$steps, 1L)
      expect_match(editor_error(), "already exists")
      expect_identical(record(), applied)
    }
  )
})

test_that("write-to defaults overwrite only the selected target", {
  context <- m8_context(derived_fixture())
  form <- preparation_form_defaults(context)
  form$step_type <- "calibrate"
  expect_identical(form$measurement_output, "MEDI")
  expect_null(preparation_form_arguments(form, context)$output)
  form$measurement_output <- "MEDI_processed"
  expect_identical(
    preparation_form_arguments(form, context)$output,
    "MEDI_processed"
  )
  form$measurement_output <- ""
  expect_error(
    preparation_form_arguments(form, context),
    "Enter a variable name"
  )
  recipe <- new_recipe(list(derived_step(context)))
  next_context <- preparation_new_step_context(context, recipe)
  defaults <- preparation_form_defaults(next_context)
  expect_identical(defaults$measurement_variable, "MEDI_processed")
  expect_identical(defaults$measurement_output, "MEDI_processed")
  expect_identical(defaults$variable_name, "MEDI_processed")
  expect_identical(defaults$coverage_variable, "MEDI_processed")
})

test_that("moving a value step ahead of its creator reconnects the calculation and history", {
  record <- derived_fixture()
  context <- m8_context(record)
  raw <- dataset_raw_data(record)
  creator <- derived_step(context, multiplier = 2, offset = 0)
  later <- derived_step(
    context,
    output = NULL,
    variable = "MEDI_processed",
    multiplier = 1,
    offset = 10
  )
  recipe <- new_recipe(list(creator, later))
  moved <- preparation_reconnect_order(
    recipe,
    edit_preparation_recipe(recipe, "up", id = later$id),
    context
  )
  expect_identical(moved$steps[[1]]$parameters$arguments$variable, "MEDI")
  expect_identical(
    moved$steps[[1]]$parameters$arguments$output,
    "MEDI_processed"
  )
  expect_identical(
    moved$steps[[2]]$parameters$arguments$variable,
    "MEDI_processed"
  )
  expect_null(moved$steps[[2]]$parameters$arguments$output)
  candidate <- m8_preview(record, moved)$candidate
  expect_equal(
    dataset_prepared_data(candidate)$MEDI_processed,
    (raw$MEDI + 10) * 2
  )
  expect_identical(dataset_prepared_data(candidate)$MEDI, raw$MEDI)
  expect_identical(
    vapply(
      metadata_resolve_variable(
        candidate,
        "MEDI_processed"
      )$transformation_history,
      `[[`,
      "",
      "step_id"
    ),
    c(later$id, creator$id)
  )
  expect_length(preparation_chain_changes(recipe, moved), 2L)
  restored_order <- preparation_reconnect_order(
    moved,
    edit_preparation_recipe(moved, "up", id = creator$id),
    context
  )
  expect_identical(restored_order, recipe)
})

test_that("filters moved before a derived variable use its source with the saved condition", {
  record <- derived_fixture()
  context <- m8_context(record)
  creator <- derived_step(context)
  filter <- new_preparation_step(
    "variable",
    list(
      variable = "MEDI_processed",
      mode = "categories",
      values = c("3", "9"),
      lower = 0,
      upper = 10,
      outside = FALSE,
      missing = "drop"
    ),
    context
  )
  recipe <- new_recipe(list(creator, filter))
  moved <- preparation_reconnect_order(
    recipe,
    edit_preparation_recipe(recipe, "up", id = filter$id),
    context
  )
  expect_identical(moved$steps[[1]]$parameters$arguments$variable, "MEDI")
  expect_identical(moved$steps[[1]]$parameters$arguments$values, c("3", "9"))
  expect_equal(
    nrow(dataset_prepared_data(m8_preview(record, moved)$candidate)),
    0
  )
  expect_length(preparation_chain_changes(recipe, moved), 1L)
})

test_that("renaming or retargeting a producer follows consumers and preserves separate branches", {
  record <- derived_fixture()
  context <- m8_context(record)
  raw <- dataset_raw_data(record)
  creator <- derived_step(context, offset = 0)
  consumer <- derived_step(
    context,
    NULL,
    "MEDI_processed",
    multiplier = 1,
    offset = 10
  )
  branch <- derived_step(
    context,
    "branch",
    "MEDI_processed",
    multiplier = 3,
    offset = 0
  )
  independent <- derived_step(
    context,
    "separate",
    "MEDI",
    multiplier = 4,
    offset = 0
  )
  recipe <- new_recipe(list(creator, consumer, branch, independent))
  renamed <- creator
  renamed$parameters$arguments$output <- "custom name"
  changed <- preparation_retarget_following(
    recipe,
    edit_preparation_recipe(recipe, "save", renamed),
    creator$id
  )
  expected <- raw
  expected[["custom name"]] <- raw$MEDI * 2 + 10
  expected$branch <- expected[["custom name"]] * 3
  expected$separate <- raw$MEDI * 4
  expect_identical(
    dataset_prepared_data(m8_preview(record, changed)$candidate),
    expected
  )
  changed <- preparation_reconnect_order(
    changed,
    edit_preparation_recipe(changed, "up", id = consumer$id),
    context
  )
  expect_equal(
    dataset_prepared_data(m8_preview(record, changed)$candidate)[[
      "custom name"
    ]],
    (raw$MEDI + 10) * 2
  )
  overwrite <- creator
  overwrite$parameters$arguments$output <- NULL
  overwrite$parameters$mapping_version <- 1L
  retargeted <- preparation_retarget_following(
    recipe,
    edit_preparation_recipe(recipe, "save", overwrite),
    creator$id
  )
  expect_identical(retargeted$steps[[2]]$parameters$arguments$variable, "MEDI")
  expect_equal(
    dataset_prepared_data(m8_preview(record, retargeted)$candidate)$MEDI,
    raw$MEDI * 2 + 10
  )
})

test_that("preview axes honor dashboard preferences and explicit bounds", {
  record <- derived_fixture()
  context <- m8_context(record)
  result <- m8_preview(record, new_recipe(list(derived_step(context))))
  record$analysis_settings$display_scale <- "linear"
  defaults <- preparation_axis_settings(record)
  expect_identical(defaults$scale, "linear")
  for (scale in c("linear", "symlog", "log")) {
    settings <- preparation_axis_settings(
      record,
      list(
        scale = scale,
        symlog_threshold = 0.1,
        y_limits = dashboard_y_axis_limits(0.1, 100, scale),
        y_scope = "shared",
        domain = c(0, 100),
        positive_domain = c(0.1, 100)
      )
    )
    plot <- preparation_preview_plot(result$comparison$view, context, settings)
    expect_identical(attr(plot, "llw_dashboard_scale"), scale)
    expect_equal(plot$coordinates$limits$y, c(0.1, 100))
    expect_equal(attr(plot, "llw_symlog_threshold"), 0.1)
    expect_no_error(ggplot2::ggplot_build(plot))
  }
  expect_error(
    preparation_axis_settings(record, list(scale = "log", y_limits = c(0, 1))),
    "greater than zero"
  )
  expect_identical(result$candidate$raw_payload, record$raw_payload)
})

test_that("module reorder retarget and Undo keep the preview and applied recipe distinct", {
  original <- derived_fixture()
  context <- m8_context(original)
  creator <- derived_step(context, multiplier = 2, offset = 0)
  consumer <- derived_step(
    context,
    NULL,
    "MEDI_processed",
    multiplier = 1,
    offset = 10
  )
  original_recipe <- new_recipe(list(creator, consumer))
  applied <- m8_preview(original, original_recipe)$candidate
  record <- shiny::reactiveVal(applied)
  axis <- shiny::reactiveVal(list(scale = "linear"))
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
      on_commit = m8_commit(record),
      plot_settings = shiny::reactive(axis())
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      next_context <- preparation_new_step_context(
        result$context(),
        result$recipe(),
        variable_catalog()
      )
      m8_set_form(session, next_context)
      expect_identical(form_base()$measurement_variable, "MEDI_processed")
      expect_identical(form_base()$measurement_output, "MEDI_processed")
      session$setInputs(step_order = c(consumer$id, creator$id))
      m8_set_form(
        session,
        preparation_new_step_context(
          result$context(),
          result$recipe(),
          variable_catalog()
        )
      )
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(
        dataset_prepared_data(result$preview()$candidate)$MEDI_processed,
        (dataset_raw_data(original)$MEDI + 10) * 2
      )
      expect_identical(record(), applied)
      expect_match(output$chain_changes$html, "now uses MEDI")
      before_axis <- result$preview()
      axis(list(scale = "symlog", symlog_threshold = 0.1, y_limits = c(0, 100)))
      session$flushReact()
      expect_match(output$plot_axis_label, "threshold 0.1")
      expect_identical(result$preview(), before_axis)
      session$setInputs(undo_edit = 1L)
      m8_set_form(
        session,
        preparation_new_step_context(
          result$context(),
          result$recipe(),
          variable_catalog()
        )
      )
      m8_wait(session, function() !is.null(result$preview()))
      expect_identical(result$recipe(), original_recipe)
      expect_identical(record(), applied)
      session$setInputs(selected_step = creator$id, edit = 1L)
      m8_set_form(
        session,
        result$context(),
        "calibrate",
        measurement_output = "MEDI_processed",
        calibration_multiplier = 2,
        calibration_offset = 0,
        calibration_reference = "Traceable coefficient"
      )
      expect_null(loading_form())
      session$setInputs(measurement_output = "renamed")
      session$setInputs(save_step = 1L)
      expect_null(editor_error())
      m8_wait(session, function() !is.null(result$preview()))
      expect_identical(
        result$recipe()$steps[[2]]$parameters$arguments$variable,
        "renamed"
      )
      expect_equal(
        dataset_prepared_data(result$preview()$candidate)$renamed,
        dataset_raw_data(original)$MEDI * 2 + 10
      )
      session$setInputs(new_step = 1L)
      expect_identical(form_base()$measurement_variable, "renamed")
      expect_identical(form_base()$measurement_output, "renamed")
      expect_identical(record(), applied)
    }
  )
})

test_that("reordered category editors retain saved values outside the new input catalog", {
  original <- derived_fixture()
  creator <- derived_step(m8_context(original))
  record <- shiny::reactiveVal(
    m8_preview(original, new_recipe(list(creator)))$candidate
  )
  runtime <- new_session_runtime(resolve_runtime_profile(
    "hosted",
    workers = 1L
  ))
  on.exit(runtime$cleanup(), add = TRUE)
  if (inherits(runtime$startup_error, "llw_error"))
    skip("mirai worker sockets are unavailable")
  recording_session <- shiny::MockShinySession$new()
  recording_session$userData$category_messages <- list()
  recording_session$sendInputMessage <- function(inputId, message) {
    if (endsWith(inputId, "variable_values") && !is.null(message$options))
      recording_session$userData$category_messages <- c(
        recording_session$userData$category_messages,
        list(message)
      )
    invisible()
  }
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    session = recording_session,
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      next_context <- preparation_new_step_context(
        result$context(),
        result$recipe(),
        variable_catalog()
      )
      m8_set_form(session, next_context)
      m8_set_form(
        session,
        next_context,
        "variable",
        variable_name = "MEDI_processed",
        variable_mode = "categories",
        variable_values = c("3", "9"),
        variable_missing = "drop"
      )
      session$setInputs(save_new = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      filter_id <- result$recipe()$steps[[2]]$id
      session$setInputs(selected_step = filter_id, up = 1L)
      message <- tail(recording_session$userData$category_messages, 1L)[[1]]
      expect_match(message$options, 'value="3"', fixed = TRUE)
      expect_match(message$options, 'value="9"', fixed = TRUE)
      expect_identical(message$value, c("3", "9"))
      m8_set_form(
        session,
        result$context(),
        "variable",
        variable_name = "MEDI",
        variable_mode = "categories",
        variable_values = c("3", "9"),
        variable_missing = "drop",
        variable_lower = 3,
        variable_upper = 9
      )
      expect_null(loading_form())
      expect_false(result$dirty())
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(nrow(dataset_prepared_data(result$preview()$candidate)), 0)
      expect_identical(
        result$recipe()$steps[[1]]$parameters$arguments$values,
        c("3", "9")
      )
      session$setInputs(save_step = 1L)
      expect_null(editor_error())
      m8_wait(session, function() !is.null(result$preview()))
    }
  )
})

test_that("editing later steps defaults other tools to the preceding derived variable", {
  original <- derived_fixture()
  source_context <- m8_context(original)
  creator <- derived_step(source_context)
  consumer <- derived_step(source_context, NULL, "MEDI_processed")
  duration <- new_preparation_step(
    "duration",
    list(length = 2, unit = "elapsed_days", from = "start", scope = "overall"),
    source_context
  )
  branch <- derived_step(source_context, "MEDI_later", "MEDI_processed")
  explicit_raw <- new_preparation_step(
    "coverage",
    list(variable = "MEDI", minimum = 0, basis = "observed_group"),
    source_context
  )
  applied <- m8_preview(
    original,
    new_recipe(list(creator, consumer, duration, branch, explicit_raw))
  )$candidate
  record <- shiny::reactiveVal(applied)
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
      m8_set_form(
        session,
        preparation_new_step_context(
          result$context(),
          result$recipe(),
          variable_catalog()
        )
      )
      session$setInputs(selected_step = duration$id, edit = 1L)
      expect_identical(form_base()$coverage_variable, "MEDI_processed")
      expect_identical(form_base()$variable_name, "MEDI_processed")
      expect_identical(form_base()$measurement_variable, "MEDI_processed")
      expect_identical(form_base()$measurement_output, "MEDI_processed")
      prefix_context <- preparation_new_step_context(
        result$context(),
        new_recipe(list(creator, consumer)),
        variable_catalog()
      )
      m8_set_form(
        session,
        prefix_context,
        "duration",
        duration_length = 2,
        duration_unit = "elapsed_days"
      )
      expect_null(loading_form())
      expect_false(result$dirty())
      session$setInputs(
        step_type = "coverage",
        coverage_basis = "observed_group",
        coverage_minimum = 0
      )
      expect_identical(form()$coverage_variable, "MEDI_processed")
      session$setInputs(save_step = 1L)
      expect_null(editor_error())
      m8_wait(session, function() !is.null(result$preview()))
      expect_identical(
        result$recipe()$steps[[3]]$parameters$arguments$variable,
        "MEDI_processed"
      )
      expect_identical(record(), applied)

      session$setInputs(selected_step = explicit_raw$id, edit = 2L)
      expect_identical(form_base()$coverage_variable, "MEDI")
      expect_identical(form_base()$measurement_variable, "MEDI_later")
      session$setInputs(selected_step = creator$id, edit = 3L)
      expect_identical(form_base()$coverage_variable, "MEDI")
      expect_identical(form_base()$measurement_variable, "MEDI")
      expect_identical(form_base()$measurement_output, "MEDI_processed")
    }
  )
})

test_that("saving a following step refreshes unused selectors without clearing the editor", {
  original <- derived_fixture()
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
        "calibrate",
        measurement_output = "MEDI_processed",
        calibration_multiplier = 2
      )
      session$setInputs(save_new = 1L)
      m8_set_form(
        session,
        preparation_new_step_context(
          result$context(),
          result$recipe(),
          variable_catalog()
        ),
        "calibrate",
        calibration_multiplier = 2
      )
      m8_wait(session, function() !is.null(result$preview()))
      creator <- result$recipe()$steps[[1]]
      session$setInputs(
        preparation_category = "filter",
        step_type = "duration",
        duration_length = 2,
        duration_unit = "elapsed_days"
      )
      session$setInputs(save_new = 2L)
      expect_identical(form_base()$coverage_variable, "MEDI_processed")
      expect_identical(form_base()$variable_name, "MEDI_processed")
      expect_identical(form_base()$measurement_variable, "MEDI_processed")
      expect_identical(form_base()$measurement_output, "MEDI_processed")
      expect_null(loading_form())
      expect_false(result$dirty())
      m8_wait(session, function() !is.null(result$preview()))
      session$setInputs(step_type = "coverage", coverage_minimum = 0)
      expect_identical(form()$coverage_variable, "MEDI_processed")
      session$setInputs(save_new = 3L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_identical(
        result$recipe()$steps[[3]]$parameters$arguments$variable,
        "MEDI_processed"
      )
      session$setInputs(coverage_variable = "MEDI", save_step = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_identical(form_base()$coverage_variable, "MEDI")
      expect_identical(
        result$recipe()$steps[[3]]$parameters$arguments$variable,
        "MEDI"
      )
      expect_identical(record(), original)
    }
  )
})

test_that("saving a creator advances measurement and coverage defaults to its output", {
  original <- derived_fixture()
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
      m8_set_form(session, result$context(), "invalidate")
      session$setInputs(save_new = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      session$setInputs(
        value_type = "variability",
        measurement_output = "MEDI_processed"
      )
      session$setInputs(save_new = 2L)
      expect_null(editor_error())
      expect_length(result$recipe()$steps, 2L)
      creator <- result$recipe()$steps[[2]]
      expect_identical(creator$parameters$arguments$variable, "MEDI")
      expect_identical(creator$parameters$arguments$source, "MEDI")
      expect_identical(creator$parameters$arguments$output, "MEDI_processed")
      expect_null(editing_id())
      expect_identical(output$editor_title, "Define next step")
      expect_identical(form_base()$measurement_variable, "MEDI_processed")
      expect_identical(form_base()$measurement_output, "MEDI_processed")
      expect_identical(form_base()$coverage_variable, "MEDI_processed")
      expect_identical(form_base()$variability_source, "MEDI_processed")
      next_context <- preparation_new_step_context(
        result$context(),
        result$recipe(),
        variable_catalog()
      )
      m8_set_form(session, next_context, "variability")
      expect_null(loading_form())
      expect_false(result$dirty())
      m8_wait(session, function() !is.null(result$preview()))
      expect_identical(record(), original)
      session$setInputs(preparation_category = "filter", step_type = "coverage")
      expect_identical(form()$coverage_variable, "MEDI_processed")
      session$setInputs(
        preparation_category = "values",
        value_type = "calibrate"
      )
      expect_identical(form()$measurement_variable, "MEDI_processed")
      expect_identical(form()$measurement_output, "MEDI_processed")
      session$setInputs(calibration_offset = 10, save_new = 3L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_length(result$recipe()$steps, 3L)
      expect_identical(
        result$recipe()$steps[[3]]$parameters$arguments$variable,
        "MEDI_processed"
      )
      expect_null(result$recipe()$steps[[3]]$parameters$arguments$output)
      session$setInputs(selected_step = creator$id, edit = 1L)
      expect_identical(form_base()$measurement_variable, "MEDI")
      expect_identical(form_base()$measurement_output, "MEDI_processed")
      expect_identical(form_base()$variability_source, "MEDI")
      m8_set_form(
        session,
        result$context(),
        "variability",
        measurement_output = "MEDI_processed"
      )
      expect_null(loading_form())
      expect_false(result$dirty())
      expect_identical(record(), original)
    }
  )
})
